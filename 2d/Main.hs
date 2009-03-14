{-# INCLUDE <hack.c> #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL ( Color4 )
import System.Exit ( exitWith, ExitCode(..) )
import Data.IORef -- ( IORef, newIORef )
import System.IO
import System.Process
import Data.Array 
import Control.Monad
import Data.Char ( ord )
import Data.Word ( Word8, Word16 )
import Data.Bits ( testBit )
import System.Random
import Foreign ( Ptr, allocaBytes, pokeElemOff, peekElemOff, peek, castPtr,
                 Storable, mallocBytes, free, copyBytes )
import System.IO.Unsafe ( unsafePerformIO )
import Data.Time.Clock
import Data.Word

import qualified Graphics.Chalkboard as CB

import Graphics.Rendering.FTGL

main :: IO ()
main = do

     getArgsAndInitialize 

     initialDisplayMode $= [ DoubleBuffered , Multisampling ]

     initialWindowSize $= Size 800 600

     -- window starts at upper left corner of the screen
     initialWindowPosition $= Position 0 0

     createWindow "Small 2D"

     polygonSmooth $= Enabled
     hint PolygonSmooth $= Nicest

     multisample $= Enabled

     initGL

     tm <- getCurrentTime
     [texName] <- genObjectNames 1


     font <- createTextureFont "/Library/Fonts/Courier New.ttf"
--     font <- createBufferFont "/Library/Fonts/Courier New.ttf"
--     font <- createBitmapFont "/Library/Fonts/Courier New.ttf"
--     font <- createPixmapFont "/Library/Fonts/Courier New.ttf"
	
     setFontFaceSize font 72 72

     v <- newIORef (0 :: Int,tm,texName,font)


     displayCallback $= (drawTheScene v)

--     idleCallback $= Just (drawTheScene v)

     reshapeCallback $= Just resizeScene

     let anim = addTimerCallback 10 $ do
			postRedisplay Nothing
			anim
     anim

--     keyboardMouseCallback $= Just (keyPressed sceneState)

     mainLoop

initGL :: IO ()
initGL = do
  textureFunction $= Replace -- Modulate -- ReverseBlend -- Modulate -- Blend -- LessEqual -- Decal -- or Replace?
  texture Texture2D $= Enabled

--  blend $= Enabled
--  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

--  pointSmooth $= Enabled
-- hint PointSmooth $=Nicest

--  lineSmooth $= Enabled
--  hint LineSmooth $=Nicest

 -- polygonSmooth $= Enabled
 -- hint PolygonSmooth $=Nicest
{-
const XSize = 640, YSize = 480
glMatrixMode (GL_PROJECTION)
glLoadIdentity ()
glOrtho (0, XSize, YSize, 0, 0, 1)
glDisable(GL_DEPTH_TEST)
glMatrixMode (GL_MODELVIEW)
glLoadIdentity()
' Displacement trick for exact pixelization
glTranslatef(0.375, 0.375, 0)
-}

  polygonMode $= (Fill,Fill)

  clearColor $= Color4 0 0 0 0 -- Clear the background color to black
--  clearDepth $= 1 -- enables clearing of the depth buffer
--  depthFunc  $= Just Less -- type of depth test
--  shadeModel $= Smooth -- enables smooth color shading
--  polygonMode $= (Line,Line)
--  polygonMode $= (Fill,Line)
  Size width height <- get windowSize
  resizeScene (Size width height)

-- changes state, hmm.
resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  let sz = 1
  viewport   $= (Position 0 0, s)	-- the whole screen
  matrixMode $= Projection
  loadIdentity
  let w = fromIntegral width / sz
      h = fromIntegral height
  ortho2D (-w) w (-h) (h) --  (fromIntegral width / sz) 0 (fromIntegral height / sz)
  depthFunc $= Nothing
  matrixMode $= Modelview 0
  loadIdentity
-- translate (Vector3 (0.375 :: Float) 0.375 0)
--  rotate (Vector3 0 0 (0::Float))

  flush
--  postRedisplay Nothing
  -- request redraw??

drawTheScene v = do
  Size width height <- get windowSize
  let sz = 3
  (n,tm,texName,font) <- readIORef v
  tm' <- getCurrentTime
  if (n `mod` 100 == 0) then do
      putStrLn $ show ((1 / (diffUTCTime tm' tm)) * 100) ++ " fps (" ++ show n ++ ")"
      writeIORef v (succ n,tm',texName,font)
    else  writeIORef v (succ n,tm,texName,font)

  loadIdentity
  rotate ((fromIntegral n / 20)::Float) (Vector3 0 0 1)

  clearColor $= Color4 0.3 0.3 0.3 0 -- Clear the background color to black
  clear [ColorBuffer] -- clear the scree

  pointSize $= realToFrac sz 

  let toW8 n | n > 1 = 255
	     | n < 0 = 0
	     | otherwise = round (n * 255)


  let full = 512
  let sc   = 8
  let the_scale = fromIntegral (min width height)
  let half_scale = fromIntegral the_scale
  let arr = CB.boardToArray (full `div` sc - 1,full `div` sc - 1) 3 $ 
		        CB.scale (fromIntegral (full `div` sc)) $
			CB.move (0.5,0.5) $
			fmap (\ (CB.RGB r g b) -> Color3 (toW8 r) (toW8 g) (toW8 b)) $ (exampleBoard n) -- (fromIntegral n / 20))


  to <- buildTexture texName arr
{-
                         [ ((x,y),Color3 (fromIntegral (x * 3 + if odd x then 50 else 0)) (fromIntegral (y * 4)) (fromIntegral n))
                          | x <- [0..63]
                          , y <- [0..63]
                          ]
-}

  let wh = fromIntegral (max width height) / 1.5


  renderPrimitive Quads $ do
            texCoord (TexCoord2 0 (0 :: Float))
            vertex (Vertex2 (-wh) (-wh :: Float))
            texCoord (TexCoord2 0 (1 :: Float))
            vertex (Vertex2 (-wh) (wh :: Float))
            texCoord (TexCoord2 1 (1 :: Float))
            vertex (Vertex2 wh (wh :: Float))
            texCoord (TexCoord2 1 (0 :: Float))
            vertex (Vertex2 wh (-wh :: Float))
{0
  setFontFaceSize font 7 10

  sequence [ do
  	color (Color3 1 0 (0::Float))
  	renderFont font "Hello world!" All
  	rotate (8.5 ::Float) (Vector3 0 0 1)
  	translate (Vector3 1 1 (0:: Float))
	scale 1.01 1  (0:: Float)
	| n <- [1..40]
	]
-}
{-
  sequence_ [ do
    renderPrimitive Points $ do
            color (Color3 (fromIntegral ((x + n) `mod` 800) / 800) (y / 600) (((fromIntegral (n `mod` 100)) / 100) :: Float))
            vertex (Vertex2 (fromIntegral x) (y :: Float))
    | x <- take (800 `div` sz) [0,1..799]
    , y <- take (600 `div` sz) [0,1..599]
    ] 
-}

  return ()

  swapBuffers

--glClearColor(.3, .3, .3, 0)
--glClear(GL_COLOR_BUFFER_BIT)


------------------------------------------------------------------------------


-- The x direction needs to be a power of 2.
buildTexture :: TextureObject -> Array (Int,Int) (Color3 Word8) -> IO ()
buildTexture texName arr = do
  let (width,height) = case bounds arr of
 			 ((0,0),(w,h)) -> (w+1,h+1)
  p <- mallocBytes (width * height * 3)	-- choice
  sequence_ [ case arr ! (w,h) of
	       (Color3 r g b) -> do pokeElemOff p (off+0) r
				    pokeElemOff p (off+1) g
				    pokeElemOff p (off+2) b
	    | (off,(w,h)) <- zip [0,3 ..] [ (w,h) | h <- [ 0 .. height - 1 ], w <- [ 0 .. width - 1 ], True]
	    ]

--  print $ last $  zip [0,3 ..] [ (w,h) | h <- [ 0 .. height - 1 ], w <- [ 0 .. width - 1 ]]
--  print $	(width * height * 3)	-- choice
  textureBinding Texture2D $= Just texName
  textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)

  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)

  let p' = image

--  let pd = PixelData RGBA UnsignedByte p'
--  texImage2D Nothing NoProxy 0 RGB' (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 pd
  let pd = PixelData RGBA UnsignedByte p'
  texImage2D Nothing NoProxy 0 RGB' (TextureSize2D 16 16) 0 pd
  free p
  return ()
{-

  (Image (Size w h) pd) <- bitmapLoad "Data/k9.bmp"

-}
exampleBoard :: Int -> CB.Board CB.RGB
exampleBoard n = fmap (CB.choose CB.green CB.white) $ CB.rotate 0 $ CB.scale (0.1 + 0.001 * fromIntegral 1) $ CB.checker

instance CB.Average Word8 where
  average xs = fromIntegral ((sum (map fromIntegral xs) `div` fromIntegral (length xs)):: Int)


instance CB.Average c => CB.Average (Color3 c) where
  average cs = Color3 (CB.average reds) (CB.average greens) (CB.average blues)
     where
        reds   = [ r | Color3 r _ _ <- cs ]
        greens = [ g | Color3 _ g _ <- cs ]
        blues  = [ b | Color3 _ _ b <- cs ]

foreign import ccall "&" image :: Ptr Word8