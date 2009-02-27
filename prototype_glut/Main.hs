module Main where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL ( Color4 )
import System.Exit ( exitWith, ExitCode(..) )
import Data.IORef ( IORef, newIORef )
import System.IO
import System.Process
import Util
import Data.Array 
import Control.Monad
import Data.Char ( ord )
import Data.Word ( Word8, Word16 )
import Data.Bits ( testBit )
import System.Random
import Foreign ( Ptr, allocaBytes, pokeElemOff, peekElemOff, peek, castPtr,
                 Storable, mallocBytes, free, copyBytes )
import System.IO.Unsafe ( unsafePerformIO )

import Scene
import Tablet

tincrement, qincrement :: GLfloat
tincrement = 0.2
qincrement = -0.15

initGL :: IO ()
initGL = do
  textureFunction $= Modulate -- ReverseBlend -- Modulate -- Blend -- LessEqual -- Decal -- or Replace?
  texture Texture2D $= Enabled

{-
-}
  blend $= Enabled
  blendFunc $= (SrcAlpha, OneMinusSrcAlpha)

--  pointSmooth $= Enabled
-- hint PointSmooth $=Nicest

--  lineSmooth $= Enabled
--  hint LineSmooth $=Nicest

 -- polygonSmooth $= Enabled
 -- hint PolygonSmooth $=Nicest

  clearColor $= Color4 0 0 0 0.5 -- Clear the background color to black
  clearDepth $= 1 -- enables clearing of the depth buffer
  depthFunc  $= Just Less -- type of depth test
  shadeModel $= Smooth -- enables smooth color shading
--  polygonMode $= (Line,Line)
  polygonMode $= (Fill,Line)
  Size width height <- get windowSize
  resizeScene (Size width height)

resizeScene :: Size -> IO ()
resizeScene (Size w 0) = resizeScene (Size w 1) -- prevent divide by zero
resizeScene s@(Size width height) = do
  viewport   $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  perspective 75 (fromIntegral width/fromIntegral height) 0.1 100
  matrixMode $= Modelview 0
  flush

data SceneState = SceneState { theDrawState :: DrawState
			     , theTime      :: Double
			     , theRot       :: Double
			     , theWobbles   :: [((Double,Double),(Double,Double))]
			     }
		

drawTheScene :: IORef SceneState -> IO ()
drawTheScene sceneState = do
  SceneState
    { theDrawState = drawState,
      theTime      = tm,
      theRot       = rot,
      theWobbles   = wobbles } <- get sceneState

  loadIdentity
  normalize $= Enabled 

  clear [ColorBuffer, DepthBuffer] -- clear the screen and the depth bufer
  texture Texture2D $= Enabled
  lighting $= Enabled

-- does this go here?
--  lineSmooth $= Enabled
--  hint LineSmooth $=Nicest

  position (Light 0) $= Vertex4 1 1 0 1 
  light (Light 0) $= Enabled 

--  lightModel undefined

  ambient (Light 0)  $= Color4 0.1 0.1 0.1 1	--
  diffuse (Light 0)  $= Color4 0.9 0.9 0.9 1	--
--  specular (Light 0) $= Color4 0.5 0.5 0.5 1

  shadeModel $= Smooth

--  materialEmission FrontAndBack $= Color4 0.1 0.1 0.1 1
--  materialSpecular FrontAndBack $= Color4 1 1 1 1
--  materialDiffuse FrontAndBack  $= Color4 0.5 0.5 0.5 1
--  materialDiffuse FrontAndBack  $= Color4 0.5 0.5 0.5 1
   
  colorMaterial $= Just (FrontAndBack,AmbientAndDiffuse)
--  colorMaterial $= Just (FrontAndBack,Specular)


  textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)

--  tm <- get time
--  rt <- get rot

  let tablet  = mkTabletData 2 1 0.1 0.2 "This is\na Tablet"

--  rt <- get rtri

--  let rt = 0
  loadIdentity

  let ink_layer    = 0.01
  let shadow_layer = 0.1
  let tab_layer    = 1.0

  let paper = Splat $ Surface (SurfColor (Color3 128 128 182)) (Normal3 0 0 1) $
				QUAD (Vertex3 2 1 0)
				     (Vertex3 (-2) 1 0)
				     (Vertex3 (-2) (-1) 0)
				     (Vertex3 2 (-1) 0)

  let lineCount = 30
  let ink = Splat $ SLines (Color3 0 0 255)
			  ([ (Vertex3 (-2) x ink_layer,Vertex3 2 x ink_layer)
			   | x <- map (/fromIntegral (succ lineCount)) (map fromIntegral [-lineCount .. lineCount ])
		           ] ++
			   [ (Vertex3 x (-1) ink_layer,Vertex3 x 1 ink_layer)
			   | x <- map (/fromIntegral (succ lineCount)) (map fromIntegral [-(lineCount*2) .. (lineCount*2) ])
		           ])


--  st <- get drawSt
  let fid = fromRational . toRational

  let placeTablet x y = joinScene
             [ Transform (Translate (Vector3 x y tab_layer))
	     $ splatTablet tablet
 	     , Transform (Translate (Vector3 x y shadow_layer))
	     $ Transform (Scale 1.1 1.1 0.0001)
	     $ mapScene shadowSurface
	     $ splatTablet tablet
	     ]
  

  let offsets =  [(xo,yo)| xo <- [-3..3], yo <- [-3..3]]

  stdGen <- newStdGen
  let randoms :: [Double]
      randoms = randomRs (-0.01,0.01) stdGen
 
      randoms' = take (length offsets) [ (x,y) | (x,y) <- zip randoms (drop (length offsets) randoms) ]

  let wobbles2 = [ let xd' = max (min (xd + wx) 0.1) (-0.1)
		       xd'' = xd' * 0.95
		       yd' = max (min (yd + wy) 0.1) (-0.1)
		       yd'' = yd' * 0.95
		   in ((x+xd'',y+yd''),(xd'',yd''))
	         | (((x,y),(xd,yd)),(wx,wy)) <- zip wobbles randoms'
	         ]
  
--  print wobbles2

  drawState' <- drawScene drawState
		      $ Transform (Translate (Vector3 0 0 (-25.0)))
		      $ Transform (Rotate (fid rot) (Vector3 1 0 0))
 		      $ joinScene $
	     [ id
	     $ Transform (Scale 21 21 1)
	     $  Join paper ink
	     ] ++ 
	     [ placeTablet (fromIntegral (x * 5) + x') (fromIntegral (y * 3) + y')
	     | ((x,y),((x',y'),_)) <- zip offsets wobbles2
	     ]

  sceneState $= SceneState
    { theDrawState = drawState',
      theTime      = tm + 1,
      theRot       = rot,
      theWobbles   = wobbles2 } 

  -- since this is double buffered, swap the buffers to display what was just
  -- drawn
  swapBuffers

  addTimerCallback 20 $ postRedisplay Nothing

keyPressed :: IORef SceneState -> KeyboardMouseCallback
keyPressed sceneState key Down _ _ = do
    state <- get sceneState
    state' <-
       case key of
        (Char '\27') -> exitWith ExitSuccess
        (Char 'q')   -> return $ state { theRot = theRot state + 2 }
        (Char 'a')   -> return $ state { theRot = theRot state - 2 }
	_            -> return $ state
    sceneState $= state'
    return ()
keyPressed sceneState key _ _ _ = return ()
{-
    { theDrawState = drawState,
      theTime      = tm,
      theRot       = rot,
      theWobbles   = wobbles } 


-- 27 is ESCAPE

(Char '\27') Down _ _ = exitWith ExitSuccess

keyPressed time rot (Char '\27') Down _ _ = exitWith ExitSuccess
keyPressed time rot (Char 'f')   Down _ _ = do tm <- get time
					       time $= tm + 0.2
					       return ()
keyPressed time rot (Char 'F')   Down _ _ = do tm <- get time
					       time $= tm + 5
					       return ()
keyPressed time rot (Char 'b')   Down _ _ = do tm <- get time
					       time $= tm - 0.2
					       return ()
keyPressed time rot (Char 'B')   Down _ _ = do tm <- get time
					       time $= tm - 5
					       return ()
keyPressed time rot (Char 'q')   Down _ _ = do rt <- get rot
					       rot $= rt - 2
					       return ()
keyPressed time rot (Char 'a')   Down _ _ = do rt <- get rot
					       rot $= rt + 2
					       return ()
keyPressed time rot _            _    _ _ = return ()
-}


main :: IO ()
main = do
     putStrLn "press (f or F) (b or B)"

     -- Initialize GLUT state - glut will take any command line arguments
     -- that pertain to it or X windows -- look at its documentation at
     -- http://reality.sgi.com/mjk/spec3/spec3.html

     getArgsAndInitialize 

     -- select type of display mode:
     -- Double buffer
     -- RGBA color
     -- Alpha components supported
     -- Depth buffer
     initialDisplayMode $= [ DoubleBuffered, RGBAMode, WithDepthBuffer, 
                              WithAlphaComponent,  Multisampling ]
{-

     initialDisplayCapabilities $= [ 
		With DisplayDouble,
		With DisplayRGBA,
		With DisplayDepth,
		With DisplayAlpha,
		With DisplaySamples ]
-}
--		Where DisplaySlow IsEqualTo 1 ]

{-	
--			] -- With DisplayConformant ]

-}
     -- get a 640 x 480 window
     initialWindowSize $= Size 800 600
     -- window starts at upper left corner of the screen
     initialWindowPosition $= Position 0 0

{-
     fullScreen
     enterGameMode
-}
     -- open a window
     createWindow "FieldTrip Demo (1)"
--     hello


     multisample $= Enabled
--     sampleAlphaToCoverage $= Enabled 
--     sampleBuffers $= 1
     x <- get sampleBuffers
     print x

--     hint PointSmooth $= Nicest

--     exts <- get glExtensions
--     putStrLn $ unlines exts

     -- register the function to do all our OpenGL drawing
     rt <- newIORef 0
     rq <- newIORef 0
     time <- newIORef 0.0
     rot <- newIORef (-20.0)
     drawSt <- newIORef initState

     -- 
     sceneState <- newIORef $ SceneState
	    { theDrawState = initState,
	      theTime      = 0.0,
	      theRot       = (-20.0),
	      theWobbles   = repeat ((0,0),(0,0))
	    } 

     initGL

     displayCallback $= (drawTheScene sceneState)

     -- go fullscreen. This is as soon as possible.
     -- even if there are no events, redraw our gl scene
--     idleCallback $= Just (drawTheScene sceneState)
     -- register the funciton called when our window is resized
     reshapeCallback $= Just resizeScene
     -- register the function called when the keyboard is pressed.
     keyboardMouseCallback $= Just (keyPressed sceneState)
     -- initialize our window.

--     print (scene 10)
     -- start event processing engine
     mainLoop



--foreign import ccall "hello" hello :: IO ()
