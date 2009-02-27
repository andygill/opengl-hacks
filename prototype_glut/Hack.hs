module Hack where

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
import Foreign ( Ptr, allocaBytes, pokeElemOff, peekElemOff, peek, castPtr,
                 Storable, mallocBytes, free, copyBytes )
import System.IO.Unsafe ( unsafePerformIO )


loadGLTextures :: Color3 Word8 -> Color3 Word8 -> String -> IO TextureObject
loadGLTextures paper ink msg = do
  (stdin,stdout,stderr, handle) <- runInteractiveCommand $ "pbmtext " 

  hPutStrLn stdin "1234567890"
  hPutStrLn stdin "The quick brown fox"
  hPutStrLn stdin "jumped over the lazy dog,"
  hPutStrLn stdin "again, and again"
  hClose stdin
  str <- hGetContents stderr
--  print str
  _ <- hGetLine stdout
  szs <- hGetLine stdout
--  print (map show (lines szs))
  let [width,height] = map read (words szs)
--  print width
--  print height   
  str <- hGetContents stdout
--  print str

--  print (length str,width * height)
--  print str
--  print 
--			      [ (w,h) | h <- [0 .. (height - 1)] 
--				      , w <- [0 , 8 .. (width - 1)]
--			      ]
  let po8 x = if (x `mod` 8) == 7 then x else po8 (succ x)

  let extra_width = po8 (width - 1) 
  let extra_height = po8 (height - 1) 

  let arr_lsts = 
	       [ ((b + w,h),col)
	       | (c,(w,h)) <- zip str
			      [ (w,h) | h <- [0 .. (height - 1)] 
				      , w <- [0 , 8 .. (width - 1)]
			      ]
	       , let i = (fromIntegral (ord c) :: Word8)
	       , b <- [0..7]
	       , b + w < width
	       , let col = if testBit i (7 - b) then ink else paper
	       ] ++
	       [ ((w,h),paper) | w <- [ width .. extra_width], h <- [0.. height - 1] ] ++
	       [ ((w,h),paper) | w <- [ 0 .. width - 1 ], h <- [height .. extra_height ]] ++
	       [ ((w,h),paper) | w <- [ width .. extra_width], h <- [height .. extra_height ]]

--  print extra_width  

  let arr = array ((0,0),(extra_width,extra_height)) arr_lsts

--  print (extra_width,extra_height)
{-
  putStrLn $ unlines [ [ case arr ! (w,h) of 
			    Color3 x _ _ | x < 128 -> ' '
			    _ 		  	   -> '#'
		       |  w <- [0 .. extra_width - 1] ]
		     | h <- [0 .. extra_height - 1]
		     ]
-}
  buildTexture arr

-- mod 8 sized edges
buildTexture :: Array (Int,Int) (Color3 Word8) -> IO TextureObject
buildTexture arr = do
  let (width,height) = case bounds arr of
 			 ((0,0),(w,h)) -> (w+1,h+1)
  texName <- liftM (head . drop 1) (genObjectNames 2)
  p <- mallocBytes (width * height * 3)	-- choice
  sequence_ [ case arr ! (w,h) of
	       (Color3 r g b) -> do pokeElemOff p (off+0) r
				    pokeElemOff p (off+1) g
				    pokeElemOff p (off+2) b
	    | (off,(w,h)) <- zip [0,3 ..] [ (w,h) | h <- [ 0 .. height - 1 ], w <- [ 0 .. width - 1 ]]
	    ]

--  print $ last $  zip [0,3 ..] [ (w,h) | h <- [ 0 .. height - 1 ], w <- [ 0 .. width - 1 ]]
--  print $	(width * height * 3)	-- choice
  textureBinding Texture2D $= Just texName
  textureFilter  Texture2D $= ((Nearest, Nothing), Nearest)

  textureWrapMode Texture2D S $= (Repeated, Repeat)
  textureWrapMode Texture2D T $= (Repeated, Repeat)

  let pd = PixelData RGB UnsignedByte p
  texImage2D Nothing NoProxy 0 RGB' (TextureSize2D (fromIntegral width) (fromIntegral height)) 0 pd
  return texName
{-

  (Image (Size w h) pd) <- bitmapLoad "Data/k9.bmp"

-}

{-# NOINLINE tex #-}				  	
tex = unsafePerformIO $ loadGLTextures (Color3 255 255 255) (Color3 0 0 0) "Hello\nWorld"

graphPaper = unsafePerformIO $ buildTexture graphPaperArray

graphPaperArray = array ((0,0),(15,15)) [
		( (x,y)
		, if x == 0 || y == 0 
		  then Color3 0 0 192
		  else Color3 240 240 255 )
		| x <- [0..15]
		, y <- [0..15]
		]


