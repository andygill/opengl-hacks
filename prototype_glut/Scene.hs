module Scene where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL 

import Hack	-- for now

-- Make this an ADT later

data Trans = Translate      (Vector3 Double)
	   | Rotate Float (Vector3 Float)
	   | Scale  Double  Double Double
	deriving (Show,Eq)

data Scene = Transform  Trans Scene
	   | Join      Scene Scene 
	   | Splat     Surface
	deriving Show

initState = DrawState { trans = [], cxtTrans = [], textures = [] }

drawScene :: DrawState -> Scene -> IO DrawState
drawScene state scene = 
		  do loadIdentity
		     drawScene' scene (state { trans = [], cxtTrans = [] })

data DrawState = DrawState { trans    :: [Trans]	-- what do you want to do, on the *result*
			   , cxtTrans :: [Trans]	-- what matrix operations *have* been done?
			   , textures :: [(MyTexture,TextureObject)]
			   }

drawScene' :: Scene -> DrawState -> IO DrawState
drawScene' (Transform t scene) state = drawScene' scene (state { trans = trans state ++ [t] })
drawScene' (Join s1 s2) state    = do
   state' <- drawScene' s1 state
   drawScene' s2 (state' { trans = trans state })	-- run 2nd in trans context of orginal 
drawScene' (Splat polygon) state = do
   -- first set up the matrixes
   if cxtTrans state /= trans state 	-- if need to reset state matrix
     then do loadIdentity
	     sequence_ [ case t of
			   Translate vec -> translate vec
			   Rotate r vec  -> rotate r vec
			   Scale  x y z  -> scale x y z
		       | t <- trans state
		       ]
     else return ()
   let state' = state { cxtTrans = trans state }
   -- now draw the polygon
   drawSurface polygon state'

------------------------------------------------------------------------------

data Surface = Surface  SurfCommon (Normal3 Double) (SurfPoints (Vertex3 Double))
	     | SurfaceN SurfCommon                  (SurfPoints (Normal3 Double,Vertex3 Double))
	     | SLines    (Color3 Float) 	    [(Vertex3 Double,Vertex3 Double)]	-- should these be individual surfaces?
	deriving Show

data SurfCommon = SurfColor (Color3 Float)
	        | SurfTexture (Color3 Float) MyTexture GLfloat
		| SurfShadow (Color4 Float)
	deriving Show

data MyTexture = TextTexture String	
	deriving (Show,Eq)

data SurfPoints v = QUAD v v v v
		  | TRIANGLE_FAN v v v [v]
	deriving Show


corners = [ TexCoord2 1 (0::GLfloat)
	  , TexCoord2 0 (0::GLfloat)
	  , TexCoord2 0 (1::GLfloat)
	  , TexCoord2 1 (1::GLfloat)
	  ]

drawSurface :: Surface -> DrawState -> IO DrawState
drawSurface (Surface common norm points) state = do
	-- The common part *can* change the state
	state' <- drawCommon common state
	-- These points can not change the state (except the color, which is not in *our* state)
	let vertex' (v,coord) =
		case common of
	         (SurfTexture _ _ scale) -> do texCoord (case coord of
							  TexCoord2 x y -> TexCoord2 (x*scale) (y*scale))
				               vertex v
	         _               -> vertex v
 	case points of
	  QUAD v1 v2 v3 v4 -> 
		renderPrimitive Quads (normal norm >> mapM_ vertex' (zip [v1,v2,v3,v4] corners))
	  TRIANGLE_FAN v1 v2 v3 vs -> 
		renderPrimitive TriangleFan (normal norm >> (mapM_ vertex $ [v1,v2] ++ vs ++ [v3]))
	-- and return the modified state
	return state'
drawSurface (SurfaceN common points') state = do
	-- The common part *can* change the state
	state' <- drawCommon common state
	-- These points can not change the state (except the color, which is not in *our* state)
 	case points' of
	  QUAD v1 v2 v3 v4 -> 
		renderPrimitive Quads (mapM_ vertex' [v1,v2,v3,v4])
	  TRIANGLE_FAN v1 v2 v3 vs -> 
		renderPrimitive TriangleFan (mapM_ vertex' $ [v1,v2] ++ vs ++ [v3])
	-- and return the modified state
	return state'
  where
	vertex' (norm,vec) = normal norm >> vertex vec
drawSurface (SLines col points) state = do
	-- The common part *can* change the state
	state' <- drawCommon (SurfColor col) state
	renderPrimitive Lines (mapM_ vertex $ concat [ [x,y] | (x,y) <- points ])
	return state'

drawCommon :: SurfCommon -> DrawState -> IO DrawState
drawCommon (SurfColor col) state = do
	color col
	textureBinding Texture2D $= Nothing 	-- hack for now, use state to do the correct thing
	return state
drawCommon (SurfTexture col tex _) state = do
	(state',tex') <- findTex tex state
	textureBinding Texture2D $= Just tex'
	color col
	return state'				-- store binding in state, please
drawCommon (SurfShadow col) state =  do
	color col
	textureBinding Texture2D $= Nothing 	-- hack for now, use state to do the correct thing
	return state

------------------------------------------------------------------------------

flipSurfPoints (QUAD v1 v2 v3 v4)         = QUAD v1 v4 v3 v2
flipSurfPoints (TRIANGLE_FAN v1 v2 v3 vs) = TRIANGLE_FAN v1 v3 v2 (reverse vs)


------------------------------------------------------------------------------

findTex :: MyTexture -> DrawState -> IO (DrawState,TextureObject)
findTex tex@(TextTexture str) state = 
	case lookup tex (textures state) of
          Nothing -> do print "loading..."
			tex' <- loadGLTextures (Color3 255 255 255) (Color3 0 0 0) str
			return (state { textures = (tex,tex') : textures state },tex')
	  Just tex' -> return (state,tex')

joinScene :: [Scene] -> Scene
joinScene = foldl1 Join


------------------------------------------------------------------------------

-- shadow color
shadowColor = Color4 0 0 0 0.5

shadowSurfCommon = SurfShadow shadowColor

shadowSurface :: Surface -> Surface
shadowSurface (Surface  comm norm points) = Surface  shadowSurfCommon norm points
shadowSurface (SurfaceN comm     points') = SurfaceN shadowSurfCommon points'
shadowSurface (SLines   col     lines)    = SLines   col     lines	-- wrong!!

mapScene :: (Surface -> Surface) -> Scene -> Scene
mapScene f (Transform trans scene) = Transform trans (mapScene f scene)
mapScene f (Join scene1 scene2)    = Join (mapScene f scene1) (mapScene f scene2)
mapScene f (Splat scene)           = Splat (f scene)

