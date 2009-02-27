module Tablet where

import Graphics.UI.GLUT

import Scene
import MyUtils

data TabletData v = TabletData 
	{ tablet_msg_corner 	:: v
	, tablet_top_border	:: v
	, tablet_side_border	:: v
	, tablet_corner		:: [v]		-- smooth curve
	, tablet_color		:: Color3 Float
	, tablet_texture	:: MyTexture
	}
	deriving (Show)

mkTabletData :: (Enum a,Floating a, Scaleable a a) => a -> a -> a -> a -> String -> TabletData (Vertex3 a)
mkTabletData width height depth border txt = TabletData
	{ tablet_msg_corner  = corner
	, tablet_top_border  = top
	, tablet_side_border = side
	, tablet_corner      = map (\ (Vertex3 x y _z) -> Vertex3 x y depth) $ fan corner side top 3
	, tablet_color	     = Color3 0.5 0.7 0.5
	, tablet_texture     = TextTexture txt
	}
  where
	corner	= Vertex3 width height depth
	top     = Vertex3 width (height + border) depth
	side	= Vertex3 (width + border) height depth

splatTablet :: TabletData (Vertex3 Double) -> Scene
splatTablet tablet = foldr1 Join $ concat
     [  [ Splat $ Surface msg norm $ h $
			(QUAD (g (corner))
			      (g (reflectX corner))
			      (g (reflectX (reflectY corner)))
			      (g (reflectY corner)))
	] ++ concat
	[ [ Splat $ Surface col norm $ h'
		       $ QUAD (f (top_border))
			      (f (reflectX top_border))
			      (f (reflectX corner))
			      (f (corner))
	  ]
	  | (f,h') <- zip [g,g . reflectY] [h,flipSurfPoints . h]
	] ++ concat
	[ [ Splat $ Surface col norm $ h'
		       $ QUAD (f (side_border))
			      (f (corner))
			      (f (reflectY corner))
			      (f (reflectY side_border))
	  ]
	  | (f,h') <- zip [g,g . reflectX] [h,flipSurfPoints . h]
	]  ++ 
	 [ Splat $ Surface col norm $ h'
		     $ TRIANGLE_FAN (f corner)
				    (f side_border)
				    (f top_border)
				    (map f (tablet_corner tablet))
	 | (f,h') <- [ (g,h) ,
		  (g . reflectX,flipSurfPoints . h) ,
		  (g . reflectY,flipSurfPoints . h),
		  (g . reflectX . reflectY,h) 
	        ]
	 ]
      |  (msg,g,h,norm) <- [ (SurfTexture (tablet_color tablet) (tablet_texture tablet) 1,id,id,Normal3 0 0 1)
		           , (col,reflectZ,flipSurfPoints,Normal3 0 0 (-1))
		      ] 
      ] ++
	[  Splat $ SurfaceN band $
		     QUAD v1 (reflectZ v1) (reflectZ v2)  v2
        | (v1,v2) <- zip edges (tail edges ++ [last edges])
        ]
	
  where 
	col  = SurfColor (tablet_color tablet)
	band = SurfColor (Color3 0.1 0.2 0.1)
	corner = tablet_msg_corner tablet
	top_border = tablet_top_border tablet
	side_border = tablet_side_border tablet 
	top_normal  = Normal3 0 1 0
	side_normal  = Normal3 1 0 0
	one_edge    = [(Normal3 1 0 0,side_border)] ++ map addNormal (tablet_corner tablet) ++ [(Normal3 0 1 0,top_border)]
--	one_edge    = [side_border] ++ tablet_corner tablet ++ [top_border]
	edges	    = one_edge 
	           ++ reverse (map reflectX one_edge)
		   ++ map (reflectX . reflectY) one_edge
   	           ++ reverse (map reflectY one_edge)
		   ++ [ head edges ]

	addNormal v = (case (v - corner) of
			 Vertex3 x y _z -> Normal3 x y 0,v)
