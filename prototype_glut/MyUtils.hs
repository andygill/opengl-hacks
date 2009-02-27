module MyUtils where

import Graphics.UI.GLUT

-- These might be elsewhere, check!
class Reflectable a where
   reflectX :: a -> a
   reflectY :: a -> a
   reflectZ :: a -> a

instance (Reflectable a,Reflectable b) => Reflectable (a,b) where
  reflectX (a,b) = (reflectX a,reflectX b)
  reflectY (a,b) = (reflectY a,reflectY b)
  reflectZ (a,b) = (reflectZ a,reflectZ b)

instance (Num a) => Reflectable (Vertex3 a) where
  reflectX (Vertex3 x y z) = Vertex3 (-x) y z
  reflectY (Vertex3 x y z) = Vertex3 x (-y) z
  reflectZ (Vertex3 x y z) = Vertex3 x y (-z)

instance (Num a) => Reflectable (Normal3 a) where
  reflectX (Normal3 x y z) = Normal3 (-x) y z
  reflectY (Normal3 x y z) = Normal3 x (-y) z
  reflectZ (Normal3 x y z) = Normal3 x y (-z)

-- done not really make sense
instance Num a => Num (Vertex3 a) where
  (Vertex3 x y z) + (Vertex3 x' y' z') = Vertex3 (x + x') (y + y') (z + z')
  (Vertex3 x y z) - (Vertex3 x' y' z') = Vertex3 (x - x') (y - y') (z - z')
  _ * _ = error "multiplying Vertex3"
  abs _ = error "abs Vertex3"
  signum _ =  error "signum Vertex3"
  fromInteger _ = error "fromInteger Vertex3"

addVertex3 :: (Num a) => Vertex3 a -> Vertex3 a -> Vertex3 a
addVertex3 = (+)

subVertex3 :: (Num a) => Vertex3 a -> Vertex3 a -> Vertex3 a
subVertex3 = (-)

scaleVertex3 :: (Num a) => a -> Vertex3 a -> Vertex3 a
scaleVertex3 s (Vertex3 x y z) =
	  Vertex3 (x * s)
		  (y * s)
	          (z * s)

class (Num a) => Scaleable c a | c -> a where
  scaleBy :: a -> c -> c


instance Scaleable Float Float where
   scaleBy u x = u * x

instance Scaleable Double Double where
   scaleBy u x = u * x

instance (Num a) => Scaleable (Vertex3 a) a where
   scaleBy s (Vertex3 x y z) =
	  Vertex3 (x * s)
		  (y * s)
	          (z * s)

instance (Num a) => Scaleable (Vector3 a) a where
   scaleBy s (Vector3 x y z) =
	  Vector3 (x * s)
		  (y * s)
	          (z * s)

instance (Num a) => Scaleable (Normal3 a) a where
   scaleBy s (Normal3 x y z) =
	  Normal3 (x * s)
		  (y * s)
	          (z * s)


combineUsing :: (Num c,Scaleable c a) => a -> c -> c -> c
combineUsing u x x' = scaleBy t x + scaleBy u x'
    where
	t = 1 - u

fan :: (Scaleable a a,Scaleable (Vertex3 a) a,Enum a,Floating a) => Vertex3 a -> Vertex3 a -> Vertex3 a -> Int -> [Vertex3 a]
fan common left right count = [ let ver1 = combineUsing u left_v right_v
				    scaler = combineUsing u left_sz right_sz / len ver1
				in addVertex3 (scaleVertex3 scaler ver1) common
			      | u <- samples
			      ]
  where
	left_v  = subVertex3 left common
	right_v  = subVertex3 right common
	left_sz = len left_v
	right_sz = len right_v
	count'  = fromIntegral (succ count)  -- n+1 gaps to sample n things, disgarding the end points
	samples = init (tail (map (/ count') [0..count']))

midPoints :: (Enum a,Floating a) => Int -> [a]
midPoints count = samples
   where
	count'  = fromIntegral (succ count)  -- n+1 gaps to sample n things, disgarding the end points
	samples = init (tail (map (/ count') [0..count']))

len :: (Floating a) => Vertex3 a -> a
len (Vertex3 x y z) = sqrt (x * x + y * y + z * z)


vertexToNormal :: Vertex3 a -> Normal3 a
vertexToNormal (Vertex3 x y z) = Normal3 x y z