module Main where

import Data.Bits
import Data.Word
--import Data.Vector.Storable as S
import Codec.BMP
import Linear.V3
import Data.Function
import Linear.Vector
import Data.Matrix (Matrix, (!), fromList, multStrassenMixed)
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Debug.Trace as T

type ImageBuffer = B.ByteString

-- vertex is at the back of the camera.
-- depth is the distance from vertex to the center of the camera screen
--data Camera = Camera { vertex :: V3 Float, rotation :: V3 Float, depth :: Float}

data Ray = Ray { vertex :: V3 Float, direction :: V3 Float } deriving (Show)

type Scene = [Triangle]

-- todo: add more objects later
data Triangle = Triangle { a :: V3 Float, b :: V3 Float, c :: V3 Float, color :: [Word8] } deriving (Show)

data Light = Light { virtex :: V3 Float }

normal :: Triangle -> V3 Float
normal (Triangle a b c _) = (b ^-^ a) `cross` (c ^-^ a)

width :: Int
width = 256

height :: Int
height = 256

v1 = testSceneVerts !! 0
v2 = testSceneVerts !! 1
v3 = testSceneVerts !! 2
v4 = testSceneVerts !! 3
v5 = testSceneVerts !! 4
v6 = testSceneVerts !! 5

red = [255, 0, 0, 255]
yellow = [255, 191, 0, 255]
lime = [127, 255, 0, 255]
green = [0, 255, 63, 255]
cyan = [0, 255, 255, 255]
blue = [0, 63, 255, 255]
purple = [127, 0, 255, 255]
pink = [255, 0, 191, 255]

testSceneVerts :: [V3 Float]
testSceneVerts = map ((+ (V3 1 0 0)) . (`rotVert` testRotMatrix))
    [V3 1    0    0
	,V3 0    (-1) 0
	,V3 (-1) 0    0
	,V3 0    1    0
	,V3 0    0    1
	,V3 0    0    (-1)
	]

rotVert :: V3 Float -> Matrix Float -> V3 Float
rotVert vert mat = toV3 (fromV3 vert * mat)
    where toV3 matr  = V3 (matr ! (1,1)) (matr ! (1,2)) (matr ! (1,3))
          fromV3 (V3 a b c) = fromList 1 3 [a,b,c]

testScene :: [Triangle]
testScene = 
	[ Triangle v2 v1 v5 red
	, Triangle v3 v2 v5 yellow
	, Triangle v4 v3 v5 lime
	, Triangle v1 v4 v5 green
	, Triangle v1 v2 v6 cyan
	, Triangle v2 v3 v6 blue
	, Triangle v3 v4 v6 purple
	, Triangle v4 v1 v6 pink
	, Triangle (V3 10 10 (-2)) (V3 10 (-10) (-2)) (V3 (-10) 0 (-2)) [255, 255, 255, 255]
	]

testRotMatrix :: Matrix Float
testRotMatrix = rotMatrixRads (pi/16) (pi/16) 0

rotMatrixRads :: Float -> Float -> Float -> Matrix Float
rotMatrixRads alp bet gam = foldl1 (*) . map (fromList 3 3) $
	[[cos alp,  -sin alp, 0,
	  sin alp,  cos alp,  0,
	  0,        0,        1]
	 ,
	 [cos bet,  0,        sin bet,
	  0,        1,        0,
	  -sin bet, 0,        cos bet]
	 ,
	 [1,        0,        0,
	  0,        cos gam,  -sin gam,
	  0,        sin gam,  cos gam]]

-- maybe Intersectable a => Ray -> a -> Maybe (v3 Float) but thats probs overkill
getIntersect :: Ray -> Triangle -> Maybe (Float, [Word8])
getIntersect ray tri@(Triangle a b c _)
	| direction ray `dot` normal tri == 0 = Nothing
	| otherwise = 
		let rayDist = ((a - vertex ray) `dot` normal tri) 
		              / (direction ray `dot` normal tri)
		    intersectPoint = vertex ray + ((rayDist *) <$> (direction ray))
		in  if pointInTriangle intersectPoint tri then
			    Just (rayDist, color tri)
			else
				Nothing

pointInTriangle :: V3 Float -> Triangle -> Bool
pointInTriangle p tri@(Triangle a b c _) =
	let insideAB = (b - a) `cross` (p - a)
	    insideBC = (c - b) `cross` (p - b)
	    insideCA = (a - c) `cross` (p - c)
	in  all ((>0).(`dot` normal tri)) [insideAB, insideBC, insideCA]

magnitude :: V3 Float -> Float
magnitude (V3 x y z) = sqrt (x*x + y*y + z*z)

rays :: Float -> [Ray]
rays camCenter = [rayFromVerts (V3 (-1.5) 0.0 0.0) ((V3 camCenter 0 0) + V3 0.0 y z) |
		z <- take height [ 0.5,  0.5 - 1 / fromIntegral height ..],
		y <- take width  [-0.5, -0.5 + 1 / fromIntegral width  ..]
	   ]
-- this is in reverse order due to list comprehension evaluation nonsense

rayBuffer :: Float -> ImageBuffer
rayBuffer camCenter = B.pack $ concatMap (traceScene testScene) (rays camCenter)

traceScene :: [Triangle] -> Ray -> [Word8]
traceScene tris ray = fst . getClosest $ map (trace ray) tris
    where getClosest traces = head . L.sortBy (compare `on` snd) . L.filter ((>0).snd) $ traces

-- takes a ray and a triangle and returns a tuple containing the RGBA color and the distance the ray traveled
trace :: Ray -> Triangle -> ([Word8], Float)
trace ray tri = case getIntersect ray tri of
	Just x -> if fst x > 0 then
		          (snd x, fst x)
		      else ([255,200,200,64], fst x)
	Nothing -> ([0, 0, 0, 255], infinity)
		where infinity = 1.0/0.0

rayFromVerts :: V3 Float -> V3 Float -> Ray
rayFromVerts a b = Ray a (b - a)

dot :: Num a => V3 a -> V3 a -> a
dot (V3 a b c) (V3 d e f) = a*d + b*e + c*f

main :: IO ()
main = do
	let camera = -0.8
	let bmp = packRGBA32ToBMP width height (rayBuffer camera)
	writeBMP "./render/result.bmp" bmp

{-
result :: Floating a => Scene -> [a] -> ImageBuffer
result scene randseeds = avg $ zipWith (raytrace scene $) randseeds

raytrace :: Floating a => Camera -> Scene -> a -> ImageBuffer
raytrace 
-}


{-
pointInTriangle' :: V3 Float -> Triangle -> Bool
pointInTriangle' p tri@(Triangle a b c) =
	let areaABC = magnitude ((b - a) `cross` (c - a)) / 2
	    alpha   = magnitude ((b - p) `cross` (c - p)) / (2 * areaABC)
	    beta    = magnitude ((c - p) `cross` (a - p)) / (2 * areaABC)
	    gamma   = magnitude ((a - p) `cross` (b - p)) / (2 * areaABC)
	in  alpha + beta + gamma <= 1.01-}
-- VERY extremely bad