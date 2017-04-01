{-|
Module      : Lib
Description : Rendering logic
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}

module Lib
    ( makeCamera, render
    ) where

import           Color
import           Geometry

import           Codec.BMP
import           Control.DeepSeq
import qualified Data.ByteString as B
import           Data.Function
import           Data.Maybe
import           Data.Word
import           Data.List       hiding (intersect)
import           Linear.V3
import           System.Random

data Camera a = Camera a

type ImageBuffer = B.ByteString

data Intersection = Intersection { intersectPoint :: V3 Float, dist :: Float, mat :: Material Float, surfNorm :: V3 Float }

{-
The camera is currently assumed to be situated on the X axis, pointing in the positive
direction (see resultImage). The Camera argument just describes field of view.
-}
render :: Scene -> Int -> Int -> Maybe String -> Camera Float -> IO ()
render scene w h path cam = do
    randGenerator <- getStdGen
    let bmp = packRGBA32ToBMP w h (resultImage randGenerator scene w h cam)
    bmp `seq` writeBMP savePath bmp
        where savePath = fromMaybe "./render/result.bmp" path

makeCamera :: Num a => a -> Camera a
makeCamera x = Camera x

resultImage :: StdGen -> Scene -> Int -> Int -> Camera Float -> ImageBuffer
resultImage randgen scene w h (Camera _) = B.pack imageAsList
    where imageAsList = concat $ zipWith' ($)
            [getColorAt x y | y <- [0..h - 1], x <- [0..w - 1]] 
            (replicateStdGen randgen)
          getColorAt x y gen =
            let xoffs = (fromIntegral x - (fromIntegral w / 2)) / fromIntegral w
                yoffs = (fromIntegral y - (fromIntegral h / 2)) / fromIntegral h
                orig  = V3 0 7 0
                cameraRotation = rotMatrixRads (pi/2) 0 0
                vert  = ((V3 1 xoffs yoffs) `rotVert` cameraRotation) + orig
                ray   = rayFromVerts orig vert
            in  rayToPixel scene ray gen

{-
This is used to force the pixels to be evaluated one at a time, instead of building up
a list of thunks. Using regular zipWith, memory usage scales with sample count;
using this function, it is constant.

zipWith ($) [getColorAt x y | y <- [0..h-1], x <- [0..w-1] (replicateStdGen randgen)]
= (getColorAt x y g1) : zipWith ($) [getColorAt x y | y <- [0..h-1], x <- [0..w-1] (replicateStdGen randgen)]
= (getColorAt 0 0 g1) : (getColorAt 0 1 g2) : (getColorAt 0 2 g3) : ...
-}
zipWith' :: NFData c => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (x:xs) (y:ys) =
    let head' = f x y
    in  head' `deepseq` (head' : zipWith' f xs ys)

rayToPixel :: Scene -> Ray -> StdGen -> [Word8]
rayToPixel scene ray g = colorToPixel . averageOfRaySamples g scene $ ray

-- instead of capping the pixel at 255, it'd be better to convert
-- to hsl and then apply log to the l and then convert back to rgb
-- if you just apply log to the raw rgb, it shifts the hue value
-- or maybe just always treat color as hsl?
colorToPixel :: RGB Float -> [Word8]
colorToPixel (RGB r g b) = map badAtan [r, g, b] ++ [255]
    where badAtan   = floor . (*255) . (*(2/pi)) . atan
          --badLog    = min 255 . floor . (\x -> log x * x * 100) . (+1)
          --badLinear = min 255 . floor
          
averageOfRaySamples :: StdGen -> Scene -> Ray -> RGB Float
averageOfRaySamples g s r = 
    let samples = map f (take desiredSamples $ replicateStdGen g)
    in  averageColors samples
    where f gen = resultOfRay gen s r
          desiredSamples = 1

replicateStdGen :: StdGen -> [StdGen]
replicateStdGen gen =
    let (g1,g2) = split gen
    in  g1 : replicateStdGen g2

{-
Given a ray: if it intersects something, its color is equal to
the incoming light times the color of the intersected surface, 
plus the surface's emittance.
If it intersects nothing, return black.
-}
resultOfRay :: StdGen -> Scene -> Ray -> RGB Float
resultOfRay gen scene ray
    | bounces ray >= maxBounces = black
    | otherwise =
        case intersectInScene scene ray of
            Just inter -> 
                resultOfRay newGen scene (newRay inter) * color (mat inter) + emittance (mat inter)
            Nothing ->
                black
    where maxBounces = 5
          newRay inter = bounceRay gen ray inter
          newGen = snd (next gen)
{-
Pick a random vector and, if necessary, flip it so that its direction 'bounces' off the
surface. (If the old ray points in the same hemisphere as the surface normal, the new 
ray should point in the opposite one, and vice versa.)
-}
bounceRay :: StdGen -> Ray -> Intersection -> Ray
bounceRay gen ray inter =
    let newDir = randomVector gen
        old = signum (direction ray `dot` surfNorm inter)
        new = signum (newDir        `dot` surfNorm inter)
    in  if old == new then
        Ray (intersectPoint inter) (-newDir) (bounces ray + 1)
    else
        Ray (intersectPoint inter)   newDir  (bounces ray + 1)

{-
This should be decomposed into a intersect :: Ray -> Triangle -> Maybe (V3 Float) in Geometry
and a intersectData :: Ray -> Triangle -> Maybe Intersection in Lib

The magic number 0.01 here is acting as an epsilon, in case a ray is found to 
intersect with the triangle it's bouncing off. If it's too high, light is able to slip
through corners; if it's too small, rounding error can make light fail to bounce off.

A better solution would be to always return Nothing on the intersection of a ray and the
face that it's bouncing off. Not sure how to implement that.
-}
intersect :: Ray -> Triangle -> Maybe Intersection
intersect ray tri 
    | direction ray `dot` normal tri == 0 = Nothing
    | otherwise = 
        let rayDist = ((tFirst tri - vertex ray) `dot` normal tri) 
                      / (direction ray `dot` normal tri)
            inter = vertex ray + ((rayDist *) <$> (direction ray))
        in  if rayDist > 0.01 && pointInTriangle inter tri then
                Just (Intersection inter rayDist (material tri) (normal tri))
            else
                Nothing

randomVector :: StdGen -> V3 Float
randomVector gen = 
    let (u, gen2) = randomR (0,1) gen
        (v, _)    = randomR (0,1) gen2
        th = 2 * pi * u
        ph = acos (2 * v - 1)
    in  V3 (cos th * sin ph) (sin th * sin ph) (cos ph)

intersectInScene :: Scene -> Ray -> Maybe Intersection
intersectInScene scene ray = 
    let checks = map (intersect ray) (tris scene)
        allIntersects = catMaybes checks
    in  listToMaybe . sortBy (compare `on` dist) $ allIntersects

{-
data AABB a = AABB { low :: V3 Float, high :: V3 Float, contents :: a } 
    deriving (Show)

bvh :: Scene -> AABB Triangle -> Tree (AABB Triangle)
bvh scene box
    | contents box == [] = Leaf box
    | otherwise =
        let (first, second) = split box
        in  Branch (bvh scene first) (bvh scene second)

Tree (Tree box empty) (Tree (Tree box empty) (Tree box empty))

split :: AABB Triangle -> (AABB Triangle, AABB Triangle)
split box = undefined
    -- do heuristics to split the box so that there's an even number of
    -- triangles in each child

intersectBVH :: Ray -> Tree (AABB Triangle) -> Maybe Intersection
intersectBVH ray = undefined
    -- ???
-}