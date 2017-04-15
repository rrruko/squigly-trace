{-|
Module      : Lib
Description : Rendering logic
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}

module Lib
    ( Camera(..)
    , render
    ) where

import           BIH
import           Color
import           Geometry

import           Codec.BMP
import           Control.DeepSeq
import qualified Data.ByteString as B
import           Data.Matrix     (Matrix)
import           Data.Maybe
import           Data.Word
import           Linear.V3
import           System.Random

data Camera = Camera { position :: V3 Float, rotation :: Matrix Float }
    deriving (Show)

type ImageBuffer = B.ByteString

render :: Scene -> (Int, Int) -> Maybe String -> Camera -> Int -> IO ()
render scene (w, h) path cam samples = do
    randgen <- getStdGen
    let bmp = packRGBA32ToBMP w h (resultImage scene (w, h) cam randgen samples)
    bmp `seq` writeBMP savePath bmp
        where savePath = fromMaybe "./render/result.bmp" path

resultImage :: Scene -> (Int, Int) -> Camera -> StdGen -> Int -> ImageBuffer
resultImage scene (w, h) cam randgen samples = B.pack imageAsList
    where imageAsList = concat $ zipWith' ($)
            [getColorAt x y | y <- [1..h], x <- [1..w]]
            (replicateStdGen randgen)
          getColorAt x y gen =
            let ww = fromIntegral w
                hh = fromIntegral h
                xoffs = (fromIntegral x - (ww / 2)) / ww
                yoffs = (fromIntegral y - (hh / 2)) / hh
                dir = ((V3 1 xoffs yoffs) `rotVert` (rotation cam))
                ray   = Ray (position cam) dir 0
            in  colorToPixel $ averageOfRaySamples scene ray gen samples

{-
This is used to force the pixels to be evaluated one at a time, instead of
building up a list of thunks. Using regular zipWith, memory usage scales with
sample count; using this function, it is constant.
-}
zipWith' :: NFData c => (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ []     _      = []
zipWith' _ _      []     = []
zipWith' f (x:xs) (y:ys) =
    let head' = f x y
    in  head' `deepseq` (head' : zipWith' f xs ys)

-- instead of capping the pixel at 255, it'd be better to convert
-- to hsl and then apply log to the l and then convert back to rgb
-- if you just apply log to the raw rgb, it shifts the hue value
-- or maybe just always treat color as hsl?
colorToPixel :: RGB Float -> [Word8]
colorToPixel (RGB r g b) = map badAtan [r, g, b] ++ [255]
    where badAtan   = floor . (*255) . (*(2/pi)) . atan
          --badLog    = min 255 . floor . (\x -> log x * x * 100) . (+1)
          --badLinear = min 255 . floor

averageOfRaySamples :: Scene -> Ray -> StdGen -> Int -> RGB Float
averageOfRaySamples scene ray randgen sampleCount =
    let samples = map f (take sampleCount $ replicateStdGen randgen)
    in  averageColors samples
    where f gen = resultOfRay gen scene ray

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
        case BIH.intersectBIH scene ray of
            Just inter ->
                resultOfRay newGen scene (newRay inter) *
                color (material (surface inter)) +
                emittance (material (surface inter))
            Nothing ->
                black
    where maxBounces = 4
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
        old = signum (direction ray `dot` normal (surface inter))
        new = signum (newDir        `dot` normal (surface inter))
    in  if old == new then
        Ray (intersectPoint inter) (-newDir) (bounces ray + 1)
    else
        Ray (intersectPoint inter)   newDir  (bounces ray + 1)

randomVector :: StdGen -> V3 Float
randomVector gen =
    let (u, gen2) = randomR (0,1) gen
        (v, _)    = randomR (0,1) gen2
        th = 2 * pi * u
        ph = acos (2 * v - 1)
    in  V3 (cos th * sin ph) (sin th * sin ph) (cos ph)

{-
intersectInScene :: Scene -> Ray -> Maybe Intersection
intersectInScene scene ray =
    let checks = map (intersectTri ray) (tris scene)
        allIntersects = catMaybes checks
    in  listToMaybe . sortBy (compare `on` dist) $ allIntersects
-}
