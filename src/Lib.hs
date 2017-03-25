{-|
Module      : Lib
Description : Rendering logic
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Lib
    ( Camera, render, testScene
    ) where

import Color (Material(..), RGB(..), black, white)
import Geometry (Ray(..), Scene(..), Triangle(..), normal, rotMatrixRads, rotVert)
--import Data.Bits
import Data.Word
import Data.Maybe
import Codec.BMP
import Linear.V3
import Data.Function
import System.Random
import Data.Matrix (Matrix)
import Data.List
import qualified Data.ByteString as B
--import qualified Debug.Trace as T

type ImageBuffer = B.ByteString

type Camera = Float

data TraceData = Trace { intersectPoint :: V3 Float, dist :: Float, mat :: Material Float, surfNorm :: V3 Float }

testSceneVerts :: [V3 Float]
testSceneVerts = map ((+ (V3 1 0 0)) . (`rotVert` testRotMatrix))
    [V3 1    0    0
    ,V3 0    (-1) 0
    ,V3 (-1) 0    0
    ,V3 0    1    0
    ,V3 0    0    1
    ,V3 0    0    (-1)
    ]

testScene :: Scene
testScene = Scene
    [ Triangle v2 v1 v5 (Mat col noEmission)
    , Triangle v3 v2 v5 (Mat col noEmission)
    , Triangle v4 v3 v5 (Mat col noEmission)
    , Triangle v1 v4 v5 (Mat col noEmission)
    , Triangle v1 v2 v6 (Mat col noEmission)
    , Triangle v2 v3 v6 (Mat col noEmission)
    , Triangle v3 v4 v6 (Mat col noEmission)
    , Triangle v4 v1 v6 (Mat col noEmission)
    , Triangle (V3 10 10 (-2)) (V3 10 (-10) (-2)) (V3 (-10) 0 (-2)) (Mat white noEmission)
    , Triangle (V3 10 10 (-2)) (V3 10 (-10) (-2)) (V3 10    0 10  ) (Mat white noEmission)
    , Triangle (V3 (-1) 0 2) (V3 1 (-1) 2) (V3 1 1 2) (Mat white (RGB 1000 1000 1000))
    ]
    where col = RGB 0.25 0 0.125
          noEmission = black
          v1 = testSceneVerts !! 0
          v2 = testSceneVerts !! 1
          v3 = testSceneVerts !! 2
          v4 = testSceneVerts !! 3
          v5 = testSceneVerts !! 4
          v6 = testSceneVerts !! 5

testScene' :: Scene
testScene' = Scene
    [ t 1 3 2 (Mat black (RGB 1 1 1))
    , t 0 1 2 (Mat black (RGB 1 1 1))
    , t 5 7 6 (Mat black (RGB 1 1 1))
    , t 4 5 6 (Mat black (RGB 1 1 1))
    ]
    where verts =
            [ V3 (-0.5) (-0.5) (-0.2), V3 0.5 (-0.5) (-0.2), V3 (-0.5) 0.5 (-0.2), V3 0.5 0.5 (-0.2)
            , V3 (-0.5) (-0.5) (0.2),  V3 0.5 (-0.5) (0.2),  V3 (-0.5) 0.5 0.2, V3 0.5 0.5 0.2]
          t x y z = Triangle (verts!!x) (verts!!y) (verts!!z)


testRotMatrix :: Matrix Float
testRotMatrix = rotMatrixRads (pi/8) (pi/16) 0

getIntersect :: Ray -> Triangle -> Maybe TraceData
getIntersect ray tri
    | direction ray `dot` normal tri == 0 = Nothing
    | otherwise = 
        let rayDist = ((tFirst tri - vertex ray) `dot` normal tri) 
                      / (direction ray `dot` normal tri)
            inter = vertex ray + ((rayDist *) <$> (direction ray))
        in  if rayDist > 0.1 && pointInTriangle inter tri then
                Just (Trace inter rayDist (material tri) (normal tri))
            else
                Nothing

pointInTriangle :: V3 Float -> Triangle -> Bool
pointInTriangle p tri@(Triangle a b c _) =
    let insideAB = (b - a) `cross` (p - a)
        insideBC = (c - b) `cross` (p - b)
        insideCA = (a - c) `cross` (p - c)
    in  all ((>0).(`dot` normal tri)) [insideAB, insideBC, insideCA]

makeRays :: Int -> Int -> Camera -> [Ray]
makeRays w h camera = [rayFromVerts (V3 (-1.5) 0.0 0.0) ((V3 camera 0 0) + V3 0.0 y z) |
        z <- take h  [ 0.5,  0.5 - 1 / fromIntegral h ..]
      , y <- take w  [-0.5, -0.5 + 1 / fromIntegral w ..]
      ]

resultImage :: StdGen -> Scene -> Int -> Int -> Float -> ImageBuffer
resultImage randgen scene w h camera = 
    let rays = makeRays w h camera
    in  B.pack . concat $ zipWith (rayToPixel scene) rays (replicateStdGen randgen)

colorToPixel :: RGB Float -> [Word8]
colorToPixel (RGB r g b) = map (floor . (*255) . (*(2/pi)) . atan) [r, g, b, 255]

rayToPixel :: Scene -> Ray -> StdGen -> [Word8]
rayToPixel scene ray g = colorToPixel . averageOfRaySamples g scene $ ray

desiredSamples :: Num a => a
desiredSamples = 5

averageOfRaySamples :: StdGen -> Scene -> Ray -> RGB Float
averageOfRaySamples g s r = 
    averageColors (map f
                       (take desiredSamples $ replicateStdGen g))
    where f gen = resultOfRay gen s r

averageColors :: [RGB Float] -> RGB Float
averageColors cs = fmap (/(genericLength cs)) (foldl1 (+) cs)

replicateStdGen :: StdGen -> [StdGen]
replicateStdGen gen = 
    let (g1,g2) = split gen
    in  g1 : replicateStdGen g2

-- this is apparently one of many memory leaks in this program; 
-- replacing the return value with just (resultOfRay newGen scene newRay)
-- cuts memory usage almost in half
-- but this program's memory usage still shouldnt scale with sample count
resultOfRay :: StdGen -> Scene -> Ray -> RGB Float
resultOfRay gen scene ray
    | bounces ray >= maxBounces = black
    | otherwise = 
        let inter = getIntersectInScene scene ray
        in  case inter of
                Just x ->
                    let newDirection = randomVector gen
                        newGen = snd (next gen)
                        oldRayDotSurfNormSignum = signum $ direction ray `dot` surfNorm x
                        newRayDotSurfNormSignum = signum $ newDirection `dot` surfNorm x
                        newRay = 
                            if oldRayDotSurfNormSignum == newRayDotSurfNormSignum then
                                Ray (intersectPoint x) (-newDirection) (bounces ray + 1) -- flip the direction
                            else
                                Ray (intersectPoint x) (newDirection) (bounces ray + 1)
                        --lightIntensity = 1 / (dist x)**2
                    in  ((color $ mat x) *
                        (resultOfRay newGen scene newRay)) +
                        (emittance $ mat x)
                Nothing ->
                    RGB 0 0 0
    where maxBounces = 7

randomVector :: StdGen -> V3 Float
randomVector gen = 
    let (u, gen2) = randomR (0,1) gen
        (v, _)    = randomR (0,1) gen2
        th = 2 * pi * u
        ph = acos (2 * v - 1)
    in  V3 (cos th * sin ph) (sin th * sin ph) (cos ph)

getIntersectInScene :: Scene -> Ray -> Maybe TraceData
getIntersectInScene scene ray = 
    let checks = map (getIntersect ray) (tris scene)
        allIntersects = catMaybes checks
    in  listToMaybe . sortBy (compare `on` dist) $ allIntersects

rayFromVerts :: V3 Float -> V3 Float -> Ray
rayFromVerts a b = Ray a (b - a) 0

dot :: Num a => V3 a -> V3 a -> a
dot (V3 a b c) (V3 d e f) = a*d + b*e + c*f

render :: Camera -> Scene -> Int -> Int -> Maybe String -> IO ()
render cam scene w h path = do 
    randGenerator <- getStdGen
    let bmp = packRGBA32ToBMP w h (resultImage randGenerator scene w h cam)
    bmp `seq` writeBMP savePath bmp
        where savePath = fromMaybe "./render/result.bmp" path