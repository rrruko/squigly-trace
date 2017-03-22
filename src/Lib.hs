module Lib
    ( Camera, render, testScene
    ) where

import Data.Bits
import Data.Word
import Data.Maybe
--import Data.Vector.Storable as S
import Codec.BMP
import Linear.V3
import Data.Function
import System.Random
import Linear.Vector
import Data.Matrix (Matrix, (!), fromList)
import qualified Data.List as L
import qualified Data.ByteString as B
import qualified Debug.Trace as T

type ImageBuffer = B.ByteString

-- vertex is at the back of the camera.
-- depth is the distance from vertex to the center of the camera screen
--data Camera = Camera { vertex :: V3 Float, rotation :: V3 Float, depth :: Float}

data Ray = Ray { vertex :: V3 Float, direction :: V3 Float, bounces :: Int } deriving (Show)
data Scene = Scene { tris :: [Triangle] }

data Triangle = Triangle { 
    a :: V3 Float, 
    b :: V3 Float, 
    c :: V3 Float, 
    material :: Material Float 
} deriving (Show)

data RGB a = RGB a a a

addRGB  (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1+r2) (g1+g2) (b1+b2)
multRGB (RGB r1 g1 b1) (RGB r2 g2 b2) = RGB (r1*r2) (g1*g2) (b1*b2)

instance Functor RGB where
    fmap f (RGB r g b) = RGB (f r) (f g) (f b)
instance Show a => Show (RGB a) where
    show (RGB r g b) = unwords ["RGB", show r, show g, show b]

data Material a = Mat { color :: RGB a, emittance :: RGB a } deriving (Show)

data TraceData = Trace { 
    intersectPoint :: V3 Float, 
    dist :: Float, 
    mat :: Material Float, 
    surfNorm :: V3 Float
}

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

red = RGB 1 0 0 
yellow = RGB 1 0.75 0
lime = fmap (/0.5) (RGB 0.5 1 0)
green = RGB 0 1 0.25
cyan = RGB 0 1 1
blue = RGB 0 0.25 1
purple = RGB 0.5 0 1
pink = RGB 1 0 0.75

black = RGB 0 0 0
white = RGB 1 1 1

noEmission = black

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

testScene :: Scene
testScene = Scene
    [ Triangle v2 v1 v5 (Mat lime noEmission)
    , Triangle v3 v2 v5 (Mat lime noEmission)
    , Triangle v4 v3 v5 (Mat lime noEmission)
    , Triangle v1 v4 v5 (Mat lime noEmission)
    , Triangle v1 v2 v6 (Mat lime noEmission)
    , Triangle v2 v3 v6 (Mat lime noEmission)
    , Triangle v3 v4 v6 (Mat lime noEmission)
    , Triangle v4 v1 v6 (Mat lime noEmission)
    , Triangle (V3 10 10 (-2)) (V3 10 (-10) (-2)) (V3 (-10) 0 (-2)) (Mat white noEmission)
    , Triangle (V3 10 10 (-2)) (V3 10 (-10) (-2)) (V3 10    0 10  ) (Mat white noEmission)
    , Triangle (V3 (-1) 0 2) (V3 1 (-1) 2) (V3 1 1 2) (Mat white (RGB 100 100 100))
    ]

testRotMatrix :: Matrix Float
testRotMatrix = rotMatrixRads (pi) (pi/16) (pi/16)

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
getIntersect :: Ray -> Triangle -> Maybe TraceData
getIntersect ray tri@(Triangle a b c _)
    | direction ray `dot` normal tri == 0 = Nothing
    | otherwise = 
        let rayDist = ((a - vertex ray) `dot` normal tri) 
                      / (direction ray `dot` normal tri)
            intersectPoint = vertex ray + ((rayDist *) <$> (direction ray))
        in  if rayDist > 0.1 && pointInTriangle intersectPoint tri then
                Just (Trace intersectPoint rayDist (material tri) (normal tri))
            else
                Nothing

-- the color of a ray from a surface to a light source is just the color
-- of the light divided by the square of the distance


pointInTriangle :: V3 Float -> Triangle -> Bool
pointInTriangle p tri@(Triangle a b c _) =
    let insideAB = (b - a) `cross` (p - a)
        insideBC = (c - b) `cross` (p - b)
        insideCA = (a - c) `cross` (p - c)
    in  all ((>0).(`dot` normal tri)) [insideAB, insideBC, insideCA]

magnitude :: V3 Float -> Float
magnitude (V3 x y z) = sqrt (x*x + y*y + z*z)

makeRays :: Int -> Int -> Camera -> [Ray]
makeRays w h camera = [rayFromVerts (V3 (-1.5) 0.0 0.0) ((V3 camera 0 0) + V3 0.0 y z) |
        z <- take h  [ 0.5,  0.5 - 1 / fromIntegral h ..]
      , y <- take w  [-0.5, -0.5 + 1 / fromIntegral w ..]
      ]
-- this is in reverse order due to list comprehension evaluation nonsense

resultImage :: StdGen -> Scene -> Int -> Int -> Float -> ImageBuffer
resultImage randgen scene w h camera = 
    let rays = makeRays w h camera
    in  B.pack . concat $ zipWith (rayToPixel scene) rays (replicateStdGen randgen)--[rayToPixel r g | r <- rays, g <- take (length rays) (replicateStdGen randgen)]

colorToPixel :: RGB Float -> [Word8]
colorToPixel (RGB r g b) = map (floor . (*255) . (*(2/pi)) . atan) [r, g, b, 255]

rayToPixel :: Scene -> Ray -> StdGen -> [Word8]
rayToPixel scene ray g = colorToPixel . averageOfRaySamples g scene $ ray --colorToPixel . resultOfRay g scene $ ray

desiredSamples :: Num a => a
desiredSamples = 20

averageOfRaySamples :: StdGen -> Scene -> Ray -> RGB Float
averageOfRaySamples g s r = averageColors (map (\gen -> resultOfRay gen s r) (take desiredSamples $ replicateStdGen g))

averageColors :: [RGB Float] -> RGB Float
averageColors cs = fmap (/(fromIntegral $ length cs)) (foldl1 addRGB cs)


{-
resultImage :: StdGen -> Scene -> Int -> Int -> Float -> ImageBuffer
resultImage randgen scene w h camera =
    B.pack . average $ samples
    where samples = [makeImage gen scene (makeRays w h camera) | gen <- sampleSeeds]
          sampleSeeds = take 10 (replicateStdGen randgen)
makeImage :: StdGen -> Scene -> [Ray] -> [Word8]
makeImage gen scene rays = concatMap (colorToPixel . resultOfRay gen scene) rays
-}

replicateStdGen :: StdGen -> [StdGen]
replicateStdGen gen = 
    let (g1,g2) = split gen
    in  g1 : replicateStdGen g2


{-
resultOfRay' :: StdGen -> Scene -> Ray -> Color
resultOfRay' gen scene ray = 
    case intersect of
        Just x -> 
            if bounces ray >= 0 then
                let pathsToLightSources = map ((\y -> Ray (intersectPoint x) y 0).lightVert) (lights scene)
                    clearPaths = catMaybes $ map (getIntersectInScene gen scene) pathsToLightSources
                    lightContributionsOfPaths = map (\x -> col x `divRGB` (dist x * dist x)) clearPaths
                    sumOfLightSourceContributions = foldl (zipWith (+)) [0,0,0,255] lightContributionsOfPaths
                in  sumOfLightSourceContributions
            else
                averageRayResults $ map (resultOfRay gen scene) (spawnRays gen scene ray)
        Nothing -> [0,0,0,255]
    where averageRayResults cols = T.trace ("cols: " ++ show cols) $
            map (round.(/fromIntegral(length cols)).fromIntegral) $ foldl1 (zipWith (+)) cols
          spawnRays g s r = [Ray (intersectPoint (fromJust intersect)) (surfNorm (fromJust intersect)) (bounces r + 1)]
          intersect = getIntersectInScene gen scene ray
-}

resultOfRay :: StdGen -> Scene -> Ray -> RGB Float
resultOfRay gen scene ray
    | bounces ray >= maxBounces = RGB 0 0 0
    | otherwise = 
        let intersect = getIntersectInScene gen scene ray
            newDirection = randomVector gen
            newGen = snd (next gen)
        in  case intersect of
                Just x ->
                    let oldRayDotSurfNormSignum = signum $ direction ray `dot` surfNorm x
                        newRayDotSurfNormSignum = signum $ newDirection `dot` surfNorm x
                        newRay = if oldRayDotSurfNormSignum == newRayDotSurfNormSignum then
                                        Ray (intersectPoint x) (-newDirection) (bounces ray + 1) -- flip the direction
                                 else
                                        Ray (intersectPoint x) (newDirection) (bounces ray + 1)
                        lightIntensity = 1 / (dist x)^2
                    in  ((color $ mat x) `multRGB`
                        (resultOfRay newGen scene newRay)) `addRGB`
                        (fmap (*lightIntensity) . emittance $ mat x)
                Nothing ->
                    RGB 0 0 0
    where maxBounces = 3

randomVector :: StdGen -> V3 Float
randomVector gen = 
    let (u, gen2) = randomR (0,1) gen
        (v, _)    = randomR (0,1) gen2
        th = 2 * pi * u
        ph = acos (2 * v - 1)
    in  V3 (cos th * sin ph) (sin th * sin ph) (cos ph)

-- you should stop representing light color as [Word8]
-- during raytracing, treat light color as an unbounded (though strictly positive) rgb _float_ triple
-- then when you render the image, convert the triple to [word8] with a logarithm

-- ****
-- resultOfRay is a very important function.
-- ultimately, what you want it to do is:
--   let the point that the ray is going to hit be P
--   check if the ray has bounced maxBounce times
--   if so:
--     its color should be the sum of the light at P from each VISIBLE light in the scene
--     i.e. all lightsources that can be seen from P
--   otherwise:
--     its color should be the average of the colors of threeish random rays bounced off the triangle it's hitting
--     given a triangle and a ray and a source of randomness, you can get a random bounce ray as follows:
--       let signum(triangle's normal `dot` ray's direction) be S.
--       if S is 0, abort.
--       make a random vector called direction.
--       if signum(direction) == S, flip direction.
--       return Ray P direction.
--     make three of those, and average resultOfRay of each of them, and that's the color for a ray that hasn't hit maxBounce.
-- ****

{-
flatShade :: V3 Float -> RGB Float -> V3 Float -> V3 Float -> RGB Float
flatShade position color faceNormal lightPosition =
    let lightIntensity = 10 -- lol, should get this from the actual light
        distanceVector = lightPosition - position
        intensity = lightIntensity/(1 + (magnitude distanceVector)^2)
        cosAngle = abs (faceNormal `dot` distanceVector) / (magnitude faceNormal * magnitude distanceVector)
    in  fmap ((*intensity) . (*cosAngle)) color
    -- fluxish gives bad results because the dot product is not the same as angle
-}

{-
divRGB :: Color -> Float -> Color
divRGB c@[r,g,b,a] f
    | f < 0 = error "divRGB tried to divide a color by a negative float"
    | otherwise = 
        let floatRGB = map ((/f).fromIntegral) $ take 3 c
        in  map (\x -> if x > 255 then 255 else round x) floatRGB ++ [a]-}

{-
1, 1, 1, 255
50, 50, 50, 255
255, 255, 255, 255

-}

{-
traceScene :: StdGen -> Scene -> Ray -> Color
traceScene randGenerator scene ray = snd . getClosest $ map (trace ray) (tris scene)
    where getClosest traces = head . L.sortBy (compare `on` fst) . L.filter ((>0).fst) $ traces

traceScene :: StdGen -> Scene -> Ray -> Color
traceScene randGenerator scene ray
    | bounces ray < maxBounces = bounceRay ray
    | otherwise =
        let pathsToLightSources = map (Ray (getIntersectInScene)) (lightSources scene)
            colorOfRay = sum . map lightColor
    where maxBounces = 0
-}
getIntersectInScene :: StdGen -> Scene -> Ray -> Maybe TraceData
getIntersectInScene gen scene ray = 
    let checks = map (getIntersect ray) (tris scene)
        allIntersects = catMaybes checks
    in  listToMaybe . L.sortBy (compare `on` dist) $ allIntersects

{-
traceScene' :: StdGen -> Scene -> Ray -> Color
traceScene' randGenerator scene ray =
    let getClosest = head . L.sortBy (compare `on` fst) . L.filter ((>0).fst)
        intersectPoint = getClosest $ map (trace ray) scene
    in  if bounces ray == maxBounces then
            snd intersectPoint
        else
            averageColors $ map (traceScene scene) (makeBranches randGenerator ray)

perturbRay :: StdGen -> Ray -> Ray
perturbRay gen ray@(Ray origin direction bounces) = Ray origin newDirection bounces
    where (u, gen2) = randomR (0, 1) gen
          (v, _   ) = randomR (0, 1) gen2
          th = 2 * pi * u
          ph = acos (2 * v - 1)
          randomVector = V3 (cos th * sin ph) (sin th * sin ph) (cos ph)
          newDirection = if signum (randomVector `dot`
-}

-- this is a dumbass shit function
-- takes a ray and a triangle and returns a tuple containing the distance the ray traveled and the RGBA color

rayFromVerts :: V3 Float -> V3 Float -> Ray
rayFromVerts a b = Ray a (b - a) 0

dot :: Num a => V3 a -> V3 a -> a
dot (V3 a b c) (V3 d e f) = a*d + b*e + c*f

{-traceSingleRay :: StdGen -> Scene -> Ray -> Color
traceSingleRay gen scene ray =
    if bounces ray < 0 then
        averageRays $ spawnRays gen ray
    else
        getRayColor (rayFromVerts (vertex ray) lightSource)
    where averageRays = undefined
          spawnRays   = undefined
          getRayColor r@(Ray a b) = (lightColor lightSource) / (magnitude $ b - a)-}

type Camera = Float

render :: Camera -> Scene -> Int -> Int -> Maybe String -> IO ()
render cam scene w h path = do 
    randGenerator <- getStdGen
    let bmp = packRGBA32ToBMP w h (resultImage randGenerator scene w h cam)
    writeBMP savePath bmp
        where savePath = case path of
                Just p -> p
                Nothing -> "./render/result.bmp"

{-
    raytracing algorithm (diffuse only):

    raytrace scene rays = map (traceRay scene) rays
        where traceRay scene ray = 
            let (intersectPoint, color, normal) = getIntersect scene ray
            in  average (traceSomeRays scene (Ray intersectPoint normal))

    raytrace scene rays = map (traceRay scene) rays
        where traceRay scene ray =
            let (intersectPoint, color, reflectRatio, normal) = getIntersect scene ray
            in  weightedAverageByReflectRatio (traceSomeRays scene (Ray intersectPoint normal))
-}