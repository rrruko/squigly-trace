-----------------------------------------------------------------------------
-- |
-- Module      : Lib
-- Description : Rendering logic
-- Maintainer  : rukokarasu@gmail.com
-- Stability   : experimental
--
-----------------------------------------------------------------------------

module Lib
    ( Camera(..)
    , Settings(..)
    , render
    ) where

import           BIH
import           Color
import           Geometry

import           Codec.Picture
import           Data.Matrix   (Matrix)
import           Linear.V3
import           System.Random

data Camera = Camera { position :: V3 Float, rotation :: Matrix Float }
    deriving (Show)

data Settings = Settings {
    dimensions :: (Int, Int),
    path :: String,
    samples :: Int
}

-- | Compute the image and write it to a file
render :: Scene -> Camera -> Settings -> IO ()
render scene cam settings@(Settings (w,h) path' _) = do
    rng <- getStdGen
    let (_, png) = generateFoldImage (computePixel scene cam settings) rng w h
    png `seq` writePng path' png -- evaluate it before trying to write it

-- | Compute the ray corresponding to a pair of screen coordinates and return
-- its value (with an rng to allow randomness down the pipeline)
-- This is called exactly once per pixel.
computePixel :: Scene -> Camera -> Settings -> StdGen -> Int -> Int -> (StdGen, PixelRGB8)
computePixel scene cam (Settings (w,h) _ samples') randgen x y =
    (newRand, colorToPixelRGB8 $ average rays)
    where newRand = snd (last raysTup)
          rays = map (\(f,gen) -> resultOfRay gen scene (f gen)) raysTup
          raysTup = zip (replicate samples' (\g -> makeRay (w,h) cam g x y))
                        (replicateStdGen randgen)

-- | Called once per sample per pixel.
makeRay :: (Int, Int) -> Camera -> StdGen -> Int -> Int -> Ray
makeRay (w,h) cam gen x y = 
    let ww = fromIntegral w
        hh = fromIntegral h
        (dx, gen') = randomR (0,1) gen  -- random slight offset for 
        (dy, _   ) = randomR (0,1) gen' --     antialiasing
        xoffs = (fromIntegral x + dx - (ww / 2)) / ww
        yoffs = ((hh / 2) - fromIntegral y + dy) / hh
        dir = V3 1 xoffs yoffs `rotVert` rotation cam
    in  Ray (position cam) dir 0

-- | Take an RGB Float representing unbounded light intensity in each color,
-- and turn it into a PixelRGB8, which is bounded at 255. There are a couple ways 
-- to approach this:
--  - Clamp all light above a certain intensity down to 255
--    (unrealistic, but easy)
--  - Create a new pixel with the same hue as the light and brightness 
--    corresponding to the log of the brightness of the original
--    (this is close to how human vision works)
-- I did the second thing, but used atan instead of log. 
-- It is debatable wheeher atan is a better choice; while it provides a continuous
-- map from R+ to [0,1], its derivative tapers off much more quickly than log's,
-- meaning that the difference between high intensities might be less apparent.
-- This is something you have to experiment with.
colorToPixelRGB8 :: RGB Float -> PixelRGB8
colorToPixelRGB8 col =
    let (RGB r g b) = newRGB
    in  PixelRGB8 r g b
    where newRGB = (min 255 . floor . (* 255)) <$> scaleTo1 col
          scaleTo1 col' = fmap (* (atansity (lightness col') / mx col')) col'
          mx (RGB r g b)  = maximum [r,g,b]
          mn (RGB r g b)  = minimum [r,g,b]
          lightness col'  = (mx col' + mn col') / 2
          atansity x      = atan x / (pi / 2)

avgOfRaySamples :: Scene -> Ray -> StdGen -> Int -> RGB Float
avgOfRaySamples scene ray randgen sampleCount =
    let samples' = map f (take sampleCount $ replicateStdGen randgen)
    in  average samples'
    where f gen = resultOfRay gen scene ray
        
-- | This is used to split the RNG belonging to each pixel into a separate
-- RNG for each sample of the pixel.
replicateStdGen :: StdGen -> [StdGen]
replicateStdGen gen =
    let (g1,g2) = split gen
    in  g1 : replicateStdGen g2

-- | A ray that intersects nothing returns black.
-- Otherwise, the color is determined by the type of surface:
-- Diffuse: the ray's original color times the surface color 
--     (the ray reflects randomly)
-- Emit: the color of the emissive surface
-- Reflect: the ray's original color times the surface color 
--     (the ray reflects deterministically)
-- Since raytracing works backwards by tracing from the camera into the scene,
-- the "original color" is determined later in time by bouncing the ray.
-- If a ray has bounced 4 times, its original color is defined as black.
resultOfRay :: StdGen -> Scene -> Ray -> RGB Float
resultOfRay gen scene ray
    | bounces ray >= maxBounces = black
    | otherwise =
        case BIH.intersectBIH scene ray of
            Just inter ->
                case surfaceType . material $ surface inter of
                    Diffuse -> resultOfRay newGen scene (newRay inter) *
                               color (material (surface inter))
                    Emit    -> color (material (surface inter))
                    Reflect -> resultOfRay newGen scene (newRayReflect inter) *
                               color (material (surface inter))
            Nothing ->
                black
    where maxBounces = 4
          newRay = bounceRay gen ray
          newRayReflect = bounceRayReflect ray
          newGen = snd (next gen)

-- | Pick a random vector and, if necessary, flip it so that its direction
-- bounces off the surface. (If the old ray points in the same hemisphere
-- as the surface normal, the new ray should point in the opposite one, and 
-- vice versa.)
bounceRay :: StdGen -> Ray -> Intersection -> Ray
bounceRay gen ray inter =
    let newDir = randomVector gen
        old = signum (direction ray `dot` normal (surface inter))
        new = signum (newDir        `dot` normal (surface inter))
    in  if old == new then
        Ray (intersectPoint inter) (-newDir) (bounces ray + 1)
    else
        Ray (intersectPoint inter)   newDir  (bounces ray + 1)

-- | This does not take a StdGen parameter because it deterministically reflects
-- the ray.
bounceRayReflect :: Ray -> Intersection -> Ray
bounceRayReflect ray inter =
    let dn = normalize $ normal (surface inter)
        di = direction ray
        newDir = di - fmap (* (2 * (dn `dot` di))) dn
    in  Ray (intersectPoint inter) newDir (bounces ray + 1)

-- | Turn a random number generator into a vector of length 1 with
-- an equal chance of any angle.
randomVector :: StdGen -> V3 Float
randomVector gen =
    let (u, gen2) = randomR (0,1) gen
        (v, _)    = randomR (0,1) gen2
        th = 2 * pi * u
        ph = acos (2 * v - 1)
    in  V3 (cos th * sin ph) (sin th * sin ph) (cos ph)
