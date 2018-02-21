-----------------------------------------------------------------------------
-- |
-- Module      : Lib
-- Description : Rendering logic
-- Maintainer  : rukokarasu@gmail.com
-- Stability   : experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiWayIf         #-}
{-# LANGUAGE RecordWildCards    #-}

module Lib
    ( Settings(..)
    , raytrace
    , raycast
    , render
    ) where

import           BIH
import           Color
import           Geometry

import           Data.Data (Data)
import           Data.Massiv.Array (Array, Ix2(..), Comp(..), D(..), S(..),
                     computeAs, makeArray)
import           Data.Massiv.Array.IO
import           Data.Typeable (Typeable)
import           Graphics.ColorSpace (Word8)
import qualified Graphics.ColorSpace as M
import           Linear.Metric (dot, norm, normalize)
import           Linear.V3
import           Linear.Vector
import           System.Random

data Settings = Settings
    { samples    :: Int
    , dimensions :: (Int, Int)
    , savePath   :: FilePath
    , objPath    :: FilePath
    , cameraPath :: FilePath
    , debug      :: Bool
    , debugPath  :: FilePath
    , cast       :: Bool
    } deriving (Show, Data, Typeable)

type Pixel = M.Pixel M.RGB Word8

-- | Compute the image and write it to a file
render :: Scene a -> Camera -> Settings -> IO ()
render scene cam Settings {..} =
    let (w,h) = dimensions
        dims = (w :. h)
        shader = renderPixel scene cam samples cast dims
        buf = makeArray Par dims shader :: Array D Ix2 Pixel
        img = computeAs S buf
    in  img `seq` writeImage savePath img
    -- ^ evaluate it before trying to write it

-- | For each pixel, cast @sampleCount@ rays and average the results.
renderPixel :: Scene a -> Camera -> Int -> Bool -> Ix2 -> Ix2 -> Pixel
renderPixel scene cam sampleCount cast dims@(w :. h) ix@(y :. x) =
    let ray = makeRay dims ix cam
        traceMethod
            | cast      = \_ -> raycast    scene ray
            | otherwise = \r -> raytrace r scene ray 0
        rix      = sampleCount * (x + y * w)
        rngs     = take sampleCount $ map mkStdGen [rix..rix + sampleCount]
        outcomes = map traceMethod rngs
        avg = fst $ foldr
            (\rayOutcome (col, i) -> ((1 / i) *^ (rayOutcome + col), i + 1))
            (V3 0 0 0, 1 :: Float)
            outcomes
    in  rgbFloatToPixelRGB avg

-- | Scale unbounded floating-point color to 3-byte RGB color.
-- Use atan instead of the more standard log.
rgbFloatToPixelRGB :: RGB Float -> Pixel
rgbFloatToPixelRGB inColor@(V3 r g b) =
    let maxComponent = max (max r g) b
        minComponent = min (min r g) b
        avg x y      = 0.5 * (x + y)
        lightness    = avg maxComponent minComponent
        intensity    = atan lightness / (pi / 2)
        scaledTo1    = (intensity / maxComponent) *^ inColor
        V3 outR outG outB = min 255 . floor . (* 255) <$> scaledTo1
    in  M.PixelRGB outR outG outB

-- | Called once per pixel.
makeRay :: Ix2 -> Ix2 -> Camera -> Ray
makeRay (w :. h) (y :. x) cam =
    let ww = fromIntegral w
        hh = fromIntegral h
        xoffs = (fromIntegral x - (ww / 2)) / ww
        yoffs = ((hh / 2) - fromIntegral y) / hh
        dir = V3 1 xoffs yoffs `rotVert` rotation cam
    in  Ray (position cam) dir

-- | A ray that intersects nothing returns black.
-- Otherwise, the color is determined by the type of surface:
-- Diffuse: the ray's original color times the surface color
--     (the ray reflects randomly)
-- Emit: the color of the emissive surface
-- Reflect: the ray's original color times the surface color
--     (the ray reflects deterministically)
-- Since raytracing works backwards by tracing from the camera into the scene,
-- the "original color" is determined later in time by bouncing the ray.
-- If a ray bounces enough times without hitting a light source, we can assume
-- it's black.
raytrace :: StdGen -> Scene a -> Ray -> Int -> RGB Float
raytrace gen scene@(Scene geom isect) ray bounces
    | bounces == 4 = black
    | Nothing    <- isectResult = black
    | Just inter <- isectResult =
        let Mat _ref refColor emit emitCol = material $ surface inter
            newRay = bounceRay gen ray inter
            newGen = snd (next gen)
        in  raytrace newGen scene newRay (bounces + 1) * refColor
                + fmap (*emit) emitCol
    where isectResult = isect geom ray

-- I'm not interested in properly implementing raycasting right now so there's
-- just a single light here at a hardcoded position.
raycast :: Scene a -> Ray -> RGB Float
raycast (Scene geom isect) ray
    | Nothing    <- isectResult = black
    | Just inter <- isectResult =
        let color = surfColor . material $ surface inter
            heaven = V3 0 3 (-1)
            shadowRay = intersectPoint inter `to` heaven
            distanceToHeaven = norm (intersectPoint inter - heaven)
            shadowed = maybe False
                ((< distanceToHeaven) . dist)
                (isect geom shadowRay)
            in  if shadowed then black else (2/distanceToHeaven) *^ color
    where isectResult = isect geom ray

-- | Bounce a ray off a surface, either sending it in a random direction or
-- reflecting it like a mirror with a probability dependent on the material.
bounceRay :: StdGen -> Ray -> Intersection -> Ray
bounceRay gen ray inter
    | ref < x   = scatterRay gen ray inter -- ref% chance of scattering
    | otherwise = reflectRay ray inter -- (1-ref)% chance of reflecting
    where ref = reflective . material $ surface inter
          x = fst $ randomR (0,1) gen

-- | Pick a random vector and, if necessary, flip it so that its direction
-- bounces off the surface. (If the old ray points in the same hemisphere
-- as the surface normal, the new ray should point in the opposite one, and
-- vice versa.)
scatterRay :: StdGen -> Ray -> Intersection -> Ray
scatterRay gen ray inter
    | old == new = Ray (intersectPoint inter) (-newDir)
    | otherwise  = Ray (intersectPoint inter)   newDir
    where newDir = randomVector gen
          old = signum (direction ray `dot` normal (surface inter))
          new = signum (newDir        `dot` normal (surface inter))

-- | This does not take a StdGen parameter because it deterministically reflects
-- the ray.
reflectRay :: Ray -> Intersection -> Ray
reflectRay ray inter =
    let dn = normalize $ normal (surface inter)
        di = direction ray
        newDir = di - dn ^* (2 * (dn `dot` di))
    in  Ray (intersectPoint inter) newDir

-- | Turn a random number generator into a vector of length 1 with
-- an equal chance of any angle.
randomVector :: StdGen -> V3 Float
randomVector gen =
    let (u, gen2) = randomR (0,1) gen
        (v, _)    = randomR (0,1) gen2
        th = 2 * pi * u
        ph = acos (2 * v - 1)
    in  V3 (cos th * sin ph) (sin th * sin ph) (cos ph)
