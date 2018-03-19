-----------------------------------------------------------------------------
-- |
-- Module      : Lib
-- Description : Rendering logic
-- Maintainer  : rukokarasu@gmail.com
-- Stability   : experimental
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RecordWildCards            #-}

module Lib
    ( Settings(..)
    , CameraPath(..)
    , DebugPath(..)
    , ObjPath(..)
    , SavePath(..)
    , raytrace
    , raycast
    , render
    ) where

import           BIH
import           Color
import           Geometry
import           V3

import           Control.Monad        (guard)
import           Data.Data            (Data)
import           Data.Massiv.Array    (Array, Comp (..), D (..), Ix2 (..),
                                       S (..), computeAs, makeArray)
import           Data.Massiv.Array.IO
import           Data.Maybe           (fromMaybe)
import           Data.Typeable        (Typeable)
import           Data.Word
import           Graphics.ColorSpace  (Word8)
import qualified Graphics.ColorSpace  as M
import           System.Random.TF
import           System.Random.TF.Gen
import           System.Console.CmdArgs.Default (Default)

newtype SavePath   = SavePath   { unSavePath   :: FilePath }
    deriving (Show, Data, Default)
newtype ObjPath    = ObjPath    { unObjPath    :: FilePath }
    deriving (Show, Data, Default)
newtype CameraPath = CameraPath { unCameraPath :: FilePath }
    deriving (Show, Data, Default)
newtype DebugPath  = DebugPath  { unDebugPath  :: FilePath }
    deriving (Show, Data, Default)

data Settings = Settings
    { samples    :: Int
    , dimensions :: (Int, Int)
    , savePath   :: SavePath
    , objPath    :: ObjPath
    , cameraPath :: CameraPath
    , debug      :: Bool
    , debugPath  :: DebugPath
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
    in  img `seq` writeImage (unSavePath savePath) img
    -- ^ evaluate it before trying to write it

-- | For each pixel, cast @sampleCount@ rays and average the results.
renderPixel :: Scene a -> Camera -> Int -> Bool -> Ix2 -> Ix2 -> Pixel
renderPixel scene cam sampleCount cast dims@(w :. h) ix@(y :. x) =
    let ray = makeRay dims ix cam
        traceMethod
            | cast      = \_ -> raycast    scene ray
            | otherwise = \r -> raytrace r scene ray 0
        rix      = sampleCount * (x + y * w)
        rngs     = take sampleCount $ map mkTFGen [rix..]
        outcomes = map traceMethod rngs
        avg = (1 / fromIntegral sampleCount) *^ sum outcomes
    in  rgbFloatToPixelRGB avg

-- | Scale unbounded floating-point color to 3-byte RGB color.
-- Use atan instead of the more standard log.
rgbFloatToPixelRGB :: RGB -> Pixel
rgbFloatToPixelRGB inColor@(V3 r g b) =
    let maxComponent = max (max r g) b
        minComponent = min (min r g) b
        avg x y      = 0.5 * (x + y)
        lightness    = avg maxComponent minComponent
        intensity    = atan lightness / (pi / 2)
        V3 s1x s1y s1z = (intensity / maxComponent) *^ inColor
        outR = min 255 (floor (s1x * 255))
        outG = min 255 (floor (s1y * 255))
        outB = min 255 (floor (s1z * 255))
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
raytrace :: TFGen -> Scene a -> Ray -> Int -> RGB
raytrace gen scene@(Scene geom isect) ray bounces = fromMaybe black $ do
    inter <- isect geom ray
    let Mat {..} = material $ surface inter
        newRay = bounceRay gen ray inter
        newGen = snd (next gen)
        nextBounce = surfColor * raytrace newGen scene newRay (bounces + 1)
        emitContribution = emissive *^ emitColor
    pure $ nextBounce + emitContribution

-- I'm not interested in properly implementing raycasting right now so there's
-- just a single light here at a hardcoded position.
raycast :: Scene a -> Ray -> RGB
raycast (Scene geom isect) ray = fromMaybe black $ do
    inter <- isect geom ray
    let Mat {..} = material $ surface inter
        hardCodedLight = V3 0 3 (-1)
        shadowRay = intersectPoint inter `to` hardCodedLight
        distanceToLight = norm (intersectPoint inter - hardCodedLight)
    guard $ maybe True
        (\pos -> dist pos > distanceToLight)
        (isect geom shadowRay)
    pure $ (2 / distanceToLight) *^ surfColor

-- | Bounce a ray off a surface, either sending it in a random direction or
-- reflecting it like a mirror with a probability dependent on the material.
bounceRay :: TFGen -> Ray -> Intersection -> Ray
bounceRay gen ray inter
    | ref < x   = scatterRay gen ray inter -- ref% chance of scattering
    | otherwise = reflectRay ray inter -- (1-ref)% chance of reflecting
    where ref = reflective . material $ surface inter
          x = fst $ randomR (0,1) gen

-- | Pick a random vector and, if necessary, flip it so that its direction
-- bounces off the surface. (If the old ray points in the same hemisphere
-- as the surface normal, the new ray should point in the opposite one, and
-- vice versa.)
scatterRay :: TFGen -> Ray -> Intersection -> Ray
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
        newDir = di - ((2 * (dn `dot` di)) *^ dn)
    in  Ray (intersectPoint inter) newDir

randomR :: (Float, Float) -> TFGen -> (Float, TFGen)
randomR (lo, hi) g =
    let (n, g') = next g
        p = fromIntegral n / fromIntegral (maxBound :: Word32)
        r = hi - lo
    in  (lo + r * p, g')

-- | Turn a random number generator into a vector of length 1 with
-- an equal chance of any angle.
randomVector :: TFGen -> V3
randomVector gen =
    let (u, gen2) = randomR (0,1) gen
        (v, _)    = randomR (0,1) gen2
        th = 2 * pi * u
        ph = acos (2 * v - 1)
    in  V3 (cos th * sin ph) (sin th * sin ph) (cos ph)
