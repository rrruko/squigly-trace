{-|
Module      : Color
Description : Simple type for color and light
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Color
    (Material(..),
     RGB,
     average,
     black,
     diffuse,
     emission,
     gray,
     noEmit,
     rgb,
     white,
     whiteLight
    ) where

-- maybe there should be a type for material color and for emittance
-- and then a class for things that can be treated as RGB.
-- then the module only exposes functions mat, emit to make Mat, Emit types
-- so that mat clamps its args to [0,1] and emit to [0,inf)
-- though maybe faces shouldn't have mat and emit in the first place

import           V3

import           Data.List              (genericLength)

-- |Datatype for light and material colors.
type RGB = V3

rgb :: Float -> Float -> Float -> RGB
rgb = V3

{-
data HSL a = HSL a a a

instance Show a => Show (HSL a) where
    show (HSL h s l) = unwords ["HSL", show h, show s, show l]
-}

black :: RGB
black = rgb 0 0 0

gray :: RGB
gray = rgb 0.3 0.3 0.3

white :: RGB
white = rgb 1 1 1

noEmit :: RGB
noEmit = rgb 0 0 0

{-
-- |Copied from https://en.wikipedia.org/wiki/HSL_and_HSV#General_approach
-- Needs modification to work with RGB values greater than 1.
toHSL :: RGB Float -> HSL Float
toHSL (RGB r g b) =
    let max'   = maximum [r,g,b]
        min'   = minimum [r,g,b]
        chroma = max' - min'
        h' | chroma == 0 = 0
           | max' == r = ((g - b) / chroma) `mod'` 6
           | max' == g = ((b - r) / chroma) + 2
           | max' == b = ((r - g) / chroma) + 4
           | otherwise = error "The impossible happened in toHSL"
        h = 60 * h'
        l = (max' + min') / 2
        s = if l == 1 then 0 else chroma / (1 - abs (2 * l - 1))
    in  HSL h s l
-}

average :: [RGB] -> RGB
average cs = (1 / genericLength cs) *^ sum cs

data Material = Mat {
    reflective :: Float, -- ^ 0 = diffuse, 1 = reflective
    surfColor  :: RGB,   -- ^ surface color
    emissive   :: Float, -- ^ multiplied by emit color
    emitColor  :: RGB    -- ^ color of light the material emits
} deriving (Show)

diffuse :: RGB -> Material
diffuse col = Mat 0 col 0 0

emission :: RGB -> Material
emission col = Mat 0 0 1 col

whiteLight :: Float -> RGB -> Material
whiteLight = Mat 0 0
