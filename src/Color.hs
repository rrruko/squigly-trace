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

import Control.Lens.Operators ((<&>))
import Data.List (genericLength)
import Linear.V3

-- |Datatype for light and material colors.
type RGB = V3

rgb :: a -> a -> a -> RGB a
rgb = V3

{-
data HSL a = HSL a a a

instance Show a => Show (HSL a) where
    show (HSL h s l) = unwords ["HSL", show h, show s, show l]
-}

black :: Num a => RGB a
black = rgb 0 0 0

gray :: Fractional a => RGB a
gray = rgb 0.3 0.3 0.3

white :: Num a => RGB a
white = rgb 1 1 1

noEmit :: Num a => RGB a
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

average :: Fractional a => [RGB a] -> RGB a
average cs = sum cs <&> (/ genericLength cs)

data Material = Mat {
    reflective :: Float, -- ^ 0 = diffuse, 1 = reflective
    surfColor :: RGB Float, -- ^ surface color
    emissive :: Float, -- ^ multiplied by emit color
    emitColor :: RGB Float -- ^ color of light the material emits
} deriving (Show)

diffuse :: RGB Float -> Material
diffuse col = Mat 0 col 0 0

emission :: RGB Float -> Material
emission col = Mat 0 0 1 col

whiteLight :: Float -> RGB Float -> Material
whiteLight = Mat 0 0
