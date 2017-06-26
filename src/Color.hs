{-|
Module      : Color
Description : Simple type for color and light
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Color
    (Material(..),
     RGB(..),
     average,
     black,
     diffuse,
     emission,
     gray,
     noEmit,
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

-- |Datatype for light and material colors.
data RGB a = RGB a a a deriving (Eq)

instance Functor RGB where
    fmap f (RGB r g b) = RGB (f r) (f g) (f b)

instance Show a => Show (RGB a) where
    show (RGB r g b) = unwords ["RGB", show r, show g, show b]

instance Num a => Num (RGB a) where
    (RGB r g b) + (RGB r2 g2 b2) = RGB (r + r2) (g + g2) (b + b2)
    (RGB r g b) - (RGB r2 g2 b2) = RGB (r - r2) (g - g2) (b - b2)
    (RGB r g b) * (RGB r2 g2 b2) = RGB (r * r2) (g * g2) (b * b2)
    abs = fmap abs
    signum = fmap signum
    fromInteger n = fmap fromInteger (RGB n n n)

instance Fractional a => Fractional (RGB a) where
    (RGB r g b) / (RGB r2 g2 b2) = RGB (r/r2) (g/g2) (b/b2)
    recip (RGB r g b) = RGB (1/r) (1/g) (1/b)
    fromRational rat = RGB (fromRational rat) (fromRational rat) (fromRational rat)

{-
data HSL a = HSL a a a

instance Show a => Show (HSL a) where
    show (HSL h s l) = unwords ["HSL", show h, show s, show l]
-}

black :: Num a => RGB a
black = RGB 0 0 0

gray :: Fractional a => RGB a
gray = RGB 0.3 0.3 0.3

white :: Num a => RGB a
white = RGB 1 1 1

noEmit :: Num a => RGB a
noEmit = RGB 0 0 0

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
diffuse rgb = Mat 0 rgb 0 0

emission :: RGB Float -> Material
emission rgb = Mat 0 0 1 rgb

whiteLight :: Float -> RGB Float -> Material
whiteLight = Mat 0 0
