module Color
    (Material(..), RGB(..), black, gray, white
    ) where

-- |Datatype for light and material colors.
data RGB a = RGB a a a

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

black :: Num a => RGB a
black = RGB 0 0 0

gray :: Fractional a => RGB a
gray = RGB 0.3 0.3 0.3

white :: Num a => RGB a
white = RGB 1 1 1

data Material a = Mat { color :: RGB a, emittance :: RGB a } deriving (Show)