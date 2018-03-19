-- V3 specialized to Float

module V3 where

data V3 = V3 { _x, _y, _z :: {-# UNPACK #-} !Float } deriving (Eq, Ord, Show)

instance Num V3 where
  V3 u v w + V3 x y z = V3 (u + x) (v + y) (w + z)
  V3 u v w * V3 x y z = V3 (u * x) (v * y) (w * z)
  abs    = vmap abs    -- ???
  signum = vmap signum -- ???
  negate = vmap negate -- ???
  fromInteger n = V3 (fromInteger n) (fromInteger n) (fromInteger n)

vmap :: (Float -> Float) -> V3 -> V3
vmap f (V3 x y z) = V3 (f x) (f y) (f z)

(*^) :: Float -> V3 -> V3
r *^ (V3 x y z) = V3 (r * x) (r * y) (r * z)

cross :: V3 -> V3 -> V3
cross (V3 a b c) (V3 d e f) = V3 (b*f-c*e) (c*d-a*f) (a*e-b*d)
{-# INLINABLE cross #-}

dot :: V3 -> V3 -> Float
dot (V3 a b c) (V3 d e f) = (a*d) + (b*e) + (c*f)

quadrance :: V3 -> Float
quadrance v = dot v v

norm :: V3 -> Float
norm v = sqrt (quadrance v)

normalize :: V3 -> V3
normalize v@(V3 a b c) =
    let vnorm = norm v
    in  V3 (a / vnorm) (b / vnorm) (c / vnorm)
