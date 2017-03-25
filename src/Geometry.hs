module Geometry
    (Ray(..), Scene(..), Triangle(..), normal, rotMatrixRads, rotVert
    ) where

import Color(Material(..), RGB(..))
import Linear.V3
import Data.Matrix (Matrix, (!), fromList)

data Ray = Ray { vertex :: V3 Float, direction :: V3 Float, bounces :: Int } deriving (Show)

data Scene = Scene { tris :: [Triangle] } deriving (Show)

-- triangle should be modified so that it points to its vertices. e.g.
-- data Triangle = Triangle { iFirst :: Int, iSecond :: Int, iThird :: Int, material :: Material Float }
-- getFirst triangle verts = verts !! (iFirst triangle)
-- etc.
data Triangle = Triangle { tFirst :: V3 Float, tSecond :: V3 Float, tThird :: V3 Float, material :: Material Float } deriving (Show)

normal :: Triangle -> V3 Float
normal (Triangle a b c _) = (b - a) `cross` (c - a)

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

rotVert :: V3 Float -> Matrix Float -> V3 Float
rotVert vert matr = toV3 (fromV3 vert * matr)
    where toV3 m  = V3 (m ! (1,1)) (m ! (1,2)) (m ! (1,3))
          fromV3 (V3 x y z) = fromList 1 3 [x, y, z]

{-
maybeIntersect :: Ray -> Triangle -> Maybe (V3 Float)
maybeIntersect ray tri
-}