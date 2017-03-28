{-|
Module      : Geometry
Description : Geometric types based on V3s
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Geometry
    (Ray(..), 
     Scene(..),
     Triangle(..),
     dot, 
     normal,
     rayFromVerts, 
     pointInTriangle,
     rotate, 
     rotMatrixRads, 
     rotVert
    ) where

import Color(Material)
import Linear.V3
import Data.Matrix (Matrix, (!), fromList)

data Ray = Ray { vertex :: V3 Float, direction :: V3 Float, bounces :: Int } deriving (Show)

data Scene = Scene { tris :: [Triangle] } deriving (Show)

data Triangle = Triangle { tFirst :: V3 Float, tSecond :: V3 Float, tThird :: V3 Float, material :: Material Float } deriving (Show)

normal :: Triangle -> V3 Float
normal (Triangle a b c _) = (b - a) `cross` (c - a)

-- |Returns its vector argument rotated by the euler angles alp, bet, and gam
rotate :: Float -> Float -> Float -> V3 Float -> V3 Float
rotate alp bet gam vert = rotVert vert (rotMatrixRads alp bet gam)

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

pointInTriangle :: V3 Float -> Triangle -> Bool
pointInTriangle p tri@(Triangle a b c _) =
    let insideAB = (b - a) `cross` (p - a)
        insideBC = (c - b) `cross` (p - b)
        insideCA = (a - c) `cross` (p - c)
    in  all ((>0).(`dot` normal tri)) [insideAB, insideBC, insideCA]

dot :: Num a => V3 a -> V3 a -> a
dot (V3 a b c) (V3 d e f) = a*d + b*e + c*f

rayFromVerts :: V3 Float -> V3 Float -> Ray
rayFromVerts a b = Ray a (b - a) 0

{-
maybeIntersect :: Ray -> Triangle -> Maybe (V3 Float)
maybeIntersect ray tri
-}