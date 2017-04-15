{-|
Module      : Geometry
Description : Geometric types based on Linear.V3
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Geometry
    (Intersection(..),
     Ray(..),
     Triangle(..),
     dot,
     intersectTri,
     normal,
     rayFromVerts,
     pointInTriangle,
     rotate,
     rotMatrixRads,
     rotVert,
     vertices
    ) where

import Color(Material)
import Linear.V3
import Data.Matrix (Matrix, (!), fromList)

data Ray = Ray {
    vertex :: V3 Float,
    direction :: V3 Float,
    bounces :: Int
} deriving (Show)

data Triangle = Triangle {
    tFirst :: V3 Float,
    tSecond :: V3 Float,
    tThird :: V3 Float,
    material :: Material Float
}

instance Show Triangle where
    show (Triangle f s t m) = unwords [show f, show s, show t, show m]

data Intersection = Intersection {
    intersectPoint :: V3 Float,
    dist :: Float,
    surface :: Triangle
} deriving (Show)

normal :: Triangle -> V3 Float
normal (Triangle a b c _) = (b - a) `cross` (c - a)

vertices :: Triangle -> [V3 Float]
vertices tri = [tFirst tri, tSecond tri, tThird tri]

-- |Returns its vector argument rotated by the euler angles alp, bet, and gam.
-- Maybe using quaternions would be better.
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

{-
Known bug: rayDist is inversely proportional to direction ray, which is NOT
required to be normalized. Therefore, if `direction ray` is small, the distance
returned is big, so it doesn't reflect the actual distance. This might not be
seen by testing this program, since there's not much difference in length among
the initial rays.

The magic number 0.001 here is acting as an epsilon, in case a ray is found to
intersect with the triangle it's bouncing off. If it's too high, light is able
to slip through corners; if it's too small, rounding error can make light fail
to bounce off.

A better solution would be to always return Nothing on the intersection of a ray
and the face that it's bouncing off. Not sure how to implement that.
-}
intersectTri :: Ray -> Triangle -> Maybe Intersection
intersectTri ray tri
    | direction ray `dot` normal tri == 0 = Nothing
    | otherwise =
        let rayDist = ((tFirst tri - vertex ray) `dot` normal tri)
                      / (direction ray `dot` normal tri)
            inter = vertex ray + ((rayDist *) <$> (direction ray))
        in  if rayDist > 0.001 && pointInTriangle inter tri then
                Just (Intersection inter rayDist tri)
            else
                Nothing

pointInTriangle :: V3 Float -> Triangle -> Bool
pointInTriangle p tri@(Triangle a b c _) =
    let insideAB = (b - a) `cross` (p - a)
        insideBC = (c - b) `cross` (p - b)
        insideCA = (a - c) `cross` (p - c)
    in  all ((>0) . (`dot` normal tri)) [insideAB, insideBC, insideCA]

dot :: Num a => V3 a -> V3 a -> a
dot (V3 a b c) (V3 d e f) = a*d + b*e + c*f

rayFromVerts :: V3 Float -> V3 Float -> Ray
rayFromVerts a b = Ray a (b - a) 0

{-
maybeIntersect :: Ray -> Triangle -> Maybe (V3 Float)
maybeIntersect ray tri
-}
