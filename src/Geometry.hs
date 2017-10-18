{-|
Module      : Geometry
Description : Geometric types based on Linear.V3
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Geometry
    (Axis(..),
     Bounds,
     Intersection(..),
     Ray(..),
     Scene(..),
     Triangle(..),
     averagePoints,
     boundingBox,
     dim,
     getBounds,
     longestAxis,
     intersectsBB,
     intersectTri,
     naiveIntersect,
     normal,
     pointInTriangle,
     projectToAxis,
     rotate,
     rotMatrixRads,
     rotVert,
     to,
     vertices
    ) where

import Color(Material)

import Control.Lens ((^.))
import Data.List hiding (intersect)
import Data.Matrix (Matrix, (!), fromList)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Debug.Trace (trace)
import Linear.Vector ((*^))
import Linear.Metric (dot, norm)
import Linear.V3

data Ray = Ray {
    vertex :: V3 Float,
    direction :: V3 Float
} deriving (Show)

data Triangle = Triangle {
    tFirst :: V3 Float,
    tSecond :: V3 Float,
    tThird :: V3 Float,
    material :: Material
}

data Axis = X | Y | Z
    deriving (Show)

instance Show Triangle where
    show (Triangle f s t m) = unwords [show f, show s, show t, show m]

data Scene a = Scene {
    geometry :: a,
    intersect :: a -> Ray -> Maybe Intersection
}

instance Show a => Show (Scene a) where
  show (Scene g _) = show g

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
rotMatrixRads alp bet gam = foldr1 (*) . map (fromList 3 3) $
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
    | rayDist > 0.001 && pointInTriangle inter tri = Just (Intersection inter rayDist tri)
    | otherwise = Nothing
        where normDir = norm $ direction ray
              rayDist = ((tFirst tri - vertex ray) `dot` normal tri)
                  / (direction ray `dot` normal tri)
              inter = vertex ray + (rayDist *^ direction ray)

naiveIntersect :: [Triangle] -> Ray -> Maybe Intersection
naiveIntersect tris ray =
    let intersections = catMaybes $ intersectTri ray <$> tris
    in  case intersections of
            [] -> Nothing
            xs -> Just $ minimumBy (comparing dist) xs


pointInTriangle :: V3 Float -> Triangle -> Bool
pointInTriangle p tri@(Triangle a b c _) =
    let insideAB = (b - a) `cross` (p - a)
        insideBC = (c - b) `cross` (p - b)
        insideCA = (a - c) `cross` (p - c)
    in  all ((>0) . (`dot` normal tri)) [insideAB, insideBC, insideCA]

to :: V3 Float -> V3 Float -> Ray
a `to` b = Ray a (b - a)

{-
An axis-aligned bounding box can be defined by only two points,
where one is the low bound on each axis and the other is the high
bound on each axis. e.g. a cube centered at the origin with side
length 1 is (V3 -0.5 -0.5 -0.5, V3 0.5 0.5 0.5)
-}
type Bounds = (V3 Float, V3 Float)

getBounds :: [V3 Float] -> Bounds
getBounds verts =
    let xProject = map (^._x) verts
        yProject = map (^._y) verts
        zProject = map (^._z) verts
        [minX, minY, minZ] = map minimum [xProject, yProject, zProject]
        [maxX, maxY, maxZ] = map maximum [xProject, yProject, zProject]
    in  (V3 minX minY minZ, V3 maxX maxY maxZ)

intersectsBB :: Bounds -> Ray -> Bool
intersectsBB (V3 lx ly lz, V3 hx hy hz) (Ray (V3 vx vy vz) (V3 dirx diry dirz)) =
    let (V3 dfx dfy dfz) = V3 (1/dirx) (1/diry) (1/dirz)
        t1 = (lx  - vx) * dfx
        t2 = (hx - vx) * dfx
        t3 = (ly  - vy) * dfy
        t4 = (hy - vy) * dfy
        t5 = (lz  - vz) * dfz
        t6 = (hz - vz) * dfz
        tmin = max (max (min t1 t2) (min t3 t4)) (min t5 t6)
        tmax = min (min (max t1 t2) (max t3 t4)) (max t5 t6)
    in  tmax > 0 && tmin < tmax

averagePoints :: [V3 Float] -> V3 Float
averagePoints verts = fmap (/genericLength verts) (sum verts)

dim :: Axis -> Bounds -> Float
dim X b = snd b ^. _x - fst b ^. _x
dim Y b = snd b ^. _y - fst b ^. _y
dim Z b = snd b ^. _z - fst b ^. _z

longestAxis :: Bounds -> Axis
longestAxis b =
    fst . maximumBy (comparing snd) $ zip [X,Y,Z] [dim X b, dim Y b, dim Z b]

boundingBox :: [Triangle] -> Bounds
boundingBox = getBounds . concatMap vertices

projectToAxis :: Axis -> V3 Float -> Float
projectToAxis ax (V3 x y z) =
    case ax of
        X -> x
        Y -> y
        Z -> z

area :: Triangle -> Float
area (Triangle a b c _) = norm ((b - a) `cross` (c - a)) / 2
