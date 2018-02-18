{-|
Module      : Geometry
Description : Geometric types based on Linear.V3
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Geometry
    (Axis(..),
     Bounds(..),
     Camera(..),
     Intersection(..),
     Ray(..),
     Scene(..),
     Triangle(..),
     area,
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
import Linear.Vector ((*^))
import Linear.Metric (dot, norm, normalize)
import Linear.V3

data Camera = Camera { position :: V3 Float, rotation :: Matrix Float }
    deriving (Show)

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

-- |Clump of data describing a successful intersection test.
data Intersection = Intersection {
    intersectPoint :: V3 Float, -- ^ The point where the intersection happened
    dist :: Float, -- ^ The distance from the ray's origin to @`intersectPoint`@
    surface :: Triangle -- ^ The object hit, because we need its texture later
} deriving (Show)

-- |All squigly-trace triangles are double-sided, which means a triangle is
-- treated exactly the same if its normal is multiplied by -1.
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
The magic number 0.001 here is acting as an epsilon, in case a ray is found to
intersect with the triangle it's bouncing off. If it's too high, light is able
to slip through corners; if it's too small, rounding error can make light fail
to bounce off.

A better solution would be to always return Nothing on the intersection of a ray
and the face that it's bouncing off. Not sure how to implement that.
-}
-- |Get the intersection of a ray and a triangle (if there is one).
intersectTri :: Ray -> Triangle -> Maybe Intersection
intersectTri ray@(Ray (V3 ox oy oz) (V3 dx dy dz)) tri@(Triangle (V3 ax ay az) (V3 bx by bz) (V3 cx cy cz) _)
    | rayDist > 0.001 && pointInTriangle inter tri = Just (Intersection inter rayDist tri)
    | otherwise = Nothing
        where normDir = normalize $ direction ray
              rayDist = ((tFirst tri - vertex ray) `dot` normal tri)
                  / (normDir `dot` normal tri)
              inter = vertex ray + rayDist *^ normDir

-- |Try to intersect every triangle with a ray, without even trying to optimize.
naiveIntersect :: [Triangle] -> Ray -> Maybe Intersection
naiveIntersect tris ray =
    let intersections = catMaybes $ intersectTri ray <$> tris
    in  case intersections of
            [] -> Nothing
            xs -> Just $ minimumBy (comparing dist) xs

-- |Point-in-triangle test using barycentric coordinates.
pointInTriangle :: V3 Float -> Triangle -> Bool
pointInTriangle p tri@(Triangle a b c _) =
    let insideAB = (b - a) `cross` (p - a)
        insideBC = (c - b) `cross` (p - b)
        insideCA = (a - c) `cross` (p - c)
    in  all ((>0) . (`dot` normal tri)) [insideAB, insideBC, insideCA]

-- | @a \`to\` b@ makes a Ray that points from @a@ to @b@.
{-# INLINE to #-}
to :: V3 Float -> V3 Float -> Ray
a `to` b = Ray a (b - a)

-- |An axis-aligned bounding box is uniquely defined by its minimum and maximum
-- extents on each axis.
-- For example, a cube centered at the origin with side length 1 is
-- (V3 -0.5 -0.5 -0.5, V3 0.5 0.5 0.5)
data Bounds = Bounds {-# UNPACK #-} !(V3 Float)
                     {-# UNPACK #-} !(V3 Float)
                     deriving Show

getBounds :: [V3 Float] -> Bounds
getBounds verts =
    let xProject = map (^._x) verts
        yProject = map (^._y) verts
        zProject = map (^._z) verts
        [minX, minY, minZ] = map minimum [xProject, yProject, zProject]
        [maxX, maxY, maxZ] = map maximum [xProject, yProject, zProject]
    in  Bounds (V3 minX minY minZ) (V3 maxX maxY maxZ)

-- |Whether a ray intersects with a bounding box.
intersectsBB :: Bounds -> Ray -> Bool
intersectsBB (Bounds (V3 lx ly lz) (V3 hx hy hz)) (Ray (V3 vx vy vz) (V3 dirx diry dirz)) =
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

-- |The average of a list of vectors. If the vectors represent vertices, this
-- gets their center of mass.
averagePoints :: [V3 Float] -> V3 Float
averagePoints verts = fmap (/genericLength verts) (sum verts)

-- |The size of an axis-aligned bounding box along the given axis.
dim :: Axis -> Bounds -> Float
dim X (Bounds lo hi) = hi ^. _x - lo ^. _x
dim Y (Bounds lo hi) = hi ^. _y - lo ^. _y
dim Z (Bounds lo hi) = hi ^. _z - lo ^. _z

-- |The longest axis of an axis-aligned bounding box.
longestAxis :: Bounds -> Axis
longestAxis b =
    fst . maximumBy (comparing snd) $ zip [X,Y,Z] [dim X b, dim Y b, dim Z b]

-- |The smallest axis-aligned bounding box of a list of triangles.
boundingBox :: [Triangle] -> Bounds
boundingBox = getBounds . concatMap vertices

-- |Get the X, Y, or Z component of a vector.
projectToAxis :: Axis -> V3 Float -> Float
projectToAxis ax (V3 x y z) =
    case ax of
        X -> x
        Y -> y
        Z -> z

-- |The area of a triangle.
area :: Triangle -> Float
area (Triangle a b c _) = norm ((b - a) `cross` (c - a)) / 2
