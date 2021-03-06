{-|
Module      : Geometry
Description : Geometric types
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
     mollerTrumbore,
     naiveIntersect,
     normal,
     projectToAxis,
     rotate,
     rotMatrixRads,
     rotVert,
     to,
     vertices
    ) where

import           Color         (Material)
import           V3

import           Data.List     hiding (intersect)
import           Data.Matrix   (Matrix, fromList, (!))
import           Data.Maybe    (catMaybes)
import           Data.Ord      (comparing)

data Camera = Camera { position :: V3, rotation :: Matrix Float }
    deriving (Show)

data Ray = Ray {
    vertex    :: !V3,
    direction :: !V3
} deriving (Show)

data Triangle = Triangle {
    tFirst   :: !V3,
    tSecond  :: !V3,
    tThird   :: !V3,
    material :: !Material
}

data Axis = X | Y | Z
    deriving (Show)

instance Show Triangle where
    show (Triangle f s t m) = unwords [show f, show s, show t, show m]

data Scene a = Scene {
    geometry  :: a,
    intersect :: a -> Ray -> Maybe Intersection
}

instance Show a => Show (Scene a) where
  show (Scene g _) = show g

-- |Clump of data describing a successful intersection test.
data Intersection = Intersection {
    intersectPoint :: !V3, -- ^ The point where the intersection happened
    dist           :: !Float, -- ^ The distance from the ray's origin to @`intersectPoint`@
    surface        :: !Triangle -- ^ The object hit, because we need its texture later
} deriving (Show)

-- |All squigly-trace triangles are double-sided, which means a triangle is
-- treated exactly the same if its normal is multiplied by -1.
normal :: Triangle -> V3
normal (Triangle a b c _) = (b - a) `cross` (c - a)

vertices :: Triangle -> [V3]
vertices tri = [tFirst tri, tSecond tri, tThird tri]

-- |Returns its vector argument rotated by the euler angles alp, bet, and gam.
-- Maybe using quaternions would be better.
rotate :: Float -> Float -> Float -> V3 -> V3
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

rotVert :: V3 -> Matrix Float -> V3
rotVert vert matr = toV (fromV vert * matr)
    where toV m  = V3 (m ! (1,1)) (m ! (1,2)) (m ! (1,3))
          fromV (V3 x y z) = fromList 1 3 [x, y, z]

-- |Try to intersect every triangle with a ray, without even trying to optimize.
naiveIntersect :: [Triangle] -> Ray -> Maybe Intersection
naiveIntersect tris ray =
    let intersections = catMaybes $ mollerTrumbore ray <$> tris
    in  case intersections of
            [] -> Nothing
            xs -> Just $ minimumBy (comparing dist) xs

mollerTrumbore :: Ray -> Triangle -> Maybe Intersection
mollerTrumbore ray tri
    | a > -eps && a < eps = Nothing
    | u < 0 || u > 1      = Nothing
    | v < 0 || u + v > 1  = Nothing
    | t > eps             = Just $ Intersection outInter rayDist tri
    | otherwise           = Nothing
    where
        rayVert = vertex ray
        rayDir  = direction ray
        vertex0 = tFirst tri
        vertex1 = tSecond tri
        vertex2 = tThird tri
        edge1 = vertex1 - vertex0
        edge2 = vertex2 - vertex0
        h = rayDir `cross` edge2
        a = edge1 `dot` h
        outInter = rayVert + t *^ rayDir
        f = 1 / a
        s = rayVert - vertex0
        u = f * (s `dot` h)
        q = s `cross` edge1
        v = f * rayDir `dot` q
        t = f * edge2 `dot` q
        rayDist = norm (outInter - rayVert)
        eps = 0.0001

-- | @a \`to\` b@ makes a Ray that points from @a@ to @b@.
{-# INLINE to #-}
to :: V3 -> V3 -> Ray
a `to` b = Ray a (b - a)

-- |An axis-aligned bounding box is uniquely defined by its minimum and maximum
-- extents on each axis.
-- For example, a cube centered at the origin with side length 1 is
-- (V3 -0.5 -0.5 -0.5, V3 0.5 0.5 0.5)
data Bounds = Bounds !V3 !V3 deriving Show

getBounds :: [V3] -> Bounds
getBounds verts =
    let xProject = map _x verts
        yProject = map _y verts
        zProject = map _z verts
        (minX, maxX) = (minimum xProject, maximum xProject)
        (minY, maxY) = (minimum yProject, maximum yProject)
        (minZ, maxZ) = (minimum zProject, maximum zProject)
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
averagePoints :: [V3] -> V3
averagePoints verts = vmap (/genericLength verts) (sum verts)

-- |The size of an axis-aligned bounding box along the given axis.
dim :: Axis -> Bounds -> Float
dim X (Bounds lo hi) = _x hi - _x lo
dim Y (Bounds lo hi) = _y hi - _y lo
dim Z (Bounds lo hi) = _z hi - _z lo

-- |The longest axis of an axis-aligned bounding box.
longestAxis :: Bounds -> Axis
longestAxis b =
    fst . maximumBy (comparing snd) $ zip [X,Y,Z] [dim X b, dim Y b, dim Z b]

-- |The smallest axis-aligned bounding box of a list of triangles.
boundingBox :: [Triangle] -> Bounds
boundingBox = getBounds . concatMap vertices

-- |Get the X, Y, or Z component of a vector.
projectToAxis :: Axis -> V3 -> Float
projectToAxis ax (V3 x y z) =
    case ax of
        X -> x
        Y -> y
        Z -> z

-- |The area of a triangle.
area :: Triangle -> Float
area (Triangle a b c _) = norm ((b - a) `cross` (c - a)) / 2
