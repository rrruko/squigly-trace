module BIH
    (BIH,
     Scene(..),
     boundingBox,
     flatten,
     height,
     longestLeaf,
     makeBIH,
     numLeaves,
     intersectsBB,
     intersectBIH
    ) where

--uwu
import Geometry

import Control.Lens ((^.))
import Data.List
import Data.Maybe
import Data.Ord
import Linear.V2
import Linear.V3
import Safe (maximumDef, minimumDef, minimumByMay)

data Scene = Scene { bounds :: Bounds, sceneBIH :: BIH } deriving (Show)

data Tree a b = Leaf b | Branch a (Tree a b) (Tree a b)
type BIH = Tree BIHNode [Triangle]
data Axis = X | Y | Z deriving (Show)
data BIHNode = BIHN Axis Float Float
    deriving (Show)

instance (Show a, Show b) => Show (Tree a b) where
    show t = show' t 0
        where show' (Leaf a) level = times level "  " ++ show a
              show' (Branch x l r) level =
                        times level     "  " ++ "Br " ++ show x ++
                "\n" ++ times (level+1) "  " ++ show' l (level+1) ++
                "\n" ++ times (level+1) "  " ++ show' r (level+1)
              times n xs = concat (replicate n xs)


height :: Tree a b -> Int
height (Leaf _)       = 1
height (Branch _ l r) = 1 + max (height l) (height r)

flatten :: Tree BIHNode [Triangle] -> [Triangle]
flatten (Leaf x) = x
flatten (Branch _ l r) = flatten l ++ flatten r

numLeaves :: Tree BIHNode [Triangle] -> Int
numLeaves (Branch _ l r) = numLeaves l + numLeaves r
numLeaves _ = 1

longestLeaf :: Tree a [b] -> Int
longestLeaf (Branch _ l r) = max (longestLeaf l) (longestLeaf r)
longestLeaf (Leaf l) = length l

{-
An axis-aligned bounding box can be defined by only two points,
where one is the low bound on each axis and the other is the high
bound on each axis. e.g. a cube centered at the origin with side
length 1 is (V3 -0.5 -0.5 -0.5, V3 0.5 0.5 0.5)
-}
type Bounds = (V3 Float, V3 Float)

makeBIH :: [Triangle] -> BIH
makeBIH tris = bih (boundingBox tris) tris

bih :: Bounds -> [Triangle] -> BIH
bih _    []     = Leaf []
bih _    [tri]  = Leaf [tri]
bih bbox geom
    | null leftTris  = Branch (BIHN axis lmax rmin)
                              (Leaf [])
                              (Leaf rightTris)
    | null rightTris = Branch (BIHN axis lmax rmin)
                              (Leaf leftTris)
                              (Leaf [])
    | otherwise = Branch (BIHN axis lmax rmin)
                         (bih (boundingBox leftTris ) leftTris )
                         (bih (boundingBox rightTris) rightTris)
    where (leftTris, lmax, rightTris, rmin, axis) = split bbox geom

split :: Bounds -> [Triangle] -> ([Triangle], Float, [Triangle], Float, Axis)
split bbox geom = (leftTris, lmax, rightTris, rmin, ax)
    where ax = longestAxis bbox
          leftTris  = filter underSplit         geom
          rightTris = filter (not . underSplit) geom
          leftSide  = projectToAxis ax (fst bbox)
          rightSide = projectToAxis ax (snd bbox)
          splitPlane = projectToAxis ax $ averagePoints
              [averagePoints (vertices tri) | tri <- geom]
          underSplit tri = (< splitPlane) . projectToAxis ax . averagePoints $
              vertices tri
          lmax = 0.001 + maximumDef leftSide
              [projectToAxis ax vert | vert <- leftTris  >>= vertices]
          rmin = (-0.001) + minimumDef rightSide
              [projectToAxis ax vert | vert <- rightTris >>= vertices]
          -- beware! removing the epsilons in lmax and rmin might
          -- cause some geometry to be missed due to being on the edge
          -- of a bounding box. they introduce no graphical distortion.

averagePoints :: [V3 Float] -> V3 Float
averagePoints verts = fmap (/genericLength verts) (sum verts)

-- genericLength [V3 1 1 1, V3 0 0 0] = 2.0
-- fmap (/2.0) $ sum [V3 1 1 1, V3 0 0 0]
-- fmap (/2.0) $ V3 1 1 1
-- V3 0.5 0.5 0.5

dimX, dimY, dimZ :: Bounds -> Float
dimX b = (snd b)^._x - (fst b)^._x
dimY b = (snd b)^._y - (fst b)^._y
dimZ b = (snd b)^._z - (fst b)^._z

longestAxis :: Bounds -> Axis
longestAxis b =
    fst . maximumBy (comparing snd) $ zip [X,Y,Z] [dimX b, dimY b, dimZ b]

boundingBox :: [Triangle] -> Bounds
boundingBox geom = getBounds . concatMap vertices $ geom

projectToAxis :: Axis -> V3 Float -> Float
projectToAxis ax (V3 x y z) =
    case ax of
        X -> x
        Y -> y
        Z -> z

{- Convenience function -}
intersectBIH :: Scene -> Ray -> Maybe Intersection
intersectBIH scene ray =
    intersectBIH' (bounds scene) (sceneBIH scene) ray

intersectBIH' :: Bounds -> BIH -> Ray -> Maybe Intersection
intersectBIH' _ (Leaf geom) ray =
        let intersections = catMaybes (map (intersectTri ray) geom)
        in  case intersections of
                [] -> Nothing
                _  -> Just (minimumBy (comparing dist) intersections)

{-
Recursively intersect each child in the order that the ray reaches it. I
previously wrote this:
| intersectsLeft && intersectsRight =
    case signum (projectToAxis ax (direction ray)) of
        1.0  -> intersectBIH' left l ray <|> intersectBIH' right r ray
        -1.0 -> intersectBIH' right r ray <|> intersectBIH' left l ray
        0.0  -> let intersections = catMaybes
                        [intersectBIH' left l ray, intersectBIH' right r ray]
                in  minimumByMay (comparing dist) intersections
This assumes that if the ray is going left to right and intersects both left
and right bounding boxes, then any intersections in the left bounding box will
be closer than those in the right one.

Which isn't correct, because boxes can overlap and so members of the right child
can be closer than members of the left child.
-}
intersectBIH' bbox (Branch (BIHN ax lmax rmin) l r) ray
    | intersectsLeft && intersectsRight =
        let intersections = catMaybes
                [intersectBIH' left l ray, intersectBIH' right r ray]
        in  minimumByMay (comparing dist) intersections
    | intersectsLeft  = intersectBIH' left l ray
    | intersectsRight = intersectBIH' right r ray
    | otherwise       = Nothing
    where intersectsLeft  = intersectsBB left ray
          intersectsRight = intersectsBB right ray
          left =
              let (low, V3 x y z) = bbox
              in  case ax of
                      X -> (low, V3 lmax y z)
                      Y -> (low, V3 x lmax z)
                      Z -> (low, V3 x y lmax)
          right =
              let (V3 x y z, high) = bbox
              in  case ax of
                      X -> (V3 rmin y z, high)
                      Y -> (V3 x rmin z, high)
                      Z -> (V3 x y rmin, high)

intersectsBB :: Bounds -> Ray -> Bool
intersectsBB (low, high) (Ray v dir _) =
    let dirfrac = fmap (1/) dir
        t1 = (low^._x  - v^._x) * dirfrac^._x
        t2 = (high^._x - v^._x) * dirfrac^._x
        t3 = (low^._y  - v^._y) * dirfrac^._y
        t4 = (high^._y - v^._y) * dirfrac^._y
        t5 = (low^._z  - v^._z) * dirfrac^._z
        t6 = (high^._z - v^._z) * dirfrac^._z
        tmin = maximum [min t1 t2, min t3 t4, min t5 t6]
        tmax = minimum [max t1 t2, max t3 t4, max t5 t6]
    in  tmax > 0 && tmin < tmax

getBounds :: [V3 Float] -> Bounds
getBounds verts =
    let xProject = map (^._x) verts
        yProject = map (^._y) verts
        zProject = map (^._z) verts
        [minX, minY, minZ] = map minimum [xProject, yProject, zProject]
        [maxX, maxY, maxZ] = map maximum [xProject, yProject, zProject]
    in  (V3 minX minY minZ, V3 maxX maxY maxZ)

{-projectToPlane :: Axis -> V3 Float -> V2 Float
projectToPlane ax (V3 x y z) = case ax of
    X -> V2 y z
    Y -> V2 x z
    Z -> V2 x y-}
