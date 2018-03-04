module BIH
    (BIH(..),
     Scene(..),
     boundingBox,
     flatten,
     height,
     longestLeaf,
     makeBIH,
     numLeaves,
     intersectsBB,
     intersectBIH,
     pretty,
    ) where

import           Geometry

import           Data.List
import           Data.Maybe
import           Data.Ord       (comparing)
import           Data.Semigroup
import           Data.Vector    (Vector)
import qualified Data.Vector    as V
import           Linear.V3
import           Safe           (maximumDef, minimumByMay, minimumDef)

data Tree a b = Leaf b | Branch a (Tree a b) (Tree a b) deriving (Show)

pretty :: Scene BIH -> String
pretty scene = show' (tree $ geometry scene) 0
    where show' (Leaf a) level = times level "  " ++ show a
          show' (Branch x l r) level =
                    times level     "  " ++ "Br " ++ show x ++
            "\n" ++ times (level+1) "  " ++ show' l (level+1) ++
            "\n" ++ times (level+1) "  " ++ show' r (level+1)
          times n xs = concat (replicate n xs)

data BIHNode = BIHN Axis Float Float
    deriving (Show)

type BIHTree = Tree BIHNode (Vector Triangle)
data BIH = BIH {
    bounds :: Bounds,
    tree   :: BIHTree
} deriving (Show)

height :: Tree a b -> Int
height (Leaf _)       = 1
height (Branch _ l r) = 1 + max (height l) (height r)

flatten :: BIHTree -> Vector Triangle
flatten (Leaf x)       = x
flatten (Branch _ l r) = flatten l <> flatten r

numLeaves :: BIHTree -> Int
numLeaves (Branch _ l r) = numLeaves l + numLeaves r
numLeaves _              = 1

longestLeaf :: Tree a (Vector b) -> Int
longestLeaf (Branch _ l r) = max (longestLeaf l) (longestLeaf r)
longestLeaf (Leaf l)       = V.length l

makeBIH :: [Triangle] -> BIH
makeBIH tris =
    let bbox = boundingBox tris
    in  BIH bbox (bih bbox (V.fromList tris))

bih :: Bounds -> Vector Triangle -> BIHTree
bih bbox geom
    | length geom < leafLimit = Leaf geom
    | null leftTris = Branch (BIHN axis lmax rmin)
                             (Leaf V.empty)
                             (Leaf $ V.fromList rightTris)
    | null rightTris = Branch (BIHN axis lmax rmin)
                              (Leaf $ V.fromList leftTris)
                              (Leaf V.empty)
    | otherwise = Branch (BIHN axis lmax rmin)
                         (bih (boundingBox leftTris) $ V.fromList leftTris)
                         (bih (boundingBox rightTris) $ V.fromList rightTris)
    where (leftTris, lmax, rightTris, rmin, axis) = split bbox $ V.toList geom
          leafLimit = 15

split :: Bounds -> [Triangle] -> ([Triangle], Float, [Triangle], Float, Axis)
split bbox@(Bounds lo hi) geom = (leftTris, lmax, rightTris, rmin, ax)
    where ax = longestAxis bbox
          leftTris  = filter underSplit         geom
          rightTris = filter (not . underSplit) geom
          leftSide  = projectToAxis ax lo
          rightSide = projectToAxis ax hi
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

intersectBIH :: BIH -> Ray -> Maybe Intersection
intersectBIH b = intersectBIH' (bounds b) (tree b)

intersectBIH' :: Bounds -> BIHTree -> Ray -> Maybe Intersection
intersectBIH' _ (Leaf geom) ray =
    let intersections = V.mapMaybe (mollerTrumbore ray) geom
    in  if V.null intersections
            then Nothing
            else Just $ minimumBy (comparing dist) intersections

intersectBIH' bbox (Branch (BIHN ax lmax rmin) l r) ray
    | not (intersectsBB bbox ray) = Nothing
    | intersectsLeft && intersectsRight = case near of
        Just n | isClose n -> near
        Just n  -> minimumByMay (comparing dist) intersections
        Nothing -> far
    | intersectsLeft  = intersectBIH' left l ray
    | intersectsRight = intersectBIH' right r ray
    | otherwise       = Nothing
        where intersections = catMaybes [near, far]
              isClose v
                  | leftToRight = projectToAxis ax (intersectPoint v) < rmin
                  | otherwise   = projectToAxis ax (intersectPoint v) > lmax
              [near, far]
                  | leftToRight = [intersectBIH' left l ray, intersectBIH' right r ray]
                  | otherwise   = [intersectBIH' right r ray, intersectBIH' left l ray]
              leftToRight = projectToAxis ax (direction ray) > 0
              intersectsLeft  = intersectsBB left ray
              intersectsRight = intersectsBB right ray
              left =
                  let Bounds low (V3 x y z) = bbox
                  in  case ax of
                          X -> Bounds low (V3 lmax y z)
                          Y -> Bounds low (V3 x lmax z)
                          Z -> Bounds low (V3 x y lmax)
              right =
                  let Bounds (V3 x y z) high = bbox
                  in  case ax of
                          X -> Bounds (V3 rmin y z) high
                          Y -> Bounds (V3 x rmin z) high
                          Z -> Bounds (V3 x y rmin) high
