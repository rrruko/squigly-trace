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

import Geometry

import Data.List
import Data.Maybe
import Data.Ord (comparing)
import Linear.V2
import Linear.V3
import Safe (maximumDef, minimumDef, minimumByMay)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Semigroup
import Debug.Trace

data Scene = Scene { bounds :: Bounds, sceneBIH :: BIH } deriving (Show)

data Tree a b = Leaf b | Branch a (Tree a b) (Tree a b) deriving (Show)

pretty :: BIH -> String
pretty t = show' t 0
    where show' (Leaf a) level = times level "  " ++ show a
          show' (Branch x l r) level =
                    times level     "  " ++ "Br " ++ show x ++
            "\n" ++ times (level+1) "  " ++ show' l (level+1) ++
            "\n" ++ times (level+1) "  " ++ show' r (level+1)
          times n xs = concat (replicate n xs)

data BIHNode = BIHN Axis Float Float
    deriving (Show)

type BIH = Tree BIHNode (Vector Triangle)

height :: Tree a b -> Int
height (Leaf _)       = 1
height (Branch _ l r) = 1 + max (height l) (height r)

flatten :: BIH -> Vector Triangle
flatten (Leaf x) = x
flatten (Branch _ l r) = flatten l <> flatten r

numLeaves :: BIH -> Int
numLeaves (Branch _ l r) = numLeaves l + numLeaves r
numLeaves _ = 1

longestLeaf :: Tree a (Vector b) -> Int
longestLeaf (Branch _ l r) = max (longestLeaf l) (longestLeaf r)
longestLeaf (Leaf l) = V.length l

makeBIH :: [Triangle] -> BIH
makeBIH tris = bih (boundingBox tris) (V.fromList tris)

bih :: Bounds -> Vector Triangle -> BIH
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

{- Convenience function -}
intersectBIH :: Scene -> Ray -> Maybe Intersection
intersectBIH scene =
    intersectBIH' (bounds scene) (sceneBIH scene)

intersectBIH' :: Bounds -> BIH -> Ray -> Maybe Intersection
intersectBIH' _ (Leaf geom) ray =
    let intersections = V.mapMaybe (intersectTri ray) geom
    in  if V.null intersections
            then Nothing
            else Just $ minimumBy (comparing dist) intersections

intersectBIH' bbox (Branch (BIHN ax lmax rmin) l r) ray
    | intersectsLeft && intersectsRight =
        let intersections = catMaybes $
                if leftToRight then [intersectBIH' left l ray, intersectBIH' right r ray]
                               else [intersectBIH' right r ray, intersectBIH' left l ray]
        in  minimumByMay (comparing dist) intersections
    | intersectsLeft  = intersectBIH' left l ray
    | intersectsRight = intersectBIH' right r ray
    | otherwise       = Nothing
        where leftToRight = projectToAxis ax (direction ray) > 0
              intersectsLeft  = intersectsBB left ray
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
