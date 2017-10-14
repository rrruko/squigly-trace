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

import Control.Lens ((^.))
import Data.List
import Data.Maybe
import Data.Ord
import Linear.V2
import Linear.V3
import Safe (maximumDef, minimumDef, minimumByMay)
import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Semigroup

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

data Axis = X | Y | Z
    deriving (Show)
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

{-
An axis-aligned bounding box can be defined by only two points,
where one is the low bound on each axis and the other is the high
bound on each axis. e.g. a cube centered at the origin with side
length 1 is (V3 -0.5 -0.5 -0.5, V3 0.5 0.5 0.5)
-}
type Bounds = (V3 Float, V3 Float)

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

averagePoints :: [V3 Float] -> V3 Float
averagePoints verts = fmap (/genericLength verts) (sum verts)

-- genericLength [V3 1 1 1, V3 0 0 0] = 2.0
-- fmap (/2.0) $ sum [V3 1 1 1, V3 0 0 0]
-- fmap (/2.0) $ V3 1 1 1
-- V3 0.5 0.5 0.5

dimX, dimY, dimZ :: Bounds -> Float
dimX b = snd b ^. _x - fst b ^. _x
dimY b = snd b ^. _y - fst b ^. _y
dimZ b = snd b ^. _z - fst b ^. _z

longestAxis :: Bounds -> Axis
longestAxis b =
    fst . maximumBy (comparing snd) $ zip [X,Y,Z] [dimX b, dimY b, dimZ b]

boundingBox :: [Triangle] -> Bounds
boundingBox = getBounds . concatMap vertices

projectToAxis :: Axis -> V3 Float -> Float
projectToAxis ax (V3 x y z) =
    case ax of
        X -> x
        Y -> y
        Z -> z

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
        let intersections = catMaybes [intersectBIH' left l ray, intersectBIH' right r ray]
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

intersectsBB' :: Bounds -> Ray -> Bool
intersectsBB' (low, high) (Ray v dir _) =
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

{-
This cuts run time by a few percentage points over intersectsBB'

Intersecting bounding boxes is taking up the most time in this program, as
one would expect.

I ran a profile with a resolution of 540x540 with only 1 sample.
This function was entered 59809853 times, which gives an average of 205 entries
per ray. I would have expected about 20! The best possible would be about
log2(6000) = 13, but that's only if 1) the boxes always split the candidate
triangles in half and 2) every ray intersects only one box, rather than
several, which are both unrealistic expectations. Still, 200 seems very wrong.

[ruko ~ 20:38] $ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> let intersections = 59809853
Prelude> let rays = 540 * 540 * 1 -- 540x540px, 1 sample
Prelude> rays
291600
Prelude> intersections/rays
205.10923525377228

This is progress, as I was previously worried that the function is just too
slow. The new knowledge that boxes are being intersected too often indicates
I should... maybe just try a different acceleration structure.

Though I should also refactor this whole program a lot, because it's a mess

Is the scene I've been using just pathological?
    It might be since it consists of a box with a lot of stuff inside it.
    However, a good acceleration structure should be able to handle that.
Is my BIH construction algorithm fucky?
    Seems likely, since it's a complex algorithm and I implemented it sloppily.
Some other third thing?
    Probably, because computers hate me.
-}
intersectsBB :: Bounds -> Ray -> Bool
intersectsBB ((V3 lx ly lz), (V3 hx hy hz)) (Ray (V3 vx vy vz) (V3 dirx diry dirz) _) =
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

getBounds :: [V3 Float] -> Bounds
getBounds verts =
    let xProject = map (^._x) verts
        yProject = map (^._y) verts
        zProject = map (^._z) verts
        [minX, minY, minZ] = map minimum [xProject, yProject, zProject]
        [maxX, maxY, maxZ] = map maximum [xProject, yProject, zProject]
    in  (V3 minX minY minZ, V3 maxX maxY maxZ)
