module KdTree where

import Geometry

import Data.Semigroup
import Data.Vector (Vector, empty)
import qualified Data.Vector as V
import Linear.V3

data KdTree = KdLeaf (Vector Triangle) | KdTree AABB Float KdTree KdTree
type AABB = (V3 Float, V3 Float)

next :: Axis -> Axis
next X = Y
next Y = Z
next Z = X

data Side = Under | Over

build :: Vector Triangle -> KdTree
build tris = go X (getBounds tris) tris where
    go axis bounds tris =
        let newAxis = next axis
            bestSplit = undefined
            leftTris = clip Under bestSplit axis tris
            rightTris = clip Over bestSplit axis tris
            leftBounds = getBounds leftTris
            rightBounds = getBounds rightTris
            left
                | small leftBounds = KdLeaf leftTris
                | otherwise = go newAxis leftBounds leftTris
            right
                | small rightBounds = KdLeaf rightTris
                | otherwise = go newAxis rightBounds rightTris
        in  KdTree bounds bestSplit left right

splitBy :: Vector Triangle -> Float
splitBy f tris = undefined

split :: Float -> Vector Triangle -> (Vector Triangle, Vector Triangle)
split splitAt tris = undefined

clip :: Side -> Float -> Axis -> Vector Triangle -> Vector Triangle
clip side splitAt axis tris =
    let project = map (projectToAxis axis)
        unders = V.filter (all (<= splitAt) . project . vertices) tris
        overs  = V.filter (all (>  splitAt) . project . vertices) tris
        crossing = undefined
    in  case side of
            Under -> unders <> doClips Under crossing
            Over -> overs <> doClips Over crossing

clip' :: Side -> Float -> Axis -> Triangle -> [Triangle]
clip' side splitAt axis t@(Triangle a b c mat) =
    case intersectPlane <$> [(a, b), (b, c), (c, a)] of
        [Nothing, Just bc, Just ca] -> makeTris c a b n m
        [Just ab, Nothing, Just ca] -> makeTris a b c n m
        [Just ab, Just bc, Nothing] -> makeTris 
        _     -> error $ show t <> " did not intersect " <> show axis <> ", " <> show splitAt <> " exactly twice."
    where makeTris a b c n m = 
        [ Triangle a n m mat
        , Triangle 

doClips :: Side -> Vector Triangle -> Vector Triangle
doClips = undefined

intersect :: KdTree -> Ray -> Maybe Intersection
intersect = go X where
    go axis kdtree ray = undefined

{-

write a test for: 
    clip X 0.5 (Triangle (V3 0 0 0) (V3 1 0 0) (V3 1 1 0)) ~
      [ Triangle (V3 0 0 0) (V3 0.5 0 0) (V3 0.5 0.5 0)
      , Triangle something
      , Triangle something else
      ]

make sure you use ~ instead of =
or something like that
because you might not get exactly 0.5 and
the triangles might be in a different order
or have verts in a different order
idk

-}
