-- This module was ported from brandonpelfrey/Fast-BVH on github

module BVH (
    bvh,
    intersect
) where

import Geometry

import Data.Vector ((!), (//), Vector)
import qualified Data.Vector as V

data BVH = BVH {
    nNodes :: Int,
    nLeaves :: Int,
    leafSize :: Int,
    tris :: Vector Triangle,
    flatTree :: Vector BVHFlatNode
}

data BVHFlatNode = Node {
    bbox :: Bounds,
    nStart :: Int,
    nPrims :: Int,
    rightOffset :: Int
}

data BVHBuildEntry = Entry { parent :: Int, start :: Int, end :: Int }

def :: BVHBuildEntry
def = Entry 0 0 0

bvh :: Vector Triangle -> Int -> BVH
bvh = undefined

build :: Vector Triangle -- ^ Input traceables
      -> Int -- ^ Leaf size
      -> Int -- ^ Leaf count (?)
      -> Int -- ^ Node count (?)
      -> BVH
build tris leafSize_ nLeaves_ nNodes_ =

    let buildnodes = V.empty -- ^ This has its size reserved in the original...
        todo       = V.replicate 128 def
        newEntry   = Entry 0xfffffffc 0 (length tris)
        todo'      = todo // [(0, newEntry)]
    in  go todo' buildnodes 1 0 0

    where go todo buildnodes stackptr nLeaves_ nNodes_

              | stackptr > 0 =
                  let bnode   = todo ! (stackptr - 1)
                      startn  = start bnode
                      endn    = end bnode
                      nPrimsn = endn - startn
                      bb      = bound    (range startn endn tris)
                      bc      = centroid (range startn endn tris)
                      node    = Node bb startn nPrimsn untouched
                  in  if nPrimsn <= leafSize_ then
                          -- This is a leaf! We need to increment nLeaves_ if we get here
                          let node = Node bb startn nPrimsn 0
                          in  if parent bnode /= 0xfffffffc then
                                  undefined -- child touches parent
                              else
                                  undefined
                      else
                          undefined

              | otherwise = BVH nNodes_
                                nLeaves_
                                leafSize_
                                tris
                                (makeFlatTree buildnodes)

range = undefined
centroid = undefined
bound = undefined
makeFlatTree = undefined

untouched = 0xffffffff
touchedTwice = 0xfffffffd

intersectBVH :: BVH
             -> Ray
             -> Bool
             -> Maybe Intersection
intersectBVH = undefined
