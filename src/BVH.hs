-- This module was ported from brandonpelfrey/Fast-BVH on github

module BVH (
    bvh,
    intersect
) where

import Geometry

import Control.Monad (when)
import Control.Monad.ST
import Data.STRef
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

              | stackptr > 0 = runST $ do
                  stackPtrRef <- newSTRef stackptr
                  modifySTRef stackPtrRef pred
                  stackPtrNew <- readSTRef stackPtrRef
                  let bnode   = todo ! stackPtrNew
                      startn  = start bnode
                      endn    = end bnode
                      nPrimsn = endn - startn
                      bb      = bound    (range startn endn tris)
                      bc      = centroid (range startn endn tris)
                  nLeavesRef <- newSTRef nLeaves_
                  node <- newSTRef (Node bb startn nPrimsn untouched)
                  when (nPrimsn <= leafSize_) $
                      modifySTRef nLeavesRef succ
                  nLeavesNew <- readSTRef nLeavesRef

                  pure $ go todo buildnodes stackPtrNew nLeavesNew nNodes_

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
