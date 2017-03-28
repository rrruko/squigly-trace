{-|
Module      : Geometry
Description : Geometric types based on V3s
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Geometry
    (Ray(..), Scene(..), Triangle(..), normal, rotate, rotMatrixRads, rotVert
    ) where

import Color(Material)
import Linear.V3
import Data.Matrix (Matrix, (!), fromList)

data Ray = Ray { vertex :: V3 Float, direction :: V3 Float, bounces :: Int } deriving (Show)

data Scene = Scene { verts :: [V3 Float], tris :: [Triangle] } deriving (Show)

-- is describing triangles as indices into their vertices worth passing around the vertex list?
-- if you're going to index into it, you should make it an array (vector)
data Triangle = Triangle { indFst :: Int, indSnd :: Int, indThd :: Int, material :: Material Float } deriving (Show)

getFirst :: [V3 Float] -> Triangle -> V3 Float
getFirst verts tri = verts !! (indFst tri)

getSecond :: [V3 Float] -> Triangle -> V3 Float
getSecond verts tri = verts !! (indSnd tri)

getThird :: [V3 Float] -> Triangle -> V3 Float
getThird verts tri = verts !! (indThd tri)

normal :: [V3 Float] -> Triangle -> V3 Float
normal verts t = (getSecond verts t - getFirst verts t) `cross` (getThird verts t - getFirst verts t)

-- |Returns its vector argument rotated by the euler angles alp, bet, and gam
rotate :: Float -> Float -> Float -> V3 Float -> V3 Float
rotate alp bet gam vert = rotVert vert (rotMatrixRads alp bet gam)

rotMatrixRads :: Float -> Float -> Float -> Matrix Float
rotMatrixRads alp bet gam = foldl1 (*) . map (fromList 3 3) $
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
maybeIntersect :: Ray -> Triangle -> Maybe (V3 Float)
maybeIntersect ray tri
-}