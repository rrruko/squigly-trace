{-|
Module      : Obj
Description : Loader for .obj files
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Obj
    (sceneFromObj
    ) where

import Color (Material(..), RGB(..), black, gray, white)
import Geometry (Scene(..), Triangle(..))
import Linear.V3

-- |Turn an obj file into a Scene.
sceneFromObj :: String -> Scene
sceneFromObj str = Scene $ makeFaces str

objVerts :: String -> [V3 Float]
objVerts file = 
    let vertLines = filter (startsWith 'v') $ lines file
    in  map lineToVec vertLines

lineToVec :: String -> V3 Float
lineToVec = listToVec . map readF . tail . words
    where readF s = (read s :: Float)

listToVec :: [Float] -> V3 Float
listToVec ls = V3 (ls !! 0) (ls !! 1) (ls !! 2) 

makeFaces :: String -> [Triangle]
makeFaces file =
    let verts = objVerts file
        faceLines = filter (startsWith 'f') $ lines file
    in  map (lineToFace verts) faceLines

startsWith :: Eq a => a -> [a] -> Bool
startsWith c s = if s == [] then False else head s == c

lineToFace :: [V3 Float] -> String -> Triangle
lineToFace verts line = 
    let indexList = map readInt . tail . words $ line
        is = (indexList !! 0, indexList !! 1, indexList !! 2)
    in  indexesToFace verts is
    where readInt s = (read s :: Int)

indexesToFace :: [V3 Float] -> (Int, Int, Int) -> Triangle
indexesToFace verts (i1, i2, i3) = Triangle (verts!!(i1-1)) (verts!!(i2-1)) (verts!!(i3-1)) (Mat gray white)