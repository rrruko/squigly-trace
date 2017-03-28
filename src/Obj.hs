{-|
Module      : Obj
Description : Loader for .obj files
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}
module Obj
    (sceneFromObj, testScene
    ) where

import Data.Matrix (Matrix)
import Color
import Geometry
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


{- Scene data; this will all be removed when loading obj files works better -}
testRotMatrix :: Matrix Float
testRotMatrix = rotMatrixRads (pi/8) (pi/16) (pi/16)

testSceneVerts :: [V3 Float]
testSceneVerts = map ((+ (V3 1 0 0)) . (`rotVert` testRotMatrix))
    [V3 1    0    0
    ,V3 0    (-1) 0
    ,V3 (-1) 0    0
    ,V3 0    1    0
    ,V3 0    0    1
    ,V3 0    0    (-1)
    ]

testScene :: Scene
testScene = Scene
    [ Triangle v2 v1 v5 (Mat col noEmission)
    , Triangle v3 v2 v5 (Mat col noEmission)
    , Triangle v4 v3 v5 (Mat col noEmission)
    , Triangle v1 v4 v5 (Mat col noEmission)
    , Triangle v1 v2 v6 (Mat col noEmission)
    , Triangle v2 v3 v6 (Mat col noEmission)
    , Triangle v3 v4 v6 (Mat col noEmission)
    , Triangle v4 v1 v6 (Mat col noEmission)
    , Triangle (V3 10 10 (-2)) (V3 10 (-10) (-2)) (V3 (-10) 0 (-2)) (Mat white noEmission)
    , Triangle (V3 10 10 (-2)) (V3 10 (-10) (-2)) (V3 10    0 10  ) (Mat white noEmission)
    , Triangle (V3 (-1) 0 2) (V3 1 (-1) 2) (V3 1 1 2) (Mat white (RGB 50 50 50))
    ]
    where col = RGB 0.25 0 0.125
          noEmission = black
          v1 = testSceneVerts !! 0
          v2 = testSceneVerts !! 1
          v3 = testSceneVerts !! 2
          v4 = testSceneVerts !! 3
          v5 = testSceneVerts !! 4
          v6 = testSceneVerts !! 5

testScene' :: Scene
testScene' = Scene
    [ t 1 3 2 (Mat black (RGB 1 1 1))
    , t 0 1 2 (Mat black (RGB 1 1 1))
    , t 5 7 6 (Mat black (RGB 1 1 1))
    , t 4 5 6 (Mat black (RGB 1 1 1))
    ]
    where verts =
            [ V3 (-0.5) (-0.5) (-0.2), V3 0.5 (-0.5) (-0.2), V3 (-0.5) 0.5 (-0.2), V3 0.5 0.5 (-0.2)
            , V3 (-0.5) (-0.5) (0.2),  V3 0.5 (-0.5) (0.2),  V3 (-0.5) 0.5 0.2, V3 0.5 0.5 0.2]
          t x y z = Triangle (verts!!x) (verts!!y) (verts!!z)
{- End of scene data -}