{-|
Module      : Obj
Description : Loader for .obj files
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}

{-# LANGUAGE FlexibleContexts #-}

module Obj
    (sceneFromObj
    ) where

import Color
import BIH hiding (bounds)
import Geometry

import Linear.V3

-- |Turn an obj file into a Scene.
sceneFromObj :: String -> Scene
sceneFromObj str =
    let triangles = makeFaces str ++ uwu
        bounds = boundingBox triangles
    in  Scene bounds (makeBIH triangles)
    where uwu =
            let verts = [V3 (-1) 1 2.05, V3 1 1 2.05, V3 (-1) (-1) 2.05, V3 1 (-1) 2.05]
            in  [Triangle (verts!!1) (verts!!3) (verts!!2) (whiteLight 500)
                ,Triangle (verts!!0) (verts!!1) (verts!!2) (whiteLight 500)
                ]
    -- very bad

{-
sceneFromObj :: String -> IO Scene
sceneFromObj str = do
    let (mtllib, objs) = parse loadObj "" str
    let materials      = parse loadMtl "" mtllib
    return $ makeScene objs materials

makeScene :: [Object] -> [Material] -> Scene
makeScene objs mats =
    let sortedObjs = sortOn name

data Object = Object {
    name :: String,
    mtlpath :: String,
    verts :: [V3 Float],
    faces :: [V3 Int]
} deriving Show

loadObj :: Stream s m Char => String -> ParsecT s u m (String, [Object])
loadObj f = do
  lib <- mtllib
  objs <- many parseObj
  return (lib, objs)

parseObj :: Stream s m Char => ParsecT s u m Object
parseObj = do
  name <- objectName
  verts <- many1 vertex
  mtl <- material
  optional parseS
  faces <- many1 face
  return $ Object name mtl verts faces

objectName :: Stream s m Char => ParsecT s u m String
objectName = do
    char 'o'
    spaces
    name <- many1 (alphaNum <|> oneOf "._") -- many1 $ noneOf " \t\n\r\f\v"
    spaces
    return name

vertex :: Stream s m Char => ParsecT s u m (V3 Float)
vertex = do
    char 'v'
    spaces
    v1 <- fractional
    spaces
    v2 <- fractional
    spaces
    v3 <- fractional
    spaces
    return $ fmap read (V3 v1 v2 v3)
        where fractional = do
                  m <- option "" $ string "-"
                  d1 <- many digit
                  p <- option "" $ string "."
                  d2 <- many digit
                  return $ m ++ d1 ++ p ++ d2


material :: Stream s m Char => ParsecT s u m String
material = do
    string "usemtl"
    spaces
    mtl <- many1 $ noneOf " \t\n\r\f\v"
    spaces
    return mtl

mtllib :: Stream s m Char =? ParsecT s u m String
mtllib = do
    string "mtllib"
    spaces
    filename <- many1 $ noneOf " \t\n\r\f\v"
    spaces
    return filename

parseS :: Stream s m Char => ParsecT s u m ()
parseS = do
    try (string "s on") <|> string "s off"
    spaces

face :: Stream s m Char => ParsecT s u m (V3 Int)
face = do
    char 'f'
    spaces
    f1 <- many1 digit
    spaces
    f2 <- many1 digit
    spaces
    f3 <- many1 digit
    spaces
    return $ fmap readInt (V3 f1 f2 f3)
        where readInt x = read x :: Int

loadMtl :: Stream s m Char => ParsecT s u m [Material Float]
loadMtl = do
    name <- (string "newmtl " >> noneOf " \t\n\r\f\v" >> spaces)
    satisfy (not . "Kd" `elem` . ta
-}

{- bad old code -}
objVerts :: String -> [V3 Float]
objVerts file =
    let vertLines = filter (startsWith 'v') $ lines file
    in  map lineToVec vertLines

lineToVec :: String -> V3 Float
lineToVec = listToVec . map readF . tail . words
    where readF s = (read s :: Float)

listToVec :: [Float] -> V3 Float
listToVec ls = V3 (ls !! 0) (ls !! 2) (ls !! 1)

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
indexesToFace verts (i1, i2, i3) =
    Triangle (verts !! (i1 - 1))
             (verts !! (i2 - 1))
             (verts !! (i3 - 1))
             (diffuse (RGB 0 (1/4) (1/16)))
