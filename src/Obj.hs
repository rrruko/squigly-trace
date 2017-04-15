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
import Geometry hiding (vertex)

import qualified Debug.Trace as Trace
import Linear.V3
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Combinator

-- |Load the material referenced by a .obj file and generate a Scene.
sceneFromObj :: String -> IO Scene
sceneFromObj str = do
    case parse loadObjFile "" str of
        Right (mtllib, objs) -> do
            mtlFile <- readFile ("./data/" ++ mtllib)
            case parse loadMtlFile "" mtlFile of
                Right mats -> do
                    let triangles = makeScene objs mats
                    Trace.trace (show $ head objs) (return ())
                    Trace.trace (show mats) (return ())
                    return $ Scene (boundingBox triangles) (makeBIH triangles)
                Left _ -> fail "sceneFromObj: Couldn't parse referenced MTL file."
        Left _ -> fail "sceneFromObj: Couldn't parse OBJ file."

-- |Match each object with it's material and return the resulting scene.
makeScene :: [Object] -> [(String, Material Float)] -> [Triangle]
makeScene objs mats =
    let matches = [(obj, mat) | obj <- objs, mat <- mats, mtl obj == fst mat]
    in  concatMap (makeTris allVerts) matches
        where allVerts = concatMap verts objs

-- |Convert the object to a list of triangles all having the given material.
makeTris :: [V3 Float] -> (Object, (String, Material Float)) -> [Triangle]
makeTris vertices (obj, mat) = map makeTri $ faces obj
    where makeTri (V3 a b c) =
              Triangle (vertices !! (a-1))
                       (vertices !! (b-1))
                       (vertices !! (c-1))
                       (snd mat)


data Object = Object {
    name :: String,
    mtl :: String,
    verts :: [V3 Float],
    faces :: [V3 Int]
} deriving Show

loadObjFile :: Stream s m Char => ParsecT s u m (String, [Object])
loadObjFile = do
  lib <- mtllib
  objs <- many parseObj
  return (lib, objs)

parseObj :: Stream s m Char => ParsecT s u m Object
parseObj = do
  name <- objectName
  verts <- many vertex
  mtl <- materialName
  optional parseS
  faces <- many face
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
    return $ fmap read (V3 v1 v3 v2)

fractional :: Stream s m Char => ParsecT s u m String
fractional = do
    m <- option "" $ string "-"
    d1 <- many digit
    p <- option "" $ string "."
    d2 <- many digit
    return $ m ++ d1 ++ p ++ d2

materialName :: Stream s m Char => ParsecT s u m String
materialName = do
    string "usemtl"
    spaces
    mtl <- many1 $ noneOf " \t\n\r\f\v"
    spaces
    return mtl

mtllib :: Stream s m Char => ParsecT s u m String
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

loadMtlFile :: Stream s m Char => ParsecT s u m [(String, Material Float)]
loadMtlFile = many loadMtl

loadMtl :: Stream s m Char => ParsecT s u m (String, Material Float)
loadMtl = do
    name <- (string "newmtl " >> many (noneOf " \t\n\r\f\v"))
    spaces
    optional surfaceType
    string "RGB "
    col <- v3
    string "Em "
    emit <- v3
    spaces
    return (name, Mat (toRGB col) (toRGB emit))
        where toRGB (V3 a b c) = RGB a b c

v3 :: Stream s m Char => ParsecT s u m (V3 Float)
v3 = do
    v1 <- fractional
    spaces
    v2 <- fractional
    spaces
    v3 <- fractional
    spaces
    return $ fmap read (V3 v1 v2 v3)

-- Diffuse/reflect not yet implemented but it might be in the file
surfaceType :: Stream s m Char => ParsecT s u m ()
surfaceType = do
    string "Type "
    _ <- try (string "Diffuse") <|> try (string "Reflect") <|> string "Emit"
    spaces
