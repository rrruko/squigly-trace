{-|
Module      : Obj
Description : Loader for .obj files
Maintainer  : rukokarasu@gmail.com
Stability   : experimental
-}

module Obj
    ( loadCamera
    , trisFromObj
    ) where

import           Color              (Material (..), RGB)
import           Geometry           hiding (vertex)

import           Control.Monad      (void, when)
import           Linear.V3
import           Text.Parsec
import           Text.Parsec.String

{-
This module reads .obj files as generated by Blender. Any mtl files referenced
by the .obj file are assumed to be in the same directory as the .obj file.
As of right now, the commented headers must be manually removed in order to
parse. Also, materials are described using a special .sq file format. Each
material should have an entry that looks like this:

newmtl NAME
reflective REF R G B
emissive EM R G B

where NAME is any string containing alphanumeric characters or . or _
      REF indicates the degree of reflectiveness (0-1)
      EM indicates the brightness of the emitted light (0-1)
      R, G and B are the red, green, and blue components
      RGB should be 0-1 for REF and can be any positive number for EM

squigly-trace doesn't use mtl files bc that format is based on Phong shading
and this is a raytracer, hence only the diffuse color can be relevant and the
rest is just noise. My test models don't need many materials so it's not too
much work to just manually enter the material data.

I expect this to change when I start to use more complex materials, in
particular when i want to make something not fully reflective or add refraction
or something.
-}

-- |Load the material referenced by a .obj file and generate triangles.
trisFromObj :: Bool -> String -> IO [Triangle]
trisFromObj debug str = do
    let Right (mtllib', objs) = parse loadObjFile "" str
    mtlFile <- readFile ("./data/" ++ mtllib')
    let Right mats = parse loadMtlFile "" mtlFile
    let triangles = makeScene objs mats
    when debug $ do
        print (head objs)
        print mats
    pure triangles

loadCamera :: FilePath -> IO Camera
loadCamera path = do
    cameraFile <- readFile path
    case parse parseCamera "" cameraFile of
        Right cam -> pure cam
        Left  _   -> error "Failed to parse /data/camera"

parseCamera :: Parser Camera
parseCamera = Camera <$> vec3 <*> rot
    where rot = unpackRotMatrix <$> vec3
          unpackRotMatrix (V3 rx ry rz) = rotMatrixRads rx ry rz

-- |Match each object with its material and return the resulting scene.
makeScene :: [Object] -> [(String, Material)] -> [Triangle]
makeScene objs mats =
    let matches = [(obj, mat) | obj <- objs, mat <- mats, mtl obj == fst mat]
        allVerts = concatMap verts objs
    in  concatMap (makeTris allVerts) matches

-- |Convert the object to a list of triangles all having the given material.
makeTris :: [V3 Float] -> (Object, (String, Material)) -> [Triangle]
makeTris vs (obj, mat) = map makeTri $ faces obj
    where makeTri (V3 a b c) =
              Triangle (vs !! (a-1))
                       (vs !! (b-1))
                       (vs !! (c-1))
                       (snd mat)

data Object = Object {
    verts :: [V3 Float],
    mtl   :: String,
    faces :: [V3 Int]
} deriving Show

loadObjFile :: Parser (String, [Object])
loadObjFile = (,) <$> mtllib <*> many parseObj

parseObj :: Parser Object
parseObj = Object
    <$> (objectName *> many vertex)
    <*> (materialName <* optional parseS)
    <*> many face

objectName :: Parser String
objectName = char 'o' *> spaces
    *> many1 (alphaNum <|> oneOf "._") <* spaces

vertex :: Parser (V3 Float)
vertex = char 'v' *> spaces *> fmap swapYZ vec3

swapYZ :: V3 Float -> V3 Float
swapYZ (V3 x y z) = V3 x z y

fractional :: Parser Float
fractional = do
    m <- option "" $ string "-"
    d1 <- many digit
    p <- option "" $ string "."
    d2 <- many digit
    pure . read $ m ++ d1 ++ p ++ d2

materialName :: Parser String
materialName = string "usemtl" *> spaces *> word

mtllib :: Parser String
mtllib = string "mtllib" *> spaces *> word

word :: Parser String
word = many1 (noneOf " \t\n\r\f\v") <* spaces

parseS :: Parser ()
parseS = void (try (string "s on") <|> string "s off") <* spaces

face :: Parser (V3 Int)
face = fmap readInt <$> (char 'f' *> spaces *>
    (V3 <$> numToken <*> numToken <*> numToken))
        where readInt x = read x :: Int
              numToken = many1 digit <* spaces

loadMtlFile :: Parser [(String, Material)]
loadMtlFile = many loadMtl

loadMtl :: Parser (String, Material)
loadMtl = do
    name <- string "newmtl " *> word
    spaces
    ref <- string "reflective " *> fractional
    spaces
    refColor <- rgbColor
    spaces
    emit <- string "emissive " *> fractional
    spaces
    emitCol <- rgbColor
    spaces
    pure (name, Mat ref refColor emit emitCol)

rgbColor :: Parser (RGB Float)
rgbColor = vec3

vec3 :: Parser (V3 Float)
vec3 = V3
    <$> (fractional <* spaces)
    <*> (fractional <* spaces)
    <*> (fractional <* spaces)
