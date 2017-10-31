module Main where

import BIH
import Geometry (Scene(..), Triangle(..), naiveIntersect, rotMatrixRads)
import Lib (Camera(..), Settings(..), render)
import Obj (trisFromObj)

import Control.Lens
import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format
import Linear.V3
import System.Environment
import System.Console.CmdArgs

squigly = Settings
    { samples = 10 &= name "s"
        &= help "How many samples per pixel to trace"
    , dimensions = (540, 540) &= name "d"
        &= help "Dimensions of the resulting image"
    , savePath = "./render/result.png" &= typFile &= name "p"
        &= help "Where to save the output"
    , objPath = "./data/scene.obj" &= typFile
        &= help "File to load .obj from"
    , debug = def
        &= help "Run in debug mode"
    , debugPath = def &= typFile
        &= help "File to write debug info to"
    , cast = def
        &= help "Raycast instead of raytracing (i.e. don't bounce rays)"
    } &=
    help "A cute raytracer" &=
    summary "squigly-trace was made by Ruko (https://github.com/rukokarasu/)"

main :: IO ()
main = do
    settings <- cmdArgs squigly
    let cam = Camera (V3 0 7 0.75) (rotMatrixRads (pi/2) 0 (-pi/32))
    scene <- sceneFromBIH <$> loadBIH settings
    putStrLn "Rendering scene..."
    startTime <- getCurrentTime
    putStrLn $ "Started at " ++ showTime startTime
    render scene cam settings
    endTime <- getCurrentTime
    putStrLn $ "Finished at " ++ showTime endTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Took " ++ show diff

showTime :: FormatTime f => f -> String
showTime = formatTime defaultTimeLocale "%T%P UTC"

sceneFromTris :: [Triangle] -> Scene [Triangle]
sceneFromTris tris = Scene tris naiveIntersect

sceneFromBIH :: BIH -> Scene BIH
sceneFromBIH bih = Scene bih intersectBIH

loadTris :: Bool -> FilePath -> IO [Triangle]
loadTris debug path = do
    obj <- readFile path
    trisFromObj debug obj

loadBIH :: Settings -> IO BIH
loadBIH settings = do
    tris <- loadTris (debug settings) (objPath settings)
    let bih = makeBIH tris
    let btree = tree bih
    when (debug settings) $ do
        let bihSavePath = debugPath settings
        writeFile bihSavePath (show bih)
        putStrLn $ "Wrote BIH to " ++ bihSavePath
        putStrLn $ "BIH height is " ++ show (height btree)
        putStrLn $ "Length of longest leaf is " ++ show (longestLeaf btree)
        putStrLn $ "Number of leaves is " ++ show (numLeaves btree)
    return bih
