module Main where

import BIH
import Geometry (rotMatrixRads)
import Lib (Camera(..), Settings(..), render)
import Obj (sceneFromObj)

import Data.Time.Clock
import Data.Time.Format
import Linear.V3
import System.IO

main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    let cam = Camera (V3 0 7 0.75) (rotMatrixRads (pi/2) 0 (-pi/32))
    putStrLn "Sample count? "
    samples <- fmap read getLine
    let objPath = "./data/test5-subdivide.obj"
    scene <- loadScene objPath
    let bih' = sceneBIH scene
    print bih'
    putStrLn $ "BIH height is " ++ show (height bih')
    putStrLn $ "Length of longest leaf is " ++ show (longestLeaf bih')
    putStrLn $ "Number of leaves is " ++ show (numLeaves bih')
    putStrLn "Rendering scene..."
    startTime <- getCurrentTime
    putStrLn $ "Started at " ++ showTime startTime
    let settings = Settings {
        dimensions = (540, 540),
        path = "./render/result.png",
        samples = samples
    }
    render scene cam settings
    endTime <- getCurrentTime
    putStrLn $ "Finished at " ++ showTime endTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Took " ++ show diff

showTime :: FormatTime f => f -> String
showTime time = formatTime defaultTimeLocale "%T%P UTC" time

loadScene :: FilePath -> IO Scene
loadScene path = do
    obj <- readFile path
    sceneFromObj obj
