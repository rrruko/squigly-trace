module Main where

import BIH
import Geometry (Scene(..), Triangle(..), rotMatrixRads)
import Lib (Camera(..), Settings(..), render)
import Obj (sceneFromObj)

import Control.Lens
import Data.Maybe (fromMaybe)
import Data.Time.Clock
import Data.Time.Format
import Linear.V3
import System.Environment


main :: IO ()
main = do
    args <- getArgs
    let samples = fromMaybe 1 . fmap read $ args ^? element 0
    let path    = fromMaybe "./render/result.png" $ args ^? element 1
    let objPath = fromMaybe "./data/test5-subdivide.obj" $ args ^? element 2
    let cam = Camera (V3 0 7 0.75) (rotMatrixRads (pi/2) 0 (-pi/32))
    scene <- loadScene objPath
    let bih' = geometry scene
    {-let bihSavePath = "./data/bih"
    writeFile bihSavePath (show bih')
    putStrLn $ "Wrote BIH to " ++ bihSavePath
    putStrLn $ "BIH height is " ++ show (height bih')
    putStrLn $ "Length of longest leaf is " ++ show (longestLeaf bih')
    putStrLn $ "Number of leaves is " ++ show (numLeaves bih')-}
    putStrLn "Rendering scene..."
    startTime <- getCurrentTime
    putStrLn $ "Started at " ++ showTime startTime
    let settings = Settings {
        dimensions = (540, 540),
        path = path,
        samples = samples
    }
    render scene cam settings
    endTime <- getCurrentTime
    putStrLn $ "Finished at " ++ showTime endTime
    let diff = diffUTCTime endTime startTime
    putStrLn $ "Took " ++ show diff

showTime :: FormatTime f => f -> String
showTime time = formatTime defaultTimeLocale "%T%P UTC" time

loadScene :: FilePath -> IO (Scene BIH)
loadScene path = do
    obj <- readFile path
    sceneFromObj obj
