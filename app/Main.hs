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


main :: IO ()
main = do
    args <- getArgs
    let settings = Settings {
        dimensions = (540, 540),
        samples = maybe 1 read $ args ^? element 0,
        path    = fromMaybe "./render/result.png" $ args ^? element 1
    }
    let objPath = fromMaybe "./data/test5-subdivide.obj" $ args ^? element 2
    let cam = Camera (V3 0 7 0.75) (rotMatrixRads (pi/2) 0 (-pi/32))
    scene <- sceneFromBIH <$> loadBIH True objPath
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

loadBIH :: Bool -> FilePath -> IO BIH
loadBIH debug path = do
    tris <- loadTris debug path
    let bih = makeBIH tris
    let btree = tree bih
    when debug $ do
        let bihSavePath = "./data/bih"
        writeFile bihSavePath (show bih)
        putStrLn $ "Wrote BIH to " ++ bihSavePath
        putStrLn $ "BIH height is " ++ show (height btree)
        putStrLn $ "Length of longest leaf is " ++ show (longestLeaf btree)
        putStrLn $ "Number of leaves is " ++ show (numLeaves btree)
    return bih
