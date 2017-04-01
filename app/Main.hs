module Main where

import Lib (makeCamera, render)
import Obj (sceneFromObj)

main :: IO ()
main = do
    let cam = makeCamera undefined
    let objPath = "./data/test3.obj"
    putStrLn ("Loading .obj file: " ++ objPath)
    obj <- readFile objPath
    let scene = sceneFromObj obj
    putStrLn "Rendering scene..."
    render scene 540 540 Nothing cam