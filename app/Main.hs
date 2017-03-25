module Main where

import Lib (render, testScene)
import Obj (sceneFromObj)

main :: IO ()
main = do
    let camera = -0.8
    f <- readFile "./data/test.obj"
    let scene = sceneFromObj f
    render camera testScene 512 512 Nothing