module Main where

import Lib (makeCamera, render, testScene)

main :: IO ()
main = do
    let cam = makeCamera (-1)
    render testScene 512 512 Nothing cam