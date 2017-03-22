module Main where

import Lib (render, testScene)

main :: IO ()
main = do
	let camera = -0.8
	render camera testScene 512 512 Nothing