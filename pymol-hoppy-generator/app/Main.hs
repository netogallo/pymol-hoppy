module Main where

import Foreign.Hoppy.Generator.Main (defaultMain)

import Foreign.Pymol (interfaceResult)

main :: IO ()
main = defaultMain interfaceResult

