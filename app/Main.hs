module Main (main) where

import Data.DNA
import Parser.DNA

main :: IO ()
main = (putStrLn . show . count . parseDNA) "A"
