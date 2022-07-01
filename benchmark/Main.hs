module Main (main) where

import Gauge.Main
import Data.DNA
import Parser.DNA


main :: IO ()
main = defaultMain [bench "count" (whnf (fmap count . parseDNA)  "ATGCTGATCTGATCTAGATAATAGACACACACAGAT")]
