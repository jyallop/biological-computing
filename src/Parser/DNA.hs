module Parser.DNA(parseDNA) where

import Data.DNA

parseDNA :: String -> Maybe DNA
parseDNA = sequence . (map parseNucleotide)

parseNucleotide :: Char -> Maybe Nucleotide
parseNucleotide 'A' = Just A
parseNucleotide 'T' = Just T
parseNucleotide 'G' = Just G
parseNucleotide 'C' = Just C
parseNucleotide _ = Nothing
