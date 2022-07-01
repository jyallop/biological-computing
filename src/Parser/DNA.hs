module Parser.DNA(parseDNA) where

import Data.DNA
import Data.Nucleotide

parseDNA :: String -> Either String DNA
parseDNA = sequence . (map parseDNANucleotide)

parseDNANucleotide :: Char -> Either String Nucleotide 
parseDNANucleotide 'A' = Right A
parseDNANucleotide 'T' = Right T
parseDNANucleotide 'G' = Right G
parseDNANucleotide 'C' = Right C
parseDNANucleotide x = Left ("Unable to parse character: " ++ [x])
