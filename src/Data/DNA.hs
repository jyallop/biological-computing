module Data.DNA where

data Nucleotide = A | C | G | T

type DNA = [Nucleotide]

count :: DNA -> (Int, Int, Int, Int)
count = foldr addNucleotide (0, 0, 0, 0) 

addNucleotide :: Nucleotide -> (Int, Int, Int, Int) -> (Int, Int, Int, Int)
addNucleotide A (a, c, g, t) = (a + 1, c, g, t)
addNucleotide C (a, c, g, t) = (a, c + 1, g, t)
addNucleotide G (a, c, g, t) = (a, c, g + 1, t)
addNucleotide T (a, c, g, t) = (a, c, g, t + 1)
