module Data.Nucleotide(Nucleotide(..), nucleotideComplement, addNucleotide, toChar) where

data Nucleotide = A | C | G | T | U
  deriving (Show, Eq)

addNucleotide :: Nucleotide -> (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int)
addNucleotide A (a, c, g, t, u) = (a + 1, c, g, t, u)
addNucleotide C (a, c, g, t, u) = (a, c + 1, g, t, u)
addNucleotide G (a, c, g, t, u) = (a, c, g + 1, t, u)
addNucleotide T (a, c, g, t, u) = (a, c, g, t + 1, u)
addNucleotide U (a, c, g, t, u) = (a, c, g, t, u + 1)

toChar :: Nucleotide -> Char
toChar A = 'A'
toChar C = 'C'
toChar G = 'G'
toChar T = 'T'
toChar U = 'U'

nucleotideComplement :: Nucleotide -> Nucleotide
nucleotideComplement A = T
nucleotideComplement T = A
nucleotideComplement G = C
nucleotideComplement C = G
