module Data.DNA(DNA, count, complement, reverseComplement) where

import Data.Nucleotide

type DNA = [Nucleotide]

count :: DNA -> (Int, Int, Int, Int, Int)
count = foldr addNucleotide (0, 0, 0, 0, 0) 

complement :: DNA -> DNA
complement = map nucleotideComplement

reverseComplement :: DNA -> DNA
reverseComplement = reverse . complement
