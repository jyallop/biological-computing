module Data.RNA(RNA, translate) where

import Data.Nucleotide
import Data.DNA

type RNA = [Nucleotide]

translate :: DNA -> RNA
translate = map translateNucleotide

translateNucleotide :: Nucleotide -> Nucleotide
translateNucleotide T = U
translateNucleotide x = x
