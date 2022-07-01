module Main (main) where

import Data.DNA
import Data.RNA
import Parser.DNA
import System.Console.ArgParser
import Data.Nucleotide(toChar)
import Data.Text(strip, pack, unpack)

data Computation =
    Count Bool String
  | Translate Bool String
  | Complement String
  deriving (Eq, Show)

computationParser :: IO (CmdLnInterface Computation)
computationParser = mkSubParser
  [
    ("count", setAppDescr (mkDefaultApp countParser "count")
                          "Counts nucleotides in a sequence given by a file or a command line argument"),
    ("translate", setAppDescr (mkDefaultApp translateParser "translate")
                              "translates a DNA sequence into an RNA sequence"),
    ("complement", setAppDescr (mkDefaultApp complementParser "complement")
                   "gets the complement of a dna string")
  ]

complementParser :: ParserSpec Computation
complementParser = Complement `parsedBy` reqPos "input dna" `Descr` "the dna string"

countParser :: ParserSpec Computation
countParser = Count `parsedBy` boolFlag "file" `Descr` "when set will read the input string as a file name"
                    `andBy` reqPos "input string" `Descr` "The string containing the DNA sequence or a filename"

translateParser :: ParserSpec Computation
translateParser = Translate `parsedBy` boolFlag "file" `Descr` "when set will read the input string from a file"
                            `andBy` reqPos "input string" `Descr` "the string containing the DNA sequence or the filename"

main :: IO ()
main = computationParser >>= (flip runApp) compute

compute :: Computation -> IO ()
compute (Count True file) = readFile file >>= (printOutput . fmap count . parseDNA . stripInput) 
compute (Count False string) = (printOutput . fmap count . parseDNA) string
compute (Translate True file) = readFile file >>= (printOutput . fmap translate . parseDNA . stripInput) 
compute (Translate False string) = (printOutput . fmap (foldr (\n s -> (show n) ++ s) "") . fmap translate . parseDNA) string
compute (Complement dna) = (printOutput . fmap (printDNA . reverseComplement) . parseDNA) dna

printOutput :: (Show a) => Either String a -> IO ()
printOutput (Left error) = putStrLn error
printOutput (Right output) = putStrLn $ show output

stripInput :: String -> String
stripInput = unpack . strip . pack

printDNA = map toChar

