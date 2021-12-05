module Main (main) where

import Data.DNA
import Parser.DNA
import System.Console.ArgParser
import Data.Text(strip, pack, unpack)

data Computation = Count Bool String
  deriving (Eq, Show)

computationParser :: IO (CmdLnInterface Computation)
computationParser = mkSubParser
  [
    ("count", setAppDescr (mkDefaultApp countParser "count")
                          "Counts nucleotides in a sequence given by a file or a command line argument")
  ]

countParser :: ParserSpec Computation
countParser = Count `parsedBy` boolFlag "file" `Descr` "when set will read the input string as a file name"
                    `andBy` reqPos "input string" `Descr` "The string containing the DNA sequence or a filename"

main :: IO ()
main = computationParser >>= (flip runApp) compute

compute :: Computation -> IO ()
compute (Count True file) = readFile file >>= (putStrLn . show . fmap count . parseDNA . unpack . strip . pack) 
compute (Count False string) = (putStrLn . show . fmap count . parseDNA) string
