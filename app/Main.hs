module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (optShowDefault)
import Paths_htyper (getDataDir, getDataFileName)
import TypingTest (Mode (Quote, Random))
import UI (ui)

data Args = Args
  { mode :: Bool,
    line_length :: Int,
    num_words :: Int
  }

args :: Parser Args
args =
  Args
    <$> switch (long "quote" <> short 'q' <> help "If flag is selected, displays a quote instead of a random selection of words")
    <*> option auto (long "line_length" <> short 'l' <> value 10 <> showDefault <> help "Number of words to display per line")
    <*> option auto (long "num_words" <> short 'n' <> value 50 <> showDefault <> help "Number of Words to randomly select")

main :: IO ()
main = runTest =<< execParser opts
  where
    opts = info (args <**> helper) (fullDesc <> progDesc "A cli-based typing test written in haskell" <> header "htyper")

runTest :: Args -> IO ()
runTest (Args False llen nwords) = do
  file <- getDataFileName "1000us.txt"
  ui Random file llen nwords
runTest (Args True llen nwords) = do
  file <- getDataFileName "quotes.txt"
  ui Quote file llen nwords