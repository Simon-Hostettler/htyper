module Main where

import Data.Semigroup ((<>))
import Options.Applicative
import Options.Applicative.Types (optShowDefault, readerAsk)
import Paths_htyper (getDataDir, getDataFileName)
import TypingTest
import UI (ui)

argparse :: Parser Arguments
argparse =
  Arguments
    <$> option parseMode (long "quote" <> short 'q' <> value Random <> help "Set true to select a random quote instead of random words")
    <*> option auto (long "line_length" <> short 'l' <> value 10 <> showDefault <> help "Number of words to display per line")
    <*> option auto (long "num_words" <> short 'n' <> value 50 <> showDefault <> help "Number of Words to randomly select")

parseMode :: ReadM Mode
parseMode = do
  string <- readerAsk
  return (if string == "true" then Quote else Random)

main :: IO ()
main = runTest =<< execParser opts
  where
    opts = info (argparse <**> helper) (fullDesc <> progDesc "A cli-based typing test written in haskell" <> header "htyper")

runTest :: Arguments -> IO ()
runTest (Arguments Random llen numwords) = ui (Arguments {mode = Random, llen = llen, numwords = numwords})
runTest (Arguments Quote llen numwords) = ui (Arguments {mode = Quote, llen = llen, numwords = numwords})