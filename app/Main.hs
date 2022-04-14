module Main where

import Options.Applicative
import Options.Applicative.Types (readerAsk)
import TypingTest
import UI (ui)

argparse :: Parser Arguments
argparse =
  Arguments
    <$> option parseMode (long "mode" <> short 'm' <> value Random <> help "possible arguments: quote, random, timed")
    <*> option auto (long "time" <> short 't' <> value 30 <> showDefault <> help "How long the test should run, only affects timed mode")
    <*> option auto (long "line_length" <> short 'l' <> value 12 <> showDefault <> help "Number of words to display per line")
    <*> option auto (long "num_words" <> short 'n' <> value 36 <> showDefault <> help "Number of Words to randomly select")

parseMode :: ReadM Mode
parseMode = do
  string <- readerAsk
  return
    ( if string == "quote"
        then Quote
        else
          if string == "random"
            then Random
            else Timed
    )

main :: IO ()
main = runTest =<< execParser opts
  where
    opts = info (argparse <**> helper) (fullDesc <> progDesc "A cli-based typing test written in haskell" <> header "htyper")

runTest :: Arguments -> IO ()
runTest = ui
