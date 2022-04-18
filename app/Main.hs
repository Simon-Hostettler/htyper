module Main where

import           Config
import           Options.Applicative
import           Options.Applicative.Types (readerAsk)
import           TypingTest
import           UI                        (ui)

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
  pure
    ( case string of
        "quote"  -> Quote
        "random" -> Random
        _        -> Timed
    )

main :: IO ()
main = do
  conf <- readConfig
  runTest conf =<< execParser opts
    where
      opts = info (argparse <**> helper) (fullDesc <> progDesc "A cli-based typing test written in haskell" <> header "htyper")


runTest :: Conf -> Arguments ->IO ()
runTest = ui
