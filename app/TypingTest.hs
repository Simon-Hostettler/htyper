module TypingTest where

import Brick.Main (continue)
import Brick.Types (EventM, Next)
import Brick.Widgets.FileBrowser (fileBrowserAttr)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Cursor.Simple.List.NonEmpty
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf, splitOn)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Paths_htyper (getDataFileName)
import System.Exit (die)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

data TestState = TestState
  { text :: NonEmptyCursor TestWord,
    tevents :: [TestEvent],
    done :: Bool,
    args :: Arguments
  }

data Arguments = Arguments
  { mode :: Mode,
    llen :: LineLength,
    numwords :: Int
  }

data Mode = Quote | Random deriving (Eq)

data TestWord = TestWord
  { word :: String,
    input :: String
  }

data TestEvent = TestEvent
  { timestamp :: UTCTime,
    correct :: Bool,
    input_char :: Char
  }

type Row = Int

type Col = Int

type LineLength = Int

buildInitialState :: Arguments -> Int -> IO TestState
buildInitialState args most_common = do
  case mode args of
    Quote -> do
      test_words <- getTextFile (mode args) >>= getRandomQuote
      toTestState args test_words
    Random -> do
      file <- getTextFile (mode args)
      test_words <- getRandomWords file most_common (numwords args)
      toTestState args test_words

--(#correct inputs / 5) / time passed, since the average english word is 5 characters long
getWPM :: TestState -> Double
getWPM s = (amountCorrectInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

--same as above, but counts all inputs
getRawWPM :: TestState -> Double
getRawWPM s = (amountInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

--percentage of correct inputs / total inputs
getAccuracy :: TestState -> Double
getAccuracy s = 100.0 * (amountCorrectInputs s / amountInputs s)

--the row in which the currently edited line is in
getActiveLineLoc :: TestState -> NonEmptyCursor a -> Row
getActiveLineLoc s cursor = if cursorPosition cursor + 1 > llen (args s) then 1 else 0

--the column of the active line in which the currently edited char is in
getActiveCharLoc :: TestState -> NonEmptyCursor TestWord -> Col
getActiveCharLoc s cursor =
  getLineLength (take (getCursorLocInLine s cursor) (getActiveLine s cursor))
    + length (input (nonEmptyCursorCurrent cursor))

--returns lines around the active line to draw
getActiveLines :: TestState -> Int -> NonEmptyCursor TestWord -> [[TestWord]]
getActiveLines s num_lines cursor =
  take num_lines (drop (activeLineNum s cursor - 1) (getLines s cursor))

--adds new InputEvent to State and advances State logic
handleTextInput :: TestState -> Char -> EventM n (Next TestState)
handleTextInput s c =
  case c of
    ' ' -> do
      case nonEmptyCursorSelectNext cursor of
        Nothing -> continue $ s {done = True}
        Just cursor' -> liftIO (addTestEvent True ' ' (s {text = cursor'})) >>= continue
    _ -> do
      let new_word = TestWord {word = word cur_word, input = input cur_word ++ [c]}
      let new_text = reverse (nonEmptyCursorPrev cursor) ++ [new_word] ++ nonEmptyCursorNext cursor
      case NE.nonEmpty new_text of
        Nothing -> continue s
        Just ne -> do
          case makeNonEmptyCursorWithSelection (cursorPosition cursor) ne of
            Nothing -> continue s
            Just ne' -> do
              liftIO (addTestEvent (isInputCorrect cur_word c) c (s {text = ne'})) >>= continue
  where
    cursor = text s
    cur_word = nonEmptyCursorCurrent cursor

--either removes a char from the active word or jumps to the previous word if empty
handleBackSpaceInput :: TestState -> EventM n (Next TestState)
handleBackSpaceInput s = do
  case input cur_word of
    "" -> do
      case nonEmptyCursorSelectPrev cursor of
        Nothing -> continue s
        Just cursor' -> continue $ s {text = cursor'}
    _ -> do
      let new_word = TestWord {word = word cur_word, input = init (input cur_word)}
      let new_text = reverse (nonEmptyCursorPrev cursor) ++ [new_word] ++ nonEmptyCursorNext cursor
      case NE.nonEmpty new_text of
        Nothing -> continue s
        Just ne -> do
          case makeNonEmptyCursorWithSelection (cursorPosition cursor) ne of
            Nothing -> continue s
            Just ne' -> continue $ s {text = ne'}
  where
    cursor = text s
    cur_word = nonEmptyCursorCurrent cursor

--Internal Helper Functions, not supposed to be called from outside

--number of words before current word
cursorPosition :: NonEmptyCursor a -> Int
cursorPosition cursor = length (nonEmptyCursorPrev cursor)

diffInSeconds :: (UTCTime, UTCTime) -> Double
diffInSeconds (t1, t2) = abs (realToFrac (diffUTCTime t2 t1))

--timestamp of first and last input
getStartEndTime :: TestState -> (UTCTime, UTCTime)
getStartEndTime s = (timestamp (last (tevents s)), timestamp (head (tevents s)))

amountCorrectInputs :: TestState -> Double
amountCorrectInputs s = fromIntegral (length (filter correct (tevents s)))

amountWrongInputs :: TestState -> Double
amountWrongInputs s = fromIntegral (length (filter (not . correct) (tevents s)))

amountInputs :: TestState -> Double
amountInputs s = fromIntegral (length (tevents s))

--number of lines before the active line
activeLineNum :: TestState -> NonEmptyCursor TestWord -> Int
activeLineNum s cursor = cursorPosition cursor `div` llen (args s)

--splits words into lines of linelength
getLines :: TestState -> NonEmptyCursor TestWord -> [[TestWord]]
getLines s cursor = chunksOf (llen (args s)) (NE.toList (rebuildNonEmptyCursor cursor))

--gets words in active line
getActiveLine :: TestState -> NonEmptyCursor TestWord -> [TestWord]
getActiveLine s cursor = getLines s cursor !! activeLineNum s cursor

--length of words includding spaces
getLineLength :: [TestWord] -> LineLength
getLineLength twords = sum (map ((+ 1) . length . word) twords)

--amount of words before current word in active line + 1
getCursorLocInLine :: TestState -> NonEmptyCursor TestWord -> Int
getCursorLocInLine s cursor = cursorPosition cursor `mod` llen (args s)

isInputCorrect :: TestWord -> Char -> Bool
isInputCorrect w c = (input w ++ [c]) `isPrefixOf` word w

addTestEvent :: Bool -> Char -> TestState -> IO TestState
addTestEvent b c s = do
  cur_time <- getCurrentTime
  let test_event = TestEvent {timestamp = cur_time, correct = b, input_char = c}
  return s {tevents = test_event : tevents s}

toTestWord :: String -> TestWord
toTestWord s =
  TestWord
    { word = s,
      input = ""
    }

toTestState :: Arguments -> [TestWord] -> IO TestState
toTestState args twords =
  case NE.nonEmpty twords of
    Nothing -> die "No Words to display"
    Just txt -> pure TestState {text = makeNonEmptyCursor txt, tevents = [], done = False, args = args}

--shuffles most_common amount of words from a file and returns num_words of them
getRandomWords :: FilePath -> Int -> Int -> IO [TestWord]
getRandomWords file most_common num_words = do
  rng <- newStdGen
  wordfile <- readFile file
  let word_list = take most_common (words wordfile)
  let sampled_words = take num_words (shuffle' word_list (length word_list) rng)
  return (map toTestWord sampled_words)

--gets a random quote from a file, quotes should be delimited by a ^_^
getRandomQuote :: FilePath -> IO [TestWord]
getRandomQuote file = do
  rng <- newStdGen
  quotefile <- readFile file
  let quotes = splitOn "^_^" quotefile
  let rand_quote = head (shuffle' quotes (length quotes) rng)
  return (map toTestWord (words rand_quote))

--returns the quote file or the most common words file, depending on the mode of the test
getTextFile :: Mode -> IO FilePath
getTextFile mode = if mode == Random then getDataFileName "1000us.txt" else getDataFileName "quotes.txt"