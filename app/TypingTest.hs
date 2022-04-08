module TypingTest where

import Brick.Main (continue)
import Brick.Types (EventM, Next)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Cursor.Simple.List.NonEmpty
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Exit (die)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

data TestState = TestState
  { text :: NonEmptyCursor TestWord,
    tevents :: [TestEvent],
    done :: Bool
  }

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

buildInitialState :: Int -> Int -> IO TestState
buildInitialState most_common num_words = do
  file <- readFile "lang/1000us.txt"
  rng <- newStdGen
  let word_list = take most_common (words file)
  let sampled_words = take num_words (shuffle' word_list (length word_list) rng)
  let test_words = map (\s -> TestWord {word = s, input = ""}) sampled_words
  case NE.nonEmpty test_words of
    Nothing -> die "No words in file"
    Just ne -> pure TestState {text = makeNonEmptyCursor ne, tevents = [], done = False}

getWPM :: TestState -> Double
getWPM s = (amountCorrectInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

getRawWPM :: TestState -> Double
getRawWPM s = (amountInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

getAccuracy :: TestState -> Double
getAccuracy s = 100.0 * (amountCorrectInputs s / amountInputs s)

getActiveLineLoc :: LineLength -> NonEmptyCursor a -> Row
getActiveLineLoc line_len cursor = if cursorPosition cursor + 1 > line_len then 1 else 0

getActiveCharLoc :: LineLength -> NonEmptyCursor TestWord -> Col
getActiveCharLoc line_len cursor =
  getLineLength (take (getCursorLocInLine line_len cursor) (getActiveLine line_len cursor))
    + length (input (nonEmptyCursorCurrent cursor))

getActiveLines :: Int -> LineLength -> NonEmptyCursor TestWord -> [[TestWord]]
getActiveLines num_lines line_len cursor =
  take num_lines (drop (activeLineNum line_len cursor - 1) (getLines line_len cursor))

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

cursorPosition :: NonEmptyCursor a -> Int
cursorPosition cursor = length (nonEmptyCursorPrev cursor)

diffInSeconds :: (UTCTime, UTCTime) -> Double
diffInSeconds (t1, t2) = abs (realToFrac (diffUTCTime t2 t1))

getStartEndTime :: TestState -> (UTCTime, UTCTime)
getStartEndTime s = (timestamp (last (tevents s)), timestamp (head (tevents s)))

amountCorrectInputs :: TestState -> Double
amountCorrectInputs s = fromIntegral (length (filter correct (tevents s)))

amountWrongInputs :: TestState -> Double
amountWrongInputs s = fromIntegral (length (filter (not . correct) (tevents s)))

amountInputs :: TestState -> Double
amountInputs s = fromIntegral (length (tevents s))

activeLineNum :: LineLength -> NonEmptyCursor TestWord -> Int
activeLineNum line_len cursor = cursorPosition cursor `div` line_len

getLines :: LineLength -> NonEmptyCursor TestWord -> [[TestWord]]
getLines line_len cursor = chunksOf line_len (NE.toList (rebuildNonEmptyCursor cursor))

getActiveLine :: LineLength -> NonEmptyCursor TestWord -> [TestWord]
getActiveLine line_len cursor = getLines line_len cursor !! activeLineNum line_len cursor

getLineLength :: [TestWord] -> LineLength
getLineLength twords = sum (map ((+ 1) . length . word) twords)

getCursorLocInLine :: LineLength -> NonEmptyCursor TestWord -> Int
getCursorLocInLine line_len cursor = cursorPosition cursor `mod` line_len

isInputCorrect :: TestWord -> Char -> Bool
isInputCorrect w c = (input w ++ [c]) `isPrefixOf` word w

addTestEvent :: Bool -> Char -> TestState -> IO TestState
addTestEvent b c s = do
  cur_time <- getCurrentTime
  let test_event = TestEvent {timestamp = cur_time, correct = b, input_char = c}
  return s {tevents = test_event : tevents s}