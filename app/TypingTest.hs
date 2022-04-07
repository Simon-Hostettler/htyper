module TypingTest where

import Brick.Main
import Brick.Types
import Brick.Widgets.Edit (handleEditorEvent)
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import qualified Data.Bifunctor
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Data.Time.Clock
import System.Exit
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
    correct :: Bool
  }

buildInitialState :: Int -> IO TestState
buildInitialState num_words = do
  file <- readFile "lang/1000us.txt"
  rng <- newStdGen
  let word_list = words file
  let sampled_words = take num_words (shuffle' word_list (length word_list) rng)
  let test_words = map (\s -> TestWord {word = s, input = ""}) sampled_words
  case NE.nonEmpty test_words of
    Nothing -> die "No words in file"
    Just ne -> pure TestState {text = makeNonEmptyCursor ne, tevents = [], done = False}

cursorPosition :: NonEmptyCursor a -> Int
cursorPosition cursor = length (nonEmptyCursorPrev cursor)

getWPM :: TestState -> (Double, Double)
getWPM s = do
  let startTime = timestamp (last (tevents s))
  let stopTime = timestamp (head (tevents s))
  let time_in_sec = realToFrac (diffUTCTime stopTime startTime)
  let (cor_inputs, wrong_inputs) = amountCorrWrongInputs s
  ( (fromIntegral cor_inputs / 5.0) / (time_in_sec / 60.0),
    (fromIntegral (cor_inputs + wrong_inputs) / 5.0) / (time_in_sec / 60.0)
    )

getAccuracy :: TestState -> Double
getAccuracy s = do
  let (cor_inputs, wrong_inputs) = Data.Bifunctor.bimap fromIntegral fromIntegral (amountCorrWrongInputs s)
  100.0 * (cor_inputs / (cor_inputs + wrong_inputs))

amountCorrWrongInputs :: TestState -> (Int, Int)
amountCorrWrongInputs s = (length (filter correct inputs), length (filter (not . correct) inputs))
  where
    inputs = tevents s

getActiveLineLoc :: Int -> NonEmptyCursor a -> Int
getActiveLineLoc line_len cursor = if cursorPosition cursor + 1 > line_len then 1 else 0

getActiveCharLoc :: Int -> NonEmptyCursor TestWord -> Int
getActiveCharLoc line_len cursor = do
  let active_line_num = cursorPosition cursor `div` line_len
  let active_line = chunksOf line_len (NE.toList (rebuildNonEmptyCursor cursor)) !! active_line_num
  let posInLine = sum (map ((+ 1) . length . word) (take (cursorPosition cursor `mod` line_len) active_line))
  posInLine + length (input (nonEmptyCursorCurrent cursor))

getActiveLines :: Int -> Int -> NonEmptyCursor TestWord -> [[TestWord]]
getActiveLines num_lines line_len cursor = do
  let active_line_num = cursorPosition cursor `div` line_len
  let lines = chunksOf line_len (NE.toList (rebuildNonEmptyCursor cursor))
  take num_lines (drop (active_line_num - 1) lines)

isInputCorrect :: TestWord -> Char -> Bool
isInputCorrect w c = (input w ++ [c]) `isPrefixOf` word w

addTestEvent :: Bool -> TestState -> IO TestState
addTestEvent b s = do
  cur_time <- getCurrentTime
  let test_event = TestEvent {timestamp = cur_time, correct = b}
  return s {tevents = test_event : tevents s}

handleTextInput :: TestState -> Char -> EventM n (Next TestState)
handleTextInput s c =
  case c of
    ' ' -> do
      let cursor = text s
      case nonEmptyCursorSelectNext cursor of
        Nothing -> continue $ s {done = True}
        Just cursor' -> liftIO (addTestEvent True (s {text = cursor'})) >>= continue
    _ -> do
      let tstamp = getCurrentTime
      let cursor = text s
      let cur_word = nonEmptyCursorCurrent cursor
      let new_word = TestWord {word = word cur_word, input = input cur_word ++ [c]}
      let new_text = reverse (nonEmptyCursorPrev cursor) ++ [new_word] ++ nonEmptyCursorNext cursor
      case NE.nonEmpty new_text of
        Nothing -> continue s
        Just ne -> do
          case makeNonEmptyCursorWithSelection (cursorPosition cursor) ne of
            Nothing -> continue s
            Just ne' -> do
              liftIO (addTestEvent (isInputCorrect cur_word c) (s {text = ne'})) >>= continue

handleBackSpaceInput :: TestState -> EventM n (Next TestState)
handleBackSpaceInput s = do
  let cursor = text s
  let cur_word = nonEmptyCursorCurrent cursor
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