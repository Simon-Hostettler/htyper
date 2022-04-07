module TypingTest where

import Brick.Main
import Brick.Types
import Brick.Widgets.Edit (handleEditorEvent)
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import System.Exit
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

newtype TestState = TestState
  { text :: NonEmptyCursor TestWord
  }
  deriving (Show, Eq)

data TestWord = TestWord
  { word :: String,
    input :: String
  }
  deriving (Show, Eq)

buildInitialState :: Int -> IO TestState
buildInitialState num_words = do
  file <- readFile "lang/1000us.txt"
  rng <- newStdGen
  let word_list = words file
  let sampled_words = take num_words (shuffle' word_list (length word_list) rng)
  let test_words = map (\s -> TestWord {word = s, input = ""}) sampled_words
  case NE.nonEmpty test_words of
    Nothing -> die "No words in file"
    Just ne -> pure TestState {text = makeNonEmptyCursor ne}

cursorPosition :: NonEmptyCursor a -> Int
cursorPosition cur = length (nonEmptyCursorPrev cur)

getActiveCharLoc :: Int -> NonEmptyCursor TestWord -> Int
getActiveCharLoc linelen cur = do
  let activeLineNum = cursorPosition cur `div` linelen
  let activeLine = chunksOf linelen (NE.toList (rebuildNonEmptyCursor cur)) !! activeLineNum
  let posInLine = sum (map ((+ 1) . length . word) (take (cursorPosition cur `mod` linelen) activeLine))
  posInLine + length (input (nonEmptyCursorCurrent cur))

getActiveLines :: Int -> Int -> NonEmptyCursor TestWord -> [[TestWord]]
getActiveLines numlines linelen cur = do
  let activeLineNum = cursorPosition cur `div` linelen
  let lines = chunksOf linelen (NE.toList (rebuildNonEmptyCursor cur))
  take numlines (drop activeLineNum lines)

handleTextInput :: TestState -> Char -> EventM n (Next TestState)
handleTextInput s c =
  case c of
    ' ' -> do
      let cur = text s
      case nonEmptyCursorSelectNext cur of
        Nothing -> continue s
        Just cur' -> continue $ s {text = cur'}
    _ -> do
      let cur = text s
      let curword = nonEmptyCursorCurrent cur
      let updatedWord = TestWord {word = word curword, input = input curword ++ [c]}
      let new_text = reverse (nonEmptyCursorPrev cur) ++ [updatedWord] ++ nonEmptyCursorNext cur
      case NE.nonEmpty new_text of
        Nothing -> continue s
        Just ne -> do
          case makeNonEmptyCursorWithSelection (cursorPosition cur) ne of
            Nothing -> continue s
            Just ne' -> continue $ s {text = ne'}

handleBackSpaceInput :: TestState -> EventM n (Next TestState)
handleBackSpaceInput s = do
  let cur = text s
  let curword = nonEmptyCursorCurrent cur
  case input curword of
    "" -> do
      case nonEmptyCursorSelectPrev cur of
        Nothing -> continue s
        Just cur' -> continue $ s {text = cur'}
    _ -> do
      let updatedWord = TestWord {word = word curword, input = init (input curword)}
      let new_text = reverse (nonEmptyCursorPrev cur) ++ [updatedWord] ++ nonEmptyCursorNext cur
      case NE.nonEmpty new_text of
        Nothing -> continue s
        Just ne -> do
          case makeNonEmptyCursorWithSelection (cursorPosition cur) ne of
            Nothing -> continue s
            Just ne' -> continue $ s {text = ne'}