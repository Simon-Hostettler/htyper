module UI where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit (handleEditorEvent)
import Cursor.Simple.List.NonEmpty
import Data.List
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.Split
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import System.Directory
import System.Exit

ui :: IO ()
ui = do
  initialState <- buildInitialState
  endState <- defaultMain htyper initialState
  print endState

newtype TestState = TestState
  { text :: NonEmptyCursor TestWord
  }
  deriving (Show, Eq)

data TestWord = TestWord
  { word :: String,
    input :: String
  }
  deriving (Show, Eq)

data Input
  = Input
  deriving (Show, Eq)

type Name = ()

--some constants
sel = attrName "selected"

cor = attrName "correct"

wrong = attrName "wrong"

norm = attrName "normal"

htyper :: App TestState Input Name
htyper =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleInputEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap mempty [(sel, fg red), (cor, fg green), (wrong, fg red), (norm, fg white)]
    }

buildInitialState :: IO TestState
buildInitialState = do
  file <- readFile "lang/1000us.txt"
  let in_words = lines file
  let test_words = map (\s -> TestWord {word = s, input = ""}) in_words
  case NE.nonEmpty test_words of
    Nothing -> die "No words in file"
    Just ne -> pure TestState {text = makeNonEmptyCursor ne}

drawUI :: TestState -> [Widget Name]
drawUI ts =
  let cur = text ts
   in [ borderWithLabel (str "htyper") $
          hCenter $
            vCenter $
              showCursor () (Location (getActiveCharLoc 15 cur, 0)) $
                vBox $
                  map (hBox . map drawWord) (getActiveLines 3 15 cur)
      ]

getActiveCharLoc :: Int -> NonEmptyCursor TestWord -> Int
getActiveCharLoc linelen cur = do
  let activeLineNum = cursorPosition cur `div` linelen
  let activeLine = chunksOf linelen (NE.toList (rebuildNonEmptyCursor cur)) !! max 0 activeLineNum
  let posInLine = sum (map ((+ 1) . length . word) (take (cursorPosition cur `mod` linelen) activeLine))
  posInLine + length (input (nonEmptyCursorCurrent cur))

getActiveLines :: Int -> Int -> NonEmptyCursor TestWord -> [[TestWord]]
getActiveLines numlines linelen cur = do
  let activeLineNum = cursorPosition cur `div` linelen
  let lines = chunksOf linelen (NE.toList (rebuildNonEmptyCursor cur))
  take numlines (drop activeLineNum lines)

drawWord :: TestWord -> Widget n
drawWord w =
  case input w of
    "" -> str (word w ++ " ")
    _ -> hBox $ map drawChar (zipWithPad ' ' ' ' (word w) (input w)) ++ [withAttr norm (str " ")]

drawChar :: (Char, Char) -> Widget n
drawChar (c1, c2)
  | c1 == ' ' = withAttr wrong (str [c2])
  | c2 == ' ' = withAttr norm (str [c1])
  | c1 == c2 = withAttr cor (str [c1])
  | c1 /= c2 = withAttr wrong (str [c2])
  | otherwise = str [' ']

zipWithPad :: a -> b -> [a] -> [b] -> [(a, b)]
zipWithPad a b (x : xs) (y : ys) = (x, y) : zipWithPad a b xs ys
zipWithPad a _ [] ys = zip (repeat a) ys
zipWithPad _ b xs [] = zip xs (repeat b)

cursorPosition :: NonEmptyCursor a -> Int
cursorPosition cur = length (nonEmptyCursorPrev cur)

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

handleInputEvent :: TestState -> BrickEvent n e -> EventM n (Next TestState)
handleInputEvent s i =
  case i of
    VtyEvent vtye ->
      case vtye of
        EvKey KBS [] -> handleBackSpaceInput s
        EvKey (KChar 'q') [MCtrl] -> halt s
        EvKey (KChar c) [] -> handleTextInput s c
        _ -> continue s
    _ -> continue s
