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
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import TypingTest

ui :: IO ()
ui = do
  initialState <- buildInitialState 50
  endState <- defaultMain htyper initialState
  print endState

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
      appAttrMap = const $ attrMap mempty [(sel, fg red), (cor, fg white), (wrong, fg red), (norm, fg brightBlack)]
    }

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

drawWord :: TestWord -> Widget n
drawWord w =
  case input w of
    "" -> withAttr norm (str (word w ++ " "))
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
