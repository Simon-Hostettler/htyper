{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module UI where

import Brick.AttrMap
import Brick.Main
import Brick.Types
import Brick.Util
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Core
import Brick.Widgets.Edit (handleEditorEvent)
import Control.Monad.IO.Class
import Cursor.Simple.List.NonEmpty
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Time.Clock
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')
import TypingTest

--number of words to type
num_words = 30

--number of words to display per line
line_length = 10

--number of most common words to pick from, max 1000
most_common = 200

ui :: IO ()
ui = do
  initialState <- buildInitialState most_common num_words
  endState <- defaultMain htyper initialState
  print ""

data Input
  = Input
  deriving (Show, Eq)

type Name = ()

--constant Attribute names
selected = attrName "selected"

corr = attrName "correct"

wrong = attrName "wrong"

norm = attrName "normal"

htyper :: App TestState Input Name
htyper =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleInputEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap mempty [(selected, fg red), (corr, fg white), (wrong, fg red), (norm, fg brightBlack)]
    }

round2Places :: Double -> Double
round2Places d = fromIntegral (round $ d * 1e2) / 1e2

drawUI :: TestState -> [Widget Name]
drawUI ts =
  if done ts
    then do
      [ borderWithLabel (str "results") $
          vBox $
            [ vCenter $
                hCenter $
                  vBox $
                    map
                      str
                      [ "average wpm: " ++ show (round2Places (getWPM ts)),
                        "average raw wpm: " ++ show (round2Places (getRawWPM ts)),
                        "accuracy: " ++ show (round2Places (getAccuracy ts)) ++ "%"
                      ],
              hCenter $ str "quit: CTRL-q, restart: CTRL-r"
            ]
        ]
    else
      ( let cur = text ts
         in [ borderWithLabel (str "htyper") $
                hCenter $
                  vCenter $
                    showCursor () (Location (getActiveCharLoc line_length cur, getActiveLineLoc line_length cur)) $
                      vBox $
                        map (hBox . map drawWord) (getActiveLines 3 line_length cur)
            ]
      )

drawWord :: TestWord -> Widget n
drawWord w =
  case input w of
    "" -> hBox [withAttr norm (str (word w)), withAttr corr (str " ")]
    _ -> hBox $ map drawChar (zipWithPad ' ' ' ' (word w) (input w)) ++ [withAttr corr (str " ")]

drawChar :: (Char, Char) -> Widget n
drawChar (c1, c2)
  | c1 == ' ' = withAttr wrong (str [c2])
  | c2 == ' ' = withAttr norm (str [c1])
  | c1 == c2 = withAttr corr (str [c1])
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
        EvKey KBS [] -> if not (done s) then handleBackSpaceInput s else continue s
        EvKey (KChar 'q') [MCtrl] -> halt s
        EvKey (KChar 'r') [MCtrl] -> liftIO (buildInitialState most_common num_words) >>= continue
        EvKey (KChar c) [] -> if not (done s) then handleTextInput s c else continue s
        _ -> continue s
    _ -> continue s
