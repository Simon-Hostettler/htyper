module UI where

import Brick.AttrMap (attrMap, attrName)
import Brick.Main
import Brick.Types
import Brick.Util (fg)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core
import Brick.Widgets.Edit (handleEditorEvent)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.List.NonEmpty as NE
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Paths_htyper (getDataDir, getDataFileName)
import TypingTest

ui :: Arguments -> IO ()
ui args = do
  initialState <- buildInitialState args 200
  endState <- defaultMain htyper initialState
  return ()

--constant Attribute names
standard = attrName "standard"

corr = attrName "correct"

wrong = attrName "wrong"

unfilled = attrName "normal"

type Name = ()

htyper :: App TestState () Name
htyper =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleInputEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap mempty [(standard, fg white), (corr, fg magenta), (wrong, fg red), (unfilled, fg brightBlack)]
    }

--draws either the typing test or the results depending on state
drawUI :: TestState -> [Widget Name]
drawUI s =
  if done s then drawResultScreen s else drawTestScreen s

drawResultScreen :: TestState -> [Widget Name]
drawResultScreen s =
  [ borderWithLabel (str "results") $
      vBox
        [ vCenter $
            hCenter $
              vBox $
                map
                  str
                  [ "average wpm: " ++ show (round2Places (getWPM s)),
                    "average raw wpm: " ++ show (round2Places (getRawWPM s)),
                    "accuracy: " ++ show (round2Places (getAccuracy s)) ++ "%"
                  ],
          hCenter $ str "quit: CTRL-q, restart: CTRL-r"
        ]
  ]

drawTestScreen :: TestState -> [Widget Name]
drawTestScreen s =
  [ borderWithLabel (str "htyper") $
      hCenter $
        vCenter $
          showCursor () (Location (getActiveCharLoc s cursor, getActiveLineLoc s cursor)) $
            vBox $
              map (hBox . map drawWord) (getActiveLines s 3 cursor)
  ]
  where
    cursor = text s

drawWord :: TestWord -> Widget n
drawWord w =
  case input w of
    "" -> hBox [withAttr unfilled (str (word w)), withAttr standard (str " ")]
    _ -> hBox $ map drawChar (zipWithPad ' ' (word w) (input w)) ++ [withAttr standard (str " ")]

--draws and colors a char, who's color depends on whether the input and word match
drawChar :: (Char, Char) -> Widget n
drawChar (c1, c2)
  | c1 == ' ' = withAttr wrong (str [c2])
  | c2 == ' ' = withAttr unfilled (str [c1])
  | c1 == c2 = withAttr corr (str [c1])
  | c1 /= c2 = withAttr wrong (str [c2])
  | otherwise = str [' ']

-- Ctrl-q exits htyper, Ctrl-r reloads htyper, any other input is handled by the test
handleInputEvent :: TestState -> BrickEvent n e -> EventM n (Next TestState)
handleInputEvent s i =
  case i of
    VtyEvent vtye ->
      case vtye of
        EvKey KBS [] -> if not (done s) then handleBackSpaceInput s else continue s
        EvKey (KChar 'q') [MCtrl] -> halt s
        EvKey (KChar 'r') [MCtrl] -> liftIO (rebuildInitialState s) >>= continue
        EvKey (KChar c) [] -> if not (done s) then handleTextInput s c else continue s
        _ -> continue s
    _ -> continue s

--resets the state of the test
rebuildInitialState :: TestState -> IO TestState
rebuildInitialState s = buildInitialState (args s) 200

round2Places :: Double -> Double
round2Places d = fromIntegral (round $ d * 1e2) / 1e2

--zipWith that extends the shorter String with the given char
zipWithPad :: Char -> String -> String -> [(Char, Char)]
zipWithPad c (x : xs) (y : ys) = (x, y) : zipWithPad c xs ys
zipWithPad c [] ys = zip (repeat c) ys
zipWithPad c xs [] = zip xs (repeat c)