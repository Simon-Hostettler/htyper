module UI where

import Brick.AttrMap (attrMap, attrName)
import Brick.BChan (newBChan, writeBChan)
import Brick.Main
import Brick.Types
import Brick.Util (fg)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (hCenter, vCenter)
import Brick.Widgets.Core
import Brick.Widgets.Edit (handleEditorEvent)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (sort)
import qualified Data.List.NonEmpty as NE
import Graphics.Vty (defaultConfig, mkVty)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events
import Paths_htyper (getDataDir, getDataFileName)
import System.Console.Terminal.Size (Window (height, width), size)
import System.Exit
import TypingTest

ui :: Arguments -> IO ()
ui args = do
  chan <- newBChan 10
  forkIO $
    forever $ do
      threadDelay 1000000
      writeBChan chan Tick
  let buildVty = mkVty defaultConfig
  dim <- getWindowSize
  initialVty <- buildVty
  initialState <- buildInitialState dim args 200
  endState <- customMain initialVty buildVty (Just chan) htyper initialState
  return ()

--constant Attribute names
(standard, corr, wrong, unfilled) =
  ( attrName "standard",
    attrName "correct",
    attrName "wrong",
    attrName "unfilled"
  )

data Tick = Tick

type Name = ()

htyper :: App TestState Tick Name
htyper =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleInputEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap mempty [(standard, fg white), (corr, fg magenta{-(rgbColor 204 81 195)-}), (wrong, fg red), (unfilled, fg brightBlack)]
    }

--draws either the typing test or the results depending on state
drawUI :: TestState -> [Widget Name]
drawUI s =
  if done s then drawResultScreen s else drawTestScreen s

getWindowSize :: IO (Int, Int)
getWindowSize = do
  w <- size :: IO (Maybe (Window Int))
  return (maybe 0 width w, maybe 0 height w)

drawResultScreen :: TestState -> [Widget Name]
drawResultScreen s =
  [ borderWithLabel (str " results ") $
      vBox
        [ vLimitPercent 40 $
            vCenter $
              hCenter $
                hBox
                  [ drawStats s,
                    drawKeyInfo s
                  ],
          borderWithLabel (str " wpm over time ") $
            vCenter $
              hCenter $
                drawWpmFunc (fst (dimensions s) - 5, round (0.6 * fromIntegral (snd (dimensions s))) - 5) s,
          hCenter $ str "quit: CTRL-q, restart: CTRL-r"
        ]
  ]

drawStats :: TestState -> Widget Name
drawStats s =
  borderWithLabel (str " stats ") $
    vCenter $
      hCenter $
        vBox $
          map
            str
            [ "average wpm: " ++ show (round2Places (getWPM s)),
              "\n",
              "average raw wpm: " ++ show (round2Places (getRawWPM s)),
              "\n",
              "accuracy: " ++ show (round2Places (getAccuracy s)) ++ "%",
              "\n",
              "consistency: " ++ show (round2Places (getConsistency s)) ++ "%",
              "\n",
              "correct/total inputs: " ++ getInputStats s
            ]

drawKeyInfo :: TestState -> Widget Name
drawKeyInfo s =
  borderWithLabel (str " worst keys ") $
    vCenter $
      hCenter $
        vBox $
          map
            (\cerr -> str (show (char cerr) ++ ": " ++ show (round2Places (100.0 * (1.0 - errorRate cerr))) ++ "%"))
            (take 5 (reverse (sort (getErrorsPerChar s))))

drawTestScreen :: TestState -> [Widget Name]
drawTestScreen s =
  [ borderWithLabel (str "htyper") $
      hCenter $
        vCenter $
          vBox
            [ if mode (args s) == Timed then str (show (time_left s)) else str "",
              showCursor () (Location (getCursorLoc s)) $
                vBox $
                  map (hBox . map drawWord) (getActiveLines s 3)
            ]
  ]
  where
    cursor = text s

drawWpmFunc :: (Int, Int) -> TestState -> Widget n
drawWpmFunc (cols, rows) s = do
  vBox $
    reverse
      ( [ hBox $
            prefix r (pos r) :
              [ if pos r <= wpmList !! c
                  then str "⠿"
                  else str " "
                | c <- [0 .. length wpmList - 2]
              ]
          | r <- [0 .. rows]
        ]
      )
  where
    fI = fromIntegral
    wpmList = get10KeyRawWpm cols s
    maxWpm = maximum wpmList
    minWpm = minimum wpmList
    wpmRange = 10.0 + maxWpm - minWpm
    pos x = minWpm + (wpmRange * fI x / fI rows)
    prefix i x
      | even i && x < 100.0 = str (show (round x) ++ "   ")
      | even i && x >= 100.0 = str (show (round x) ++ "  ")
      | otherwise = str "     "

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
handleInputEvent :: TestState -> BrickEvent Name Tick -> EventM n (Next TestState)
handleInputEvent s i =
  case i of
    VtyEvent vtye ->
      case vtye of
        EvKey KBS [] -> if not (done s) then handleBackSpaceInput s else continue s
        EvKey (KChar 'q') [MCtrl] -> halt s
        EvKey (KChar 'r') [MCtrl] -> liftIO (rebuildInitialState s) >>= continue
        EvKey (KChar c) [] -> if not (done s) then handleTextInput s c else continue s
        _ -> continue s
    AppEvent Tick -> do
      dim <- liftIO getWindowSize
      if mode (args s) == Timed
        then case time_left s - 1 of
          0 -> continue (s {time_left = 0, done = True, dimensions = dim})
          x -> continue (s {time_left = x, dimensions = dim})
        else continue (s {dimensions = dim})
    _ -> continue s

--resets the state of the test
rebuildInitialState :: TestState -> IO TestState
rebuildInitialState s = buildInitialState (dimensions s) (args s) 200

round2Places :: Double -> Double
round2Places d = fromIntegral (round $ d * 1e2) / 1e2

--zipWith that extends the shorter String with the given char
zipWithPad :: Char -> String -> String -> [(Char, Char)]
zipWithPad c (x : xs) (y : ys) = (x, y) : zipWithPad c xs ys
zipWithPad c [] ys = zip (repeat c) ys
zipWithPad c xs [] = zip xs (repeat c)
