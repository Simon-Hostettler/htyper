{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module UI where

import           Brick.AttrMap                (AttrName, attrMap, attrName)
import           Brick.BChan                  (newBChan, writeBChan)
import           Brick.Main
import           Brick.Types
import           Brick.Util                   (fg)
import           Brick.Widgets.Border         (borderWithLabel)
import           Brick.Widgets.Center         (hCenter, vCenter)
import           Brick.Widgets.Core           hiding (raw)
import           Config
import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Monad                (forever)
import           Control.Monad.IO.Class       (MonadIO (liftIO))
import           Data.Bifunctor               (bimap)
import           Data.Function                (on)
import           Data.List                    (sortBy, transpose)
import           Data.List.Split              (splitOn)
import           Data.Text                    (pack)
import           Formatting                   (fixed, int, sformat, stext, (%))
import           Graphics.Vty                 (Attr, Vty (update), defAttr,
                                               defaultConfig, mkVty,
                                               picForImage, rgbColor, string,
                                               withForeColor)
import           Graphics.Vty.Attributes      (brightBlack, magenta, red, white)
import           Graphics.Vty.Input.Events
import           Paths_htyper                 (getDataFileName)
import           Prelude                      hiding (concat)
import           System.Console.Terminal.Size (Window (height, width), size)
import           TypingTest

ui :: Conf -> Arguments -> IO ()
ui conf args = do
  chan <- newBChan 10
  {- ↓ separate thread that sends a tick to the brick app every second, used for timed mode-}
  _ <- forkIO $
    forever $ do
      threadDelay 1000000
      writeBChan chan Tick
  let buildVty = mkVty defaultConfig
  dim <- getWindowSize
  initialVty <- buildVty
  {- ↓ Vty does not conform well with custom escape sequences, such as changing the cursor.
    Therefore this frame is drawn before the initialisation of the brick app. It renders
    the string "\ESC[x q" which is an escape sequence to change the terminal cursor-}
  let firstFrame = picForImage (string (fg white) ("\ESC[" ++ show (cursorShape conf) ++ " q"))
  update initialVty firstFrame
  {- ↑ will be removed if Vty implements changing the cursor shape-}
  initialState <- buildInitialState dim conf args
  let fgcolor = uncurry3 rgbColor (fgColor conf)
  endState <- customMain initialVty buildVty (Just chan) (htyper (fg fgcolor)) initialState
  return ()

{-constant Attribute names-}
(standard, corr, wrong, unfilled) =
  ( attrName "standard",
    attrName "correct",
    attrName "wrong",
    attrName "unfilled"
  )



data Tick = Tick

type Name = ()

htyper :: Attr -> App TestState Tick Name
htyper fgcolor =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = handleInputEvent,
      appStartEvent = pure,
      appAttrMap = const $ attrMap mempty [(standard, fg white), (corr, fgcolor), (wrong, fg red), (unfilled, fg brightBlack)]
    }

{- draws either the typing test or the results depending on state -}
drawUI :: TestState -> [Widget Name]
drawUI s = case screen s of
             0 -> drawTestScreen s
             1 -> drawResultScreen s
             2 -> drawHistoryScreen (results s)

getWindowSize :: IO (Int, Int)
getWindowSize = do
  w <- size @Int
  return (maybe 0 width w, maybe 0 height w)

drawResultScreen :: TestState -> [Widget Name]
drawResultScreen s =
  [ borderWLabel
      " results "
      vBox
      [ vLimitPercent 40 $
          vhCenter $
            hBox
              [ drawStats s,
                drawKeyInfo s
              ],
        borderWLabel " wpm over time " vhCenter $
          drawWpmFunc (fst (dimensions s) - 7, round (0.6 * fromIntegral (snd (dimensions s))) - 5) s,
        hCenter $ str "quit: CTRL-q, restart: CTRL-r, history: CTRL-g"
      ]
  ]

drawStats :: TestState -> Widget Name
drawStats s =
  borderWLabel " stats " vhCenter $
    vBox $
      map
        txt
        [ sformat ("wpm: " % fixed 2) (getWPM s),
          "\n",
          sformat ("raw wpm: " % fixed 2) (getRawWPM s),
          "\n",
          sformat ("accuracy: " % fixed 2 % " | " % stext) (getAccuracy s) (getInputStats s),
          "\n",
          sformat ("consistency: " % fixed 2 % "%") (getConsistency s)
        ]

drawKeyInfo :: TestState -> Widget Name
drawKeyInfo s =
  borderWLabel " worst keys " vhCenter $
    vBox $
      map
        (\cerr -> txt (sformat (stext % ": " % fixed 2 % "%") (pack [char cerr]) (100.0 * (1.0 - errorRate cerr))))
        (take 5 (sortBy (flip compare `on` errorRate) (getErrorsPerChar s)))

drawTestScreen :: TestState -> [Widget Name]
drawTestScreen s =
  [ borderWLabel " htyper " vhCenter $
      vBox
        [ if mode (args s) == Timed then str (show (time_left s)) else str "",
          showCursor () (Location (bimap getCol getRow curLoc)) $
            vBox $
              map (hBox . map drawWord) (getActiveLines s 3)
        ]
  ]
  where
    curLoc = getCursorLoc s

drawHistoryScreen :: [TestRes] -> [Widget Name]
drawHistoryScreen res =
  [borderWLabel " history " vhCenter $
    (case res of
      [] -> vhCenter (str "Nothing to display")
      (x:xs) -> vBox $
        [ vLimitPercent 10 $ vhCenter $ txt $ sformat ("tests taken: " % int) (length res),
          borderWLabel " average result " (vLimitPercent 40) $ vhCenter $
          vBox $ drawRes avg,
        borderWLabel " best results " vhCenter $
          hBox $ map (hCenter . vBox) (transpose (map str ["wpm", "raw", "acc", "cons"] :
            (map str ["\n","\n","\n","\n"] : map drawResNoPre (take 10 sorted)))),
        hCenter $ str "quit: CTRL-q, restart: CTRL-r, history: CTRL-g"])]
    where
      avg = avgRes res
      sorted = sortRes res

drawRes :: TestRes -> [Widget Name]
drawRes r =
    map
      txt
        [ sformat ("wpm: " % fixed 2) (wpm r),
          "\n",
          sformat ("raw wpm: " % fixed 2) (raw r),
          "\n",
          sformat ("accuracy: " % fixed 2 % "%") (acc r),
          "\n",
          sformat ("consistency: " % fixed 2 % "%") (cons r)
        ]

drawResNoPre :: TestRes -> [Widget Name]
drawResNoPre r =
    map
      txt
        [ sformat (fixed 2) (wpm r),
          sformat (fixed 2) (raw r),
          sformat (fixed 2 % "%") (acc r),
          sformat (fixed 2 % "%") (cons r)
        ]

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
      | even i && round x < 100 = str (show (round x) ++ "   ")
      | even i && round x >= 100 = str (show (round x) ++ "  ")
      | otherwise = str "     "

drawWord :: TestWord -> Widget n
drawWord w =
  case input w of
    "" -> hBox [withAttr unfilled (str (word w)), withAttr standard (str " ")]
    _ -> hBox $ map drawChar (zipWithPad ' ' (word w) (input w)) ++ [withAttr standard (str " ")]

{- draws and colors a char, who's color depends on whether the input and word match -}
drawChar :: (Char, Char) -> Widget n
drawChar (c1, c2)
  | c1 == ' ' = withAttr wrong (str [c2])
  | c2 == ' ' = withAttr unfilled (str [c1])
  | c1 == c2 = withAttr corr (str [c1])
  | c1 /= c2 = withAttr wrong (str [c2])
  | otherwise = str [' ']

{- Ctrl-q exits htyper, Ctrl-r reloads htyper, Ctrl-g show test history, any other input is handled by the test -}
handleInputEvent :: TestState -> BrickEvent Name Tick -> EventM n (Next TestState)
handleInputEvent s i =
  case i of
    VtyEvent vtye ->
      case vtye of
        EvKey KBS [] -> if screen s == 0 then handleBackSpaceInput s else continue s
        EvKey (KChar '\t') [] -> continue s
        EvKey (KChar 'q') [MCtrl] -> halt s
        EvKey (KChar 'r') [MCtrl] -> liftIO (rebuildInitialState s) >>= continue
        EvKey (KChar 'g') [MCtrl] -> continue $ s {screen = 2}
        EvKey (KChar c) [] -> if screen s == 0 then handleTextInput s c else continue s
        _ -> continue s
    AppEvent Tick -> do
      dim <- liftIO getWindowSize
      if mode (args s) == Timed
        then case time_left s - 1 of
          0 -> continue (s {time_left = 0, screen = 1, dimensions = dim})
          x -> continue (s {time_left = x, dimensions = dim})
        else continue (s {dimensions = dim})
    _ -> continue s

vhCenter :: Widget n -> Widget n
vhCenter = vCenter . hCenter

borderWLabel :: String -> (a -> Widget n) -> a -> Widget n
borderWLabel label content = borderWithLabel (str label) . content


{- resets the state of the test -}
rebuildInitialState :: TestState -> IO TestState
rebuildInitialState s = buildInitialState (dimensions s) (config s) (args s)

{- zipWith that extends the shorter String with the given char -}
zipWithPad :: Char -> String -> String -> [(Char, Char)]
zipWithPad c (x : xs) (y : ys) = (x, y) : zipWithPad c xs ys
zipWithPad c [] ys             = zip (repeat c) ys
zipWithPad c xs []             = zip xs (repeat c)

{- transforms a function that takes three inputs to a function that takes a tuple with three elements -}
uncurry3                 :: (a -> b -> c -> d) -> ((a, b, c) -> d)
uncurry3 f (a, b, c)     =  f a b c
