{-# LANGUAGE OverloadedStrings #-}

module TypingTest
  ( {- data -}
    TestState (..),
    TestWord (..),
    TestRes (..),
    Arguments (..),
    Mode (..),
    CharErrorRate (..),
    Row (..),
    Col (..),
    {- state functions -}
    buildInitialState,
    handleTextInput,
    handleBackSpaceInput,
    {- stat functions -}
    getWPM,
    getRawWPM,
    getAccuracy,
    getConsistency,
    getInputStats,
    getErrorsPerChar,
    get10KeyRawWpm,
    {- cursor functions -}
    getCursorLoc,
    getActiveCharLoc,
    getActiveLineLoc,
    {- result functions -}
    avgRes,
    sortRes,
    {- other -}
    getActiveLines,
  )
where

import           Brick.Main                  (continue)
import           Brick.Types                 (EventM, Next)
import           Config
import           Control.Monad               ((<$!>))
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Cursor.Simple.List.NonEmpty
import qualified Data.ByteString             as BS
import qualified Data.ByteString.UTF8        as BSU
import           Data.Char                   (isSpace, toLower)
import           Data.Function               (on)
import           Data.List                   (intercalate, isPrefixOf, sortBy)
import qualified Data.List.NonEmpty          as NE
import           Data.List.Split             (chunksOf, splitOn)
import           Data.Text                   (Text)
import           Data.Time.Clock             (UTCTime, diffUTCTime,
                                              getCurrentTime)
import           Formatting                  (int, sformat, (%))
import           Paths_htyper                (getDataFileName)
import           System.Directory            (getHomeDirectory)
import           System.Exit                 (die)
import           System.Random               (newStdGen)
import           System.Random.Shuffle       (shuffle')

data TestState = TestState
  { text       :: NonEmptyCursor TestWord,
    tevents    :: [TestEvent],
    screen     :: Int, {- 0 = test, 1 = result, 2 = history -}
    dimensions :: (Int, Int),
    time_left  :: Int,
    config     :: Conf,
    args       :: Arguments,
    results    :: [TestRes]
  }

data Arguments = Arguments
  { mode     :: Mode,
    time     :: Int,
    linelen  :: LineLength,
    numwords :: Int
  }

data Mode = Quote | Random | Timed deriving (Eq)

data TestWord = TestWord
  { word  :: String,
    input :: String
  }

data TestEvent = TestEvent
  { timestamp  :: UTCTime,
    correct    :: Bool,
    input_char :: Char
  }
  deriving (Eq)

data TestRes = TestRes
  { wpm  :: Double,
    raw  :: Double,
    acc  :: Double,
    cons :: Double
  }

data CharErrorRate = CharErrorRate
  { char      :: Char,
    errorRate :: Double
  }

newtype Row = Row {getRow :: Int}

newtype Col = Col {getCol :: Int}

type LineLength = Int

buildInitialState :: (Int, Int) -> Conf -> Arguments -> IO TestState
buildInitialState dim conf args = do
  let ncw = numCommonWords conf
  textfile <- getTextFile (mode args)
  test_words <- case mode args of
    Quote  -> getRandomQuote textfile
    Random -> getRandomWords textfile ncw (numwords args)
    Timed  -> getRandomWords textfile ncw ncw
  toTestState dim conf args test_words

{-stat functions -}

{- (#correct inputs / 5) / time passed, since the average english word is 5 characters long -}
getWPM :: TestState -> Double
getWPM s = (amountCorrectInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

{- same as above, but counts all inputs -}
getRawWPM :: TestState -> Double
getRawWPM s = (amountInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

{- percentage of correct inputs / total inputs -}
getAccuracy :: TestState -> Double
getAccuracy s = 100.0 * (amountCorrectInputs s / amountInputs s)

{- gets the coefficient of variation of raw wpm per word (5 inputs) and normalizes it with a logistic function -}
getConsistency :: TestState -> Double
getConsistency = (* 100.0) . normalize . coeffOfVariation . nKeyRawWpm 5

getInputStats :: TestState -> Text
getInputStats s = sformat (int % "/" % int) (round (amountCorrectInputs s)) (round (amountInputs s))

getErrorsPerChar :: TestState -> [CharErrorRate]
getErrorsPerChar s = map (\l -> CharErrorRate {char = input_char (head l), errorRate = getErrorRate l}) (getTestEventsPerChar s)

{- 10 key rollover raw wpm, meaning the wpm at every keystroke averaged over the last 10 keystrokes -}
get10KeyRawWpm :: Int -> TestState -> [Double]
get10KeyRawWpm cols s = [getClosestToIndex inputlist timepoint | timepoint <- [0.0, step .. timespan]]
  where
    inputlist = zip (nKeyRawWpm 10 s) (getTimePoints s)
    timespan = diffInSeconds (getStartEndTime s)
    step = timespan / fromIntegral cols

{- cursor location functions -}

{- coordinates to place cursor in UI -}
getCursorLoc :: TestState -> (Col, Row)
getCursorLoc s = (getActiveCharLoc s, getActiveLineLoc s)

{- the row in which the currently edited line is in -}
getActiveLineLoc :: TestState -> Row
getActiveLineLoc s = Row {getRow = if getWordLocInText (text s) + 1 > linelen (args s) then 1 else 0}

{- the column of the active line in which the currently edited char is in -}
getActiveCharLoc :: TestState -> Col
getActiveCharLoc s =
  Col
    { getCol =
        getLengthOfWords (take (getWordLocInLine s) (getActiveLine s))
          + length (input (nonEmptyCursorCurrent (text s)))
    }

{- returns lines around the active line to draw -}
getActiveLines :: TestState -> Int -> [[TestWord]]
getActiveLines = flip take . ((drop . (+ (-1)) . activeLineNum) <*> getLines)

{- input handling functions -}

{- adds new InputEvent to State and advances State logic -}
handleTextInput :: TestState -> Char -> EventM n (Next TestState)
handleTextInput s c =
  case c of
    ' ' -> do
      case nonEmptyCursorSelectNext cursor of
        Nothing -> do
          liftIO (saveRes s)
          continue $ s {screen = 1}
        Just cursor' -> liftIO (addTestEvent True ' ' (s {text = cursor'})) >>= continue
    _ -> do
      let new_word = TestWord {word = word cur_word, input = input cur_word ++ [c]}
      let new_text = reverse (nonEmptyCursorPrev cursor) ++ [new_word] ++ nonEmptyCursorNext cursor
      case NE.nonEmpty new_text of
        Nothing -> continue s
        Just ne -> do
          case makeNonEmptyCursorWithSelection (getWordLocInText cursor) ne of
            Nothing -> continue s
            Just ne' -> do
              liftIO (addTestEvent (isInputCorrect cur_word c) c (s {text = ne'})) >>= continue
  where
    cursor = text s
    cur_word = nonEmptyCursorCurrent cursor

{- either removes a char from the active word or jumps to the previous word if empty -}
handleBackSpaceInput :: TestState -> EventM n (Next TestState)
handleBackSpaceInput s = do
  case input cur_word of
    "" -> do
      case nonEmptyCursorSelectPrev cursor of
        Nothing      -> continue s
        Just cursor' -> continue $ s {text = cursor'}
    _ -> do
      let new_word = TestWord {word = word cur_word, input = init (input cur_word)}
      let new_text = reverse (nonEmptyCursorPrev cursor) ++ [new_word] ++ nonEmptyCursorNext cursor
      case NE.nonEmpty new_text of
        Nothing -> continue s
        Just ne -> do
          case makeNonEmptyCursorWithSelection (getWordLocInText cursor) ne of
            Nothing  -> continue s
            Just ne' -> continue $ s {text = ne'}
  where
    cursor = text s
    cur_word = nonEmptyCursorCurrent cursor

{- Internal Helper functions -}

{- average raw wpm over n key presses -}
nKeyRawWpm :: Int -> TestState -> [Double]
nKeyRawWpm n = map ((60.0 * (fromIntegral n / 5.0)) /) . diffOfPairs n . reverse . tevents

{- returns timestamp in seconds relative to start time for every testinput -}
getTimePoints :: TestState -> [Double]
getTimePoints s = map (\e -> diffInSeconds (timestamp e, fst (getStartEndTime s))) (reverse (tevents s))

{- difference in time between shifted pairs in list, i.e. shift = 10 -> [t1, t2...] -> [t11 - t1, t12 - t2, ...] -}
diffOfPairs :: Int -> [TestEvent] -> [Double]
diffOfPairs shift l = zipWith (\x y -> diffInSeconds (timestamp x, timestamp y)) l (drop shift l)

{- takes list of pairs (entry, index) (sorted by index) and an arbitrary index and returns entry closest to that index -}
getClosestToIndex :: [(Double, Double)] -> Double -> Double
getClosestToIndex (x : (y : xs)) idx
  | snd x <= idx && idx < snd y = fst x
  | otherwise = getClosestToIndex (y : xs) idx
getClosestToIndex [(e, _)] _ = e
getClosestToIndex _ _ = 0

diffInSeconds :: (UTCTime, UTCTime) -> Double
diffInSeconds = uncurry (((abs . realToFrac) .) . flip diffUTCTime)

{- timestamp of first and last input -}
getStartEndTime :: TestState -> (UTCTime, UTCTime)
getStartEndTime = ((,) . timestamp . last . tevents) <*> (timestamp . head . tevents)

{- These two functions return doubles for convenience, however the output value is always a whole number -}
amountCorrectInputs :: TestState -> Double
amountCorrectInputs = fromIntegral . length . filter correct . tevents

amountInputs :: TestState -> Double
amountInputs = fromIntegral . length . tevents

{- number of lines before the active line -}
activeLineNum :: TestState -> Int
activeLineNum = (div . getWordLocInText . text) <*> (linelen . args)

{- splits words into lines of linelength -}
getLines :: TestState -> [[TestWord]]
getLines = (chunksOf . linelen . args) <*> (NE.toList . rebuildNonEmptyCursor . text)

{- gets words in active line -}
getActiveLine :: TestState -> [TestWord]
getActiveLine = ((!!) . getLines) <*> activeLineNum

{- length of words including spaces, takes the max of the length of the input or word itself -}
getLengthOfWords :: [TestWord] -> LineLength
getLengthOfWords = sum . map (\w -> 1 + max (length (input w)) (length (word w)))

{- amount of words before current word in active line -}
getWordLocInLine :: TestState -> Int
getWordLocInLine = (mod . getWordLocInText . text) <*> (linelen . args)

{- number of words in the text before the active word -}
getWordLocInText :: NonEmptyCursor a -> Int
getWordLocInText = length . nonEmptyCursorPrev

isInputCorrect :: TestWord -> Char -> Bool
isInputCorrect w c = (input w ++ [c]) `isPrefixOf` word w

{- list of list of test events for all chars [a-z], doesn't differentiate between upper and lower case -}
getTestEventsPerChar :: TestState -> [[TestEvent]]
getTestEventsPerChar s = filter (/= []) [filter ((== c) . toLower . input_char) (tevents s) | c <- ['a' .. 'z']]

{- used by getCharErrorRate, returns incorrect inputs of that char / total inputs of that char -}
getErrorRate :: [TestEvent] -> Double
getErrorRate list = fromIntegral (length (filter (not . correct) list)) / fromIntegral (length list)


addTestEvent :: Bool -> Char -> TestState -> IO TestState
addTestEvent b c s = do
  cur_time <- getCurrentTime
  let test_event = TestEvent {timestamp = cur_time, correct = b, input_char = c}
  return s {tevents = test_event : tevents s}

toTestWord :: String -> TestWord
toTestWord s = TestWord {word = s, input = ""}

toTestState :: (Int, Int) -> Conf -> Arguments -> [TestWord] -> IO TestState
toTestState dim conf args twords = do
  pastResults <- parseRes
  case NE.nonEmpty twords of
    Nothing -> die "No Words to display"
    Just txt -> pure TestState {
      text = makeNonEmptyCursor txt,
      tevents = [],
      dimensions = dim,
      screen = 0,
      args = args,
      time_left = time args,
      config = conf,
      results = pastResults
    }

{- shuffles most_common amount of words from a file and returns num_words of them -}
getRandomWords :: FilePath -> Int -> Int -> IO [TestWord]
getRandomWords file most_common num_words = do
  rng <- newStdGen
  wordfile <- readFile file
  let word_list = take most_common (words wordfile)
  let sampled_words = take num_words (shuffle' word_list (length word_list) rng)
  return (map toTestWord sampled_words)

{- gets a random quote from a file, quotes should be delimited by a ^_^ -}
getRandomQuote :: FilePath -> IO [TestWord]
getRandomQuote file = do
  rng <- newStdGen
  quotefile <- readFile file
  let quotes = splitOn "^_^" quotefile
  let rand_quote = head (shuffle' quotes (length quotes) rng)
  return (map toTestWord (words rand_quote))

{- returns the quote file or the most common words file, depending on the mode of the test -}
getTextFile :: Mode -> IO FilePath
getTextFile mode = if mode == Quote then getDataFileName "quotes.txt" else getDataFileName "1000us.txt"

{- Result saving and reading functions -}

{- saves the the results of the test to textfiles/results.txt for later evaluation-}
saveRes :: TestState -> IO ()
saveRes s = do
  let wpm = show (getWPM s)
  let rawwpm = show (getRawWPM s)
  let acc = show (getAccuracy s)
  let cons = show (getConsistency s)
  let str = intercalate "," [wpm, rawwpm, acc, cons] ++ "\n"
  home <- getHomeDirectory
  BS.appendFile (home ++ "/.config/htyper/results.txt") (BSU.fromString str)

{- returns list of testresults saved in results.txt -}
parseRes :: IO [TestRes]
parseRes = do
  home <- getHomeDirectory
  content <- BSU.toString <$!> BS.readFile (home ++ "/.config/htyper/results.txt")
  let lines = filter (not . all isSpace) (splitOn "\n" content)
  return (map (toRes . splitOn ",") lines)
    where
      toRes [a, b, c , d] = TestRes {wpm = read a, raw = read b, acc = read c, cons = read d}
      toRes _             = TestRes {wpm = 0, raw = 0, acc = 0, cons = 0}

avgRes :: [TestRes] -> TestRes
avgRes l = do
  let len = fromIntegral (length l)
  let [awpm, araw , aacc, acons] = map ((/ len) . sum) [map wpm l, map raw l, map acc l, map cons l]
  TestRes {wpm = awpm, raw = araw, acc = aacc, cons = acons}

sortRes :: [TestRes] -> [TestRes]
sortRes = sortBy (flip compare `on` wpm)

{- Math functions -}

{- normalize value from R+ to (0, 1) using an inverse exponential functon -}
normalize :: Double -> Double
normalize = exp . ((-0.9) *)

coeffOfVariation :: [Double] -> Double
coeffOfVariation = ((/) . stdev) <*> mean

mean :: [Double] -> Double
mean = ((/) . sum) <*> (fromIntegral . length)

stdev :: [Double] -> Double
stdev v = sqrt (sum (map ((** 2) . (+ (-(mean v)))) v) / fromIntegral (length v - 1))
