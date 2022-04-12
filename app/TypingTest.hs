module TypingTest
  ( --data
    TestState (..),
    TestWord (..),
    Arguments (..),
    Mode (..),
    CharErrorRate (..),
    --state functions
    buildInitialState,
    handleTextInput,
    handleBackSpaceInput,
    --stat functions
    getWPM,
    getRawWPM,
    getAccuracy,
    getConsistency,
    getInputStats,
    getErrorsPerChar,
    approxWpmFunc,
    --cursor functions
    getCursorLoc,
    getActiveCharLoc,
    getActiveLineLoc,
    --other
    getActiveLines,
    diffInSeconds,
    diffOfPairs,
    getStartEndTime,
    shrinkToSize,
  )
where

import Brick.Main (continue)
import Brick.Types (EventM, Next)
import Brick.Widgets.FileBrowser (fileBrowserAttr)
import Control.Monad (ap, liftM2)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Cursor.Simple.List.NonEmpty
import Data.Char (toLower)
import Data.List (isPrefixOf)
import qualified Data.List.NonEmpty as NE
import Data.List.Split (chunksOf, splitOn)
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import Paths_htyper (getDataFileName)
import System.Exit (die)
import System.Random (newStdGen)
import System.Random.Shuffle (shuffle')

data TestState = TestState
  { text :: NonEmptyCursor TestWord,
    tevents :: [TestEvent],
    done :: Bool,
    time_left :: Int,
    args :: Arguments
  }

data Arguments = Arguments
  { mode :: Mode,
    time :: Int,
    linelen :: LineLength,
    numwords :: Int
  }

data Mode = Quote | Random | Timed deriving (Eq)

data TestWord = TestWord
  { word :: String,
    input :: String
  }

data TestEvent = TestEvent
  { timestamp :: UTCTime,
    correct :: Bool,
    input_char :: Char
  }
  deriving (Eq)

data CharErrorRate = CharErrorRate
  { char :: Char,
    errorRate :: Double
  }

instance Eq CharErrorRate where
  (CharErrorRate _ er1) == (CharErrorRate _ er2) = er1 == er2

instance Ord CharErrorRate where
  (CharErrorRate _ er1) `compare` (CharErrorRate _ er2) = er1 `compare` er2

type Row = Int

type Col = Int

type LineLength = Int

buildInitialState :: Arguments -> Int -> IO TestState
buildInitialState args most_common = do
  case mode args of
    Quote -> do
      test_words <- getTextFile Quote >>= getRandomQuote
      toTestState args test_words
    Random -> do
      file <- getTextFile Random
      test_words <- getRandomWords file most_common (numwords args)
      toTestState args test_words
    Timed -> do
      file <- getTextFile Timed
      test_words <- getRandomWords file most_common most_common
      toTestState args test_words

{-stat functions -}

--(#correct inputs / 5) / time passed, since the average english word is 5 characters long
getWPM :: TestState -> Double
getWPM s = (amountCorrectInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

--same as above, but counts all inputs
getRawWPM :: TestState -> Double
getRawWPM s = (amountInputs s / 5.0) / (diffInSeconds (getStartEndTime s) / 60.0)

--percentage of correct inputs / total inputs
getAccuracy :: TestState -> Double
getAccuracy s = 100.0 * (amountCorrectInputs s / amountInputs s)

-- gets the coefficient of variation of raw wpm per word (5 inputs) and normalizes it with a logisitic function
getConsistency :: TestState -> Double
getConsistency = (* 100.0) . normalize . coeffOfVariation . rawWpmAvgPerWord

getInputStats :: TestState -> String
getInputStats s = show (round (amountCorrectInputs s) :: Int) ++ "/" ++ show (round (amountInputs s) :: Int)

getErrorsPerChar :: TestState -> [CharErrorRate]
getErrorsPerChar s = map (\l -> CharErrorRate {char = input_char (head l), errorRate = getErrorRate l}) (getTestEventsPerChar s)

--approximates the typing speed at a certain timestamp using lagrange interpolation
approxWpmFunc :: TestState -> Double -> Double
approxWpmFunc s timepoint =
  120.0
    / evalPoly
      (getLagrangeBasis (diffOfPairs 10 (tevents s)))
      (map ((abs . realToFrac . diffUTCTime (fst (getStartEndTime s))) . timestamp) (tevents s))
      timepoint

{- cursor location functions -}

--coordinates to place cursor in UI
getCursorLoc :: TestState -> (Col, Row)
getCursorLoc s = (getActiveCharLoc s, getActiveLineLoc s)

--the row in which the currently edited line is in
getActiveLineLoc :: TestState -> Row
getActiveLineLoc s = if getWordLocInText (text s) + 1 > linelen (args s) then 1 else 0

--the column of the active line in which the currently edited char is in
getActiveCharLoc :: TestState -> Col
getActiveCharLoc s =
  getLengthOfWords (take (getWordLocInLine s) (getActiveLine s))
    + length (input (nonEmptyCursorCurrent (text s)))

--returns lines around the active line to draw
getActiveLines :: TestState -> Int -> [[TestWord]]
getActiveLines = flip take . ap (drop . (+ (-1)) . activeLineNum) getLines

{- input handling functions -}

--adds new InputEvent to State and advances State logic
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
          case makeNonEmptyCursorWithSelection (getWordLocInText cursor) ne of
            Nothing -> continue s
            Just ne' -> do
              liftIO (addTestEvent (isInputCorrect cur_word c) c (s {text = ne'})) >>= continue
  where
    cursor = text s
    cur_word = nonEmptyCursorCurrent cursor

--either removes a char from the active word or jumps to the previous word if empty
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
          case makeNonEmptyCursorWithSelection (getWordLocInText cursor) ne of
            Nothing -> continue s
            Just ne' -> continue $ s {text = ne'}
  where
    cursor = text s
    cur_word = nonEmptyCursorCurrent cursor

{- Internal Helper functions -}

rawWpmAvgPerWord :: TestState -> [Double]
rawWpmAvgPerWord = map ((60.0 /) . sum) . chunksOf 5 . diffOfPairs 1 . tevents

--difference in time between shifted pairs in list
diffOfPairs :: Int -> [TestEvent] -> [Double]
diffOfPairs shift l = zipWith (\x y -> diffInSeconds (timestamp x, timestamp y)) l (drop shift l)

deleteAt :: Int -> [a] -> [a]
deleteAt id xs = left ++ right
  where
    (left, r : right) = splitAt id xs

shrinkToSize :: Int -> [Double] -> [Double]
shrinkToSize size list = concat (map init (group n list))
  where
    n = round (1.0 / (1.0 - (1.0 / (fromIntegral (length list) / fromIntegral size))))

group :: Int -> [Double] -> [[Double]]
group n [] = []
group n xs = take n xs : group n (drop n xs)

--number of words before current word
diffInSeconds :: (UTCTime, UTCTime) -> Double
diffInSeconds = uncurry (((abs . realToFrac) .) . flip diffUTCTime)

--timestamp of first and last input
getStartEndTime :: TestState -> (UTCTime, UTCTime)
getStartEndTime = ap ((,) . timestamp . last . tevents) (timestamp . head . tevents)

amountCorrectInputs :: TestState -> Double
amountCorrectInputs = fromIntegral . length . filter correct . tevents

amountWrongInputs :: TestState -> Double
amountWrongInputs = fromIntegral . length . filter (not . correct) . tevents

amountInputs :: TestState -> Double
amountInputs = fromIntegral . length . tevents

--number of lines before the active line
activeLineNum :: TestState -> Int
activeLineNum = ap (div . getWordLocInText . text) (linelen . args)

--splits words into lines of linelength
getLines :: TestState -> [[TestWord]]
getLines = ap (chunksOf . linelen . args) (NE.toList . rebuildNonEmptyCursor . text)

--gets words in active line
getActiveLine :: TestState -> [TestWord]
getActiveLine = ap ((!!) . getLines) activeLineNum

--length of words including spaces, takes the max of the length of the input or word itself
getLengthOfWords :: [TestWord] -> LineLength
getLengthOfWords = sum . map (\w -> 1 + max (length (input w)) (length (word w)))

--amount of words before current word in active line
getWordLocInLine :: TestState -> Int
getWordLocInLine = ap (mod . getWordLocInText . text) (linelen . args)

getWordLocInText :: NonEmptyCursor a -> Int
getWordLocInText = length . nonEmptyCursorPrev

isInputCorrect :: TestWord -> Char -> Bool
isInputCorrect w c = (input w ++ [c]) `isPrefixOf` word w

getTestEventsPerChar :: TestState -> [[TestEvent]]
getTestEventsPerChar s = filter (/= []) [filter ((== c) . toLower . input_char) (tevents s) | c <- ['a' .. 'z']]

getErrorRate :: [TestEvent] -> Double
getErrorRate list = fromIntegral (length (filter (not . correct) list)) / fromIntegral (length list)

addTestEvent :: Bool -> Char -> TestState -> IO TestState
addTestEvent b c s = do
  cur_time <- getCurrentTime
  let test_event = TestEvent {timestamp = cur_time, correct = b, input_char = c}
  return s {tevents = test_event : tevents s}

toTestWord :: String -> TestWord
toTestWord s = TestWord {word = s, input = ""}

toTestState :: Arguments -> [TestWord] -> IO TestState
toTestState args twords =
  case NE.nonEmpty twords of
    Nothing -> die "No Words to display"
    Just txt -> pure TestState {text = makeNonEmptyCursor txt, tevents = [], done = False, args = args, time_left = time args}

--shuffles most_common amount of words from a file and returns num_words of them
getRandomWords :: FilePath -> Int -> Int -> IO [TestWord]
getRandomWords file most_common num_words = do
  rng <- newStdGen
  wordfile <- readFile file
  let word_list = take most_common (words wordfile)
  let sampled_words = take num_words (shuffle' word_list (length word_list) rng)
  return (map toTestWord sampled_words)

--gets a random quote from a file, quotes should be delimited by a ^_^
getRandomQuote :: FilePath -> IO [TestWord]
getRandomQuote file = do
  rng <- newStdGen
  quotefile <- readFile file
  let quotes = splitOn "^_^" quotefile
  let rand_quote = head (shuffle' quotes (length quotes) rng)
  return (map toTestWord (words rand_quote))

--returns the quote file or the most common words file, depending on the mode of the test
getTextFile :: Mode -> IO FilePath
getTextFile mode = if mode == Quote then getDataFileName "quote.txt" else getDataFileName "1000us.txt"

{- Math functions -}

--normalize value from R to (0, 1) using an inverse exponential functon
normalize :: Double -> Double
normalize = exp . ((-0.9) *)

coeffOfVariation :: [Double] -> Double
coeffOfVariation = ap ((/) . stdev) mean

mean :: [Double] -> Double
mean = ap ((/) . sum) (fromIntegral . length)

stdev :: [Double] -> Double
stdev v = sqrt (sum (map ((** 2) . (+ (-(mean v)))) v) / fromIntegral (length v - 1))

evalPoly :: [(Double -> Double)] -> [Double] -> Double -> Double
evalPoly basis ycoords x = sum (zipWith (*) ycoords (map (\b -> b x) basis))

--returns the lagrange basis polys of a list of data points
getLagrangeBasis :: [Double] -> [(Double -> Double)]
getLagrangeBasis d = [getLagrangePolynomial d idx | idx <- [0 .. (length d - 1)]]

--returns a single lagrange basis polynomial at the specified index
getLagrangePolynomial :: [Double] -> Int -> (Double -> Double)
getLagrangePolynomial d idx = \x -> product [(x - xm) / ((d !! idx) - xm) | xm <- (deleteAt idx d)]