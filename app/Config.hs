module Config (Conf (..), readConfig) where

import           Control.Monad    (unless)
import           Data.Bits        (shiftR)
import           Data.List        (isPrefixOf)
import           Data.List.Split  (chunksOf, splitOn)
import           Numeric          (readHex)
import           System.Directory (createDirectoryIfMissing, doesFileExist,
                                   getHomeDirectory)

data Conf = Conf
    {
        fgColor        :: (Int, Int, Int),
        cursorShape    :: Int,
        numCommonWords :: Int
    }


createResFile :: IO ()
createResFile = do
    home <- getHomeDirectory
    let resfile = home ++ "/.config/htyper/results.txt"
    fileExists <- doesFileExist resfile
    unless fileExists (writeFile resfile "")

{- Creates and initializes default config in ~/.config/htyper/htyper.conf -}
createDefaultConfig :: IO ()
createDefaultConfig = do
    home <- getHomeDirectory
    _ <- createDirectoryIfMissing True (home ++ "/.config/htyper")

    let text = "fgcolor = fc6f03\ncursorshape = 5\nnumcommonwords = 250"
    writeFile (home ++ "/.config/htyper/htyper.conf") text

{- Creates Conf from settings found in ~/.config/htyper/htyper.conf, if file doesn't exist
   it creates and initializes the file -}
readConfig :: IO Conf
readConfig = do
    home <- getHomeDirectory
    let fp = home ++ "/.config/htyper/htyper.conf"
    confExists <- doesFileExist fp
    unless confExists createDefaultConfig
    createResFile
    conf <- readFile fp

    let lines = splitOn "\n" conf
    let fC = hexToRgb (fst (head (readHex (getSettingByName lines "fgcolor" "fc6f03"))))
    let cS = read (getSettingByName lines "cursorshape" "5")
    let nC = read (getSettingByName lines "numcommonwords" "250")

    return Conf {fgColor = fC, cursorShape = cS, numCommonWords = nC}

hexToRgb :: Int -> (Int, Int, Int)
hexToRgb hx = (hx `shiftR` 16, (hx `shiftR` 8) `mod` 0x100, hx `mod` 0x100)

{- Takes a list of strings of the form "setting = value" and returns the value of the specified setting
   If setting can't be found returns default -}
getSettingByName :: [String] -> String -> String -> String
getSettingByName (x:xs) setting defaultStr
    | setting `isPrefixOf` x = getSetting x
    | otherwise = getSettingByName xs setting defaultStr
getSettingByName [] setting defaultStr = defaultStr

{- returns value from string of form "setting = value" -}
getSetting :: String -> String
getSetting str = words str !! 2
