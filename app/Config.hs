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



createDefaultConfig :: IO ()
createDefaultConfig = do
    home <- getHomeDirectory
    _ <- createDirectoryIfMissing True (home ++ "/.config/htyper")

    let text = "fgcolor = ff79c6\ncursorshape = 5\nnumcommonwords = 250"
    writeFile (home ++ "/.config/htyper/htyper.conf") text


readConfig :: IO Conf
readConfig = do
    home <- getHomeDirectory
    let fp = home ++ "/.config/htyper/htyper.conf"
    confExists <- doesFileExist fp
    unless confExists createDefaultConfig
    conf <- readFile fp

    let lines = splitOn "\n" conf
    let fC = hexToRgb (fst (head (readHex (getSettingByName lines "fgcolor"))))
    let cS = getSettingByName lines "cursorshape"
    let nC = getSettingByName lines "numcommonwords"

    return Conf {fgColor = fC, cursorShape = read cS, numCommonWords = read nC}

hexToRgb :: Int -> (Int, Int, Int)
hexToRgb hx = (hx `shiftR` 16, (hx `shiftR` 8) `mod` 0xff00, hx `mod` 0xffff00)

getSettingByName :: [String] -> String -> String
getSettingByName (x:xs) setting
    | setting `isPrefixOf` x = getSetting x
    | otherwise = getSettingByName xs setting
getSettingByName [] setting = ""

getSetting :: String -> String
getSetting str = words str !! 2
