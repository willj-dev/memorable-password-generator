module WordList (WordList, loadWordList) where

import Prelude hiding (readFile)

import Paths_memorable_password_generator (getDataFileName)

import Text.Megaparsec (Parsec, ParseErrorBundle, parse)
import Text.Megaparsec.Char (eol, digitChar, tab, lowerChar)

import Control.Monad.Combinators (sepEndBy, count, some)
import Control.Applicative ((*>))

import Data.Array (Array, listArray)
import Data.Text (Text, pack)
import Data.Text.IO (readFile)
import Data.Void (Void)

type WordList = Array Int Text

type ParseError = ParseErrorBundle Text Void
type Parser = Parsec Void Text

wordListParser :: Parser WordList
wordListParser = arrList <$> wordlistLine `sepEndBy` eol where
    wordlistLine = count 5 digitChar *> tab *> (pack <$> some lowerChar)
    arrList xs = listArray (0, length xs - 1) xs

loadWordList :: IO (Either ParseError WordList)
loadWordList = do
    fileName <- getDataFileName "data/eff_large_wordlist.txt"
    listText <- readFile fileName
    return $ parse wordListParser "data/eff_large_wordlist.txt" listText