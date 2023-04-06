module MemorablePasswordGenerator where

import Prelude hiding (readFile, lines, words)

import Data.Text (Text, lines, toTitle, words)
import Data.Text.Lazy (toStrict)
import Data.Text.IO (readFile)
import Data.Text.Format
import System.Random
import Data.Array (Array, listArray, bounds, (!))

import Polysemy
import Polysemy.State
import Polysemy.Reader

type PwGen g = (g, Text)
type WordList = Array Int Text

updateGen :: Member (State (PwGen g)) r => g -> Sem r ()
updateGen g = gets snd >>= \pw -> put (g, pw)

addDigits :: (RandomGen g, Member (State (PwGen g)) r) => Sem r ()
addDigits = do
    (gen, pw) <- get
    let
        (num, gen') = randomR @Int (0, 99) gen
        pw' = toStrict $ format "%s%02d" (pw, num)
    put (gen', pw')

pickAnyWord :: (RandomGen g, Members [State (PwGen g), Reader WordList] r) => Sem r Text
pickAnyWord = do
    gen <- gets fst
    wordList <- ask
    let
        (i, gen') = randomR (bounds wordList) gen
        (shouldCapitalize, gen'') = random gen'
        possiblyCapitalize = if shouldCapitalize then toTitle else id
    updateGen gen''
    return $ possiblyCapitalize (wordList ! i)

addWord :: (RandomGen g, Members [State (PwGen g), Reader WordList] r) => Sem r ()
addWord = do
    word <- pickAnyWord
    (gen, pw) <- get
    let pw' = toStrict $ format "%s%s" (pw, word)
    put (gen, pw')

parseWl :: Text -> Array Int Text
parseWl content = listArray (0, nwords - 1) wordlistWords where
    wordlistLines = lines content
    wordlistWords = map (last . words) wordlistLines
    nwords = length wordlistWords

loadWordlist :: IO WordList
loadWordlist = do
    wordlistContent <- readFile "eff_large_wordlist.txt"
    return $ parseWl wordlistContent
