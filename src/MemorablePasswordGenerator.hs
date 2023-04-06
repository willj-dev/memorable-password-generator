module MemorablePasswordGenerator where

import Data.Text (Text, toTitle, snoc, pack)
import System.Random
import Data.Array (bounds, (!))
import Data.List ((!!))
import Text.Printf (printf)

import Polysemy
import Polysemy.State
import Polysemy.Reader

import WordList

type PwGen g = (g, Text)

symbols :: String
symbols = "!@#$%^&*()_-+=\\|[]{}'\";:/?,.<>"

nSymbols :: Int
nSymbols = length symbols

addSymbol :: (RandomGen g, Member (State (PwGen g)) r) => Sem r ()
addSymbol = do
    (gen, pw) <- get
    let (i, gen') = randomR (0, nSymbols - 1) gen
    put (gen', snoc pw (symbols !! i))


updateGen :: Member (State (PwGen g)) r => g -> Sem r ()
updateGen g = gets snd >>= \pw -> put (g, pw)

addDigits :: (RandomGen g, Member (State (PwGen g)) r) => Sem r ()
addDigits = do
    (gen, pw) <- get
    let
        (num, gen') = randomR @Int (0, 99) gen
        pw' = pack $ printf "%s%02d" pw num
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
    let pw' = pack $ printf "%s%s" pw word
    put (gen, pw')

generatePassword :: forall g. RandomGen g => g -> WordList -> Text
generatePassword g wl = runGen gen where
    gen :: Members [Reader WordList, State (PwGen g)] r => Sem r ()
    gen = addWord >> addDigits >> addSymbol >> addWord

    runGen = run . fmap (snd . fst) . runState (g, "") . runReader wl