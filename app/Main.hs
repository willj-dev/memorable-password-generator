module Main where

import WordList
import MemorablePasswordGenerator
import System.Random (newStdGen)
import Data.Text (unpack)
import Control.Monad (replicateM)
import Text.Megaparsec (errorBundlePretty)

main :: IO ()
main = do
  pwl <- loadWordList
  gens <- replicateM 10 newStdGen
  case pwl of
    Left e -> putStr (errorBundlePretty e)
    Right wl -> mapM_ (putStrLn . unpack . \g -> generatePassword g wl) gens
