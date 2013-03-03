-- http://www.haskell.org/haskellwiki/Random_shuffle 
module Shuffle where

import System.Random
import Data.Array.IO
import Control.Monad
 
-- | Randomly shuffle a list
--   /O(N)/
shuffleIO :: [a] -> IO [a]
shuffleIO xs = do
        ar <- newArray n xs
        forM [1..n] $ \i -> do
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs =  newListArray (1,n) xs
