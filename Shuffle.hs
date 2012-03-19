{- 
   Miscelaneous utility functions 
   Pedro Vasconcelos, 2009
-}

module Shuffle where
import Random
import Data.Array.MArray
import Data.Array.IO

--
-- Knuth-Fisher-Yates shuffling algorithm
--
shuffleIO :: [a] -> IO [a]
shuffleIO xs
    = do { arr <- newListArray (0,n) [0..n] :: IO (IOUArray Int Int)  
         ; sequence_ [do { j<-randomRIO (i,n)
                         ; t1<-readArray arr i
                         ; t2<-readArray arr j
                         ; writeArray arr i t2
                         ; writeArray arr j t1                            
                         } | i<-[0..n-1]]
         ; sequence [do j<-readArray arr i
                        return (xs!!j)  | i<-[0..n]]
         }
    where n = length xs - 1
                 
