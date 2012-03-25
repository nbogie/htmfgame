module Main where
import System( getArgs )
import Data.List (sort)
import Control.Parallel (par, pseq)
import Control.Concurrent
import Game hiding (main)
import ConTestData

main = do
  o <- getArgs
  let b = testboard
  let ms = legalMovesForPlayer b (nextPlayer b)
  case o of
    ("c":_) -> concurrent b ms
    ("s":_) -> simple b ms
    ("g":_) -> gen
    
    _   -> print "usage: prog c|s # for concurrent or sequential"
gen = do
  b <- initRandomBoard 5 5 4
  putStrLn $ show b

simple b ms = do
  print "simple"
  return (bestMove 5 b ms) >>= print

concurrent b ms = do
  print "concurrent"
  bestMoveConcurrent 5 b ms >>= print
