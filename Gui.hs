{-# LANGUAGE PatternGuards #-}
module Gui where
import Graphics.Gloss.Interface.Game
import Graphics.Gloss.Data.Extent
import System.Environment
import Data.List
import Data.Function
import Data.Maybe (fromJust)

import Game hiding (main)

type World = Int
main = guimain
guimain = do
  b <- initRandomBoard 8 8
  bs <- mapM (const $ initRandomBoard 8 8) [1..4]
  gameInWindow 
          "Hey That's My Fish - Haskell UI" --name of the window
          (800,700) -- initial size of the window
          (10, 10) -- initial position of the window
          white   -- background colour
          30 -- number of simulation steps to take for each second of real time
          (b,bs) -- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          (const id)

handleInput :: Event -> GS -> GS
handleInput (EventKey (Char c)               Down  _   (x,y))  b = handleChar c b
handleInput (EventKey (MouseButton LeftButton) Down  _ (x,y))  b = b
handleInput _ b = b
type GS = (Board,[Board])
handleChar :: Char -> GS -> GS
handleChar 'r' (b,bs)             = (makeRandomMoveIfUnfinished b, bs)
handleChar 'b' (b,bs)             = (makeBestMoveIfUnfinished b, bs)
handleChar 'n' (b,(nextb:others)) = (nextb,others)
handleChar _ b = b
 
drawState :: (Board,[Board])-> Picture
drawState (b,_) = Pictures $ [
  drawBoard b 
  , Color green $ Line [(10,20), (200,300)]
  ]

drawBoard b = Pictures $ [
                 drawLines (-400,300) ((lines . displayBoard) b)
               , drawLines (-400,-100) ["hello", "world"] ]

drawLines :: (Float, Float) -> [String] -> Picture
drawLines (x,y) ls = Translate x y $ 
               Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,y) = textAt 10 (y*(-20)) l

textAt x y content = Translate x y (Scale 0.1 0.1 (Text content))
