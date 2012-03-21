{-# LANGUAGE PatternGuards #-}
module Gui where
import Graphics.Gloss.Interface.Game
import Graphics.Gloss.Data.Extent
import System.Environment
import Data.List
import Data.Function
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Game hiding (main)

type World = Int
main = guimain
guimain = do
  b <- initRandomBoard 6 6
  bs <- mapM (const $ initRandomBoard 6 6) [1..4]
  gameInWindow 
          "Hey That's My Fish - Haskell UI" --name of the window
          (800,700) -- initial size of the window
          (10, 10) -- initial position of the window
          backgroundColor   -- background colour
          30 -- number of simulation steps to take for each second of real time
          (b,bs) -- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          (const id)

backgroundColor = dark $ dark $ dark blue -- makeColor8 200 200 200 100

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
drawState (b,_) = Pictures $ 
   (Translate (-100) (-100) (drawPlayingArea b)) : 
   [ drawLines white (-400,300) ((lines . displayBoard) b) ]

drawPlayingArea b = 
  Pictures $ [ drawIceState is | is <- M.assocs (posStateMap b)]

iceState :: PositionState -> IceState
iceState (PositionState istate _) = istate

drawIceState :: (Position,PositionState) -> Picture
drawIceState (p,(PositionState ist playerM)) = 
  Translate x y $ Pictures $ 
    [iceOrSea, fish ist, picOrNone penguin playerM]
  where 
    iceOrSea = Color (colorForIceState ist) $ drawHex side
    picOrNone :: (a-> Picture) -> Maybe a -> Picture 
    picOrNone f (Just v) = f v
    picOrNone f Nothing = Pictures []
    penguin :: Player -> Picture
    penguin player = Pictures $ [
      Color black $ rectangleSolid 15 25
      , Color (colorForPlayer player) $ rectangleSolid 7 18  ]
    (x,y) = toUiPos p

fish (Ice fc) = case fc of
  1 -> Color green $ rectangleSolid 25 20
  2 -> Color orange $ rectangleSolid 20 10
  3 -> Color magenta $ rectangleSolid 15 20
  other -> Color red $ textAt 0 0 (show fc)
fish _ = Pictures []
-- Color orange $ rectangleSolid 20 5

colorForPlayer Player1 = yellow
colorForPlayer Player2 = red

colorForIceState :: IceState -> Color
colorForIceState NoIce = blue
colorForIceState (Ice n) = white

type UiPosition = (Float, Float)

toUiPos :: Position -> UiPosition
toUiPos (Position x y) = (fromIntegral x*xSpacing,fromIntegral y*ySpacing)

drawLines :: Color -> (Float, Float) -> [String] -> Picture
drawLines c (x,y) ls = Translate x y $  Color c $
               Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,y) = textAt 10 (y*(-20)) l

textAt x y content = Translate x y (Scale 0.1 0.1 (Text content))


hexPath s = [(-hw,-hs), (-hw, hs), (0, hh), (hw, hs), (hw, -hs), (0, -hh)]
  where
    h = hexHeight
    w = hexWidth
    hw = w/2
    hh = h/2
    hs = s/2
 
drawHex :: Float -> Picture
drawHex s = Pictures $ [ Scale sc sc $ polygon (hexPath s) ]
  where sc = 0.95 

side = 40
hexWidth = side * sqrt 3
hexHeight = 2 * side
xSpacing = hexWidth / 2
ySpacing = (side+hexHeight) / 2
