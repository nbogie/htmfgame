module Gui where
import Graphics.Gloss.Interface.Game
import qualified Data.Map as M
import Game hiding (main)

main = guimain
guimain = do
  b <- initRandomBoard 6 6
  bs <- mapM (const $ initRandomBoard 6 6) ([1..4]::[Int])
  gameInWindow 
          "Hey That's My Fish - Haskell UI" --name of the window
          (800,700) -- initial size of the window
          (10, 10) -- initial position of the window
          backgroundColor   -- background colour
          30 -- number of simulation steps to take for each second of real time
          (b,[], bs) -- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          (const id)

backgroundColor = dark $ dark $ dark blue -- makeColor8 200 200 200 100

type GS = (Board,[Board],[Board]) -- (board in play, undo list, fresh boards)

handleInput :: Event -> GS -> GS
handleInput (EventKey (Char c)               Down  _   _pos)  b = handleChar c b
handleInput (EventKey (MouseButton LeftButton) Down  _ _pos)  b = b
handleInput _ b = b

handleChar :: Char -> GS -> GS
handleChar 'r' (b,undos,bs)             = (makeRandomMoveIfUnfinished b, b:undos, bs)
handleChar 'b' (b,undos,bs)             = (makeBestMoveIfUnfinished b, b:undos, bs)
handleChar 'n' (_, _, nextb:others) = (nextb, [], others)
handleChar 'a' (b, [], bs) = (b, [], bs)
handleChar 'a' (_, undos, bs) = (last undos, [], bs)
handleChar 'u' (_, u:undos, bs) = (u, undos, bs)
handleChar 'u' (b,[],bs)          = (b,[],bs)
handleChar _ b = b
 
drawState :: (Board,[Board],[Board])-> Picture
drawState (b,_,_) = Pictures $ 
   Translate (-100) (-100) (drawPlayingArea b) : 
   [ drawLines white (-400,300) ((lines . displayBoard) b ++ help) ]

help :: [String]
help = [ "---- Keys -------------"
       , "r - make Random move"
       , "b - make Best move"
       , "a - start same board Again"
       , "u - Undo move"
       , "n - New board (limited)" ]
drawPlayingArea b = 
  Pictures [ drawIceState is | is <- M.assocs (posStateMap b)]

iceState :: PositionState -> IceState
iceState (PositionState istate _) = istate

drawIceState :: (Position,PositionState) -> Picture
drawIceState (p, PositionState iceSt playerM) = 
  Translate x y $ Pictures 
    [iceOrSea, fish iceSt, picOrNone penguin playerM]
  where 
    iceOrSea = Color (colorForIceState iceSt) $ drawHex side
    picOrNone :: (a-> Picture) -> Maybe a -> Picture 
    picOrNone f (Just v) = f v
    picOrNone _ Nothing = Pictures []
    penguin :: Player -> Picture
    penguin player = Pictures [
      Color black $ rectangleSolid 15 25
      , Color (colorForPlayer player) $ rectangleSolid 7 18  ]
    (x,y) = toUiPos p

fish (Ice fc) = case fc of
  1 -> Color green $ rectangleSolid 25 20
  2 -> Color orange $ rectangleSolid 20 10
  3 -> Color magenta $ rectangleSolid 15 20
  other -> Color red $ textAt 0 0 (show other)
fish _ = Pictures []

colorForPlayer Player1 = yellow
colorForPlayer Player2 = red

colorForIceState :: IceState -> Color
colorForIceState NoIce = blue
colorForIceState (Ice _) = white

type UiPosition = (Float, Float)

toUiPos :: Position -> UiPosition
toUiPos (Position x y) = (fromIntegral x*xSpacing,fromIntegral y*ySpacing)

drawLines :: Color -> (Float, Float) -> [String] -> Picture
drawLines c (x,y) ls = Translate x y $  Color c $
               Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,row) = textAt 10 (row*(-20)) l

textAt x y content = Translate x y (Scale 0.1 0.1 (Text content))


hexPath s = [(-hw,-hs), (-hw, hs), (0, hh), (hw, hs), (hw, -hs), (0, -hh)]
  where
    h = hexHeight
    w = hexWidth
    hw = w/2
    hh = h/2
    hs = s/2
 
drawHex :: Float -> Picture
drawHex s = Pictures [ Scale sc sc $ polygon (hexPath s) ]
  where sc = 0.95 

side = 40
hexWidth = side * sqrt 3
hexHeight = 2 * side
xSpacing = hexWidth / 2
ySpacing = (side+hexHeight) / 2
