module Gui where
import Graphics.Gloss.Interface.Game
import qualified Data.Map as M
import Game hiding (main)

main = guimain
guimain = do
  b <- initRandomBoard 4 4 2
  bs <- mapM (const $ initRandomBoard 5 5 2) ([1..9]::[Int])
  gameInWindow 
          "Hey That's My Fish - Haskell UI" --name of the window
          (800,700) -- initial size of the window
          (10, 10) -- initial position of the window
          backgroundColor   -- background colour
          30 -- number of simulation steps to take for each second of real time
          (b,[], bs, []) -- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          (const id)

backgroundColor = colorSea -- dark $ dark $ dark blue -- makeColor8 200 200 200 100

-- (board in play, undo list, fresh boards, log msgs)
type GS = (Board,[Board],[Board], [String])

handleInput :: Event -> GS -> GS
handleInput (EventKey (Char c)               Down  _   _pos)  b = handleChar c b
handleInput (EventKey (MouseButton LeftButton) Down  _ _pos)  b = b
handleInput _ b = b

handleChar :: Char -> GS -> GS
handleChar 'r' (b,undos,bs, ls) = 
  (b', b:undos, bs, ("random move score "++show i):ls)
  where (i, b') = makeRandomMoveIfUnfinished b

handleChar 'b' (b,undos,bs, ls) = 
  (b', b:undos, bs, ("best move scored " ++ show i): ls)
  where (i, b') = makeBestMoveIfUnfinished b

handleChar 'n' (_, _, nextb:others,ms) = (nextb, [], others,ms)
handleChar 'a' (b, [], bs,ms)          = (b, [], bs,ms)
handleChar 'a' (_, undos, bs,ms)       = (last undos, [], bs,ms)
handleChar 'u' (_, u:undos, bs,ms)     = (u, undos, bs,ms)
handleChar 'u' (b,[],bs,ms)            = (b,[],bs,ms)
handleChar _ b = b

drawState :: (Board,[Board],[Board],[String])-> Picture
drawState (b,_,_, msgs) = Pictures $ 
   Translate (-100) (-100) (drawPlayingArea b) : 
   [ drawLines colorForText (-400,300) $  messages ]
  where messages = concat $ ((lines . displayBoard) b) : help: [take 5 msgs]
     

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
    [iceOrSea, fishCountPic iceSt, picOrNone penguin playerM]
  where 
    (x,y) = toUiPos p
    iceOrSea = Color (colorForIceState iceSt) $ drawHex side
    penguin player = Pictures [
        Color black $ rectangleSolid 14 25
      , Color (colorForPlayer player) $ rectangleSolid 8 20  ]

picOrNone :: (a-> Picture) -> Maybe a -> Picture 
picOrNone f (Just v) = f v
picOrNone _ Nothing = Pictures []

fishCountPic (Ice fc) = Pictures $ fishPics fc ++
   [] -- [Color black $ textAt 10 (-20) (show fc)]
fishCountPic NoIce = Pictures []

fishPics fc = map (\v -> Rotate (fromIntegral (v * (360 `div` fc))) $ 
              Translate 0 20 $ fishPic fc) [1..fc]

fishPic fc = Color (colorForFish fc) $ Pictures [
               rectangleSolid 25 10, 
               Translate (-7) 2 $ Color (dark $ colorForFish fc) $ circleSolid 2 ]

colorForText = white

colorForPlayer Player1 = colorGoldfish -- dark yellow
colorForPlayer Player2 = black -- dark yellow

colorSea      = makeColor8 46 90 107 255

colorForIceState :: IceState -> Color
colorForIceState NoIce   = makeColor8 72 163 159 255
colorForIceState (Ice _) = white 

colorSeaGlass = makeColor8 163 204 188 255
colorGoldfish = makeColor8 255 147 84 255
colorAnnajak  = makeColor8 252 223 184 255

colorForFish _fc = colorSeaGlass
type UiPosition = (Float, Float)

toUiPos :: Position -> UiPosition
toUiPos (Position x y) = (fromIntegral x*xSpacing,fromIntegral y*ySpacing)

drawLines :: Color -> (Float, Float) -> [String] -> Picture
drawLines c (x,y) ls = Translate x y $  Color c $
               Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,row) = textAt 10 (row*(-20)) l

textAt x y content = Translate x y (Scale 0.12 0.12 (Text content))


hexPath s = [(-hw,-hs), (-hw, hs), (0, hh), (hw, hs), (hw, -hs), (0, -hh)]
  where
    h = hexHeight
    w = hexWidth
    hw = w/2
    hh = h/2
    hs = s/2
 
drawHex :: Float -> Picture
drawHex s = Pictures [ Scale sc sc $ polygon (hexPath s) ]
  where sc = 0.90 

side = 40
hexWidth = side * sqrt 3
hexHeight = 2 * side
xSpacing = hexWidth / 2
ySpacing = (side+hexHeight) / 2

-- color scheme
-- http://www.colourlovers.com/palette/437321/Sweet_penguin
