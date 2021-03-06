module Gui where
import qualified Data.Map as M
import Game hiding (main)
import Graphics.Gloss.Interface.Pure.Game
import System.Random(getStdGen,StdGen)

initStandardRandomBoard gen = initRandomBoard gen 6 6 4

main = guimain
guimain = do
  gen <- getStdGen 
  let (start, gen') = initStandardRandomBoard gen
  -- http://hackage.haskell.org/packages/archive/gloss/1.7.8.2/doc/html/Graphics-Gloss.html#v:play
  play 
          (InWindow "Penguins, Fish, Ice - Haskell UI" --name of the window
            (700,600) -- initial size of the window
            (0, 0) -- initial position of the window
          )
          backgroundColor   -- background colour
          30 -- number of simulation steps to take for each second of real time
          (initialGameState start gen')  -- the initial world
          drawStateMgr  -- A function to convert the world into a picture
          handleInputMgr -- A function to handle input events
          (const id)

initialGameState ::  Board -> StdGen -> GS
initialGameState start gen = (GS start [] [] handleSelect [] SplashScreen gen)

drawStateMgr :: GS -> Picture
drawStateMgr gs = draw (gameMode gs) $ gs
  where
    draw GamePlay     = drawStateInGame
    draw SplashScreen = drawStateSplash

handleInputMgr :: Event -> GS -> GS
handleInputMgr evt gs =  (handle (gameMode gs)) evt gs
  where
    handle GamePlay     = handleInputInGame
    handle SplashScreen = handleInputInSplash

handleSelect :: ClickHandler
handleSelect (EventKey _ _ _ uiPos) gs = 
  if nextPlayer (b gs) == Player1 && 
     isPositionInBoard (b gs) bp && 
     isPlayerAt (b gs) Player1 bp
    then gs { clickHdlr = handleMove bp } { hilitPosition = [bp]}
    else gs
  where bp = uiPosToBoardPos uiPos
handleSelect _ gs = gs

handleMove origPos (EventKey _ _ _ uiPos) gs =
  if isPositionInBoard (b gs) bp && mv `elem` legalMovesFrom (b gs) origPos
    then (logMsg gs ("handleMove from " ++ show origPos)) { clickHdlr = handleSelect, hilitPosition = [] } { b = makeMove (b gs) mv } 
    else resetHandlers gs
    where
      bp = uiPosToBoardPos uiPos
      mv = Move origPos bp
handleMove _ _ gs = gs

highlightBP :: UIPos -> GS -> GS 
highlightBP uiPos gs = (logMsg gs ("highlightBP: " ++ show uiPos ++ show bp)) { hilitPosition = [bp] }
  where bp = uiPosToBoardPos uiPos

-- TODO: this should snap to nearest bp.  
-- some roundings do not have a hex centred on them 
-- e.g. (2, 0) is between (1,0) and (3,0)
uiPosToBoardPos :: (Float, Float) -> Position
uiPosToBoardPos (x,y) = Position (round (x' / xSpacing)) (round (y' / ySpacing))
  where 
    x' = x + 100
    y' = y + 100

toUiPos :: Position -> UiPosition
toUiPos (Position x y) = (fromIntegral x*xSpacing,fromIntegral y*ySpacing)

logMsg gs msg = gs { logs = take 5 (msg : logs gs) }
backgroundColor = colorSea -- dark $ dark $ dark blue -- makeColor8 200 200 200 100

type ClickHandler = Event -> GS -> GS

data GameMode = SplashScreen | GamePlay deriving (Show, Eq)

data GS = GS { b             :: Board
             , undos         :: [Board]
             , logs          :: [String]
             , clickHdlr     :: ClickHandler
             , hilitPosition :: [Position]
             , gameMode      :: GameMode 
             , rndGen        :: StdGen
             }
resetHandlers gs = gs { clickHdlr = handleSelect, hilitPosition = [] } 

handleInputInSplash :: Event -> GS -> GS
handleInputInSplash (EventKey (Char _) Down _ _)  gs = gs { gameMode = GamePlay }
handleInputInSplash _ gs = gs 

handleInputInGame :: Event -> GS -> GS
handleInputInGame (EventKey (Char c) Down _ _)  gs = handleChar c (resetHandlers gs)
handleInputInGame ev@(EventKey (MouseButton LeftButton) Down  _ _pos)  gs = clickHdlr gs ev gs 
-- handleIneputInGame (EventMotion uiPos) gs = highlightBP uiPos gs
handleInputInGame _ gs = gs 
type UIPos = (Float, Float)

handleChar :: Char -> GS -> GS
handleChar 'r' gs =
  gs { b = b', undos = b gs:undos gs
             , logs = ("random move score "++show i):logs gs}
  where (i, b') = makeRandomMoveIfUnfinished (b gs)

handleChar 'b' gs = 
  gs { b = b', undos = b gs:undos gs
             , logs = ("best move scored " ++ show i):logs gs }
  where (i, b') = makeBestMoveIfUnfinished (b gs)

handleChar 'n' gs = gs { b = next, undos = [], rndGen = gen' } 
  where (next,gen') = initStandardRandomBoard (rndGen gs)

handleChar 'a' gs = case undos gs of
  []            -> gs
  us -> gs { b = last us, undos = [] }

handleChar 'u' gs = case undos gs of
  []            -> gs
  (prev:others) -> gs { b = prev, undos = others }
handleChar _ gs = gs 
drawStateSplash :: GS -> Picture
drawStateSplash _gs = 
   Pictures [ drawLines colorForText (-200,0) ["Penguins, Fish, Ice.  Press any key to start!"] ]
  

drawStateInGame :: GS -> Picture
drawStateInGame gs = Pictures $ 
   Translate (-100) (-100) 
   (drawPlayingArea (b gs) (hilitPosition gs)) : 
   [ drawLines colorForText (-300,0) messages ]
  where messages = concat $ (lines . displayBoard) (b gs) : help: [take 5 (logs gs)]
     
help :: [String]
help = [ "---- Keys -------------"
       , "r - make Random move"
       , "b - make Best move"
       , "a - start same board Again"
       , "u - Undo move"
       , "n - New board" ]

drawPlayingArea ::  Board -> [Position] -> Picture
drawPlayingArea bd hilitPosns = 
  Pictures $ [ drawIceState is | is <- M.assocs (posStateMap bd)] ++ [drawHilight (pos, posState bd pos) | pos <- hilitPosns]

drawHilight :: (Position, PositionState) -> Picture
drawHilight (p, _s) = Color (makeColor8 0 255 0 100) $ Translate x y $ drawHex side
  where (x,y) = toUiPos p

iceState :: PositionState -> IceState
iceState (PositionState istate _) = istate

posText :: Position -> UIPos -> Picture
posText (Position px py) _uip = Pictures [ miniText 0 (show (px,py)) ]
                                        -- , miniText 1 (show uip)]
  where miniText row str = Translate (-20) (-15 * row)  $ Scale 0.1 0.1 $ Text str
drawIceState :: (Position,PositionState) -> Picture
drawIceState (p, PositionState iceSt playerM) = 
  Translate x y $ Pictures 
    [iceOrSea, fishCountPic iceSt, picOrNone penguin playerM, posText p (x,y)]
  where 
    (x,y) = toUiPos p
    iceOrSea = Color (colorForIceState iceSt) $ drawHex side
    penguin player = Pictures [
        Color black $ rectangleSolid 14 25
      , Color (colorForPlayer player) $ rectangleSolid 8 20 ]

picOrNone :: (a-> Picture) -> Maybe a -> Picture 
picOrNone f (Just v) = f v
picOrNone _ Nothing = Pictures []

fishCountPic (Ice fc) = Pictures $ fishPics fc ++
   [] -- [Color black $ textAt 10 (-20) (show fc)]
fishCountPic NoIce = Pictures []

fishPics fc = map (\v -> Rotate (fromIntegral (v * (360 `div` fc))) $ 
              Translate 0 19 $ fishPic fc) [1..fc]

fishPic fc = Color (colorForFish fc) $ Pictures [
               rectangleSolid 25 8, 
               Translate 10 0 $ rectangleSolid 5 12, 
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

side ::  Float
side = 40
hexWidth = side * sqrt 3
hexHeight = 2 * side
xSpacing = hexWidth / 2
ySpacing = (side+hexHeight) / 2

-- color scheme
-- http://www.colourlovers.com/palette/437321/Sweet_penguin
