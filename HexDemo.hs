module Gui where
import Graphics.Gloss.Interface.Game
import Graphics.Gloss.Data.Extent
import System.Environment
type MousePtr = (Float, Float)
type World = (MousePtr, Int)
main = 
  gameInWindow 
          "hexen" --name of the window
          (800,700) -- initial size of the window
          (10, 10) -- initial position of the window
          backgroundColor   -- background colour
          30 -- number of simulation steps to take for each second of real time
          ((0,0), 0) -- the initial world
          drawState -- A function to convert the world into a picture
          handleInput -- A function to handle input events
          (const id)

backgroundColor = dark $ dark $ dark blue -- makeColor8 200 200 200 100

handleInput (EventKey (Char c)               Down  _   (x,y))  b = handleChar c b
handleInput (EventKey (MouseButton LeftButton) Down  _ (x,y))  b = b
handleInput (EventMotion mp) (_,i) = (mp, i)
handleInput _ b = b

handleChar 'b' (mp,w)  = (mp,w+1)
handleChar _ (mp,w)    = (mp,w)

hexPath s = [(-hw,-hs), (-hw, hs), (0, hh), (hw, hs), (hw, -hs), (0, -hh)]
  where
    h = hexHeight
    w = hexWidth
    hw = w/2
    hh = h/2
    hs = s/2
 
drawHex :: Float -> Picture
drawHex s = Pictures [ Scale sc sc $ Color white $ polygon (hexPath s) ]
  where sc = 0.95 

side = 40
hexWidth = side * sqrt 3
hexHeight = 2 * side
xSpacing = hexWidth / 2
ySpacing = (side+hexHeight) / 2

drawState :: World -> Picture
drawState (mp,n) = Pictures $ drawHexGrid ++ [Color green $ drawMousePointer mp]

drawMousePointer (x,y) = Translate x y $ circleSolid 6

drawHexGrid = map (drawHexAt white . diag fromIntegral) genFakeBoardPosns

drawHexAt c (x,y) = Color c $ Translate (x*xSpacing) (y*ySpacing) (drawHex side)

genFakeBoardPosns = [(x,y) | y <- [1..5], x<-[1..8], (x+y) `mod` 2 == 0] 

diag f (a,b) = (f a, f b)

type UiPosition = (Float, Float)
data Position = Position Int Int

toUiPos :: Position -> UiPosition
toUiPos (Position x y) = (fromIntegral x*side,fromIntegral y*side)

drawLines :: Color -> (Float, Float) -> [String] -> Picture
drawLines c (x,y) ls = Translate x y $  Color c $
               Pictures $ map drawLine $ zip ls [1..]
  where drawLine (l,y) = textAt 10 (y*(-20)) l

textAt x y content = Translate x y (Scale 0.1 0.1 (Text content))
