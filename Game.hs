import Data.List (sort)
import qualified Data.Map as M
data Position = Position Int Int deriving (Eq, Ord)
instance Show Position where
  show (Position x y) = "(" ++ show x ++ ","++show y++")"
type PosStateMap =(M.Map Position PositionState) 
type ScoreMap =(M.Map Player Int) 
type PlayerFinishedMap =(M.Map Player Bool) 
data Board = Board (Int,Int) Player ScoreMap PlayerFinishedMap PosStateMap deriving (Show, Eq, Ord)

posStateMap :: Board -> PosStateMap
posStateMap (Board _ _ _ _ m) = m

nextPlayer :: Board -> Player
nextPlayer (Board _ np _ _ _) = np

scoreMap :: Board -> ScoreMap
scoreMap (Board _ _ scm _ _) = scm

playerFinishedMap :: Board -> PlayerFinishedMap
playerFinishedMap (Board _ _ scm pfm _) = pfm

height, width :: Board -> Int
width  (Board (w,_) _ _ _ _) = w
height (Board (_,h) _ _ _ _) = h

updateNextPlayer :: (Player -> Player) -> Board -> Board
updateNextPlayer fn (Board dim p scm pfm psm) = Board dim (fn p) scm pfm psm

updatePosStateMap :: (PosStateMap -> PosStateMap) -> Board -> Board
updatePosStateMap f (Board dim p scm pfm psm) = Board dim p scm pfm (f psm)

updateScoreMap :: (ScoreMap -> ScoreMap) -> Board -> Board
updateScoreMap f (Board dim p scm pfm psm) = Board dim p (f scm) pfm psm

updatePlayerFinishedMap :: (PlayerFinishedMap -> PlayerFinishedMap) -> Board -> Board
updatePlayerFinishedMap f (Board dim p scm pfm psm) = Board dim p scm (f pfm) psm

data PositionState = PositionState IceState (Maybe Player) deriving (Show, Eq, Ord)
data IceState = NoIce | Ice Int deriving (Show, Eq, Ord)
data Player = Player1| Player2 deriving (Show, Eq, Ord, Enum, Bounded)
data Direction = N | E | S | W deriving (Show, Eq, Ord, Enum, Bounded)
legalPositionsFrom :: Board -> Position -> [Position]
legalPositionsFrom b p = concatMap (legalMovesInDirection b p) allDirections
legalMovesFrom :: Board -> Position -> [Move]
legalMovesFrom b startPos = zipWith Move (repeat startPos) (legalPositionsFrom b startPos)

allDirections = [minBound .. maxBound]

offsets N = (0, 1)
offsets E = (1, 0)
offsets S = (0, -1)
offsets W = (-1, 0)

nextPosition :: Position -> Direction -> Position
nextPosition (Position x y) d = let (dx, dy) = offsets d in (Position (x + dx) (y + dy))

isAvailable :: Board -> Position  -> Bool
isAvailable b p = isInBounds b p && isIcePresent b p && not (isPenguinPresent b p)

posState :: Board -> Position -> PositionState
posState b p = case M.lookup p (posStateMap b) of
  Just s -> s
  Nothing -> error $ "No position state in map at " ++ show p ++ " on board " ++ show b

isIcePresent b p = 
  case posState b p of
    (PositionState (Ice _) _) -> True
    _ -> False

isPenguinPresent b p = 
  case posState b p of
    (PositionState _ (Just _)) -> True
    _ -> False
  
isInBounds b (Position x y) = x >= 1 && x <= (width b) && y >= 1 && y <= (height b)

legalMovesInDirection :: Board -> Position -> Direction -> [Position]
legalMovesInDirection b origPos d = 
  if isAvailable b nextPos 
    then nextPos : legalMovesInDirection b nextPos d
    else []
  where nextPos = nextPosition origPos d

initBoard w h = Board (w, h) Player1 initScoreMap initPlayerFinishedMap (initPositions w h)
initPositions w h = foldl addPlayerAtPos onlyIceMap playerPositions
  where onlyIceMap = M.fromList [(Position x y, startPosState) | x <- [1..w], y <- [1..h]]
        startPosState = PositionState (Ice 1) Nothing
        playerPositions = [(Player1, Position 1 1), (Player1, Position 2 3),(Player2, Position 3 3), (Player2, Position 1 2)]

addPlayerAtPos :: PosStateMap -> (Player, Position) -> PosStateMap
addPlayerAtPos psm (player, position) = M.adjust (\(PositionState i p)-> PositionState i (Just player)) position psm

initPlayerFinishedMap = M.fromList (zip allPlayers (repeat False))
initScoreMap = M.fromList (zip allPlayers (repeat 0))
allPlayers = [minBound .. maxBound]


-- TODO: return a type which includes the possibility of IllegalMove
makeMove :: Board -> Move -> Board
makeMove b (Move p1 p2) = 
  case map (posState b) [p1,p2] of
    [(PositionState (Ice fishOnIce) (Just player)), ps2@(PositionState (Ice _) Nothing)] ->  
          (updateToNextPlayer . incrementFishCount player fishOnIce ) $ 
          updatePosStateMap (M.adjust moveOffState p1 . M.adjust (addPenguinToState player) p2) b
    [ps1Bad, ps2Bad] -> error $ show ("Bad position states ", ps1Bad, ", ", ps2Bad, " at ", p1, " and ", p2)

otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

updateToNextPlayer :: Board -> Board
updateToNextPlayer = updateNextPlayer otherPlayer


incrementFishCount :: Player -> Int -> Board -> Board
incrementFishCount player offset = updateScoreMap (M.adjust (+offset) player)

moveOffState :: PositionState -> PositionState
moveOffState (PositionState _ _) = PositionState NoIce Nothing

addPenguinToState :: Player -> PositionState -> PositionState
addPenguinToState player (PositionState iceState _) = PositionState iceState (Just player)

main = do
  autoplay (initBoard 6 6)

data Move = Move Position Position deriving (Eq)
instance Show Move where
  show (Move p1 p2) = "Move " ++ show p1 ++ "->"++show p2

allLegalMovesForPlayer :: Board -> Player -> [Move]
allLegalMovesForPlayer b player = 
  concatMap (legalMovesFrom b) startPosns
  where startPosns = allPlayerPositions b player

allPositions :: Board -> [Position]
allPositions b = [Position x y | y <- [h, (h-1)..1], x <- [1..w]]
  where h = height b; w = width b

allPlayerPositions :: Board -> Player -> [Position]
allPlayerPositions b player = 
  map fst $ filter (isPenguinInPosState player . snd) . M.assocs $ posStateMap b

isPenguinInPosState player (PositionState _ (Just actualPlayer)) = player == actualPlayer
isPenguinInPosState _ _ = False

displayBoard :: Board -> String
displayBoard b@(Board dim player scoreMap pfm posStateMap) = 
  "Next: " ++ show player ++ ", Scores: " ++ show scoreMap ++ ", finished players: " ++ finMapStr  ++ "\n" ++ makeBoardGrid b
  where finMapStr = show $ map fst . filter snd . M.assocs $ pfm

wrap n xs | length xs <= n = [xs]
wrap n xs | otherwise      = take n xs : wrap n (drop n xs)


makeBoardGrid b = unlines $ map concat $ wrap (width b) $ map (displayPosState . posState b) (allPositions b)
displayPosState :: PositionState -> String
displayPosState (PositionState (Ice val) p) = show val ++ (playerSym p)
  where playerSym Nothing = " "
        playerSym (Just Player1) = "R"
        playerSym (Just Player2) = "Y"
displayPosState (PositionState NoIce _) = "~ "

markPlayerFinished :: Player -> Board -> Board
markPlayerFinished player = (updateToNextPlayer . updatePlayerFinishedMap (M.adjust (const True) player))

allPlayersFinished ::Board -> Bool
allPlayersFinished b = all id $ map snd . M.assocs $ playerFinishedMap b 

autoplay :: Board -> IO ()
autoplay b = do
  putStrLn $ displayBoard b
  if allPlayersFinished b
    then putStrLn "All players finished - game over"
    else do
      let player = nextPlayer b
      case allLegalMovesForPlayer b player of
        [] -> do 
          putStrLn $ "Found that Player cannot play: "++ show player
          autoplay $ markPlayerFinished player b
        (m1:_) -> do
          putStrLn $ "Players first legal move: " ++ show m1
          autoplay (makeMove b m1)
