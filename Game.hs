import Data.List (sort, maximumBy, find)
import Data.Ord (comparing)
import System( getArgs )
import qualified Data.Map as M
import System.Random
import Shuffle

data Position = Position Int Int deriving (Eq, Ord)
instance Show Position where
  show (Position x y) = "(" ++ show x ++ ","++show y++")"
type PosStateMap =(M.Map Position PositionState) 
type ScoreMap =(M.Map Player Int) 
type PlayerFinishedMap =(M.Map Player Bool) 
data Board = Board (Int,Int) Player ScoreMap PlayerFinishedMap PosStateMap deriving (Show, Eq, Ord)

posStateMap :: Board -> PosStateMap
posStateMap (Board _ _ _ _ m) = m
setPosStateMap (Board a b c d e) m = (Board a b c d m) 

nextPlayer :: Board -> Player
nextPlayer (Board _ np _ _ _) = np
currentPlayer = otherPlayer . nextPlayer
allPlayers = [minBound .. maxBound]

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

availablePositions :: Board -> [Position]
availablePositions b = [p | p <- M.keys (posStateMap b), isAvailable b p]

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

addRandomPlayers :: Board -> IO Board
addRandomPlayers b = do
  rndPosns <- shuffleIO (availablePositions b)
  let playerPositions = zip (cycle allPlayers) (take 2 rndPosns)
  let newPSM = foldl addPlayerAtPos (posStateMap b) playerPositions
  return $ setPosStateMap b newPSM

initRandomBoard :: Int -> Int -> IO Board
initRandomBoard w h = do
  rndNs <- fmap (randomRs (1,3)) getStdGen
  let b = Board (w, h) Player1 initScoreMap initPFM (initPSM w h rndNs)
  addRandomPlayers b
  where 
    initScoreMap      = M.fromList (zip allPlayers (repeat 0))
    initPFM           = M.fromList (zip allPlayers (repeat False))
    initPSM w h iceNs = M.fromList $ zip [Position x y | x <- [1..w], y <- [1..h]] (map mkpos iceNs)
      where
        mkpos v = PositionState (Ice v) Nothing

addPlayerAtPos :: PosStateMap -> (Player, Position) -> PosStateMap
addPlayerAtPos psm (player, position) = M.adjust (\(PositionState i p)-> PositionState i (Just player)) position psm

-- TODO: return a type which includes the possibility of IllegalMove
makeMove :: Board -> Move -> Board
makeMove b (Move p1 p2) = 
  case map (posState b) [p1,p2] of
    [(PositionState (Ice fishOnIce) (Just player)), ps2@(PositionState (Ice fishOnIceP2) Nothing)] ->  
          (removeAnyLostPenguins p2 player fishOnIceP2) . 
          (updateToNextPlayer . incrementFishCount player fishOnIce ) . 
          updatePosStateMap (M.adjust moveOffState p1 . M.adjust (addPenguinToState player) p2) $ b
    [ps1Bad, ps2Bad] -> error $ show ("Bad position states ", ps1Bad, ", ", ps2Bad, " at ", p1, " and ", p2)

-- update score, AND remove penguin
-- TODO: also consider other (or just other adjacent) penguins who we might have blocked in, too
removeAnyLostPenguins :: Position -> Player -> Int -> Board -> Board
removeAnyLostPenguins p player fishOnIce b | null (legalMovesFrom b p) = 
  incrementFishCount player fishOnIce .  
  updatePosStateMap (M.adjust moveOffState p . M.adjust (addPenguinToState player) p) $ b
removeAnyLostPenguins _ _ _ b |otherwise = b --do nothing

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

data StrategyName = Best | Rnd deriving (Show, Read, Eq)

stratFor :: StrategyName -> Strategy
stratFor Best = pickBestMove
stratFor Rnd = pickRandomMove

main = do
  args <- getArgs
  case args of
    [w,h,strat1,strat2,logLevel] -> do
      b <- initRandomBoard (read w) (read h)
      putStrLn $ "Starting with strategies: " ++ strat1 ++ ", " ++ strat2
      autoplay (read logLevel) (stratFor $ read strat1) (stratFor $ read strat2) $ b
    otherArgs -> error "Usage: prog width height strat1 strat2 (Debug|Info)"

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
displayPosState (PositionState (Ice val) p) = show val ++ (playerSym p) ++ " "
  where playerSym Nothing = " "
        playerSym (Just Player1) = "P"
        playerSym (Just Player2) = "Q"
displayPosState (PositionState NoIce _) = "~  "

giveUpMove :: Board -> Board
giveUpMove b = let p = currentPlayer b in markPlayerFinished p b

markPlayerFinished :: Player -> Board -> Board
markPlayerFinished player = (updateToNextPlayer . updatePlayerFinishedMap (M.adjust (const True) player))

allPlayersFinished ::Board -> Bool
allPlayersFinished b = all id $ map snd . M.assocs $ playerFinishedMap b 

isGameOver = allPlayersFinished

pickRandomMove :: Board -> Maybe Move
pickRandomMove b = case allLegalMovesForPlayer b (nextPlayer b) of
  [] -> Nothing
  (m:_) -> Just m

pickBestMove :: Board -> Maybe Move
pickBestMove b = 
  if isGameOver b 
  then Nothing
  else case allLegalMovesForPlayer b (nextPlayer b) of
    [] -> Nothing
    ms -> Just . snd . maximumBy (comparing fst) $ mapAnnotate (makeAndScore b aiScore) $ ms
      where 
        scoreMove m = aiScore $ makeMove b m

-- from point of view of the player who made the last move
aiScore :: Board -> Int
aiScore b = case isGameOver b of
  True -> if whoWon b == currentPlayer b then 1 * scoreForCurrentPlayer b else (-1)
  False -> case allLegalMovesForPlayer b (nextPlayer b) of
    [] -> negate $ aiScore (giveUpMove b)
    ms -> negate $ fst $ maximumBy (comparing fst) $ mapAnnotate (makeAndScore b aiScore) $ ms

makeAndScore :: Board -> (Board -> Int) -> Move -> Int
makeAndScore b strategy m = strategy $ makeMove b m

mapAnnotate :: (a -> b) -> [a] -> [(b, a)]
mapAnnotate f = map (\x -> (f x, x))
    
whoWon b = if s1 >= s2 then Player1 else Player2
  where 
    s1 = scoreFor b Player1
    s2 = scoreFor b Player2

data Logging = Debug | Info deriving (Eq, Ord, Enum, Bounded, Show, Read)

scoreForCurrentPlayer :: Board -> Int
scoreForCurrentPlayer b = scoreFor b $ currentPlayer b

staticEval :: Board -> Player -> Int
staticEval b p = length $ allLegalMovesForPlayer b p 
-- TODO: consider current fish score totals, remaining connected fish score totals
--       total connected fish
--       tiles connected more than once

scoreFor :: Board -> Player -> Int
scoreFor b p = (scoreMap b) M.! p

type Strategy = (Board -> Maybe Move)

autoplay :: Logging -> Strategy -> Strategy -> Board -> IO ()
autoplay logging strat otherStrat b = do
  if allPlayersFinished b
    then info $ "All players finished - game over.  " ++ displayBoard b
    else do
        debug $ displayBoard b
        case strat b of
          Nothing -> (debug $ "Cannot play: "++ (show . nextPlayer) b ) 
                      >> (autoplay logging otherStrat strat $ giveUpMove b)
          Just mv -> (debug $ show (nextPlayer b) ++ " making move: " ++ show mv) 
                      >> autoplay logging otherStrat strat (makeMove b mv)
  where 
    debug = log Debug; info = log Info
    log level = if logging <= level then putStrLn else const $ return ()


-- TODO: log game to file so curious situations can be replayed
-- TODO: have the AI try to maximize the delta between player scores, 
--       rather than the abs score of current player
