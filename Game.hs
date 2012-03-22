module Game where

import Control.Monad (when)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import System( getArgs )
import System.Random
import qualified Data.Map as M
import Shuffle

data Position = Position Int Int deriving (Eq, Ord)
instance Show Position where
  show (Position x y) = "(" ++ show x ++ ","++show y++")"

type Strategy = (Board -> Maybe Move)
data StrategyName = Best | Rnd deriving (Show, Read, Eq)
type PosStateMap =(M.Map Position PositionState) 
type ScoreMap =(M.Map Player Int) 
type PlayerFinishedMap =(M.Map Player Bool) 

data Board = Board { dimensions :: (Int,Int)
                   , nextPlayer :: Player
                   , scoreMap :: ScoreMap
                   , playerFinishedMap :: PlayerFinishedMap
                   , posStateMap :: PosStateMap 
                   } deriving (Show, Eq, Ord)

data PositionState = PositionState IceState (Maybe Player) deriving (Show, Eq, Ord)
data IceState = NoIce | Ice Int deriving (Show, Eq, Ord)
data Direction = NE | E | SE | SW | W | NW deriving (Show, Eq, Ord, Enum, Bounded)

data Player = Player1| Player2 deriving (Show, Eq, Ord, Enum, Bounded)

currentPlayer = otherPlayer . nextPlayer
allPlayers = [minBound .. maxBound]

data Move = Move Position Position deriving (Eq)
instance Show Move where
  show (Move p1 p2) = "Move " ++ show p1 ++ "->"++show p2

data Logging = Debug | Info | Silent deriving (Eq, Ord, Enum, Bounded, Show, Read)

height, width :: Board -> Int
width b = fst (dimensions b)
height b = snd (dimensions b)

updateNextPlayer :: (Player -> Player) -> Board -> Board
updateNextPlayer f b = b { nextPlayer = f $ nextPlayer b }

updateScoreMap :: (ScoreMap -> ScoreMap) -> Board -> Board
updateScoreMap f b = b { scoreMap = f $ scoreMap b }

updatePlayerFinishedMap :: (PlayerFinishedMap -> PlayerFinishedMap) -> Board -> Board
updatePlayerFinishedMap f b = b { playerFinishedMap = f $ playerFinishedMap b }

updatePosStateMap :: (PosStateMap -> PosStateMap) -> Board -> Board
updatePosStateMap f b = b { posStateMap = f $ posStateMap b }
setPosStateMap b m = b { posStateMap = m }

legalPositionsFrom :: Board -> Position -> [Position]
legalPositionsFrom b p = concatMap (legalMovesInDirection b p) allDirections
legalMovesFrom :: Board -> Position -> [Move]
legalMovesFrom b startPos = zipWith Move (repeat startPos) (legalPositionsFrom b startPos)

allDirections = [minBound .. maxBound]

offsets NE = (1, 1)
offsets E = (2, 0)
offsets SE = (1, -1)
offsets SW = (-1, -1)
offsets W = (-2, 0)
offsets NW = (-1, 1)

nextPosition :: Position -> Direction -> Position
nextPosition (Position x y) d = let (dx, dy) = offsets d in (Position (x + dx) (y + dy))

availablePositions :: Board -> [Position]
availablePositions b = [p | p <- M.keys (posStateMap b), isAvailable b p]

posState :: Board -> Position -> PositionState
posState b p = 
  fromMaybe 
    (error $ "No position state in map at " ++ show p ++ " on board " ++ show b)
    (M.lookup p (posStateMap b))

isAvailable :: Board -> Position  -> Bool
isAvailable b p = isInBounds b p && isIcePresent b p && not (isPenguinPresent b p)

isIcePresent b p = 
  case posState b p of
    (PositionState (Ice _) _) -> True
    _ -> False

isPenguinPresent b p = 
  case posState b p of
    (PositionState _ (Just _)) -> True
    _ -> False
  
isInBounds b (Position x y) = x >= 1 && x <= width b && y >= 1 && y <= height b

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
  return $ updatePosStateMap (\m -> foldl addPlayerAtPos m playerPositions) b



initRandomBoard :: Int -> Int -> IO Board
initRandomBoard w h = do
  rndNs <- fmap (randomRs (1,3)) getStdGen
  let b = Board (w, h) Player1 initScoreMap initPFM (initPSM rndNs)
  addRandomPlayers b
  where 
    initScoreMap   = M.fromList (zip allPlayers (repeat 0))
    initPFM        = M.fromList (zip allPlayers (repeat False))
    initPSM iceNs  = M.fromList (zip hexGrid (map mkpos iceNs))
      where
        hexGrid = [Position x y | y <- [1..h], x<-[1..2*w], 
                                  (x+y) `mod` 2 == 0]
        mkpos v = PositionState (Ice v) Nothing

addPlayerAtPos :: PosStateMap -> (Player, Position) -> PosStateMap
addPlayerAtPos psm (player, position) = 
  M.adjust (\(PositionState i _p)-> PositionState i (Just player)) position psm

makeBestMoveIfUnfinished :: Board -> Board
makeBestMoveIfUnfinished = makeMoveIfUnfinished pickBestMove
makeRandomMoveIfUnfinished :: Board -> Board
makeRandomMoveIfUnfinished = makeMoveIfUnfinished pickRandomMove

makeMoveIfUnfinished strat b = case strat b of
  Nothing -> giveUpMove b
  Just m -> makeMove b m

-- TODO: return a type which includes the possibility of IllegalMove
makeMove :: Board -> Move -> Board
makeMove b (Move p1 p2) = 
  case diag (posState b) (p1,p2) of
    (PositionState (Ice fishOnIce) (Just player), 
     PositionState (Ice fishOnIceP2) Nothing) ->  
          removeAnyLostPenguins p2 player fishOnIceP2 . 
          updateToNextPlayer . incrementFishCount player fishOnIce . 
          updatePosStateMap (M.adjust moveOffState p1 . M.adjust (addPenguinToState player) p2) $ b
    (ps1Bad, ps2Bad) -> error $ show ("Bad position states ", ps1Bad, ", ", ps2Bad, " at ", p1, " and ", p2)

diag f (x,y) = (f x, f y)

-- update score, AND remove penguin
-- TODO: also consider other (or just other adjacent) penguins who we might have blocked in, too
removeAnyLostPenguins :: Position -> Player -> Int -> Board -> Board
removeAnyLostPenguins p player fishOnIce b | null (legalMovesFrom b p) = 
  incrementFishCount player fishOnIce .  
  updatePosStateMap (M.adjust moveOffState p . M.adjust (addPenguinToState player) p) $ b
removeAnyLostPenguins _ _ _ b = b --do nothing

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

stratFor :: StrategyName -> Strategy
stratFor Best = pickBestMove
stratFor Rnd = pickRandomMove

allLegalMovesForPlayer :: Board -> Player -> [Move]
allLegalMovesForPlayer b player = 
  concatMap (legalMovesFrom b) startPosns
  where startPosns = allPlayerPositions b player

allPlayerPositions :: Board -> Player -> [Position]
allPlayerPositions b player = 
  map fst $ filter (isPenguinInPosState player . snd) . M.assocs $ posStateMap b

isPenguinInPosState player (PositionState _ (Just actualPlayer)) = player == actualPlayer
isPenguinInPosState _ _ = False

displayBoard :: Board -> String
displayBoard b = 
  "Next to play " ++ show (nextPlayer b) 
  ++ ", Finished players: " ++ finMapStr  
  ++ "\n\tScores: " ++ show (M.assocs (scoreMap b))
    where finMapStr = show $ map fst . filter snd . M.assocs $ playerFinishedMap b

displayPosState :: PositionState -> String
displayPosState (PositionState (Ice val) p) = show val ++ playerSym p ++ " "
  where playerSym Nothing = " "
        playerSym (Just Player1) = "P"
        playerSym (Just Player2) = "Q"
displayPosState (PositionState NoIce _) = "~  "

giveUpMove :: Board -> Board
giveUpMove b = let p = currentPlayer b in markPlayerFinished p b

markPlayerFinished :: Player -> Board -> Board
markPlayerFinished player = updateToNextPlayer . updatePlayerFinishedMap (M.adjust (const True) player)

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
    ms -> Just $ snd $ bestMove defaultDepth b ms

defaultDepth = 4

bestMove :: Depth -> Board -> [Move] -> (Int, Move)
bestMove depth b ms = maximumBy (comparing fst) $ mapAnnotate (negamax depth . makeMove b) ms

-- from point of view of the player who made the last move
negamax :: Int -> Board -> Int
negamax depth b
  | isGameOver b = 
      case whoWon b of
      Just winner | winner == currentPlayer b -> 1000 * scoreForCurrentPlayer b
                  | otherwise                 -> -1000
      Nothing                                 -> 0
  | depth == 0 = staticEval b (currentPlayer b)
  | otherwise = case allLegalMovesForPlayer b (nextPlayer b) of
            [] -> negate $ negamax (depth-1) (giveUpMove b)
            ms -> negate $ fst $ bestMove (depth-1) b ms

type Depth = Int

mapAnnotate :: (a -> b) -> [a] -> [(b, a)]
mapAnnotate f = map (\x -> (f x, x))

whoWon ::  Board -> Maybe Player
whoWon b 
  | s1 == s2  = Nothing
  | s1  > s2  = Just Player1
  | otherwise = Just Player2
  where s1 = scoreFor b Player1
        s2 = scoreFor b Player2


scoreForCurrentPlayer :: Board -> Int
scoreForCurrentPlayer b = scoreFor b $ currentPlayer b

staticEval :: Board -> Player -> Int
staticEval b p = avMovesScore + fishScoreDelta
 where
  me = p
  you = otherPlayer p
  fishScoreDelta = difference (scoreFor b) (me, you)
  avMovesScore   = difference (length . allLegalMovesForPlayer b) (me, you)

difference f (a,b) = f a - f b
-- TODO: consider current fish score totals, remaining connected fish score totals
--       total connected fish
--       tiles connected more than once

scoreFor :: Board -> Player -> Int
scoreFor b p = scoreMap b M.! p

scoresForPlayers :: Board -> [Int]
scoresForPlayers b = map (scoreFor b) allPlayers
showResultPlain :: Board -> String
showResultPlain b = winner ++ " " ++ scores
  where
    winner = maybe "Draw" show (whoWon b)
    scores = unwords $ map show $ scoresForPlayers b

autoplay :: Logging -> Strategy -> Strategy -> Board -> IO Board
autoplay logging strat otherStrat b = 
  if allPlayersFinished b
    then info ("All players finished - game over.\n" ++ displayBoard b)
         >> logIt Silent (showResultPlain b)
         >> return b
    else do
        debug $ displayBoard b
        case strat b of
          Nothing -> debug ("Cannot play: "++ (show . nextPlayer) b ) 
                      >> autoplay logging otherStrat strat (giveUpMove b)
          Just mv -> debug (show (nextPlayer b) ++ " making move: " ++ show mv)
                     >> autoplay logging otherStrat strat (makeMove b mv)
  where 
    debug = logIt Debug; info = logIt Info
    logIt level = if logging <= level then putStrLn else const $ return ()

main = nonguimain
nonguimain = do
  args <- getArgs
  case args of
    [w,h,strat1,strat2,logLevel] -> do
      let logging = (read logLevel)
      b <- initRandomBoard (read w) (read h)
      when (logging <= Info) $ 
        putStrLn $ "Starting with strategies: " ++ strat1 ++ ", " ++ strat2
      autoplay logging (stratFor $ read strat1) (stratFor $ read strat2) b
    _args -> error "Usage: prog width height strat1 strat2 (Debug|Info|Silent)"

-- TODO: implement the AI so it's not moronic.
-- TODO: log game to file so curious situations can be replayed.
-- TODO: have the AI try to maximize the delta between player scores, 
--       rather than the abs score of current player
-- TODO: do away with player finished flags.  Correct and immediate removal of 
-- stuck penguins from the board will allow us to ask simply "any available moves" for isGameOver impl.
