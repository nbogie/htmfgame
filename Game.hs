module Game where

import Control.Monad (when)
import Data.List (maximumBy)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)
import System.Environment( getArgs )
import System.Random
import qualified Data.Map as M
import Shuffle

data Position = Position Int Int deriving (Eq, Ord)
instance Show Position where
  show (Position x y) = "(" ++ show x ++ ","++show y++")"

type Strategy = (Board -> Maybe (Int, Move))
data StrategyName = Best | Rnd deriving (Show, Read, Eq)
type PosStateMap =(M.Map Position PositionState) 
type ScoreMap =(M.Map Player Int) 

data Board = Board { dimensions :: (Int,Int)
                   , nextPlayer :: Player
                   , scoreMap :: ScoreMap
                   , posStateMap :: PosStateMap 
                   } deriving (Show, Eq, Ord)

data PositionState = PositionState IceState (Maybe Player) deriving (Show, Eq, Ord)
data IceState = NoIce | Ice Int deriving (Show, Eq, Ord)
data Direction = NE | E | SE | SW | W | NW deriving (Show, Eq, Ord, Enum, Bounded)

data Player = Player1| Player2 deriving (Show, Eq, Ord, Enum, Bounded)

playedLast ::  Board -> Player
playedLast = otherPlayer . nextPlayer
players ::  [Player]
players = [minBound .. maxBound]

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

updatePosStateMap :: (PosStateMap -> PosStateMap) -> Board -> Board
updatePosStateMap f b = b { posStateMap = f $ posStateMap b }
setPosStateMap ::  Board -> PosStateMap -> Board
setPosStateMap b m = b { posStateMap = m }

isPositionInBoard ::  Board -> Position -> Bool
isPositionInBoard b p = M.member p $ posStateMap b

legalPositionsFrom :: Board -> Position -> [Position]
legalPositionsFrom b p = concatMap (legalMovesInDirection b p) directions

legalMovesFrom :: Board -> Position -> [Move]
legalMovesFrom b startPos = zipWith Move (repeat startPos) (legalPositionsFrom b startPos)

cantMoveFrom :: Position -> Board -> Bool
cantMoveFrom pos b = null $ legalMovesFrom b pos
cantMove :: Board -> Player -> Bool
cantMove b pl = null $ legalMovesForPlayer b pl
nextPosition :: Position -> Direction -> Position
nextPosition (Position x y) d = let (dx, dy) = offsets d in (Position (x + dx) (y + dy))

isPlayerAt :: Board -> Player -> Position -> Bool
isPlayerAt b pl pos = pos `elem` playerPositions b pl

vacantPositions :: Board -> [Position]
vacantPositions b = [p | p <- M.keys (posStateMap b), isVacant b p]

posState :: Board -> Position -> PositionState
posState b p = 
  fromMaybe 
    (error $ "No position state in map at " ++ show p ++ " on board " ++ show b)
    (M.lookup p (posStateMap b))

isVacant :: Board -> Position  -> Bool
isVacant b p = isPositionInBoard b p && isIcePresent b p && not (isPenguinPresent b p)

isIcePresent ::  Board -> Position -> Bool
isIcePresent b p = 
  case posState b p of
    (PositionState (Ice _) _) -> True
    _ -> False

isPenguinPresent ::  Board -> Position -> Bool
isPenguinPresent b p = 
  case posState b p of
    (PositionState _ (Just _)) -> True
    _ -> False
  
directions ::  [Direction]
directions = [minBound .. maxBound]

offsets ::  (Num t1, Num t) => Direction -> (t, t1)
offsets NE = (1, 1)
offsets E = (2, 0)
offsets SE = (1, -1)
offsets SW = (-1, -1)
offsets W = (-2, 0)
offsets NW = (-1, 1)

legalMovesInDirection :: Board -> Position -> Direction -> [Position]
legalMovesInDirection b origPos d = 
  if isVacant b nextPos 
    then nextPos : legalMovesInDirection b nextPos d
    else []
  where nextPos = nextPosition origPos d

addRandomPlayers :: RandomGen g => g -> Int -> Board -> (Board, g)
addRandomPlayers g nPieces b = (b', g')
  where
    (rndPosns, g') = shuffle g (vacantPositions b)
    b' = updatePosStateMap (\m -> foldl addPlayerAtPos m pps) b
    pps = zip (cycle players) (take nPieces rndPosns)

initRandomBoard :: RandomGen g => g -> Int -> Int -> Int -> (Board, g)
initRandomBoard g w h nPieces = addRandomPlayers g nPieces b
  where 
    rndNs = randomRs (1,3) g
    b = Board (w, h) Player1 initScoreMap (initPSM rndNs)
    initScoreMap   = M.fromList (zip players (repeat 0))
    initPSM iceNs  = M.fromList (zip hexGrid (map mkpos iceNs))
      where
        hexGrid = [Position x y | y <- [1..h], x<- [1..2*w], 
                                  (x+y) `mod` 2 == 0]
        mkpos v = PositionState (Ice v) Nothing

addPlayerAtPos :: PosStateMap -> (Player, Position) -> PosStateMap
addPlayerAtPos psm (player, position) = 
  M.adjust (\(PositionState i _p)-> PositionState i (Just player)) position psm

makeBestMoveIfUnfinished :: Board -> (Int, Board)
makeBestMoveIfUnfinished = makeMoveIfUnfinished pickBestMove

makeRandomMoveIfUnfinished :: Board -> (Int, Board)
makeRandomMoveIfUnfinished = makeMoveIfUnfinished pickRandomMove

makeMoveIfUnfinished :: Num t => (Board -> Maybe (t, Move)) -> Board -> (t, Board)
makeMoveIfUnfinished strat b = case strat b of
  Nothing -> (0, togglePlayer b)
  Just (i,m) -> (i, makeMove b m)

-- TODO: return a type which includes the possibility of IllegalMove
makeMove :: Board -> Move -> Board
makeMove b (Move p1 p2) = 
  case diag (posState b) (p1,p2) of
    (PositionState (Ice fc) (Just player), 
     PositionState (Ice _) Nothing) ->  
       removeAllLostPenguins .
       togglePlayer . 
       incrementFishCount player fc . 
       updatePosStateMap 
        (M.adjust moveOffState p1 . M.adjust (addPenguinToState player) p2) $ b
    (ps1Bad, ps2Bad) -> 
       error $ show ("Bad pos states ", ps1Bad, ", ", ps2Bad, " at ", p1, " and ", p2)

    where diag f (x,y) = (f x, f y)

difference ::  Num a => (t -> a) -> (t, t) -> a
difference f (a,b) = f a - f b

fishCountAt :: Position -> Board -> Int
fishCountAt p b = case posState b p of
  (PositionState (Ice fc) _) -> fc
  other -> error $ "unexpected state in fishCountAt.  posstate: "++show other
-- for each player, for each penguin position.  if there are no moves possible
-- for that penguin, remove that penguin, attributing score as necessary.

stuckPenguins ::  Board -> [(Player, Position, Int)]
stuckPenguins b = [(pl,pos,fc) | pl <- players, pos <- playerPositions b pl, 
                                 let fc = fishCountAt pos b, cantMoveFrom pos b ]

removeAllLostPenguins ::  Board -> Board
removeAllLostPenguins b = 
  foldl (\b' (pl, pos, fc) -> removeAndScore pos pl fc b') b (stuckPenguins b)

removeAndScore ::  Position -> Player -> Int -> Board -> Board
removeAndScore pos pl fc b = 
  incrementFishCount pl fc .  
  updatePosStateMap (M.adjust moveOffState pos) $ b

otherPlayer ::  Player -> Player
otherPlayer Player1 = Player2
otherPlayer Player2 = Player1

togglePlayer :: Board -> Board
togglePlayer = updateNextPlayer otherPlayer

incrementFishCount :: Player -> Int -> Board -> Board
incrementFishCount player offset = updateScoreMap (M.adjust (+offset) player)

moveOffState :: PositionState -> PositionState
moveOffState (PositionState _ _) = PositionState NoIce Nothing

addPenguinToState :: Player -> PositionState -> PositionState
addPenguinToState player (PositionState iceState _) = PositionState iceState (Just player)

legalMovesForPlayer :: Board -> Player -> [Move]
legalMovesForPlayer b player = 
  concatMap (legalMovesFrom b) startPosns
  where startPosns = playerPositions b player

playerPositions :: Board -> Player -> [Position]
playerPositions b player = 
  map fst $ filter (present . snd) . M.assocs $ posStateMap b
  where 
    present (PositionState _ (Just actualPlayer)) = player == actualPlayer
    present _ = False

displayBoard :: Board -> String
displayBoard b = 
  unlines [
    if isGameOver b 
      then "Game Over" 
      else "Next to play " ++ show (nextPlayer b)
  , "Scores: " ++ show (M.assocs (scoreMap b)) ]

isGameOver ::  Board -> Bool
isGameOver b = all (cantMove b) players

pickRandomMove :: Board -> Maybe (Int, Move)
pickRandomMove b = case legalMovesForPlayer b (nextPlayer b) of
  [] -> Nothing
  (m:_) -> Just (0, m)

pickBestMove ::  Board -> Maybe (Int, Move)
pickBestMove b = 
  if isGameOver b 
  then Nothing
  else case legalMovesForPlayer b (nextPlayer b) of
    [] -> Nothing
    ms -> Just $ bestMove defaultDepth b ms

type Depth = Int
defaultDepth ::  Depth
defaultDepth = 5

bestMove :: Depth -> Board -> [Move] -> (Int, Move)
bestMove depth b ms = maximumBy (comparing fst) $ mapAnnotate (negamax depth . makeMove b) ms

-- from point of view of the player who made the last move
negamax :: Int -> Board -> Int
negamax depth b
  | isGameOver b = 
      case whoWon b of
      Just winner | winner == playedLast b -> 30 + fishScoreDeltaForLastPlayer b
                  | otherwise              -> -10
      Nothing                              -> 0
  | depth == 0   = staticEval b (playedLast b)
  | otherwise = case legalMovesForPlayer b (nextPlayer b) of
                  [] -> negate $ negamax (depth-1) (togglePlayer b)
                  ms -> negate $ fst $ bestMove (depth-1) b ms

fishScoreDeltaForLastPlayer ::  Board -> Int
fishScoreDeltaForLastPlayer b = difference (scoreFor b) (playedLast b, nextPlayer b)

mapAnnotate :: (a -> b) -> [a] -> [(b, a)]
mapAnnotate f = map (\x -> (f x, x))

whoWon ::  Board -> Maybe Player
whoWon b 
  | s1 == s2  = Nothing
  | s1  > s2  = Just Player1
  | otherwise = Just Player2
  where s1 = scoreFor b Player1
        s2 = scoreFor b Player2


staticEval :: Board -> Player -> Int
staticEval b p = fishScoreDelta -- avMovesScore + fishScoreDelta
 where
  me = p
  you = otherPlayer p
  fishScoreDelta = difference (scoreFor b) (me, you)
  avMovesScore   = difference (length . legalMovesForPlayer b) (me, you)

-- TODO: consider the following for static eval, (generally deltas of the property for me vs you)
--       current fish score totals, 
--       remaining connected fish score totals
--       tiles connected more than once
--       number of remaining penguins
--       ...

scoreFor :: Board -> Player -> Int
scoreFor b p = scoreMap b M.! p

scoresForPlayers :: Board -> [Int]
scoresForPlayers b = map (scoreFor b) players

showResultPlain :: Board -> String
showResultPlain b = winner ++ " " ++ scores
  where
    winner = maybe "Draw" show (whoWon b)
    scores = unwords $ map show $ scoresForPlayers b

autoplay :: Logging -> Strategy -> Strategy -> Board -> IO Board
autoplay logging strat otherStrat b = 
  if isGameOver b
    then info ("Game over.\n" ++ displayBoard b)
         >> logIt Silent (showResultPlain b)
         >> return b
    else do
        debug $ displayBoard b
        case strat b of
          Nothing -> debug ("Cannot play: "++ (show . nextPlayer) b ) 
                      >> autoplay logging otherStrat strat (togglePlayer b)
          Just (i,mv) -> debug (show (nextPlayer b) ++ " making move: " ++ show mv ++ " of score " ++ show i)
                     >> autoplay logging otherStrat strat (makeMove b mv)
  where 
    debug = logIt Debug; info = logIt Info
    logIt level = if logging <= level then putStrLn else const $ return ()

main ::  IO Board
main = nonguimain

nonguimain ::  IO Board
nonguimain = do
  args <- getArgs
  gen  <- getStdGen
  case args of
    [w,h,nPieces,strat1,strat2,logLevel] -> do
      let logging = (read logLevel)
      let (b, _gen') = initRandomBoard gen (read w) (read h) (read nPieces)
      when (logging <= Info) $ 
        putStrLn $ "Starting with strategies: " ++ strat1 ++ ", " ++ strat2 ++ " on a board of dimensions " ++ show (w,h) ++ " with " ++ show nPieces ++ " pieces"
      autoplay logging (stratFor $ read strat1) (stratFor $ read strat2) b
    _args -> error "Usage: prog width height strat1 strat2 (Debug|Info|Silent)"
  where 
    stratFor Best = pickBestMove
    stratFor Rnd = pickRandomMove

-- TODO: implement the AI so it's not moronic.
-- TODO: log game to file so curious situations can be replayed.
-- TODO: have the AI try to maximize the delta between player scores, 
--       rather than the abs score of current player
-- TODO: do away with player finished flags.  Correct and immediate removal of 
-- stuck penguins from the board will allow us to ask simply "any available moves" for isGameOver impl.
-- TODO: parallelize the search.  (will make ab pruning less effective)
-- TODO: Add ab pruning to the minimax.
-- TODO: let the user play.
-- TODO: If we are not connected to the enemy, adversarial search can stop.
