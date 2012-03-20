module Game where

import Data.List (sort, maximumBy, find, intercalate)
import Data.Ord (comparing)
import System( getArgs )
import qualified Data.Map as M
import System.Random
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
data Direction = N | E | S | W deriving (Show, Eq, Ord, Enum, Bounded)

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

offsets N = (0, 1)
offsets E = (1, 0)
offsets S = (0, -1)
offsets W = (-1, 0)

nextPosition :: Position -> Direction -> Position
nextPosition (Position x y) d = let (dx, dy) = offsets d in (Position (x + dx) (y + dy))

availablePositions :: Board -> [Position]
availablePositions b = [p | p <- M.keys (posStateMap b), isAvailable b p]

posState :: Board -> Position -> PositionState
posState b p = case M.lookup p (posStateMap b) of
  Just s -> s
  Nothing -> error $ "No position state in map at " ++ show p ++ " on board " ++ show b

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
  return $ updatePosStateMap (\m -> foldl addPlayerAtPos m playerPositions) b

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

makeBestMoveIfUnfinished :: Board -> Board
makeBestMoveIfUnfinished b = makeMoveIfUnfinished pickBestMove b
makeRandomMoveIfUnfinished :: Board -> Board
makeRandomMoveIfUnfinished b = makeMoveIfUnfinished pickRandomMove b

makeMoveIfUnfinished strat b = case strat b of
  Nothing -> giveUpMove b
  Just m -> makeMove b m

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

stratFor :: StrategyName -> Strategy
stratFor Best = pickBestMove
stratFor Rnd = pickRandomMove

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
    "Next to play " ++ show player 
    ++ ", Finished players: " ++ finMapStr  
    ++ "\n\tScores: " ++ show (M.assocs scoreMap) 
    ++ "\n"++ makeBoardGrid b
      where finMapStr = show $ map fst . filter snd . M.assocs $ pfm

makeBoardGrid b = unlines $ map concat $ wrap (width b) $ map (displayPosState . posState b) (allPositions b)
  where
    wrap n xs | length xs <= n  = [xs]
    wrap n xs | otherwise       = take n xs : wrap n (drop n xs)

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
    ms -> Just $ snd $ bestMove defaultDepth b ms

defaultDepth = 4

bestMove :: Depth -> Board -> [Move] -> (Int, Move)
bestMove depth b ms = maximumBy (comparing fst) $ mapAnnotate (aiScore depth . makeMove b) $ ms

-- from point of view of the player who made the last move
aiScore :: Int -> Board -> Int
aiScore depth b = case isGameOver b of
  True -> case whoWon b of
    Just winner | winner == currentPlayer b -> 1000 * scoreForCurrentPlayer b
                | otherwise                 -> (-1000)
    Nothing                                 -> 0
  False -> 
      if depth == 0 
        then staticEval b (currentPlayer b)
        else case allLegalMovesForPlayer b (nextPlayer b) of
    [] -> negate $ aiScore (depth-1) (giveUpMove b)
    ms -> negate $ fst $ bestMove (depth-1) b ms

type Depth = Int

mapAnnotate :: (a -> b) -> [a] -> [(b, a)]
mapAnnotate f = map (\x -> (f x, x))

whoWon ::  Board -> Maybe Player
whoWon b = if s1==s2 
            then Nothing 
            else if s1 > s2 then Just Player1 else Just Player2
  where 
    s1 = scoreFor b Player1
    s2 = scoreFor b Player2


scoreForCurrentPlayer :: Board -> Int
scoreForCurrentPlayer b = scoreFor b $ currentPlayer b

staticEval :: Board -> Player -> Int
staticEval b p = avMovesScore + fishScoreDelta
 where
  me = p
  you = otherPlayer p
  fishScoreDelta = let [mine, his] = map (scoreFor b) [me, you] in mine - his
  avMovesScore = let [mine, his] = map (length . allLegalMovesForPlayer b) [me, you]
                 in mine - his

-- TODO: consider current fish score totals, remaining connected fish score totals
--       total connected fish
--       tiles connected more than once

scoreFor :: Board -> Player -> Int
scoreFor b p = (scoreMap b) M.! p

scoresForPlayers :: Board -> [Int]
scoresForPlayers b = map (scoreFor b) allPlayers
showResultPlain :: Board -> String
showResultPlain b = winner b ++ " " ++ scores b
  where
    winner b = maybe "Draw" show (whoWon b)
    scores :: Board -> String
    scores b = intercalate " " $ map show $ scoresForPlayers b

autoplay :: Logging -> Strategy -> Strategy -> Board -> IO Board
autoplay logging strat otherStrat b = do
  if allPlayersFinished b
    then do
      info $ "All players finished - game over.\n" ++ displayBoard b
      >> log Silent (showResultPlain b)
      >> return b
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

main = nonguimain
nonguimain = do
  args <- getArgs
  case args of
    [w,h,strat1,strat2,logLevel] -> do
      let logging = (read logLevel)
      b <- initRandomBoard (read w) (read h)
      if logging <= Info
        then putStrLn $ "Starting with strategies: " ++ strat1 ++ ", " ++ strat2
        else return ()
      autoplay logging (stratFor $ read strat1) (stratFor $ read strat2) $ b
    otherArgs -> error "Usage: prog width height strat1 strat2 (Debug|Info|Silent)"

-- TODO: log game to file so curious situations can be replayed
-- TODO: have the AI try to maximize the delta between player scores, 
--       rather than the abs score of current player
