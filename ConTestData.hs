module ConTestData where
import Data.Map
import Game
testboard = Board {dimensions = (5,5), nextPlayer = Player1, scoreMap = fromList [(Player1,0),(Player2,0)], posStateMap = fromList [(Position 1 1,PositionState (Ice 2) Nothing),(Position 1 3,PositionState (Ice 2) Nothing),(Position 1 5,PositionState (Ice 2) Nothing),(Position 2 2,PositionState (Ice 3) Nothing),(Position 2 4,PositionState (Ice 2) Nothing),(Position 3 1,PositionState (Ice 2) Nothing),(Position 3 3,PositionState (Ice 1) (Just Player2)),(Position 3 5,PositionState (Ice 1) Nothing),(Position 4 2,PositionState (Ice 2) Nothing),(Position 4 4,PositionState (Ice 3) Nothing),(Position 5 1,PositionState (Ice 3) Nothing),(Position 5 3,PositionState (Ice 3) Nothing),(Position 5 5,PositionState (Ice 3) Nothing),(Position 6 2,PositionState (Ice 2) Nothing),(Position 6 4,PositionState (Ice 1) Nothing),(Position 7 1,PositionState (Ice 2) Nothing),(Position 7 3,PositionState (Ice 2) Nothing),(Position 7 5,PositionState (Ice 3) (Just Player1)),(Position 8 2,PositionState (Ice 3) Nothing),(Position 8 4,PositionState (Ice 1) Nothing),(Position 9 1,PositionState (Ice 3) (Just Player1)),(Position 9 3,PositionState (Ice 1) Nothing),(Position 9 5,PositionState (Ice 2) Nothing),(Position 10 2,PositionState (Ice 3) (Just Player2)),(Position 10 4,PositionState (Ice 3) Nothing)]}
