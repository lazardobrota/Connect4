{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Player
( switchTurn
, playerPiece
) where

import Board

data Player = Player1 | Player2 deriving (Eq, Show)

newtype GameStateOp s a = GameStateOp {runState :: s -> (Either String a, s)}

data BoardState a =  BoardState { board::Board a, player :: Player} deriving Show

testing :: GameStateOp Int Int
testing = do
   a <- pure 5
   return a

-- runState (pure 5 >>= \s -> pure 3) 6
-- runState (pure 5 >> pure 3) 6

instance Functor (GameStateOp st) where
  fmap f (GameStateOp state) = GameStateOp $ \s ->
    let (x, newState) = state s
    in  (fmap f x, newState)

instance Applicative (GameStateOp st) where
  pure x = GameStateOp $ \s -> (Right x, s)
  (GameStateOp fstate) <*> (GameStateOp xstate) = GameStateOp $ \s ->
    let (f, fstate') = fstate s
        (x, xstate') = xstate fstate'
    in  (f <*> x, xstate')

instance Monad (GameStateOp st) where
  return = pure
  (GameStateOp state) >>= ffake = GameStateOp $ \s ->
    let (x, state') = state s
    in case x of Left err -> (Left err, state')
                 Right a -> runState (ffake a) state'

-- runState SomeState - returns function part
-- Uzima state i dodaje mu na primner stack [3, 4, 5] u \s ->, funkcija f je \_ -> DrugiState(pop) tako da kada se prosledi njemu "a" (tj Either) on zapravo vraca skroz drugi state(pop) i na njemu primenjuje state'(drugi deo tuple od push 3)
-- push 3 >>= \_ -> pop 
board1 = Board [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]]

boardState = BoardState {board = Board [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]], player = Player1}

-- TODO it should be red in 63 but current player
applyMove :: Int -> GameStateOp (BoardState Piece) Piece
applyMove column = GameStateOp $ \boardState@(BoardState {board, player}) ->
  case not (checkIfMovesLeft board) || checkWinCon board of
    True -> (Left "Game already finish", boardState)
    False -> 
      case column < 0 || width board <= column of 
        True -> (Left "Out of bounds", boardState)
        False -> 
          case newBoard == board of
            True -> (Left "Column is already full", boardState)
            False -> (Right Red, BoardState {board = newBoard, player = switchTurn player})
          where newBoard = movePlayed board column (playerPiece player)

applyMoves :: GameStateOp (BoardState Piece) Piece
applyMoves = do
  applyMove 1
  applyMove 0
  applyMove 0

playGame = 
  let (x, state) = runState applyMoves boardState 
  in case x of Right a -> show $ board state
               Left err -> err

switchTurn :: Player -> Player
switchTurn Player1 = Player2
switchTurn Player2 = Player1

playerPiece :: Player -> Piece
playerPiece Player1 = Red
playerPiece Player2 = Yellow
