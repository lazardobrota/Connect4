module Player 
( switchTurn
, playerPiece
) where

import Board

data Turn = Player1 | Player2 deriving Eq

data Status b a = Wrong b | Correct a deriving Eq --same as Either but different Show implementationb

newtype GameStateOp s a = GameStateOp {runState :: s -> (Either String a, s)}

testing :: GameStateOp Int Int
testing = do
   a <- pure 5
   return a

-- runState (pure 5 >>= \s -> pure 3) 6
-- runState (pure 5 >> pure 3) 6

instance (Show a, Show b) => Show (Status a b) where
  show (Correct a) = show a
  show (Wrong b) = show b

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
  (GameStateOp state) >>= f = GameStateOp $ \s -> 
    let (x, state') = state s  
    in case x of Left err -> (Left err, state')
                 Right a -> runState (f a) state'

-- runState SomeState - returns function part
-- lol :: Status (Board Piece) String -> Int
-- lol (Correct board) = 5

switchTurn :: Turn -> Turn
switchTurn Player1 = Player2
switchTurn Player2 = Player1

playerPiece :: Turn -> Piece
playerPiece Player1 = Red
playerPiece Player2 = Yellow