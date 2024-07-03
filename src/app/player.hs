{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Player
( Player(..)
, switchTurn
, playerPiece
) where

import Board
import Control.Exception (throw)
import Distribution.Compat.Prelude (readMaybe)
import Text.Parsec
    ( Parsec, many1, oneOf, parse, newline, many, digit )
import Text.Parsec.Char (char)

data Player = Player1 | Player2 deriving (Eq, Show)

newtype GameStateOp s a = GameStateOp {runState :: s -> (Either String a, s)}

data BoardState a =  BoardState { board::Board a, player :: Player} deriving Show


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

boardState2 = BoardState {board = Board [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]], player = Player1}

applyMove :: Int -> GameStateOp (BoardState Piece) Piece
applyMove column = GameStateOp $ \boardState@(BoardState {board, player}) ->
  let gameFinished =  if not (checkIfMovesLeft board) || checkWinCon board then (Left "Game already finish", boardState) else outOfBounds
      outOfBounds = if column < 0 || width board <= column then (Left "Out of bounds", boardState) else columnFull
      columnFull =  if newBoard == board then (Left "Column is already full", boardState) else (Right Red, BoardState {board = newBoard, player = switchTurn player})
      newBoard = movePlayed board column (playerPiece player)
  in  gameFinished

applyMoves :: GameStateOp (BoardState Piece) Piece
applyMoves = do
  applyMove 1
  applyMove 0
  applyMove 0

applyMovesFromList :: [Int] -> GameStateOp (BoardState Piece) Piece
applyMovesFromList [] = applyMove (-1) --instead of throwing error
applyMovesFromList [xs] = applyMove xs
applyMovesFromList (x:xs) = do
  applyMove x
  applyMovesFromList xs

playGame =
  let (x, state) = runState applyMoves boardState2
  in case x of Right a -> show $ board state
               Left err -> err

playGame2 list =
  let (x, state) = runState (applyMovesFromList list) boardState2
  in case x of Right a -> show $ board state
               Left err -> err
-- putStrLn $ playGame2 [1, 0, 0]

switchTurn :: Player -> Player
switchTurn Player1 = Player2
switchTurn Player2 = Player1

playerPiece :: Player -> Piece
playerPiece Player1 = Red
playerPiece Player2 = Yellow

rm :: String -> Maybe Int
rm = readMaybe

isValidNum :: String -> Bool
isValidNum num =  case rm num of Just a -> True
                                 _      -> False

-- IO /*
startGame :: IO ()
startGame = do
  putStrLn "Welcome to Connect 4, choose using numbers"
  putStrLn "1) Create new board"
  putStrLn "2) Use existing board from matrix.txt"
  numStr <- getLine

  if read numStr == 1 then do
    putStrLn "Enter width od board bigger then 0"
    w <- getLine
    putStrLn "Enter height od board bigger then 0"
    h <- getLine
    if isValidNum w && isValidNum h && read w > 0 && read h > 0 then
      updateGame $ BoardState {board = createBoard (read w) (read h), player = Player1}
    else do
      putStrLn "Invalid input"
      startGame
    else do
      readBoardFromFile

updateGame :: BoardState Piece -> IO()
updateGame boardState = do
  putStrLn "Choose option with number"
  putStrLn "1) Play normal"
  putStrLn "2) Play multiple rounds at once"
  numStr <- getLine
  let num = read numStr in
    if num == 1 then do
      playInfRound boardState
    else do
      addListBeforePlay boardState

  -- 1) /*
playInfRound :: BoardState Piece -> IO()
playInfRound boardState = do
  putStrLn "Choose column or \"exit\" to start new game"
  str <- getLine
  if isValidNum str then
    let newGameState = playRound (read str) boardState in do
      putStrLn $ printMove newGameState
      playInfRound $ snd newGameState
  else
    startGame
  -- 1) */

  -- 2) /*
addListBeforePlay :: BoardState Piece -> IO ()
addListBeforePlay boardState = do
  putStrLn "Write all column indexes seperated by space"

prebuildBoard :: (Either String Piece, BoardState Piece) -> IO()
prebuildBoard newGameState = do
  putStrLn $ printMove newGameState
  playInfRound $ snd newGameState

readBoardFromFile = do
  contents <- readFile "../matrix.txt"
  if null contents then
    return ()
    else do
      case parse calc "" contents of 
        Right (board, list) -> let boardState = BoardState { board = board, player = Player1 } 
                               in  prebuildBoard (runState (applyMovesFromList list) boardState)
        Left err    -> print err
  -- 2) */


printMove :: (Either String Piece, BoardState Piece) -> String
printMove (x, boardState) = case x of Right a -> show $ board boardState
                                      Left err -> err

playRound :: Int -> BoardState Piece -> (Either String Piece, BoardState Piece)
playRound column = runState (applyMove column)
-- IO */

pieceParser :: Parsec String () Piece
pieceParser = do
  letter <- oneOf " RY"
  char '|'
  case letter of ' ' -> return Empty
                 'R' -> return Red
                 'Y' -> return Yellow


rowParser :: Parsec String () [Piece]
rowParser = do
  char '|'
  row <- many1 pieceParser
  newline
  return row

matrixParser :: Parsec String () [[Piece]]
matrixParser = many1 rowParser

boardParser :: Parsec String () (Board Piece)
boardParser = do
  matrix <- matrixParser
  return (Board $ transpose matrix)

listParser :: Parsec String () Int
listParser = do
  num <- many1 digit
  newline
  return (read num)

calc :: Parsec String () (Board Piece, [Int])
calc = do
  board <- boardParser
  list <- many listParser
  return (board, list)
