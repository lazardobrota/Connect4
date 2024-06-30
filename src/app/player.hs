module Player 
( switchTurn
, playerPiece
) where

import Board

data Turn = Player1 | Player2 deriving Eq

switchTurn :: Turn -> Turn
switchTurn Player1 = Player2
switchTurn Player2 = Player1

playerPiece :: Turn -> Piece
playerPiece Player1 = Red
playerPiece Player2 = Yellow