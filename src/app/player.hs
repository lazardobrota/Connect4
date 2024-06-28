data Turn = Player1 | Player2 deriving Eq


switchTurn :: Turn -> Turn
switchTurn Player1 = Player2
switchTurn Player2 = Player1