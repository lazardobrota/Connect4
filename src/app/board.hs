data Piece = Yellow | Red | Empty | OutOfBoard deriving Eq
data Turn = Player1 | Player2 deriving Eq
newtype Column a = Column [a] deriving (Eq, Show)
newtype Board a = Board [[a]] deriving Eq --Board [Column] (Head is top, end of list is bottom)


instance Functor Column where
  fmap f (Column list) = Column $ map f list 

instance Show a => Show (Board a) where
  show (Board columns) = let (Board rows) = transposeBoard (Board columns) in matrixToString rows

matrixToString :: Show a => [[a]] -> String
matrixToString [] = ""
matrixToString (x:xs) = rowToString x ++ "\n" ++ matrixToString xs

rowToString:: Show a => [a] -> String
rowToString  = foldl (\acc x -> acc ++ show x ++ "|") "|"

-- transposeBoard :: Board Piece -> [[Piece]]
-- transposeBoard (Board ([]:_)) = []
-- transposeBoard (Board row) = map headB row : transposeBoard (Board (map tailB row))
--   where headB (Column pieces) = head pieces
--         tailB (Column pieces) = Column (tail pieces)

transposeBoard :: Board a -> Board a
transposeBoard (Board ([]:_)) = Board []
transposeBoard (Board columns) = Board (transpose columns)

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

board = Board [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]]
matrix = [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]]

instance Show Piece where
  show Yellow = "Y"
  show Red = "R"
  show Empty = " "


switchTurn :: Turn -> Turn
switchTurn Player1 = Player2
switchTurn Player2 = Player1

-- Za poruke slican tip kao either, ukoliko je nevalidno vraca drugi tip konstruktora sa String porukicom, sledeci Bad >>= odmah vraca taj Bad i samo radimo Show obican za Bad da ispice String