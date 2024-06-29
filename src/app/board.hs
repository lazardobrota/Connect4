data Piece = Yellow | Red | Empty | OutOfBoard deriving Eq
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

--Takes board only for it to pass its matrix to tranpose function 
transposeBoard :: Board a -> Board a
transposeBoard (Board ([]:_)) = Board []
transposeBoard (Board columns) = Board (transpose columns)

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

board = Board [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]]
matrix = [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]]

-- Check free spaces in matrix
checkFreeSpaces :: Board Piece -> [(Int, Int)]
checkFreeSpaces (Board colums) = columnSpaces colums 0

columnSpaces :: [[Piece]] -> Int -> [(Int, Int)]
columnSpaces columns j
  | j < length columns = rowSpaces (columns !! j) 0 j ++ columnSpaces columns (j + 1)
  | otherwise = []

rowSpaces :: [Piece] ->  Int -> Int -> [(Int, Int)]
rowSpaces row i j
  | i < length row = [(i, j) | elem == Empty] ++ rowSpaces row (i + 1) j
  | otherwise = []
  where elem = row !! i


instance Show Piece where
  show Yellow = "Y"
  show Red = "R"
  show Empty = " "

test = [[1, 2, 3], [4, 5, 6], [7, 8, 9]] !! 1

-- Za poruke slican tip kao either, ukoliko je nevalidno vraca drugi tip konstruktora sa String porukicom, sledeci Bad >>= odmah vraca taj Bad i samo radimo Show obican za Bad da ispice String