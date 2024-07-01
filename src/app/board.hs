module Board
( Piece(..)
, Board(..)
, transposeBoard
, transpose
, checkFreeSpaces
, matrixWithIndexes
, listWithIndexes
, movePlayed
, checkIfMovesLeft
) where

data Piece = Yellow | Red | Empty | OutOfBoard deriving Eq
newtype Column a = Column [a] deriving (Eq, Show)
newtype Board a = Board [[a]] deriving Eq --Board [Column] (Head is top, end of list is bottom)

instance Show Piece where
  show Yellow = "Y"
  show Red = "R"
  show Empty = " "

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
winboardd = Board [[Empty, Empty, Red, Yellow], [Empty, Yellow, Yellow, Red], [Empty, Yellow, Red, Red], [Yellow, Yellow, Red, Yellow]]
winboardv = Board [[Empty, Empty, Yellow, Red], [Yellow, Yellow, Yellow, Yellow], [Empty, Empty, Yellow, Red]]
winboardh = Board [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow], [Empty, Empty, Yellow, Red], [Empty, Empty, Yellow, Red]]
matrix = [[Empty, Empty, Yellow, Red], [Empty, Yellow, Yellow, Yellow]]
matrix2 = [[Empty, Empty, Red, Yellow], [Empty, Yellow, Yellow, Red], [Empty, Yellow, Red, Red], [Yellow, Yellow, Red, Yellow]]

-- Check free spaces in matrix
checkFreeSpaces :: Board Piece -> [(Int, Int)]
checkFreeSpaces board = [ (i, j) |  (j, row) <-columnsWithIndexs, (i, elem) <- row, elem == Empty]
  where columnsWithIndexs = matrixWithIndexes board

-- Make matrix with indexes, external index is column, internal is row
matrixWithIndexes :: Board a -> [(Int, [(Int, a)])]
matrixWithIndexes (Board columns) = zip [0, 1..] (map (zip [0,1 ..] ) columns)

listWithIndexes :: [[a]] -> [(Int, [a])]
listWithIndexes = zip [0, 1..]

-- TODO Update which player is next
-- update table on move played /*
movePlayed :: Board Piece -> Int -> Piece -> Board Piece
movePlayed (Board columns) j piece = Board [ row | (j, row)<- indexColums]
  where indexColums = modifyColumns (listWithIndexes columns) j piece

modifyColumns :: [(Int, [Piece])] -> Int -> Piece -> [(Int, [Piece])]
modifyColumns [] _ _ = []
modifyColumns ((j1, row):xs) j piece
  | j1 == j = (j1, modifyRow row piece) : xs
  | otherwise = (j1, row) : modifyColumns xs j piece

modifyRow :: [Piece] -> Piece -> [Piece]
modifyRow [] _ = []
modifyRow [elem] _ = [elem]
modifyRow (elem:x:xs) newElem
  | elem == Empty && x /= Empty = newElem:x:xs
  | otherwise = elem : modifyRow (x:xs) newElem
-- update table on move played */

-- TODO If win condition is met, check which player is currently playing and say that they won
-- end game conditions /*
checkIfMovesLeft :: Board Piece -> Bool
checkIfMovesLeft (Board columns) = Empty `elem` [ elem | row <- columns, elem <- row, elem == Empty]

-- gets diagonals in this position "/"
-- gets matrix(transposed matrix) but returns list of diagonals(diagonal also is a list)
-- stolen from: https://stackoverflow.com/questions/32465776/getting-all-the-diagonals-of-a-matrix-in-haskell
diagonals :: [[a]] -> [[a]]
diagonals [] = []
diagonals ([]:xss) = xss
diagonals rows = zipWith (++) list1 list2
  where list1 = map ((:[]) . head) rows ++ repeat []
        list2 = []:diagonals (map tail rows)

checkDiagonalsWin :: Board Piece -> Bool
checkDiagonalsWin (Board columns) = any checkFourInRow (diagonals matrix) || any checkFourInRow (diagonals revMatrix)
  where matrix = transpose columns
        revMatrix = map reverse matrix

checkVerticalWin :: Board Piece -> Bool
checkVerticalWin (Board columns) = any checkFourInRow columns

checkHorizontalWin :: Board Piece -> Bool
checkHorizontalWin (Board columns) = any checkFourInRow (transpose columns)

checkFourInRow :: [Piece] -> Bool
checkFourInRow row = any allSame (makeSublistsOfFour row)
  where allSame [] = False
        allSame (x:xs) = (x /= Empty) && (length (takeWhile (==x) xs) + 1 >= 4)

makeSublistsOfFour :: [Piece] -> [[Piece]]
makeSublistsOfFour [] = []
makeSublistsOfFour list = take 4 list : makeSublistsOfFour (tail list)

-- end game conditions */


-- Za poruke slican tip kao either, ukoliko je nevalidno vraca drugi tip konstruktora sa String porukicom, sledeci Bad >>= odmah vraca taj Bad i samo radimo Show obican za Bad da ispice String