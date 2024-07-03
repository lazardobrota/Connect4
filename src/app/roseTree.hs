module RoseTree
( size
, rheight
, leavesCount
, elemsOnDepth
, foldRose
, generateRose
) where

import Board
import Player

data Rose a = Node a [Rose a] deriving (Eq, Show)

instance Functor Rose where
  fmap f (Node x list) = Node (f x) [fmap f y | y <- list]

size :: Rose a -> Int
size (Node x ys) = foldl  (\acc y -> acc + size y) 1 ys
-- size (Node _ []) = 1
-- size (Node x (y:ys)) = size y + size (Node x ys)


rheight :: Rose a -> Int
rheight (Node _ []) = 1
rheight (Node x (y:ys)) = max (rheight y + 1) (rheight (Node x ys))

leavesCount :: Rose a -> Int
leavesCount (Node _ []) = 1
leavesCount (Node x ys) = foldl (\acc y -> acc + leavesCount y) 0 ys

elemsOnDepth :: Int -> Rose a -> [a]
elemsOnDepth 0 (Node x _) = [x]
elemsOnDepth _ (Node x []) = []
elemsOnDepth num (Node x (y:ys)) = elemsOnDepth (num - 1) y ++ elemsOnDepth num (Node x ys)

foldRose :: (a -> b -> a) -> a -> Rose b -> a
foldRose f acc (Node x list) = foldl (foldRose f) (f acc x) list
-- foldRose f acc (Node x []) = f acc x 
-- foldRose f acc (Node x (y:ys)) = foldRose f newAcc (Node x ys)
--   where newAcc = foldRose f acc y

generateRose :: a -> Int -> (a -> [a]) -> Rose a
generateRose root 0 f = Node root []
generateRose root depth f = Node root [ generateRose newRoot (depth - 1) f | newRoot <- f root ]


generatePaths :: Board Piece -> Int -> Rose (Board Piece)
generatePaths board depth = makeAllPaths (Node board []) depth Player1

makeAllPaths :: Rose (Board Piece) -> Int -> Player -> Rose (Board Piece)
makeAllPaths rose 0 _ = rose 
makeAllPaths (Node board@(Board columns) list) depth player = Node board [makeAllPaths (makeRose board j player) (depth - 1) (switchTurn player) | j <- [0..width board - 1] ]
  where makeRose board j player = Node (movePlayed board j (playerPiece player)) []

board2 = Board [[Empty, Empty, Yellow, Red], [Empty, Red, Yellow, Yellow]]

rose = Node 5 [
  Node 4 [],
  Node 3 [], 
  Node 2 []
  ]

rose2 = Node 8 [
    Node 4 [
        Node 5 [
          Node 6 [], 
          Node 9 []
        ],
        Node 7 []
    ],
    Node 3 [], 
    Node 2 [
      Node 1 []
    ]
  ]

rose3 = Node "yoo, " [Node "hi" [Node "how" [], Node "are" []]]

a = (\x->[[x+1],[x+2],[x+3]]) 5
