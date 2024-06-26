
data Rose a = Node a [Rose a] deriving (Eq, Show)

instance Functor Rose where
  fmap f (Node x list) = Node (f x) [fmap f y | y <- list]

size :: Rose a -> Int
size (Node _ []) = 1
size (Node x (y:ys)) = size y + size (Node x ys)
-- size (Node x ys) = foldl  (\acc y -> acc + size y) 1 ys

height :: Rose a -> Int
height (Node _ []) = 1
height (Node x (y:ys)) = max (height y + 1) (height (Node x ys))

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