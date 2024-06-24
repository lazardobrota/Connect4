data Rose a = Node a [Rose a] deriving (Eq, Show)

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