module BinomialHeap where 

data Tree a = Node Int a [Tree a]
data Heap = [Tree]

link :: Ord a => Tree a -> Tree a -> Tree a
link (n1@(Node r y c1)) (n2@(Node r2 y2 c2)) = 
  if y <= y2 then
    Node (r + 1) y (n2 : c1) 
  else
    Node (r + 1) y2 (n1 : c2)