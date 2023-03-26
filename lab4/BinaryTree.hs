data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)

size :: Tree a -> Int
size Empty = 0
size (Node _ e d) = 1 + size e + size d

height :: Tree a -> Int
height Empty = 0
height (Node _ e d) = 1 + max (height e) (height d)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal (Node a al ar) (Node b bl br) = a == b && equal al bl && equal ar br
equal _ _ = False

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic (Node a al ar) (Node b bl br) = a == b && equal al br && equal ar bl
isomorphic _ _ = False

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node a al ar) = [a] ++ ((preOrder al) ++ (preOrder ar))

inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node a al ar) = (inOrder al) ++ [a] ++ (inOrder ar)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node a al ar) = (postOrder al) ++ (postOrder ar) ++ [a]

breadthFirst :: Tree a -> [a]
breadthFirst t = bfs [t]
    where
        bfs [] = []
        bfs (Empty:xs) = bfs xs
        bfs ((Node a lt rt):trees) = a : (bfs (trees ++ [lt,rt]))

build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build (x:preorder) inorder = Node x (build lpreOrder linOrder) (build rpreOrder rinOrder)
    where
        linOrder = takeWhile (/= x) inorder
        rinOrder = tail (dropWhile (/= x) inorder)
        lpreOrder = take (length linOrder) preorder
        rpreOrder = drop (length linOrder) preorder

overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap _ Empty Empty = Empty
overlap _ t Empty = t
overlap _ Empty t = t
overlap f (Node a al ar) (Node b bl br) = Node (f a b) leftTree rightTree
    where
        leftTree = overlap f al bl
        rightTree = overlap f ar br
