data Queue a = Queue [a] [a]
    deriving (Show)

create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push a (Queue p s) = Queue p (a:s)

pop :: Queue a -> Queue a
pop (Queue [] s) = pop (Queue (reverse s) [])
pop (Queue (x:p) s) = Queue p s

top :: Queue a -> a
top (Queue [] s) = last s
top (Queue p s) = head p

empty :: Queue a -> Bool
empty (Queue p s)
    | length p == length s && length s == 0 = True
    | otherwise = False

instance Eq a => Eq (Queue a)
    where
	(Queue p1 s1) == (Queue p2 s2) = (p1 ++ reverse s1) == (p2 ++ reverse s2)
