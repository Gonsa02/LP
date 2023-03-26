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

--Second part
instance Functor Queue where
    fmap f (Queue p s) = (Queue (map f p) (map f s))

translation :: Num b => b -> Queue b -> Queue b
translation a q = fmap (+a) q

instance Applicative Queue where
    pure x = Queue  [x] []
    Queue xf yf <*> Queue x y = Queue (xf <*> x) (yf <*> y)

union :: Queue a -> Queue a -> Queue a
union (Queue xs ys) (Queue as bs) = Queue (xs ++ reverse ys ++ as ++ reverse bs) []

instance Monad Queue where
    return = pure
    (Queue [] []) >>= _ = Queue [] []
    (Queue (x:xs) ys) >>= f = union (f x) (Queue xs ys >>= f)
    (Queue [] rs) >>= f = Queue (reverse rs) [] >>= f

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f q = do
    x <- q
    if f x
        then return x
        else (Queue [] [])