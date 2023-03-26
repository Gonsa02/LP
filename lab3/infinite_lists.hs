ones :: [Integer]
ones = 1:ones

nats :: [Integer]
nats = [0..]

ints :: [Integer]
ints = 0:concatMap (\x -> [x, -x]) [1..]

triangulars :: [Integer]
triangulars = scanl (+) 0 [1..]

factorials :: [Integer]
factorials = scanl (*) 1 [1..]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

primes :: [Integer]
primes = sieve [2..]
    where
	sieve (x:xs) = x : sieve (filter (\y -> y `mod` x /= 0) xs)

hammings :: [Integer]
hammings = 1 : merge (map (2*) hammings) (merge (map (3*) hammings) (map (5*) hammings))
    where
	merge (x:xs) (y:ys)
	    | x < y = x : merge xs (y:ys)
	    | x > y = y : merge (x:xs) ys
	    | otherwise = x : merge xs ys
