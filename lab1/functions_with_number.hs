absValue :: Int -> Int
absValue n
    | n >= 0 = n
    | otherwise = -n


power :: Integer -> Integer -> Integer
power _ 0 = 1
power x n 
    | even n = y*y
    | otherwise = y*y*x
    where
	y = power x n_halved
	n_halved = div n 2

isPrime :: Int -> Bool
isPrime k
    | k > 2 = null [ x | x <- [2..(ceiling (sqrt (fromIntegral k)))], k `mod` x == 0]
    | k == 2 = True
    | otherwise = False

slowFib :: Int -> Int
slowFib 0 = 0
slowFib 1 = 1
slowFib n = slowFib (n-1) + slowFib (n-2)

quickFib :: Int -> Int
quickFib x = fst(quickFibAux x)

quickFibAux 0 = (0, 1)
quickFibAux x = (p, p+q)
    where (q, p) = quickFibAux(x-1)

