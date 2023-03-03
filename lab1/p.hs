factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

eleva :: Int -> Int -> Int
eleva x 0 = 1
eleva x n = x * eleva x (n - 1)
