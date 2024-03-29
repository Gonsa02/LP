eql :: [Int] -> [Int] -> Bool
eql x y = length x == length y && and (zipWith (==) x y)

prod :: [Int] -> Int
prod = foldl (*) 1

prodOfEvens :: [Int] -> Int
prodOfEvens x = foldl (*) 1 (filter even x)

powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct x y = foldl (+) 0 (zipWith (*) x y)
