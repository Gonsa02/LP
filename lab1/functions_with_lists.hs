import Data.List

myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
    | (myMaximum xs) > x = myMaximum xs
    | otherwise = x

mySum :: [Int] -> Int
mySum [] = 0
mySum [x] = x
mySum (x:xs) = x + mySum xs

average :: [Int] -> Float
average x = (fromIntegral (mySum x) / fromIntegral (myLength x))

buildPalindrome :: [Int] -> [Int]
buildPalindrome x = reverse x ++ x

remove :: [Int] -> [Int] -> [Int]
remove [] _ = []
remove (x : xs) y
    | elem x y = remove xs y
    | otherwise = x : remove xs y

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x : xs) = foldl (++) x xs

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs)
    | even x = (a, x : b)
    | odd x = (x : a, b)
    where (a, b) = oddsNevens xs

primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors x
    | d == [] = [x]
    | otherwise = nub (d ++ primeDivisors(div x (head d)))
    where d = take 1 ( filter (\y -> (mod x y) == 0) [2..(x-1)] )
