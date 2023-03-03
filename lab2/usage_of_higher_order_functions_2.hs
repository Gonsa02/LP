import Data.Char (isSpace)

flatten :: [[Int]] -> [Int]
flatten [] = []
flatten x = foldl (++) [] x

myLength :: String -> Int
myLength x = foldl (\x _ -> x+1) 0 x

myReverse :: [Int] -> [Int]
myReverse x = foldl (\z y -> y:z) [] x

countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (count x) l
    where count n = length . filter (==n)

firstWord :: String -> String
firstWord s = takeWhile (not.isSpace) $ dropWhile (isSpace) s
--isAlpha is for detect characters
