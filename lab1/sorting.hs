insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (x:xs) y
    | y > x = x : insert xs y
    | otherwise = y : x : xs

isort :: [Int] -> [Int]
isort [] = []
isort (x:xs) = insert (isort xs) x

remove :: [Int] -> Int -> [Int]
remove [] x = []
remove (x:xs) y
    | x == y = xs
    | otherwise = x : remove xs y

ssort :: [Int] -> [Int]
ssort [] = []
ssort x = minimum x : ssort (remove x (minimum x))

merge :: [Int] -> [Int] -> [Int]
merge x [] = x
merge [] y = y
merge (x:xs) (y:ys)
    | x <= y = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys

msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort x = merge (msort left) (msort right)
    where
	left = take half x
	right = drop half x
	half = div (length x) 2

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) = (qsort menors) ++ [x] ++ (qsort majors)
    where
	menors = [p | p <- xs, p < x]
	majors = [p | p <- xs, p >= x]

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort (x:xs) = (genQsort menors) ++ [x] ++ (genQsort majors)
    where
	menors = [p | p <- xs, p < x]
	majors = [p | p <- xs, p >= x]
