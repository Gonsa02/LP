countIf :: (Int -> Bool) -> [Int] -> Int
countIf _ [] = 0
countIf f (x:xs)
    | f x = 1 + countIf f xs
    | otherwise = countIf f xs

pam :: [Int] -> [Int -> Int] -> [[Int]]
pam l f = map (`map` l) f

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 l f = map (\x -> map ($ x) f) l

filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl f o a l = foldl o a $ filter f l

insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert _ [] e = [e]
insert f (x:xs) e
    | f x e = x:insert f xs e
    | otherwise = e:x:xs

insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort f l = foldr (\x xs -> insert f xs x) [] l
