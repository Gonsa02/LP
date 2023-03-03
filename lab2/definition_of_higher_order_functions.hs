myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ b [] = b
myFoldl f b (l:xs) = myFoldl f (f b l) xs

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (l:xs) = f l $ myFoldr f b xs

myIterate :: (a -> a) -> a -> [a]
myIterate f b = b : myIterate f (f b)

myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil b f c
    | b c = c
    | otherwise = myUntil b f $ f c

myMap :: (a -> b) -> [a] -> [b]
myMap f l = myFoldr (\x xs -> (f x):xs) [] l

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f l = myFoldr (\x xs -> if f x then x:xs else xs) [] l

myAll :: (a -> Bool) -> [a] -> Bool
myAll f l = and $ myMap f l

myAny :: (a -> Bool) -> [a] -> Bool
myAny f l = or $ myMap f l

myZip :: [a] -> [b] -> [(a,b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f x y = myFoldr (\x y -> (f (fst x) (snd x)):y) [] $ myZip x y
