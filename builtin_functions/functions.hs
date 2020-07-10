-- head, tail, last, init, lenght
 
head' :: Eq (a) => [a] -> a
head' [] = error "list empty"
head' [x] = x
head' (x:xs) = x

tail' :: Eq (a) => [a] -> [a]
tail' []     = error "list empty"
tail' (x:xs) = xs

last' :: Eq (a) => [a] -> a
last' []     = error "list empty"
last' [x]    = x
last' (x:xs) = last' xs

init' :: Eq (a) => [a] -> [a]
init' []     = error "list empty"
init' [x]    = []
init' (x:xs) = x : init' xs

length' :: [a] -> Int
length' []     = 0
length' [x]    = 1
length' (x:xs) = length' xs + 1

-- null, reverse, take, drop, maximum

null' :: [a] -> Bool
null' []     = True
null' (x:xs) = False

reverse' :: Eq (a) => [a] -> [a]
reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

take' :: Eq (a) => Int -> [a] -> [a]
take' _ []     = []
take' 1 (x:xs) = [x]
take' y (x:xs) = [x] ++ take' (y - 1) xs

drop' :: Eq (a) => Int -> [a] -> [a]
drop' _ []     = []
drop' 1 (x:xs) = xs
drop' y (x:xs) = drop' (y - 1) xs

max' :: Ord (a) => [a] -> a
max' [x]    = x
max' (x:xs) | x > head xs = x
            | otherwise = max' xs

-- minimum, sum, products, elem, foldl , foldr

min' :: Ord (a) => [a] -> a
min' [x]    = x
min' (x:xs) | x < head xs = x
            | otherwise = min' xs

sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

prod' :: Num a => [a] -> a
prod' [] = 1
prod' (x:xs) = x * prod' xs

elem' :: Eq (a) => a -> [a] -> Bool
elem' _ [] = False
elem' y (x:xs) | y == x = True
               | otherwise = elem' y xs

