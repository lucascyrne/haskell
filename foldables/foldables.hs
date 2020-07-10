import Prelude hiding (reverse, head, last, tail, length, sum, max, elem)

length :: [Int] -> Int
length = foldl (\acc _ -> acc + 1) 0

sum :: [Int] -> Int
sum = foldl (\acc curr -> acc + curr) 0

max :: [Int] -> Int
max (x:xs) = foldl (\acc curr -> if curr > acc then curr else acc) x xs

elem :: Eq a => a -> [a] -> Bool
elem item = foldl (\acc curr -> if curr == item then True else acc) False

delete :: Eq a => a -> [a] -> [a]
delete item = foldl (\acc curr -> if curr == item then acc else acc ++ [curr]) []

head :: Eq a => [a] -> a
head (x:xs) = foldl (\acc _ -> acc) x xs

last :: Eq a => [a] -> a
last (x:xs) = foldl (\_ curr -> curr) x xs

tail :: Eq a => [a] -> [a]
tail (x:xs) = foldl (\acc curr -> acc ++ [curr]) [] xs

reverse :: Eq a => [a] -> [a]
reverse = foldl (\acc curr -> curr : acc) []