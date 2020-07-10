import Prelude hiding (elem, nub)

elem :: Eq a => a -> [a] -> Bool
elem e [] = False
elem e (x:xs) = (e == x) || (elem e xs)

soma :: Num (a) => a -> a -> a
soma = (\x y -> x + y)

-- foldl (\x y -> x * y) 1 [1,2,3,4,5]

{-
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' _ [] = False
elem' = (\e (x:xs) -> if (e == x) then True else foldl (elem' x xs)) 
-}

nub :: Eq a => [a] -> [a]
nub [] = [] 
nub (x:xs) 
    | x `elem` xs = nub xs
    | otherwise   = x : nub xs

isAsc :: [Int] -> Bool
isAsc []  = True
isAsc [x] = True
isAsc (x:xs)
    | x <= (head xs) = isAsc xs 
    | otherwise = False

isAsc' :: [Int] -> Bool
isAsc' []  = True
isAsc' [x] = True
isAsc' (x:y:ys) = (x <= y) && isAsc' (y:ys)

hasPath :: [(Int, Int)] -> Int -> Int -> Bool
hasPath [] _ _ = error "blah"
hasPath xs a b
    | 

