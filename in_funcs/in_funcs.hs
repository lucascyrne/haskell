{- fst(1,2)
snd(1,2)
zip[1,2,3]  [4,5,6]
zip[1..] ["banana", "laranja", "pêra"] -}

addPair :: (Int, Int) -> Int
addPair (x,y) = x + y

addPairList :: [(Int, Int)] -> [Int]
addPairList [] = []
addPairList (x:xs) = (addPair x) : addPairList xs

addPairList' :: [(Int, Int)] -> [Int]
addPairList' xs = [a+b | (a,b) <- xs]

digits :: String -> String
digits [] = []
digits (l:ls) | l `elem` "0123456789" = l: digits ls
              | otherwise = digits ls

len :: [a] -> Int
len [] = 0
len (x:xs) = 1 + len xs

zip' :: [a] -> [b] -> [(a, b)]
zip' _ _ = []
zip' (a:as) (b:bs) = (a,b) : zip' as bs

take' :: (Int, [Int]) -> [Int]
take' 0 _ = []
take' _ [] = []
take' n (a:as) = a : take' (n-1) as

curry' :: ((a,b)->c) -> a -> b -> c
curry' f a b = f (a,b)

