-- fatorial 0 = 1
-- fatorial n = (n-1) * (n-2) * n

mult4 :: Int -> Int -- cabeçalho da função
mult4 x  = 4 * x

-- ":l aula_01.hs" para compilar

allEqual :: Int -> Int -> Int -> Bool
allEqual n m p = (n == p) && (m == p)

max2 :: Int -> Int -> Int
max2 n m | n >= m = n
         | otherwise = m

max3 :: Int -> Int -> Int -> Int
max3 n m o | n >= m && n >= o = n
           | otherwise m >= n && m >= o = m
           | otherwise = o

fac :: Int -> Int 
fac 0 = 1 
fac n = n * (n-1)

power :: Int -> Int -> Int
power _ 0 = 1
power x y = x * power x (y-1)

{- OPERAÇÕES COM LISTA 

[1,2]++[1..10]
=> [1,2,1,2,3,4,5,6,7,8,9,10]
   3:[1,2]
=> [3,1,2]
   head[1..20]
=> 1
   tail[1..20]
=> [2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]
   tail[1]
=> []
   last[1..20]
=> 20
   init[1..20]
=> [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19]
   take 2 [1..10]
=> [1,2] 
     elem 2[1,2,3]
=> True
   2 `elem` [1,2,3]
=> True 
   take 3 [13,26..]
=> [13,26,39]    -}
