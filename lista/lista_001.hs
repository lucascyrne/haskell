take 24 [13,26..]
=> [13,26,39,52,65,78,91,104,117,130,143,156,169,182,195,208,221,234,247,260,273,286,299,312]

take 10 (cycle [1..3])
=> [1,2,3,1,2,3,1,2,3,1]

doubleList :: [Int] -> [Int]
doubleList as | as == [] = []
              | otherwise = (head as)*2 : doubleList (tail as)

doubleList' :: [Int] -> [Int]
doubleList' [] = []
doubleList' (a:as) = a*2 : doubleList' as

doubleList'' :: [Int] -> [Int]
doubleList'' [] = []
doubleList'' a = [b*2 | b <- a]

maxList :: [Int] -> [Int]
maxList [] = []
maxList (a:[]) = [a] -- isola o primeiro elemento (a)
maxList (a:as) | a > head(maxList as) = [a] -- confere se (a) é maior que o próximo elemento (b) na lista, se SIM, retorna (a) 
               | otherwise = maxList as -- se NÃO repete a função, isolando (b) e conferindo se ele é maior que o próximo (c). Assim por diante.


[ [i,j] | i <- [1,2], j <- [1..4] ] -- => [[1,1],[1,2],[1,3],[1,4],[2,1],[2,2],[2,3],[2,4]]
take 10 [[i,j] | i <- [1,2], j <- [1..] ]

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) = quicksort (small xs x) ++ (x: quicksort( large xs x))

small :: [Int] -> Int -> [Int]
small [] _ = []
small xs x = [y | y <- xs, y <= x]

large :: [Int] -> Int -> [Int]
large [] _ = []
large xs x = [y | y <- xs, y >= x]
