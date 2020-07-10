-- Find the K'th element of a list. The first element in the list is number 1.

kthElem :: [a] -> Int -> a
kthElem (x:_) !! 0    = x 
kthElem (_:xs) n      = xs !! (n - 1)
