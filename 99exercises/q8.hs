{- * (compress '(a a a a b c c a a d e e e e))
(A B C A D E) -}

head' :: (Eq a) => [a] -> a
head' [] = error "list empty, porra"
head' (x:xs) = x

compress :: (Eq a) => [a] -> [a]    
compress [] = []
compress [x] = [x]
compress (x:xs)
    | x == head' xs = compress xs
    | otherwise = [x] ++ compress xs