-- λ> isPalindrome [1,2,3]
-- False
-- λ> isPalindrome "madamimadam"
-- True
-- λ> isPalindrome [1,2,4,8,16,8,4,2,1]
-- True

-- https://stackoverflow.com/questions/26847192/reverse-a-list-in-haskell

reverseList :: (Eq a) => [a] -> [a]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = False
isPalindrome xs | xs == reverseList xs = True
                | otherwise            = False

isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' [_] = True
isPalindrome' xs = (head xs) == (last xs) && (isPalindrome' $ init $ tail xs)

isPalindrome'' :: (Eq a) => [a] -> Bool
isPalindrome'' xs = foldl (\acc (a,b) -> if a == b then acc else False) True input
    where
    input = zip xs (reverse xs)