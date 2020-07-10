pertence :: [Int] -> Int -> Bool
pertence [] _ = False
pertence (x:xs) n | (x == n) = True
                  | otherwise = pertence xs n

maior :: [Int] -> Int
maior [] = 0
maior [x] = x
maior (x:xs) | (x > maior xs) = x
             | otherwise = maior xs

pares :: [Int] -> Bool
pares [] = True
pares (x:xs) | (mod x 2 /= 0) = False
             | otherwise = pares xs

main :: IO ()
main = return ()
