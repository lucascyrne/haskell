count :: Eq a => [a] -> (Int, a)
count a (x:xs) | x == xs = ()

encode :: Eq a => [a] -> (Int, a) -- [ListItem a]
encode xs = count (head xs) xs