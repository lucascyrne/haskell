import Data.List

descSort :: ([b] -> [c]) -> ([a] -> [b]) -> [a] -> [c]
descSort = (\x -> reverse (sort x))


