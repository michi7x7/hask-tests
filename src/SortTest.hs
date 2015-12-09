module SortTest (testSort) where

quicksort' :: (Ord a) => [a] -> [a]
quicksort' [] = []
quicksort' (x:xs) =
    let
        low = filter (< x) xs
        high = filter (>= x) xs
    in quicksort' low ++ [x] ++ quicksort' high


testSort :: IO ()
testSort = do
    ipt <- getLine
    let
        vals :: [Int]
        vals = map read $ words ipt
    print $ quicksort' vals