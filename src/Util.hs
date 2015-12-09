module Util (formatBlock, formatArr) where

import           Data.List.Split

--- Output formating
formatBlock :: Int -> String -> String
formatBlock n = unlines . chunksOf n

formatArr :: (Show a) => Int -> [a] -> String
formatArr n arr = 
    let (lines, str) = foldl lno (0, []) arr
    in concatMap (formatBlock n) str
    where
        lno :: (Show a) => (Int, [String]) -> a -> (Int, [String])
        lno (n, s) s' = (n+1, s ++ [show n ++ ": " ++ show s' ++ "\n"] )