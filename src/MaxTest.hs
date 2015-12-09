module MaxTest where

import Data.Ratio
    
maximum' :: (Ord a) => [a] -> Maybe a
maximum' [] = Nothing
maximum' [x] = Just x
maximum' (x:xs) = let x' = maximum xs
                  in Just (if x > x' then x else x')

maximum'' :: (Ord a) => [a] -> Maybe a
maximum'' [] = Nothing
maximum'' (x:xs) = Just $ foldl max x xs

safeRead :: (Read b) => String -> Maybe b
safeRead str = case reads str of
    [(res, "")] -> Just res
    _ -> Nothing

testMax :: IO ()
testMax = do
    line <- getLine
    let
        list :: [Maybe Rational]
        list =  map safeRead $ words line
    -- (Maybe Int) is comparable
    case maximum'' list of
        Nothing -> putStrLn "No Result!"
        Just (Just max) -> do
            print max
            testMax