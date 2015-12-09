{-# LANGUAGE RankNTypes #-}
module Main where

import Data.List (cycle, transpose)
import Data.Bits (xor)

import System.Random (randomIO, Random(), randomRIO, getStdGen, randomRs )
import System.IO
import System.Environment (getArgs)
import Data.Function (fix)
import Data.Char (chr, ord)
import System.Posix.Files
import Control.Applicative ((<$>))
import Data.List (find, sort, permutations)
import Data.Maybe (fromJust)
import Control.Monad (guard, forM)
import Control.Exception
import Data.List.Split

-- import Numeric.IEEE (eqRel, delta)

-- chose n k = n! / (k! * (n-k)!)

-- choose :: (Integral a) => a -> a -> a
-- choose n 0 = 1
-- choose n k = choose n (k-1) * ((n-k+1) `div` k)

-- choose' n k = fromIntegral $ choose n k

-- crash :: Int32
-- crash = (-2147483648::Int32) * (-1)

parse = mapM readM $ words "12 345 6789" :: Maybe [Integer]
    where readM s = case reads s of
            [(s, "")] -> return s
            otherwise -> fail "no parse"

mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' f = foldr mm []
    where
        mm x xs = case f x of
            Nothing -> xs
            Just x' -> x':xs


every x   = cycle $ replicate (x-1) False ++ [True]
pattern n = map (take n . every) [1..n]
open n    = map (foldr xor False) $ transpose $ pattern n
pretty o  = map snd $ filter fst $ zip o [1..]

-- main = print $ pretty $ open 100

takeFirst :: (a -> Bool) -> (IO a) -> IO a
takeFirst c g = loop
    where loop = do
            v <- g
            if c v then return v else loop

-- rand' = takeFirst odd randomIO


-- main = do
--    x <- rand' :: IO Int
--    print x


bits = [01111001, 01101111, 01110101, 01110100, 01110101, 00101110, 01100010, 01100101, 00101111, 01100100, 01010001, 01110111, 00110100, 01110111, 00111001, 01010111, 01100111, 01011000, 01100011, 01010001];

digits :: Int -> Int
digits 0 = 0
digits n = n `mod` 2 + 2*digits (n `div` 10)

-- main = print $ map (chr.digits) bits

-- main = putStr $ (++ "...") . take 40 $ show [10 `div` n | n <- reverse [0 .. 20]]

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs' a = a  ++ iterate (*2) (beg a)
              where beg [a] = 2*a
                    beg a = sum a


-- main = print $ take 80 $ fibs -- [1,2] -- print $ let fibs = 0 : 1 : zipWith (+) fibs (tail fibs) in find (> 100000000000000) fibs

hGetCharMaybe :: Handle -> IO (Either IOException Char)
hGetCharMaybe h = try (hGetChar h)

-- main = withFile "abc.txt" AppendMode $ do
--            flip hPutStrLn "Line 1"
--            flip hPutStrLn "Line 2"
--            const $ putStrLn "Done"

sini = (floor . (*2^23) . sin . (* (pi/2 / 128)) ) `map` [0..127]

freq m = 2**((m-69)/12) * 440;
tostep f = f * 512 / (48000)
steps = (round . (*512) . tostep . freq) `map` [0..127]

-- main = print steps

median :: Ord a => [a] -> [a]
median [] = []
median a | even l    = [s !! h, s !! (h+1)]
         | otherwise = [s !! h]
         where s = sort a
               l = length a
               h = l `div` 2

-- main = print $ median [2,4,5,1,8]


solve :: [[Double]]
solve = do
    t <- permutations [1..9]
    let [a,b,c,d,e,f,g,h,i] = t
    guard $ a + 13 * b / c + d + 12 * e - f - 11 + g * h / i - 10 == 66
    return t


randoms :: IO [Int]
randoms = do
    a <- randomRIO (1,6)
    b <- randomRIO (1,6)
    c <- randomRIO (1,6)
    return [a,b,c]

splAdd :: [Int] -> (Int, Int)
splAdd a = (sum $ take 3 a, sum $ drop 3 a)

comp :: (Int, Int) -> Int
comp (a,b) = if a == b then 1 else 0



main' = do
    gen <- getStdGen 
    let rnds = take 10000000 $ chunksOf 6 $ randomRs (1,6) gen :: [[Int]]

    print $ ( / 100000000) $ fromIntegral $ sum $ comp . splAdd <$> rnds

    return ()


-- main = getArgs >>= print . (foldl apply 0)

apply :: Integer -> String -> Integer
apply x "-z" = 0
apply x "-e" = 1
apply x "-d" = 2*x
apply x "-s" = x-1
apply x "-q" = x*x
apply x _ = x

main''' = print $ sum' 0 [1..10]

sum'  = foldl (+)

hexdig :: Int -> Char
hexdig n | n >= 0  && n < 10  = chr $ ord '0' + n
         | n >= 10 && n <= 30 = chr $ ord 'A' + n - 10

convBase :: Int -> Int -> String
convBase _ 0 = ""
convBase b n = convBase b (n `div` b) ++ [hexdig (n `mod` b)]

main = print $ convBase 16 31