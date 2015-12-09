module Bitfield (testBitfield) where

import Control.Monad
import Control.Applicative ((<$>))
import Data.List (transpose)

genBits :: Int -> [[Bool]]
genBits n = replicateM n [True, False]

pretty :: Bool -> Char
pretty True  = '#'
pretty False = ' '

testBitfield = putStrLn $ unlines $ map pretty <$> transpose (genBits 7)