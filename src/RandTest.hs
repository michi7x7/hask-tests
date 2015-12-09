module RandTest where

import Control.Monad
import System.Random

genRandom :: StdGen -> Int -> [Int]
genRandom gen max = take 10 $ randomRs (0, max) gen

printStd :: Int -> [[ Int ]]
printStd 0 = []
printStd n = let
        rnds  = printStd (n-1) --recurse
        gen   = mkStdGen n
        rnds' = genRandom gen 10
    in rnds' : rnds

printLn :: (Show a) => [a] -> IO ()
printLn = prt
  where
      prt [] = return ()
      prt (v:vs) = do
          print v
          prt vs

testRand :: IO ()
testRand = let rnds = printStd 10
           in printLn rnds