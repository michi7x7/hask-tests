module Tac where

import System.Environment
import System.Exit
import Control.Applicative ((<$>))

main = getArgs >>= parse >>= putStr . tac

tac  = unlines . reverse . lines

parse ("-h":_) = usage   >> exit
parse ("-v":_) = version >> exit
parse []       = getContents
parse fs       = concat <$> mapM readFile fs

usage   = putStrLn "Usage: tac [-vh] [file ..]"
version = putStrLn "Haskell tac 0.1"
exit    = exitSuccess
die     = exitFailure
