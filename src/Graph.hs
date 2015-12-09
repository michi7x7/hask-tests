module Graph (testGraph) where

import Data.List (delete)
import Debug.Trace
import Control.Monad (guard)
import Util

type Node = Int
type Link = (Node, Node)

nodes :: [Node]
nodes = [1,2,3,4,5]

links :: [Link]
links = [(1,2), (2,3), (3,4), (4,1), (1,3), (3,5), (5,4), (4,2)]

nextLinks :: Node -> [Link] -> [Link]
nextLinks n = foldr nl []
    where nl (a,b) l
            | n == b = (b,a) : l
            | otherwise = (a,b) : l

findPath :: Node -> [Link] -> [[Link]]
findPath _ [] = []
findPath node links = do
    let links' = nextLinks node links
    lnk <- links'

    let (n, n') = lnk
    guard $ n == node

    let links'' = delete lnk links'

    if null links'' then
        return [lnk]
    else do
        path <- findPath n' links''
        return $ lnk : path

findAllPaths :: [[Link]]
findAllPaths = do
    node <- nodes
    findPath node links
    
testGraph = putStrLn $ formatArr 80 findAllPaths