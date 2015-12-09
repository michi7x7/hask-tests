module Regex where

import Text.Regex.Posix
import System.Directory
import Data.Maybe (mapMaybe)
import Control.Monad (forM, liftM)
import Control.Exception (bracket)

applyRes :: (a -> Maybe b) -> a -> Maybe (a, b)
applyRes f a = case f a of
    Just b  -> Just (a,b)
    Nothing -> Nothing

regexReplace :: String -> String -> String -> Maybe String
regexReplace expr repl str = replace repl
    where
        matches = getAllTextSubmatches  (str =~ expr) :: [String]

        match :: Int -> Maybe String
        match n | n < 0 = Nothing
                | n >= length matches = Nothing
                | otherwise = Just $ matches!!n

        replace :: String -> Maybe String
        replace ('$':s:ss) = do
            m <- match (read [s])
            replace $ m ++ ss

        replace (s:ss) = liftM (s:) (replace ss)
        replace [] = Just []


inDir :: FilePath -> IO a -> IO a
inDir dir f = bracket cd cb (const f)
            where cd = do
                    dir' <- getCurrentDirectory
                    setCurrentDirectory dir
                    return dir'
                  cb = setCurrentDirectory

renameFiles :: FilePath -> String -> String -> IO Int
renameFiles dir expr repl = inDir dir $ do
    files <- getDirectoryContents "."
    let files' = mapMaybe (applyRes (regexReplace expr repl)) files
    print files'
    -- forM files' $ \(o,n) -> renameFile o n
    return $ length files'

testRegex = renameFiles "src" "(.*).hs" "$1.hs2"
