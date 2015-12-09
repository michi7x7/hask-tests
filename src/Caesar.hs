module Caesar (caesar, caesar') where

import           Data.Char       (chr, ord, isAsciiLower, isAsciiUpper)

caesar :: Int -> String -> String
caesar s = map conv
    where
        conv c = chr $ sft $ ord c
        sft n | inRng 'a' 'z' n = rdc (n+s) (ord 'a') (ord 'z')
              | inRng 'A' 'Z' n = rdc (n+s) (ord 'A') (ord 'Z')
              | otherwise = n
        inRng l u n = let
            l' = ord l
            u' = ord u
            in (n >= l') && (n <= u')
        rdc n nmin nmax | n > nmax = rdc (n - rg) nmin nmax
                        | n < nmin = rdc (n + rg) nmin nmax
                        | otherwise = n
                        where
                            rg = nmax - nmin +1

caesar' :: Int -> String -> String
caesar' s = map trns
    where
        trns :: Char -> Char
        trns c  | isAsciiLower c = chr $ rdc (ord c + s) (ord 'a') (ord 'z')
                | isAsciiUpper c = chr $ rdc (ord c + s) (ord 'A') (ord 'Z')
                | otherwise = c
        rdc n nmin nmax | n > nmax = rdc (n - rg) nmin nmax
                        | n < nmin = rdc (n + rg) nmin nmax
                        | otherwise = n
                        where
                            rg = nmax - nmin +1
