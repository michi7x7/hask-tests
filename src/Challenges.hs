module Challenges where

-- Fix the typo to get rid of the compilation error.
val = filter (divides 3) [1..10]
    where divides n x = (x `mod` n) == 0

main = putStrLn (show val)
-- Click the button below the code editor to evaluate 'main'.

-- That was easy!
-- If you are an intermediate haskeller, try solving the following exercises, courtesy of Tony Morris.
-- Source: http://blog.tmorris.net/20-intermediate-haskell-exercises/
-- If you can get a solution to typecheck, there is a very good chance that it is correct.

class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = fmap

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry = fmap

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = f . g

newtype EitherLeft b a = EitherLeft (Either a b)
newtype EitherRight a b = EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft . Left . f $ x
  furry _ (EitherLeft (Right x))= EitherLeft . Right $ x

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry _ (EitherRight (Left x)) = EitherRight . Left $ x
  furry f (EitherRight (Right x))= EitherRight . Right . f $ x

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana $ unicorn . f

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn = return

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana _ Nothing = Nothing
  banana f (Just x) = f x
  unicorn = return
  
-- Exercise 9
-- Relative Difficulty: 6
instance Misty ((->) t) where
  banana x y z = (x $ y z) z
  unicorn f _ = f

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x)) = f x
  banana _ (EitherLeft (Right x)) = EitherLeft $ Right x
  unicorn x = EitherLeft $ Left x

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana _ (EitherRight (Left x)) = EitherRight $ Left x
  banana f (EitherRight (Right x)) = f x
  unicorn x = EitherRight $ Right x

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = banana . flip furry'

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy [] _ = unicorn []
moppy (x:xs) f = banana2 (:) (f x) $ moppy xs f

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage = flip moppy id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 = error "todo"

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 = error "todo"

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 = error "todo"

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry = error "todo"

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana = error "todo"
  unicorn = error "todo"
