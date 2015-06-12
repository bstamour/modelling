{-# LANGUAGE ScopedTypeVariables #-}

module Markov where

import System.Random
import Control.Monad
import Control.Applicative

{- Implementation of a basic markov chain. -}

type TransTable a = [(a, a, Double)]

-- Randomly pick a transition and return the next state to move to.
update :: forall a. Eq a => TransTable a -> a -> IO (Maybe a)
update table item = findRange ranges <$> getStdRandom random <*> pure 0.0
  where
    ranges = filter (\(src, dst, prob) -> src == item) table

    findRange :: TransTable a -> Double -> Double -> Maybe a
    findRange [] _ _ = Nothing
    findRange ((src, dest, prob):xs) r low
      | low <= r && r < low + prob = Just dest
      | otherwise                  = findRange xs r (low + prob)

generate :: Eq a => Int -> TransTable a -> a -> IO [a]
generate 0 _ _ = return []
generate n table food = do next <- update table food
                           rest <- case next of
                             Just st -> generate (n-1) table st
                             Nothing -> pure []
                           return (food : rest)

{- A simple example: there is a creature who

   1. Eats only once per day
   2. Will only eat cheese, grapes, or lettuce
   3. If it eats cheese today, it will eat grapes or lettuce tomorrow with equal
      probability
   4. If it eats grapes today, it will eat grapes tomorrow with probability 1/10,
      it will eat cheese tomorrow with probability 4/10, and it will eat lettuce
      tomorrow with probability 5/10
   5. If it eats lettuce today, then tomorrow it will eat grapes with probability
      4/10, and it will eat cheese with probability 6/10 -}

data Food = Cheese | Grapes | Lettuce deriving (Eq, Show)

transitions :: TransTable Food
transitions =
  [ (Cheese, Lettuce, 1/2), (Cheese, Grapes, 1/2)
  , (Grapes, Grapes, 1/10), (Grapes, Cheese, 4/10), (Grapes, Lettuce, 5/10)
  , (Lettuce, Grapes, 4/10), (Lettuce, Cheese, 6/10)
  ]

-- Run a bunch of iterations.

mealPlan :: IO [Food]
mealPlan = generate 1000 transitions Cheese
