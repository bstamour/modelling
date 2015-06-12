{-# LANGUAGE ScopedTypeVariables #-}

module Markov where

import System.Random
import Control.Monad

{- Implementation of a basic markov chain. -}

type TransTable a = [(a, a, Double)]
type TransRange a = (Double, Double, a)

-- Convert a transition table into a list of transition ranges for which we can sample from.
makeRanges :: Eq a => TransTable a -> a -> [TransRange a]
makeRanges table item = rangify movements 0.0
  where
    movements = filter (\(src, dst, prob) -> src == item) table

    rangify [] _                      = []
    rangify ((src, dst, prob):xs) low = (low, low + prob, dst) : rangify xs (low + prob)

-- Randomly pick a transition and return the next state to move to.
whichTrans :: forall a. Eq a => TransTable a -> a -> IO (Maybe a)
whichTrans table item = do let ranges = makeRanges table item
                           r <- getStdRandom random
                           return $ findRange ranges r
  where
    findRange :: [TransRange a] -> Double -> Maybe a
    findRange [] _             = Nothing
    findRange [(_, _, dest)] _ = Just dest
    findRange ((low, high, dest):xs) r | low <= r && r < high = Just dest
                                       | otherwise            = findRange xs r

{- A simple example: there is a creature who

   1. Eats only once per day
   2. Will only eat cheese, grapes, or lettuce
   3. If it eats cheese today, it will eat grapes or lettuce tomorrow with equal probability
   4. If it eats grapes today, it will eat grapes tomorrow with probability 1/10, it will eat
      cheese tomorrow with probability 4/10, and it will eat lettuce tomorrow with probability
      5/10
   5. If it eats lettuce today, then tomorrow it will eat grapes with probability 4/10, and it
      will eat cheese with probability 6/10 -}

data Food = Cheese | Grapes | Lettuce deriving (Eq, Show)

transitions :: TransTable Food
transitions = [ (Cheese, Lettuce, 1/2), (Cheese, Grapes, 1/2)
              , (Grapes, Grapes, 1/10), (Grapes, Cheese, 4/10), (Grapes, Lettuce, 5/10)
              , (Lettuce, Grapes, 4/10), (Lettuce, Cheese, 6/10)
              ]

generate :: Int -> Food -> IO [Food]
generate 0 food = return []
generate n food = do (Just next) <- whichTrans transitions food
                     rest <- generate (n-1) next
                     return (food:rest)
