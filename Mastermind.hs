module Mastermind where

import Data.List
import Control.Monad.Random (getStdGen , RandomGen)
import System.Random.Shuffle (shuffle')

-- utility functions
map2 :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
map2 f g (a,b) = (f a, g b)

map2' :: (a -> b) -> (a,a) -> (b,b)
map2' f = map2 f f

fork :: (a -> b) -> (b -> c -> d) -> (a -> c) -> (a -> d)
fork g h f a = (g a) `h` (f a)

-- APL inspired partition
partitionAPL :: [Bool] -> [a] -> ([a],[a])
partitionAPL cs xs = map2' (map snd) . partition fst $ zip cs xs

-- Pegs
data Peg = White | Black | Yellow | Orange | Red | Blue | Green | Brown
     deriving (Eq, Enum, Bounded, Ord, Show, Read)

type Code = [Peg]
type Response = (Int,Int)

allPegs :: [Peg]
allPegs = enumFrom minBound

-- Mastermind logic
checkGuess :: Code -> Code -> Response
checkGuess code guess = (length black , length white) where

  -- partition pegs in the guess by those which where the right color *and* the right position
  blackAnalysis :: ([Peg] , [Peg])
  blackAnalysis = partitionAPL (zipWith (==) code guess) guess

  black = fst blackAnalysis

  -- for the rest, find out which are the 'white' pegs
  -- (those pegs in the code but not in the right position)
  notBlack = nub (snd blackAnalysis)

  white = filter (fork (`elem` code) (&&) (`notElem` black)) notBlack

genCode :: RandomGen gen => gen -> Code
genCode g = take 4 . shuffle' allPegs (length allPegs) $ g

-- IO
genCodeIO :: IO Code
genCodeIO = do
          g <- getStdGen
          return . genCode $ g
