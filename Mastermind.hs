module Mastermind where

import Data.List
import Data.Maybe (listToMaybe)
import Control.Monad.Random (getStdGen , RandomGen)
import System.Random.Shuffle (shuffle')

-- utility functions
map2 f g (a,b) = (f a, g b)
map2' f = map2 f f
fork g h f a = (g a) `h` (f a)
fork2 g h f a b = (g a b) `h` (f a b)
fork2' g h f a b = (g a) `h` (f b)
arg1 a b = a
arg2 a b = b
(...) f g a b = f (g a b)
for = flip map
mapSnd f (a,b) = (a, f b)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- Pegs
data Peg = White | Black | Yellow | Orange | Red | Blue | Green | Brown
     deriving (Eq, Enum, Bounded, Ord, Show, Read)

type Code = [Peg]
type Response = (Int,Int)

allPegs :: [Peg]
allPegs = enumFrom minBound

-- Mastermind logic
checkGuess :: Code -> Code -> Response
checkGuess code guess =
           let (black, notBlack) = map2' (map snd) . partition (uncurry (==)) $ zip code guess
               white = filter (fork (`elem` code) (&&) (`notElem` black)) (nub notBlack)
           in map2' length (black, white)

genCode :: RandomGen gen => gen -> Code
genCode g = take 4 . shuffle' allPegs (length allPegs) $ g

-- IO
genCodeIO :: IO Code
genCodeIO = do
          g <- getStdGen
          return . genCode $ g
