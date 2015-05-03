module Mastermind where

import Data.List

data Peg = White | Black | Yellow | Orange | Red | Blue | Green | Brown
     deriving (Show, Eq)

type Code = [Peg]
type Response = (Int,Int)

-- utility functions
map2 f g (a,b) = (f a, g b)
map2' f = map2 f f

code = [White, Black, Yellow, Orange]
guess = [White, Yellow, Orange, Red]

checkGuess :: Code -> Code -> Response
checkGuess code guess = 
           let (black, notBlack) = map2' (map snd) . partition (uncurry (==)) $ zip code guess
               white = filter (`elem` code) notBlack
           in map2' length (black, white)
