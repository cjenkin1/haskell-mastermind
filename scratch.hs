module Main where

import Data.List
import Data.Maybe (listToMaybe)
import System.IO (hFlush, stdout)
import Control.Monad.Random
import System.Random.Shuffle

-- utility functions
map2 f g (a,b) = (f a, g b)
map2' f = map2 f f
fork g h f a = (g a) `h` (f a)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

-- Pegs
data Peg = White | Black | Yellow | Orange | Red | Blue | Green | Brown
     deriving (Eq, Enum, Bounded, Show, Read)

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

getGuessRetry :: IO Code
getGuessRetry = do
    putStr "Guess> "; hFlush stdout
    inputStr <- getLine
    case (maybeRead inputStr) of
         Nothing                    -> putStrLn "Invalid guess!" >> getGuessRetry
         (Just c) | (length c) /= 4 -> putStrLn "Guesses must be 4 pegs" >> getGuessRetry
                  | otherwise       -> return c

main = do
     print allPegs
     code <- genCodeIO
     -- print code
     go code 0

     where
     go code tries = do
        guess <- getGuessRetry
        case (checkGuess code guess) of
             (4, 0) -> putStrLn $ "You won in " ++ (show tries) ++ " tries!"
             resp   -> putStrLn (show resp) >> go code (tries+1)
