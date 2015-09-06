module Main where

import System.IO (hFlush, stdout)
import Data.Maybe (listToMaybe)

import Mastermind

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

getGuessRetry :: IO Code
getGuessRetry = do
    putStr "Guess> "; hFlush stdout
    inputStr <- getLine
    case (maybeRead inputStr) of
         Nothing                    -> putStrLn "Invalid guess!"         >> getGuessRetry
         (Just c) | (length c) /= 4 -> putStrLn "Guesses must be 4 pegs" >> getGuessRetry
                  | otherwise       ->                                      return c

main = do
     print allPegs
     code <- genCodeIO
     -- print code
     go code 0  where

     go code tries = do
        guess <- getGuessRetry
        case (checkGuess code guess) of
             (4, 0) -> putStrLn $ "You won in " ++ (show tries) ++ " tries!"
             resp   -> putStrLn (show resp) >> go code (tries+1)

