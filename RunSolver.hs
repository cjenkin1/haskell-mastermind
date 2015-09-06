module Main where

import Control.Monad (unless)
import qualified Data.Map.Strict as Map
import System.IO

import Mastermind
import Solver

data Mastermind = Mastermind
  { current   :: Peg
  , knowledge :: Knowledge
  , tries     :: Int
  , code      :: Code
  } deriving Show

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  cmd <- putStr "[new] or [set] code: " >> getLine
  cd <- case cmd of
    "set" -> readLn
    _     -> genCodeIO
  go $ Mastermind {code = cd, current = minBound, knowledge = [Map.empty], tries = 1}
  return () where

  go :: Mastermind -> IO ()
  go ms = do
    print ms
    let Mastermind {current = c, knowledge = kw, code = cd} = ms
    let pos = head kw
    let guess = generateGuess pos c
    putStr "Guess: " >> print guess >> getLine

    let resp = checkGuess cd guess
    putStr "Response: " >> print resp >> getLine
    unless (resp == (4 , 0)) $ do
      let kwg = mkKnowledge guess resp
      putStr "Knowledge from guess: " >> print kwg >> getLine

      let kwn = concatMap (newKnowledge kwg) kw
      putStr "New Knowledge: " >> print kwn >> getLine

      go ms {tries = tries ms + 1, knowledge = kwn, current = succS c}
