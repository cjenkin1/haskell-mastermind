module Main where

import Data.List
import Data.Maybe (listToMaybe, isJust, fromMaybe)
import System.IO (hFlush, stdout)
import Control.Monad.Random
import System.Random.Shuffle
import qualified Data.Map as Map
import Control.Monad

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
f `refApply` a = f a a

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

-- Solver
type Knowledge   = Map.Map Peg [Int]
type GuessInfo   = [Knowledge]

data RespType = BlackR | WhiteR
     deriving (Show)

mkRespType :: Response -> [RespType]
mkRespType (b,w) = replicate b BlackR ++ replicate w WhiteR

generateGuess :: Knowledge -> Peg -> Code
generateGuess kns c =
  let codeInit          = replicate 4 Nothing :: [Maybe Peg]
      codeWithKnowledge = Map.foldlWithKey updateFromKnowledge codeInit kns
  in map (fromMaybe c) codeWithKnowledge where

    updateFromKnowledge ::  [Maybe Peg] -> Peg -> [Int] -> [Maybe Peg]
    updateFromKnowledge code p [] = code
    updateFromKnowledge code p (l:ls) =
        if isJust (code !! l)
           then updateFromKnowledge code p ls
           else replaceAtIndex l (Just p) code

compactCode :: Code -> Map.Map Peg [Int]
compactCode c = Map.fromList $ map (\p -> (p, findIndices (== p) c)) (nub c)

uniformRespType :: RespType -> [Int] -> [Int]
uniformRespType BlackR = id
uniformRespType WhiteR = ([0..3] \\)

mkGuessInfo :: Code -> Response -> GuessInfo
mkGuessInfo guess resp =
  let guessPerms = permutations . Map.toList . compactCode $ guess
      resp'      = mkRespType resp
      guessPermsUniform :: [[(Peg, [Int])]]
      guessPermsUniform =
        map (zipWith (\rt -> mapSnd (uniformRespType rt)) resp') guessPerms
  in map Map.fromList (nub . map sort $ guessPermsUniform)

pegPositionConflict :: (Peg,[Int]) -> (Peg,[Int]) -> Bool
pegPositionConflict (p1,ls1) (p2,ls2) = p1 /= p2 && map2' length (ls1, ls2) == (1,1) && head ls1 == head ls2

knowledgePositioinConflict :: Knowledge -> Knowledge -> Bool
knowledgePositioinConflict kw1 kw2 =
  Map.foldlWithKey (\b  p  ls  ->
  Map.foldlWithKey (\b' p' ls' ->
    b' || pegPositionConflict (p,ls) (p',ls'))
  b kw2) False kw1

internalContradiction :: Knowledge -> Bool
internalContradiction ks =
  or [ knowledgePositioinConflict `refApply` ks ] where

-- does the second knowledge contradict the first?
contradictedBy :: Knowledge -> Knowledge -> Bool
kw1 `contradictedBy` kw2 = or [notPresentIn2 , positionConflict , noOverlap] where
  -- contradiction because the second does not mention
  -- a peice that exists in the first
  notPresentIn2 = Map.foldlWithKey (\b p _ ->  b || p `Map.notMember` kw2) False kw1

  -- contradiction because kw1 fully knows where one peg is but
  -- kw2 fully knows another peg, at the same location!
  positionConflict = knowledgePositioinConflict kw1 kw2

  noOverlap =
    Map.foldlWithKey
      (\b p ls -> b ||
        maybe True (null . intersect ls) (Map.lookup p kw2)) False kw1

newKnowledge :: Knowledge -> GuessInfo -> Knowledge
newKnowledge kw gi =
  let giPruned = filter (not . \kw' -> internalContradiction kw' || kw `contradictedBy` kw') gi
  in Map.map sort $ Map.unionWith intersect kw (Map.unionsWith union giPruned)


debugRun :: IO ()
debugRun = undefined
