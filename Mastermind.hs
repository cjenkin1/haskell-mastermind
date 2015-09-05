module Main where

import Data.List
import Data.Maybe (listToMaybe, isJust, fromMaybe, fromJust , catMaybes)
import System.IO (hFlush, stdout)
import Control.Monad.Random
import System.Random.Shuffle
import qualified Data.Map as Map
import Control.Monad
import System.IO

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
succS :: (Enum a, Bounded a, Eq a) => a -> a
succS e
  | e == maxBound = e
  | otherwise     = succ e

untilFix :: Eq a => (a -> a) -> a -> a
untilFix f a =
  if a == f a
     then a
     else untilFix f (f a)

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

-- main = do
--      print allPegs
--      code <- genCodeIO
--      -- print code
--      go code 0  where

--      go code tries = do
--         guess <- getGuessRetry
--         case (checkGuess code guess) of
--              (4, 0) -> putStrLn $ "You won in " ++ (show tries) ++ " tries!"
--              resp   -> putStrLn (show resp) >> go code (tries+1)

-- Solver
type Possibility = Map.Map Peg [Int]
type Knowledge   = [Possibility]

data RespType = BlackR | WhiteR
     deriving (Show)

mkRespType :: Response -> [RespType]
mkRespType (b,w) = replicate b BlackR ++ replicate w WhiteR

generateGuess :: Possibility -> Peg -> Code
generateGuess kns c =
  let codeInit          = replicate 4 Nothing :: [Maybe Peg]
      codeWithPossibility = Map.foldlWithKey updateFromPossibility codeInit kns
  in map (fromMaybe c) codeWithPossibility where

    updateFromPossibility ::  [Maybe Peg] -> Peg -> [Int] -> [Maybe Peg]
    updateFromPossibility code p [] = code
    updateFromPossibility code p (l:ls) =
        if isJust (code !! l)
           then updateFromPossibility code p ls
           else replaceAtIndex l (Just p) code

compactCode :: Code -> Possibility
compactCode c = Map.fromList $ map (\p -> (p, findIndices (== p) c)) (nub c)

uniformRespType :: RespType -> [Int] -> [Int]
uniformRespType BlackR = id
uniformRespType WhiteR = ([0..3] \\)

mkKnowledge :: Code -> Response -> Knowledge
mkKnowledge guess resp =
  let guessPerms = permutations . Map.toList . compactCode $ guess
      resp'      = mkRespType resp
      guessPermsUniform :: [[(Peg, [Int])]]
      guessPermsUniform =
        map (zipWith (\rt -> mapSnd (uniformRespType rt)) resp') guessPerms
  in map Map.fromList (nub . map sort $ guessPermsUniform)

pegPositionConflict :: (Peg,[Int]) -> (Peg,[Int]) -> Bool
pegPositionConflict (p1,ls1) (p2,ls2) = p1 /= p2 && map2' length (ls1, ls2) == (1,1) && head ls1 == head ls2

possibilityPositionConflict :: Possibility -> Possibility -> Bool
possibilityPositionConflict pos1 pos2 =
  Map.foldlWithKey (\b  p  ls  ->
  Map.foldlWithKey (\b' p' ls' ->
    b' || pegPositionConflict (p,ls) (p',ls'))
  b pos2) False pos1

internalContradiction :: Possibility -> Bool
internalContradiction pos =
  or [ possibilityPositionConflict `refApply` pos , presentEmpty] where

    -- a peg is present but has no possible locations
    presentEmpty = Map.foldl (\b ls -> b || null ls) False pos

-- does the second possibility contradict the first?
contradictedBy :: Possibility -> Possibility -> Bool
pos1 `contradictedBy` pos2 = or [notPresentIn2 , positionConflict , noOverlap] where
  -- contradiction because the second does not mention
  -- a peice that exists in the first
  notPresentIn2 = Map.foldlWithKey (\b p _ ->  b || p `Map.notMember` pos2) False pos1

  -- contradiction because kw1 fully knows where one peg is but
  -- kw2 fully knows another peg, at the same location!
  positionConflict = possibilityPositionConflict pos1 pos2

  noOverlap =
    Map.foldlWithKey
      (\b p ls -> b ||
        maybe True (null . intersect ls) (Map.lookup p pos2)) False pos1


newKnowledge :: Knowledge -> Possibility -> Knowledge
newKnowledge kw pos =
  let kwPruned = filter (not . \pos' -> internalContradiction pos' || pos `contradictedBy` pos') . map (untilFix handleCertainPegs) $ kw
  in map (Map.map sort . (untilFix handleCertainPegs) . Map.unionWith intersect pos) kwPruned where

    -- if we have pegs whose positions are known, remove
    -- that position from the possible positions of other pegs
    handleCertainPegs :: Possibility -> Possibility
    handleCertainPegs pos' =
      let certains = Map.filter ((== 1) . length) pos'
      in Map.mapWithKey
           (\p ls ->
             Map.foldlWithKey
               (\accum p' ls' -> if p' == p
                                    then accum
                                    else accum \\ ls') ls certains) pos'

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
