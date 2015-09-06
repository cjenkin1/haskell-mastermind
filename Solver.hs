module Solver where

import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe (catMaybes, fromJust, fromMaybe)

import Mastermind

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

-- Solver
type Possibility = Map.Map Peg [Int]
type Knowledge   = [Possibility]

data RespType = BlackR | WhiteR
     deriving (Show)

mkRespType :: Response -> [RespType]
mkRespType (b,w) = replicate b BlackR ++ replicate w WhiteR

generateGuess :: Possibility -> Peg -> Code
generateGuess pos c =
  let codeInit          = replicate 4 Nothing :: [Maybe Peg]
  in fillWith c . fromJust . find goodGuess . allGuesses $ codeInit where

    fillWith c' = map (fromMaybe c')

    goodGuess :: [Maybe Peg] -> Bool
    goodGuess mCd = length (catMaybes mCd) == length (Map.keys pos)

    -- I'm sorry for writing this...
    allGuesses :: [Maybe Peg] -> [[Maybe Peg]]
    allGuesses ci =
      Map.foldlWithKey (\accum p ls ->
        concatMap (\guess -> map (\l -> replaceAtIndex l (Just p) guess) ls) accum
      ) [ci] pos

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
