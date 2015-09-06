module Solver where

import qualified Data.Map.Strict as Map
import Data.List
import Safe.Foldable (findJust)
import Data.Maybe (fromJust , fromMaybe)
import Control.Monad (guard)

import Mastermind

-- utility functions
succS :: (Enum a, Bounded a, Eq a) => a -> a
succS e
  | e == maxBound = e
  | otherwise     = succ e

untilFix :: Eq a => (a -> a) -> a -> a
untilFix f a =
  if a == f a
     then a
     else untilFix f (f a)

mapSnd :: (a -> b) -> (c,a) -> (c,b)
mapSnd f (a,b) = (a, f b)

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- Solver
type Possibility  = Map.Map Peg [Int]
type Possibility' = [(Peg,[Int])]
type Knowledge    = [Possibility]

data RespType = BlackR | WhiteR
     deriving (Show)

mkRespType :: Response -> [RespType]
mkRespType (b,w) = replicate b BlackR ++ replicate w WhiteR

generateGuess :: Possibility -> Peg -> Code
generateGuess pos c = findJust goodGuess . permutations . take 4 $ Map.keys pos ++ repeat c where
  goodGuess :: Code -> Bool
  goodGuess = all (\(i, peg) -> maybe True (i `elem`) $ Map.lookup peg pos) . zip [0..]

compactCode :: Code -> Possibility'
compactCode c = [ (p, findIndices (== p) c) | p <- nub c ]

uniformRespType :: RespType -> [Int] -> [Int]
uniformRespType BlackR = id
uniformRespType WhiteR = ([0..3] \\)

mkKnowledge :: Code -> Response -> Knowledge
mkKnowledge guess resp =
  let guess' = compactCode guess
      resp'  = mkRespType resp
  in nub $ do
     gp <- permutations $ guess'
     return . Map.fromList $ do
       ((p,ls), rt) <- zip gp resp'
       return (p, uniformRespType rt ls)

pegPositionConflict :: (Peg,[Int]) -> (Peg,[Int]) -> Bool
pegPositionConflict (p1,ls1) (p2,ls2) =
  if p1 /= p2
    then case (ls1,ls2) of
         ([l1],[l2]) | l1 == l2 -> True
         _                      -> False
    else False

possibilityPositionConflict :: Possibility -> Possibility -> Bool
possibilityPositionConflict pos1 pos2 =
  Map.foldlWithKey (\b  p  ls  ->
  Map.foldlWithKey (\b' p' ls' ->
    b' || pegPositionConflict (p,ls) (p',ls'))
  b pos2) False pos1

internalContradiction :: Possibility -> Bool
internalContradiction pos =
  or [ possibilityPositionConflict pos pos , presentEmpty] where

    -- a peg is present but has no possible locations
    presentEmpty = Map.foldl (\b ls -> b || null ls) False pos

-- does the second possibility contradict the first?
contradictedBy :: Possibility -> Possibility -> Bool
pos1 `contradictedBy` pos2 = or [positionConflict , noOverlap] where
  -- contradiction because kw1 fully knows where one peg is but
  -- kw2 fully knows another peg, at the same location!
  positionConflict = possibilityPositionConflict pos1 pos2

  -- True if either pos1 has a peg `p` that pos2 doesn't mention, or
  -- if there's no overlap between the possible locations for p
  noOverlap =
    Map.foldlWithKey
      ( \b p ls -> (b ||) . fromMaybe True $ do
          ls' <- Map.lookup p pos2
          return . null . intersect ls $ ls'
      ) False pos1

newKnowledge :: Knowledge -> Possibility -> Knowledge
newKnowledge kw pos =
  let kwPruned = filter (not . fork internalContradiction (||) (pos `contradictedBy`)) $ kw
  in map (Map.map sort . Map.unionWith intersect pos) kwPruned
