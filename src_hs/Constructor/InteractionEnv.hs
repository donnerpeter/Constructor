module Constructor.InteractionEnv where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Tree
import Constructor.Util
import qualified Constructor.LinkedSet as LS

data MergeInfo = MergeInfo {mergeResult::[Mite], mergedHeadSide::Side} deriving (Show,Eq,Ord)
mergeLeft mites = [MergeInfo mites LeftSide]
mergeRight mites = [MergeInfo mites RightSide]

data InteractionEnv = InteractionEnv { context:: [Tree], leftTree::Tree, leftCombined::[Mite], rightCombined::[Mite], rightSets::[[Mite]], pairs::[(Mite, Mite)]}
interactionEnv context leftTree rightTree = let
  lc = _leftCombined  $ interactiveMites leftTree
  rc = _rightCombined $ interactiveMites rightTree
  in InteractionEnv {
      context = context,
      leftTree = leftTree,
      leftCombined  = lc,
      rightCombined = rc, rightSets = _rightSets $ interactiveMites rightTree,
      pairs = [(m1, m2) | m1 <- lc, m2 <- rc]
     }

leftCompatible  env m = LS.removeDups $ concat $ filter (\set -> m `elem` set) $ _leftSets  $ interactiveMites $ leftTree env
rightCompatible env m = LS.removeDups $ concat $ filter (\set -> m `elem` set) $ rightSets env

