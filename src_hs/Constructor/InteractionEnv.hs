module Constructor.InteractionEnv where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Tree
import Constructor.Util
import qualified Constructor.LinkedSet as LS

data MergeInfo = MergeInfo {mergeResult::[Mite], mergedHeadSide::Side} deriving (Show,Eq,Ord)
mergeLeft mites = [MergeInfo mites LeftSide]
mergeRight mites = [MergeInfo mites RightSide]

data InteractionEnv = InteractionEnv {leftTree::Tree, leftCombined::[Mite], rightCombined::[Mite], leftSets::[[Mite]], rightSets::[[Mite]]}
interactionEnv leftTree rightSets rightCombined = let
  leftSets = map activeHeadMites $ filter (null . _unhappyRight . _unhappy) $ allVariants leftTree
  in InteractionEnv {
      leftTree = leftTree, leftCombined = LS.removeDups $ filter isInteractive $ concat leftSets, leftSets = leftSets,
      rightSets = rightSets, rightCombined = filter isInteractive rightCombined
     }

leftCompatible  env m = LS.removeDups $ concat $ filter (\set -> m `elem` set) $ leftSets  env
rightCompatible env m = LS.removeDups $ concat $ filter (\set -> m `elem` set) $ rightSets env
pairs env = [(m1, m2) | m1 <- leftCombined env, isInteractive m1, m2 <- rightCombined env, isInteractive m2]

isInteractive mite = case cxt mite of
  Sem {} -> False
  Unify {} -> False
  EmptyCxt {} -> False
  Diversifier {} -> False
  _ -> True
