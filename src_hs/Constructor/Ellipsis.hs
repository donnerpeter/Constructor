module Constructor.Ellipsis (ClauseEllipsis(..), suggestDoubleAnchorEllipsis, suggestSingleAnchorEllipsis) where
import Constructor.Tree
import Constructor.Util
import Constructor.Mite
import Constructor.Variable
import Constructor.Sense (Sense, Frame, toFrame, framesTo, var, value, allFrameFacts)
import Constructor.Inference
import Constructor.InteractionEnv
import Data.Maybe
import Data.List (find)
import qualified Data.Set as Set
import qualified Constructor.SemanticProperties as P

enumerateActiveMites tree includeSelf = let
  own = if includeSelf then map (\m -> (m, tree)) (activeHeadMites tree) else []
  prefix = if isBranch tree then enumerateActiveMites (justRight tree) (headSide tree == LeftSide) else []
  suffix = if isBranch tree then enumerateActiveMites (justLeft tree)  (headSide tree == RightSide)  else []
  in prefix ++ own ++ suffix

findClause tree = [(v, t) | (cxt -> Clause v, t) <- enumerateActiveMites tree True]

data ClauseEllipsis = ClauseEllipsis { elidedVerb :: Variable, semantics :: [Mite] }

suggestDoubleAnchorEllipsis env ellipsisVar e1 e2 = case findClause =<< context env of
  [] -> []
  (oldClause, tree):_ -> let
    allMites = Set.elems $ allActiveMiteSet tree
    sideTrees = subTrees LeftSide tree ++ subTrees RightSide tree
    findSideTree mite = find (\t -> Set.member mite $ allActiveMiteSet t) sideTrees
    mappings = catMaybes [semanticEllipsis oldClause ellipsisVar [mapping1, mapping2] tree |
       mapping1 <- findOriginals allMites oldClause ellipsisVar e1,
       mapping2 <- findOriginals allMites oldClause ellipsisVar e2,
       findSideTree (templateMite mapping1) /= findSideTree (templateMite mapping2)]
    in mappings

suggestSingleAnchorEllipsis env ellipsisVar anchor = case findClause =<< context env of
  [] -> []
  (oldClause, tree):_ -> let
    allMites = Set.elems $ allActiveMiteSet tree
    mappings = catMaybes [semanticEllipsis oldClause ellipsisVar [mapping] tree | mapping <- findOriginals allMites oldClause ellipsisVar anchor]
    in mappings

data AnchorMapping = AnchorMapping { templateMite:: Mite, templateVar:: Variable, anchorVar:: Variable, anchorMites :: [Mite] } deriving (Show)

checkOriginal ::  Construction -> Variable -> Variable -> Mite -> Maybe AnchorMapping
checkOriginal anchor oldClause ellipsisVar candidate = case (cxt candidate, anchor) of
  (VerbalModifier a1 _ v1, VerbalModifier a2 _ v2) | a1 == a2 -> Just $ AnchorMapping candidate v1 v2 []
  (Argument kind1 v1, Argument kind2 v2) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 v2 []
  (SemArgument kind1 _ _, SemArgument kind2 attr2 v2) | kind1 == kind2 -> Just $ AnchorMapping candidate oldClause ellipsisVar [semV ellipsisVar attr2 v2]
  (Adj v1 attr1 kind1 _, Adj v2 attr2 kind2 _) | kind1 == kind2 && attr1 == attr2 -> Just $ AnchorMapping candidate v1 v2 []
  _ -> Nothing

findOriginals mites oldClause ellipsisVar anchor = catMaybes $ map (checkOriginal anchor oldClause ellipsisVar) mites

mapVar :: [AnchorMapping] -> Variable -> Variable -> (Variable -> Variable)
mapVar mappings originalCP ellipsisVar@(Variable varIndex _) _v = case find (\m -> _v == templateVar m) mappings of
  Just m -> anchorVar m
  _ -> if _v == originalCP then ellipsisVar else Variable varIndex ("_" ++ show _v)

semanticEllipsis :: Variable -> Variable -> [AnchorMapping] -> Tree -> Maybe ClauseEllipsis
semanticEllipsis _oldCP ellipsisVar mappings prevTree = let
  oldCP = normalizeVar _oldCP
  mapVariable = mapVar (map (\m -> m { templateVar = normalizeVar $ templateVar m }) mappings) oldCP ellipsisVar
  normalizeVar v = var $ toFrame sens v
  verbs = [mapVariable v | (cxt -> Verb v) <- activeHeadMites prevTree]
  singleVerb = if length verbs == 1 then head verbs else error $ "Non-single verb: " ++ show verbs ++ " in " ++ show (activeHeadMites prevTree)
  sens = sense prevTree
  unifyVerb am = if normalizeVar (templateVar am) == oldCP then [mite $ Unify (anchorVar am) ellipsisVar] else []
  in do
    toCopy <- framesToCopy prevTree oldCP mappings
    let copyMites = copySkeleton sens toCopy mapVariable $ ellipsisVar:(map anchorVar mappings)
    return $ ClauseEllipsis singleVerb $ copyMites ++ (mappings >>= unifyVerb) ++ (mappings >>= anchorMites)

copySkeleton :: Sense -> Set.Set Frame -> (Variable -> Variable) -> [Variable] -> [Mite]
copySkeleton sense originalFrames mapper anchorVars = Set.elems originalFrames >>= copyFrame where
  copyFrame frame = let
    v0 = mapper $ var frame
    copyFact = \case
      StrValue {} -> []
      VarValue attr val ->
        if Set.member (toFrame sense val) originalFrames || (mapper val) `elem` anchorVars
        then [semV v0 attr (mapper val)]
        else []
    in [semS v0 P.Elided "true", semV v0 P.EllipsisOriginal (var frame)] ++ (allFrameFacts frame >>= copyFact . value)

framesToCopy :: Tree -> Variable -> [AnchorMapping] -> Maybe (Set.Set Frame)
framesToCopy tree originalCP mappings = result where
  srcFrame = toFrame (sense tree) originalCP
  dsts = map (toFrame (sense tree) . templateVar) mappings
  paths = map (\dst -> framesTo srcFrame dst 3) dsts
  candidates = Set.unions $ catMaybes paths
  foreignCPs = [frame | frame <- Set.elems candidates, isCP frame && frame /= srcFrame]
  result = if all isJust paths && null foreignCPs then Just candidates else Nothing