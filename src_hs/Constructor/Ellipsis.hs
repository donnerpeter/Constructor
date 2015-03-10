module Constructor.Ellipsis (suggestEllipsis) where
import Constructor.Tree
import Constructor.Util
import Constructor.Mite
import Constructor.Variable
import Constructor.InteractionEnv
import Constructor.Constructions
import Data.Maybe
import Data.List (find)
import qualified Constructor.SemanticProperties as P

enumerateActiveMites tree includeSelf = let
  own = if includeSelf then map (\m -> (m, tree)) (activeHeadMites tree) else []
  prefix = if isBranch tree then enumerateActiveMites (justRight tree) (headSide tree == LeftSide) else []
  suffix = if isBranch tree then enumerateActiveMites (justLeft tree)  (headSide tree == RightSide)  else []
  in prefix ++ own ++ suffix

findClause tree = [(m, t) | (m@(cxt -> Clause _), t) <- enumerateActiveMites tree True]

suggestEllipsis env ellipsisVar e1 e2 = case findClause =<< context env of
  [] -> []
  (oldClause, tree):_ -> let
    allMites = allTreeMites tree
    mappings = catMaybes [processEllipsis oldClause ellipsisVar [mapping1, mapping2] tree |
       mapping1 <- findOriginals allMites e1,
       mapping2 <- findOriginals allMites e2]
    in xor mappings

data AnchorMapping = AnchorMapping { templateMite:: Mite, templateVar:: Variable, anchorVar:: Variable } deriving (Show)

checkOriginal ::  Construction -> Mite -> Maybe AnchorMapping
checkOriginal anchor candidate = case (cxt candidate, anchor) of
  (VerbalModifier a1 _ v1, VerbalModifier a2 _ v2) | a1 == a2 -> Just $ AnchorMapping candidate v1 v2
  (Argument kind1 v1, Argument kind2 v2) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 v2
  (SemArgument kind1 v1 _, SemArgument kind2 v2 _) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 v2
  _ -> Nothing

findOriginals mites anchor = catMaybes $ map (checkOriginal anchor) mites

processEllipsis :: Mite -> Variable -> [AnchorMapping] -> Tree -> Maybe [Mite]
processEllipsis oldClause ellipsisVar@(Variable varIndex _) mappings prevTree = let
  Clause oldCP = cxt oldClause
  templateCxts = map (cxt . templateMite) mappings
  mapVariable _v = case find (\m -> _v == templateVar m) mappings of
    Just m -> anchorVar m
    _ -> if _v == oldCP then ellipsisVar else Variable varIndex ("_" ++ show _v)
  mapMite m = case cxt m of
    Unify _v1 _v2 -> [mite $ Unify (mapVariable _v1) (mapVariable _v2)]
    Sem _v1 (StrValue attr s) ->
      if attr == P.RusNumber || attr == P.RusGender || attr == P.RusPerson then []
      else [semS (mapVariable _v1) attr s] ++ (if attr == P.Type then [semS (mapVariable _v1) P.Elided "true"] else [])
    Sem _v1 (VarValue attr _v2)  -> [mite $ Sem (mapVariable _v1) (VarValue attr $ mapVariable _v2)]
    _ -> []
  walkTree :: Tree -> ([Mite], [Construction])
  walkTree tree = let
    activeHeadMites = Constructor.Tree.activeHeadMites tree
    headCxts = map cxt activeHeadMites
    ownMapped = filter (not . contradict oldClause) activeHeadMites >>= mapMite
    folder (allMapped, allCovered) tree = let (mapped, covered) = walkTree tree in (allMapped++mapped, allCovered++covered)
    (leftMapped, leftCovered) = foldl folder ([], []) $ subTrees LeftSide tree
    (rightMapped, rightCovered) = foldl folder ([], []) $ subTrees RightSide tree
    in case find (`elem` headCxts) templateCxts of
      Just original -> ([], [original])
      _ ->
        if containsClause activeHeadMites then ([], [])
        else (leftMapped++ownMapped++rightMapped, leftCovered++rightCovered)
  (mapped, covered) = walkTree prevTree
  containsClause mites = not $ null [m | m@(cxt -> Clause cp) <- mites, oldCP /=cp]
  in
  if covered == templateCxts then Just mapped else Nothing
