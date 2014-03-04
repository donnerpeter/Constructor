module Constructor.Seq (seqLeft, seqRight) where
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import Constructor.Constructions
import Constructor.Agreement
import Constructor.Variable
import Constructor.Tree
import Constructor.Util
import Control.Monad
import Debug.Trace
import Data.Maybe

seqRight leftMites rightMites = result where
  hasSeqFull conj = flip any rightMites $ \mite -> case cxt mite of SeqFull _ c | isSeqContinuation conj c -> True; _ -> False
  result = leftMites >>= \m1 -> case cxt m1 of
    Conjunction v conj True -> if hasSeqFull conj then [] else rightMites >>= \m2 -> case cxt m2 of
      Argument kind child -> withBase [m1,m2] [semV v "member2" child, mite $ SeqRight v kind conj]
      Possessive caze agr child -> withBase [m1,m2] [semV v "member2" child, mite $ SeqRight v (PossKind caze agr) conj]
      Complement child -> withBase [m1,m2] [semV v "member2" child, mite $ SeqRight v CP conj]
      Clause force child ->
        let unhappy = filter elideable $ filter (not . happy) rightMites
            wrapped = [(mite $ ElidedArgHead $ cxt m) {baseMites = [m,m1,m2]} | m <- unhappy]
            elideable mite = case cxt mite of
              NomHead {} -> True
              _ -> False
            ellipsis = rightMites >>= \e -> case cxt e of
              Ellipsis child (Just _) (Just _) -> withBase [e] [mite $ cxt e] 
              _ -> []
            result = withBase [m1, m2] [semV v "member2" child, mite $ SeqRight v (ClauseArg Declarative) conj] ++ ellipsis ++ wrapped
        in
          result
      _ -> []
    _ -> []      

isSeqContinuation conj1 conj2 = conj1 == ","

seqLeft leftTree leftMites rightMites = concat [interactSeqLeft m1 m2 | m1 <- leftMites, m2 <- rightMites] where
  contradictsSeq conj = flip any leftMites $ \mite -> case cxt mite of SeqFull _ c | not $ isSeqContinuation c conj -> True; _ -> False
  interactSeqLeft m1 m2 = case (cxt m1, cxt m2) of
    (Argument kind child, SeqRight v kind2 conj) | kind == kind2 ->
      if contradictsSeq conj then [] else
        withBase [m1,m2] [semV v "member1" child, mite $ SeqFull v conj, mite $ Argument kind v]
    (Possessive caze1 agr1 child, SeqRight v (PossKind caze2 agr2) conj) | caze1 == caze2 && agree agr1 agr2 ->
      if contradictsSeq conj then [] else  
        withBase [m1,m2] [semV v "member1" child, mite $ SeqFull v conj, mite $ Possessive caze1 (commonAgr agr1 agr2) v]
    (Complement child, SeqRight v CP conj) ->
      if contradictsSeq conj then [] else  
        withBase [m1,m2] [semV v "member1" child, mite $ SeqFull v conj, mite $ Complement v]
    (Clause force child, SeqRight seqV (ClauseArg force2) conj) | force == force2 -> {-traceIt "clause left" $-}
       if contradictsSeq conj then [] else
       let unifications = concat [unifyMissingArgument mite1 mite2 | mite1 <- unhappyLeft ++ happyBases,
                                                                     mite2 <- rightMites]
           happyLeft = filter happy leftMites
           happyBases = LS.removeDups $ concat $ map baseMites happyLeft
           unhappyLeft = filter (not . happy) leftMites
           unifyMissingArgument aux1 aux2 = case (cxt aux1, cxt aux2) of
             (NomHead agr1 v1, ElidedArgHead (NomHead agr2 v2)) | agree agr1 agr2 -> withBase [aux1,aux2] [mite $ Unify v1 v2]
             _ -> []
           ellipsisVariants = rightMites >>= \m3 -> case cxt m3 of
             Ellipsis ellipsisVar (Just e1) (Just e2) -> map (withBase [m3]) $ processEllipsis child ellipsisVar e1 e2 leftTree 
             _ -> []
           result = withBase [m1, m2] $
             [semV seqV "member1" child, mite $ Clause force seqV, mite $ SeqFull seqV conj] ++ unifications ++ xor ellipsisVariants
       in
         result
    _ -> []

data AnchorMapping = AnchorMapping {-original-} Mite Variable {-anchor-} Construction Variable

checkOriginal ::  Construction -> Mite -> Maybe AnchorMapping
checkOriginal anchor candidate = case (cxt candidate, anchor) of
  (VerbalModifier a1 False v1, VerbalModifier a2 False v2) | a1 == a2 -> Just $ AnchorMapping candidate v1 anchor v2
  (Argument kind1 v1, Argument kind2 v2) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 anchor v2
  _ -> Nothing

findOriginals mites anchor = catMaybes $ map (checkOriginal anchor) mites 

processEllipsis :: Variable -> Variable -> Construction -> Construction -> Tree -> [[Mite]]
processEllipsis oldCP ellipsisVar@(Variable varIndex _) e1 e2 prevTree = let
  allMites = allTreeMites prevTree
  activeMiteSet = allActiveMiteSet prevTree
  mappings = catMaybes [mapConstructions mapping1 mapping2 | mapping1 <- findOriginals allMites e1, mapping2 <- findOriginals allMites e2]
  mapConstructions :: AnchorMapping -> AnchorMapping -> Maybe [Mite]
  mapConstructions (AnchorMapping mo1 vo1 a1 va1) (AnchorMapping mo2 vo2 a2 va2) = let
    o1 = cxt mo1
    o2 = cxt mo2
    mapVariable _v =
      if _v == vo1 then va1
      else if _v == vo2 then va2
      else if _v == oldCP then ellipsisVar
      else Variable varIndex ("_" ++ show _v)
    mapMite m = withBase [m] $ case cxt m of
      Unify _v1 _v2 -> [mite $ Unify (mapVariable _v1) (mapVariable _v2)]
      Sem _v1 attr (StrValue s) -> [mite $ Sem (mapVariable _v1) attr (StrValue s)]
      Sem _v1 attr (VarValue _v2)  -> [mite $ Sem (mapVariable _v1) attr (VarValue $ mapVariable _v2)]
      _ -> []
    walkTree :: Tree -> ([Mite], [Construction])
    walkTree tree = let
      activeHeadMites = Constructor.Tree.activeHeadMites tree
      headCxts = map cxt activeHeadMites
      ownMapped = activeHeadMites >>= mapMite
      folder (allMapped, allCovered) tree = let (mapped, covered) = walkTree tree in (allMapped++mapped, allCovered++covered) 
      (leftMapped, leftCovered) = foldl folder ([], []) $ subTrees LeftSide tree
      (rightMapped, rightCovered) = foldl folder ([], []) $ subTrees RightSide tree
      in
      if elem o1 headCxts then ([], [o1])
      else if elem o2 headCxts then ([], [o2])
      else if containsClause activeHeadMites then ([], [])
      else (leftMapped++ownMapped++rightMapped, leftCovered++rightCovered)
    (mapped, covered) = walkTree prevTree
    containsClause = any (\m -> case cxt m of Clause _ cp -> oldCP /= cp; _ -> False)
    in
    if covered == [o1,o2] then Just mapped else Nothing
  in mappings