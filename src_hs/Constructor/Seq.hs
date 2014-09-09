module Constructor.Seq (seqLeft, seqRight, pullThyself, liftArguments) where
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import Constructor.Constructions
import Constructor.Mite
import Constructor.Agreement
import Constructor.Variable
import Constructor.Tree
import Constructor.Util
import Control.Monad
import Data.Maybe
import qualified Constructor.SemanticProperties as P

liftArguments base mites = wrapped where
  wrapped = concat $ map wrapMite $ filter (not . happy) {-$ filter (not . contradict base)-} mites
  wrapMite m = wrapCxt m >>= \c -> optional (withBase [m] [mite c])
  wrapCxt mite = case cxt mite of
    GenHead {} -> [UnsatisfiedArgHead $ cxt mite]
    UnsatisfiedArgHead {} -> [cxt mite]
    _ -> []

seqRight leftMites rightMites = {-traceIt "seqRight" $ -}result where
  hasSeqFull conj = flip any rightMites $ \case
    (cxt -> Conjunction (SeqData { seqHasLeft=True, seqRightVar=(Just _), seqConj=c})) | isSeqContinuation conj c -> True
    _ -> False
  hasConjEmphasis = any (any isConjEmphasis . baseMites) rightMites
  isConjEmphasis = \case (cxt -> ConjEmphasis {}) -> True; _ -> False
  result = leftMites >>= \m1 -> case cxt m1 of
   Conjunction sd@(SeqData { seqVar=v, seqHasLeft=False, seqRightVar=Nothing, seqConj=conj}) ->
    if hasSeqFull conj || hasConjEmphasis then [] else rightMites >>= \m2 -> let
     conjWithRight var = mite $ Conjunction $ sd {seqKind=Just (cxt m2), seqRightVar=Just var}
     distinguished mite = case cxt mite of
       Argument (PP {}) child -> [semS child P.Distinguished "true"]
       VerbalModifier _ _ child | any isPrepHead (baseMites mite) -> [semS child P.Distinguished "true"]
       _ -> []
     isPrepHead mite = case cxt mite of
       PrepHead {} -> True
       _ -> False
     in case cxt m2 of
      Argument kind child ->
        withBase [m1,m2] ([semV v P.Member2 child, conjWithRight child] ++ distinguished m2 ++ pullThyself m2 rightMites)
          ++ liftArguments m2 rightMites
      VerbalModifier attr comma child ->
        withBase [m1,m2] $ [semV v P.Member2 child, conjWithRight child]
          ++ distinguished m2 ++ liftArguments m2 rightMites
      Adj child caze agr -> withBase [m1,m2] [semV v P.Member2 child, conjWithRight child]
      Possessive caze agr child -> withBase [m1,m2] [semV v P.Member2 child, mite $ Conjunction $ sd {seqKind=Just (Adj child caze agr), seqRightVar=Just child}]
      Complement child -> withBase [m1,m2] [semV v P.Member2 child, semS child P.Distinguished "true", conjWithRight child]
      Clause child ->
        let wrapped = [(mite $ ElidedArgHead $ cxt m) {baseMites = [m,m1,m2]} | m <- filter elideable rightMites]
            elideable mite = case cxt mite of
              NomHead _ _ Unsatisfied -> True
              _ -> False
            ellipsis = rightMites >>= \e -> case cxt e of
              Ellipsis child (Just _) (Just _) -> withBase [e] [mite $ cxt e] 
              _ -> []
            result = withBase [m1, m2] [semV v P.Member2 child, conjWithRight child] ++ ellipsis ++ wrapped
        in
          result
      _ -> []
   _ -> []

isSeqContinuation conj1 conj2 = conj1 == ","

seqLeft leftTree leftMites rightMites = {-traceIt "seqLeft" $ -}result where
  contradictsSeq conj = flip any leftMites $ \mite -> case cxt mite of
    Conjunction (SeqData { seqHasLeft=True, seqRightVar=(Just _), seqConj=c}) | not $ isSeqContinuation c conj -> True
    _ -> False
  result = rightMites >>= \m2 -> case cxt m2 of
    Conjunction sd@(SeqData { seqVar=seqV, seqReady=True, seqKind=Just seqKind, seqRightVar=Just rightVar, seqHasLeft=False, seqConj=conj}) ->
      if contradictsSeq conj then [] else leftMites >>= \m1 -> let
        conjWithLeft = mite $ Conjunction $ sd {seqHasLeft=True}
        handleAdj :: Variable -> ArgKind -> Agr -> (Agr -> Construction) -> [Mite]
        handleAdj child caze1 agr1 result = case seqKind of
          Adj _ caze2 agr2 | caze1 == caze2 && adjAgree agr1 agr2 -> let
            adjAgrVariants = [mite $ result (Agr Nothing (Just Pl) Nothing)]
            allVariants = if agree agr1 agr2 then xor [adjAgrVariants, [mite $ result (commonAgr agr1 agr2)]] else adjAgrVariants
            in withBase [m1,m2] $ [semV seqV P.Member1 child, conjWithLeft] ++ allVariants
          _ -> []
        stripVar c = case c of
          VerbalModifier attr comma _ -> VerbalModifier attr comma
          NounAdjunct attr comma _ -> NounAdjunct attr comma
        argUnifications = let
           unifications = concat [unifyMissingArgument mite1 mite2 | mite1 <- filter (not . contradict m1) leftMites,
                                                                     mite2 <- filter (not . contradict m2) rightMites]
           unifyMissingArgument aux1 aux2 = case (cxt aux1, cxt aux2) of
             (GenHead v1, UnsatisfiedArgHead (GenHead v2)) ->
                 optional $ withBase [aux1,aux2] [mite $ Unify v1 v2, mite $ GenHead v1]
             (UnsatisfiedArgHead (GenHead v1), UnsatisfiedArgHead (GenHead v2)) ->
                 optional $ withBase [aux1,aux2] [mite $ Unify v1 v2, mite $ GenHead v1]
             _ -> []
           in unifications
        combineThyself = case pullThyself m1 leftMites ++ pullThyself m2 rightMites of
          both@[Mite {cxt = ReflexiveReference ref1}, Mite { cxt = ReflexiveReference ref2 }] -> withBase both [mite $ ReflexiveReference ref1, mite $ Unify ref1 ref2]
          combined -> combined
        adjHeadCompanions child kind = if kind `elem` cases then withBase [m2] [mite $ AdjHead seqV kind (Agr Nothing (Just Pl) Nothing)] else []
        in case cxt m1 of
          Argument kind child | seqKind == Argument kind rightVar -> -- traceIt "arg" $
            withBase [m1,m2] ([semV seqV P.Member1 child, conjWithLeft, mite $ Argument kind seqV] ++ combineThyself)
            ++ adjHeadCompanions child kind ++ argUnifications
          VerbalModifier attr comma child | seqKind == VerbalModifier attr comma rightVar ->
            withBase [m1,m2] [semV seqV P.Member1 child, conjWithLeft, mite $ VerbalModifier attr comma seqV]
            ++ argUnifications
          Possessive caze1 agr1 child -> handleAdj child caze1 agr1 $ \newAgr -> Possessive caze1 newAgr seqV
          Adj child caze1 agr1 -> handleAdj child caze1 agr1 $ \newAgr -> CompositeAdj seqV caze1 newAgr
          CompositeAdj child caze1 agr1 -> handleAdj child caze1 agr1 $ \newAgr -> CompositeAdj seqV caze1 newAgr
          Complement child | seqKind == Complement rightVar -> withBase [m1,m2] [semV seqV P.Member1 child] ++ [conjWithLeft, mite $ Complement seqV]
          Clause child -> case seqKind of
            Clause _ -> let
                 unifications = xor $ filter (not . null) $
                   [unifyMissingArgument mite1 mite2 | mite1 <- filter (not . contradict m1) leftMites,
                                                       mite2 <- filter (not . contradict m2) rightMites]
                 unifyMissingArgument aux1 aux2 = case (cxt aux1, cxt aux2) of
                   (NomHead agr1 v1 satisfied, ElidedArgHead (NomHead agr2 v2 Unsatisfied)) | agree agr1 agr2 ->
                       withBase [aux1,aux2] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v1 satisfied]
                   _ -> []
                 ellipsisVariants = rightMites >>= \m3 -> case cxt m3 of
                   Ellipsis ellipsisVar (Just e1) (Just e2) | not $ contradict m3 m2 -> map (withBase [m3]) $ processEllipsis m1 ellipsisVar e1 e2 leftTree
                   _ -> []
                 result = withBase [m1, m2] $
                   [semV seqV P.Member1 child, mite $ Clause seqV, conjWithLeft] ++ unifications ++ xor ellipsisVariants
                 in result
            _ -> []
          _ -> []
    _ -> []

data AnchorMapping = AnchorMapping {-original-} Mite Variable {-anchor-} Construction Variable deriving (Show)

checkOriginal ::  Construction -> Mite -> Maybe AnchorMapping
checkOriginal anchor candidate = case (cxt candidate, anchor) of
  (VerbalModifier a1 _ v1, VerbalModifier a2 _ v2) | a1 == a2 -> Just $ AnchorMapping candidate v1 anchor v2
  (Argument kind1 v1, Argument kind2 v2) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 anchor v2
  (SemArgument kind1 v1 _, SemArgument kind2 v2 _) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 anchor v2
  (Adj v1 kind1 _, Adj v2 kind2 _) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 anchor v2
  _ -> Nothing

findOriginals mites anchor = catMaybes $ map (checkOriginal anchor) mites

processEllipsis :: Mite -> Variable -> Construction -> Construction -> Tree -> [[Mite]]
processEllipsis oldClause ellipsisVar@(Variable varIndex _) e1 e2 prevTree = let
  Clause oldCP = cxt oldClause
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
      in
      if elem o1 headCxts then ([], [o1])
      else if elem o2 headCxts then ([], [o2])
      else if containsClause activeHeadMites then ([], [])
      else (leftMapped++ownMapped++rightMapped, leftCovered++rightCovered)
    (mapped, covered) = walkTree prevTree
    containsClause = any (\m -> case cxt m of Clause cp -> oldCP /= cp; _ -> False)
    in
    if covered == [o1,o2] then Just mapped else Nothing
  in mappings

pullThyself base childMites = childMites >>= \m -> case cxt m of
  ReflexiveReference ref | not $ contradict m base -> withBase [m] [mite $ cxt m]
  _ -> []