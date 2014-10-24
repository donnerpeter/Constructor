module Constructor.Seq (seqLeft, seqRight, pullThyself) where
import qualified Constructor.LinkedSet as LS
import qualified Data.Set as Set
import Constructor.Constructions
import Constructor.Mite
import Constructor.Agreement
import Constructor.Variable
import Constructor.Tree
import Constructor.Util
import Constructor.InteractionEnv
import Control.Monad
import Data.Maybe
import qualified Constructor.SemanticProperties as P

seqRight env = {-traceIt "seqRight" $ -}result where
  hasSeqFull conj = flip any (rightCombined env) $ \case
    (cxt -> Conjunction (SeqData { seqHasLeft=True, seqHasRight=True, seqConj=c})) | isSeqContinuation conj c -> True
    _ -> False
  hasConjEmphasis = any (any isConjEmphasis . baseMites) $ rightCombined env
  isConjEmphasis = \case (cxt -> ConjEmphasis {}) -> True; _ -> False
  result = leftCombined env >>= \m1 -> case cxt m1 of
    Conjunction sd@(SeqData { seqHasLeft=False, seqHasRight=False, seqConj=conj}) -> let
      wrapped = filter seqWrappable (rightCombined env) >>= \m -> withBase [m] [mite $ SeqRight $ cxt m]
      in
        if hasSeqFull conj || hasConjEmphasis || null wrapped then []
        else withBase [m1] [mite $ Conjunction $ sd {seqHasRight=True}] ++ wrapped
    _ -> []

seqWrappable mite = hybridConjoinable (cxt mite) || case cxt mite of
  Adj {} -> True; Possessive {} -> True
  Complement {} -> True
  Clause {} -> True
  NomHead _ _ Unsatisfied -> True
  Ellipsis {} -> True
  UniversalPronoun {} -> True
  NegativePronoun {} -> True
  Wh {} -> True
  _ -> False

hybridConjoinable c = case c of
  Argument {} -> True
  AdjHead {} -> True
  VerbalModifier {} -> True
  GenHead {} -> True
  _ -> False

hybridWrapped c = case c of
  SeqLeft {} -> True
  SeqRight {} -> True
  _ -> False

isSeqContinuation conj1 conj2 = conj1 == ","

seqLeft env = {-traceIt "seqLeft" $ -}result where
  contradictsSeq conj = flip any (leftCombined env) $ \mite -> case cxt mite of
    Conjunction (SeqData { seqHasLeft=True, seqHasRight=True, seqConj=c}) | not $ isSeqContinuation c conj -> True
    _ -> False
  result = rightCombined env >>= \m2 -> case cxt m2 of
    Conjunction sd@(SeqData { seqReady=True, seqHasRight=True, seqHasLeft=False, seqConj=conj}) ->
      if contradictsSeq conj then []
      else xor $ filter (not. null) [normalSeqVariants m2 sd env, hybridSeqVariants m2 sd env]
    _ -> []

normalSeqVariants m2 sd@(SeqData { seqVar=seqV }) env =
      leftCombined env >>= \m1 -> let
        fullConj mem1 mem2 = [semV seqV P.Member1 mem1, semV seqV P.Member2 mem2, mite $ Conjunction $ sd {seqHasLeft=True}]
        handleAdj :: Variable -> ArgKind -> Agr -> (Agr -> Construction) -> [Mite]
        handleAdj mem1 caze1 agr1 result = let
          adjResult mem2 agr2 = let
            adjAgrVariants = [mite $ result (Agr Nothing (Just Pl) Nothing)]
            allVariants = if agree agr1 agr2 then xor [adjAgrVariants, [mite $ result (commonAgr agr1 agr2)]] else adjAgrVariants
            in fullConj mem1 mem2 ++ allVariants
          in rightCompatible env m2 >>= \m3 -> case cxt m3 of
            SeqRight (Adj mem2 caze2 agr2) | caze1 == caze2 && adjAgree agr1 agr2 -> withBase [m1,m2,m3] $ adjResult mem2 agr2
            SeqRight (Possessive caze2 agr2 mem2) | caze1 == caze2 && adjAgree agr1 agr2 -> withBase [m1,m2,m3] $ adjResult mem2 agr2
            _ -> []
        argUnifications = let
           unifications = concat [unifyMissingArgument mite1 mite2 | mite1 <- leftCompatible env m1,
                                                                     mite2 <- rightCompatible env m2]
           unifyMissingArgument aux1 aux2 = case (cxt aux1, cxt aux2) of
             (GenHead v1, SeqRight (GenHead v2)) ->
                 optional $ withBase [aux1,aux2] [mite $ Unify v1 v2, mite $ GenHead v2]
             _ -> []
           in unifications
        combineThyself = let
          leftRefs  = [m | m@(cxt -> ReflexiveReference _) <- leftCombined env] >>= liftMite
          rightRefs = [m | m@(cxt -> SeqRight(ReflexiveReference _)) <- rightCombined env] >>= liftMite
          in leftRefs ++ rightRefs
        adjHeadCompanions child kind = if kind `elem` cases then withBase [m2] [mite $ AdjHead seqV kind (Agr Nothing (Just Pl) Nothing)] else []
        distinguished mite = case cxt mite of
          Argument (PP {}) child -> [semS child P.Distinguished "true"]
          VerbalModifier _ _ child | any isPrepHead (baseMites mite) -> [semS child P.Distinguished "true"]
          _ -> []
        isPrepHead mite = case cxt mite of PrepHead {} -> True; _ -> False
        in rightCompatible env m2 >>= \m3 -> case (cxt m1, cxt m3) of

          (Argument kind mem1, SeqRight (Argument kind2 mem2)) | kind == kind2 ->
            withBase [m1,m2,m3] (fullConj mem1 mem2 ++ [mite $ Argument kind seqV] ++ combineThyself ++ (baseMites m3 >>= distinguished))
            ++ adjHeadCompanions mem1 kind ++ argUnifications

          (VerbalModifier attr comma mem1, SeqRight (VerbalModifier attr2 comma2 mem2)) | attr2 == attr && comma2 == comma ->
            withBase [m1,m2, m3] (fullConj mem1 mem2 ++ [mite $ VerbalModifier attr comma seqV] ++ (baseMites m3 >>= distinguished))
            ++ argUnifications

          (Possessive caze1 agr1 child, _) -> handleAdj child caze1 agr1 $ \newAgr -> Possessive caze1 newAgr seqV
          (Adj child caze1 agr1, _) -> handleAdj child caze1 agr1 $ \newAgr -> CompositeAdj seqV caze1 newAgr
          (CompositeAdj child caze1 agr1, _) -> handleAdj child caze1 agr1 $ \newAgr -> CompositeAdj seqV caze1 newAgr

          (Complement mem1, SeqRight (Complement mem2)) ->
            withBase [m1,m2,m3] $ fullConj mem1 mem2 ++ [mite $ Complement seqV, semS mem2 P.Distinguished "true"]

          (Clause mem1, SeqRight (Clause mem2)) -> let
                 unifications = xor $ filter (not . null) $
                   [unifyMissingArgument mite1 mite2 | mite1 <- leftCompatible env m1,
                                                       mite2 <- rightCompatible env m2]
                 unifyMissingArgument aux1 aux2 = case (cxt aux1, cxt aux2) of
                   (NomHead agr1 v1 satisfied, SeqRight (NomHead agr2 v2 Unsatisfied)) | agree agr1 agr2 ->
                       withBase [aux1,aux2] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v1 satisfied]
                   _ -> []
                 ellipsisVariants = rightCompatible env m2 >>= \m4 -> case cxt m4 of
                   SeqRight (Ellipsis ellipsisVar (Just e1) (Just e2)) ->
                     map (withBase [m4]) $ processEllipsis m1 ellipsisVar e1 e2 (leftTree env)
                   _ -> []
                 result = withBase [m1, m2] $
                   fullConj mem1 mem2 ++ [mite $ Clause seqV] ++ unifications ++ xor ellipsisVariants
                 in result
          _ -> []

hybridSeqVariants m2 sd@(SeqData { seqVar=seqV }) env = let
  makeHybrid m1 m3 mem1 mem2 resultCxt =
    withBase [m1,m2,m3] [mite $ Conjunction $ sd {seqHasLeft=True, seqHybrid=True},
                         semV seqV P.Member1 mem1, semV seqV P.Member2 mem2, semS seqV P.Hybrid "true",
                         mite resultCxt]
            ++ (filter (\m -> hybridConjoinable (cxt m) || hybridWrapped (cxt m)) (leftCompatible env m1) >>= \m -> withBase [m] [mite $ SeqLeft $ cxt m])
  in leftCombined env >>= \m1 -> case cxt m1 of
    UniversalPronoun mem1 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
      SeqRight (UniversalPronoun mem2) -> makeHybrid m1 m3 mem1 mem2 (UniversalPronoun seqV)
      _ -> []
    NegativePronoun mem1 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
      SeqRight (NegativePronoun mem2) -> makeHybrid m1 m3 mem1 mem2 (NegativePronoun seqV)
      _ -> []
    Wh mem1 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
      SeqRight (Wh mem2) -> makeHybrid m1 m3 mem1 mem2 (Wh seqV)
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
    containsClause mites = not $ null [m | m@(cxt -> Clause cp) <- mites, oldCP /=cp]
    in
    if covered == [o1,o2] then Just mapped else Nothing
  in mappings

pullThyself childMites = [m | m@(cxt -> ReflexiveReference _) <- childMites] >>= liftMite

liftMite m = withBase [m] [mite $ cxt m]