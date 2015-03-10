module Constructor.Seq (seqLeft, seqRight, pullThyself) where
import Constructor.Constructions
import Constructor.CopulaData
import Constructor.Mite
import Constructor.Agreement
import Constructor.Variable
import Constructor.Util
import Constructor.LexiconUtils
import Constructor.InteractionEnv
import Constructor.Coerce
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
  CopulaHead {} -> True
  TenseHead {} -> True
  NomHead {} -> True
  Ellipsis {} -> True
  UniversalPronoun {} -> True
  NegativePronoun {} -> True
  Wh {} -> True
  Negated {} -> True
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

isSeqContinuation conj1 conj2 = conj1 == "," || conj1 == "i" && (conj2 == "a" || conj2 == "no")

seqLeft env = {-traceIt "seqLeft" $ -}result where
  contradictsSeq conj = flip any (leftCombined env) $ \mite -> case cxt mite of
    Conjunction (SeqData { seqHasLeft=True, seqHasRight=True, seqConj=c}) | not $ isSeqContinuation c conj -> True
    _ -> False
  result = rightCombined env >>= \m2 -> case cxt m2 of
    Conjunction sd@(SeqData { seqReady=True, seqHasRight=True, seqHasLeft=False, seqConj=conj}) ->
      if contradictsSeq conj then []
      else xorNonEmpty [normalSeqVariants m2 sd env, hybridSeqVariants m2 sd env]
    _ -> []

withNegation sd env m1 m2 m4 f = let
  requireNegation = seqConj sd == "a"
  negatedVariants sideMites = sideMites >>= \mx -> case cxt mx of
    Negated v -> f [m1,m2,m4,mx]
    SeqRight (Negated v) -> f [m1,m2,m4,mx]
    _ -> []
  in if requireNegation then negatedVariants (leftCompatible env m1) ++ negatedVariants (rightCompatible env m4)
     else f [m1,m2,m4]

normalSeqVariants m2 sd@(SeqData { seqVar=seqV }) env = xorNonEmpty $
      leftCombined env >>= \m1 -> let
        fullConj mem1 mem2 = [semV seqV P.Member1 mem1, semV seqV P.Member2 mem2, mite $ Conjunction $ sd {seqHasLeft=True}]
        handleAdj :: Variable -> Variable -> ArgKind -> Agr -> Agr -> P.VarProperty -> Mite -> (Agr -> Construction) -> [[Mite]]
        handleAdj mem1 mem2 caze1 agr1 agr2 attr m4 result = let
          adjAgrVariants = [[mite $ result (Agr Nothing (Just Pl) Nothing)]]
          unifiedAgrVariants = [[mite $ result (commonAgr agr1 agr2)]]
          allVariants = xorNonEmpty $
            if seqConj sd == "a" then unifiedAgrVariants
            else if agree agr1 agr2 && unifiedAgrVariants /= adjAgrVariants then adjAgrVariants ++ unifiedAgrVariants
            else adjAgrVariants
          in [withNegation sd env m1 m2 m4 $ \base -> fullConj mem1 mem2 ++ withBase base allVariants]
        argUnifications = let
           unifications = concat [unifyMissingArgument mite1 mite2 | mite1 <- leftCompatible env m1,
                                                                     mite2 <- rightCompatible env m2]
           unifyMissingArgument aux1 aux2 = case (cxt aux1, cxt aux2) of
             (GenHead a1, SeqRight (GenHead a2)) ->
                 withBase [aux1,aux2] [mite $ GenHead (a1 ++ a2)]
             _ -> []
           in unifications
        combineThyself = let
          leftRefs  = [m | m@(cxt -> ReflexiveReference _) <- leftCombined env] >>= liftMite
          rightRefs = [m | m@(cxt -> SeqRight(ReflexiveReference _)) <- rightCombined env] >>= liftMite
          in leftRefs ++ rightRefs
        adjHeadCompanions child kind = if kind `elem` cases then withBase [m2] [mite $ AdjHead seqV kind pl3] else []
        distinguished mite = case cxt mite of
          Argument (PP {}) child -> [semS child P.Distinguished "true"]
          VerbalModifier _ _ child | any isPrepHead (baseMites mite) -> [semS child P.Distinguished "true"]
          _ -> []
        isPrepHead mite = case cxt mite of PrepHead {} -> True; _ -> False
        in rightCompatible env m2 >>= \m3 -> case (cxt m1, cxt m3) of

          (Argument kind mem1, SeqRight c) | Just (Argument kind2 mem2, rest) <- asNoun c, kind == kind2 ->
           withNegation sd env m1 m2 m3 $ \base ->
            [fullConj mem1 mem2 ++ withBase base [mite $ Argument kind seqV] ++ combineThyself ++ (baseMites m3 >>= distinguished) ++ rest
             ++ adjHeadCompanions mem1 kind ++ argUnifications]

          (VerbalModifier attr comma mem1, SeqRight (VerbalModifier attr2 comma2 mem2)) | attr2 == attr && comma2 == comma ->
            [fullConj mem1 mem2 ++ withBase [m1,m2, m3] [mite $ VerbalModifier attr comma seqV] ++ (baseMites m3 >>= distinguished)
             ++ argUnifications]

          (Possessive caze1 agr1 mem1, SeqRight (Possessive caze2 agr2 mem2)) | caze1 == caze2 && adjAgree agr1 agr2 ->
            handleAdj mem1 mem2 caze1 agr1 agr2 P.Arg1 m3 $ \newAgr -> Possessive caze1 newAgr seqV
          (Adj mem1 attr caze1 agr1, SeqRight (Adj mem2 _ caze2 agr2)) | caze1 == caze2 && adjAgree agr1 agr2 ->
            handleAdj mem1 mem2 caze1 agr1 agr2 attr m3 $ \newAgr -> Adj seqV attr caze1 newAgr

          (Complement mem1, SeqRight (Complement mem2)) ->
            [fullConj mem1 mem2 ++ withBase [m1,m2,m3] [mite $ Complement seqV, semS mem2 P.Distinguished "true"]]

          (Clause mem1, SeqRight (Clause mem2)) -> let
                 unifications = xorNonEmpty $ concatMap unifyMissingArguments $ rightCompatible env m2
                 unifyMissingArguments aux2 = case cxt aux2 of
                   SeqRight (NomHead agr2 v2 Unsatisfied) -> unifySubjects (leftCompatible env m1) agr2 v2
                   SeqRight (nomHead@(NomHead _ _  Satisfied)) -> [withBase [aux2] [mite nomHead]]
                   _ -> []
                 result = fullConj mem1 mem2 ++ withBase [m1, m2, m3] [mite $ Clause seqV] ++ unifications
                 in [result]
          (Clause mem1, SeqRight c) | Just (cd, rest) <- asCopula c,  copBound cd -> let
                 unifications = xorNonEmpty $ unifySubjects (leftCompatible env m1) (copAgr cd) (copSubj cd)
                 result = fullConj mem1 (copCP cd) ++ withBase [m1, m2, m3] [mite $ Clause seqV, mite $ Handicap seqV] ++ copulaSem cd ++ unifications ++ rest
                 in [result]
          (_c1, SeqRight _c2) |
               Just (CopulaData { copKind=kind, copAgr=agr1, copAttr=attr, copVar=mem1, copBound=False, copType=typ }, rest1) <- asCopula _c1,
               Just (cd2, rest2) <- asCopula _c2,
               agr2 <- copAgr cd2,
               not (copBound cd2) &&
               kind == copKind cd2 &&
               adjAgree agr1 agr2 &&
               typ == copType cd2 &&
               attr == copAttr cd2 ->
                 [fullConj mem1 (copVar cd2) ++ withBase [m1,m2,m3] (copulaHead kind (commonAgr agr1 agr2) typ attr Optional seqV) ++ rest1 ++ rest2]
          _ -> []

unifySubjects leftMites agr rightSubj = map unifyNomHead leftMites where
  unifyNomHead aux1 = case cxt aux1 of
    (NomHead agr1 v1 satisfied) | agree agr1 agr ->
      withBase [aux1] [mite $ Unify v1 rightSubj, mite $ NomHead (commonAgr agr1 agr) v1 satisfied]
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
    Wh agr1 mem1 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
      SeqRight (Wh agr2 mem2) | agree agr1 agr2 -> makeHybrid m1 m3 mem1 mem2 (Wh empty seqV)
      _ -> []
    _ -> []

pullThyself childMites = [m | m@(cxt -> ReflexiveReference _) <- childMites] >>= liftMite

liftMite m = withBase [m] [mite $ cxt m]

