module Constructor.Composition (interactNodes) where
import Constructor.CopulaData
import Constructor.Mite
import Constructor.Variable
import qualified Constructor.Agreement as A
import Constructor.Agreement (agree, commonAgr)
import Constructor.Util
import Constructor.LexiconUtils
import Constructor.InteractionEnv
import Constructor.Coerce
import Constructor.Ellipsis
import qualified Constructor.Seq as Seq
import qualified Constructor.LinkedSet as LS
import qualified Constructor.SemanticProperties as P

interactNodes:: InteractionEnv -> [MergeInfo]
interactNodes env = {-traceIt ("    interact") $ -}whResults ++ noWh where

  seqVariants = map (propagateUnclosed env) $ (if null seqRight then [] else mergeLeft seqRight) ++ (if null seqLeft then [] else mergeRight seqLeft)
  seqLeft = Seq.seqLeft env
  seqRight = Seq.seqRight env

  questionable whContext = questionableArguments env whContext
  nonQuestionable = (pairs env >>= interactUnsorted env)
                 ++ (pairs env >>= punctuationAware env)
                 ++ seqVariants
                 ++ ellipsisLeftVariants env
  noWh = questionable False ++ nonQuestionable
  whBase = questionable True

  whResults = leftCombined env >>= \whMite -> let
    fillGap cp whVar clauseMite agr =
        let fillers = filter (\info -> mergedHeadSide info == RightSide) whBase
            inSitus = rightCompatible env clauseMite >>= \inSitu -> case cxt inSitu of
              WhInSitu var -> withBase [inSitu] $ [semS var P.InSitu "true"]
              _ -> []
            infos = fillers >>= \ info -> mergeLeft (mergeResult info ++ whLinks [whMite, clauseMite] cp whVar agr ++ inSitus)
        in infos
    in case cxt whMite of
      Wh agr whVar -> rightCombined env >>= \clauseMite -> case cxt clauseMite of
        Clause cp -> fillGap cp whVar clauseMite agr
        ModalityInfinitive cp -> fillGap cp whVar clauseMite agr
        c | Just (cd, rest) <- asCopula c, copKind cd /= NomNPCopula -> let
          infos = fillGap (copula cd) whVar clauseMite (copAgr cd)
          in [MergeInfo (mites ++ rest) side | MergeInfo mites side <- infos]
        Argument Nom subj -> leftCompatible env whMite >>= \copulaMite -> case asCopula $ cxt copulaMite of
          Just (cd, rest) -> rightCompatible env clauseMite >>= \adjMite -> case cxt adjMite of
            AdjHead v3 Nom agr | agree (copAgr cd) agr && subj == v3 ->
              mergeLeft $ completeCopula cd subj ++ rest ++ whLinks [whMite, clauseMite, copulaMite, adjMite] (copula cd) whVar (copAgr cd)
            _ -> []
          _ -> []
        _ -> []
      _ -> []

whLinks base cp whVar agr = withBase base [semV cp P.Questioned whVar, semS cp P.Clausal "true"] ++ xor [[mite $ Complement cp], [mite $ RelativeClause agr cp], [mite $ TopLevelQuestion cp]]

completeCopula cd subj = [mite $ NomHead (copAgr cd) subj Satisfied, mite $ Unify (copSubj cd) subj, mite $ Verb (copula cd)] ++ copulaSem cd ++ (if copBound cd then [] else [mite $ Handicap (copula cd)])

mergeInfoHelpers m1 m2 = ( \mites -> mergeLeft (base12 mites), \mites -> mergeRight (base12 mites), base12) where
  base12 = withBase [m1,m2]

propagateUnclosed env (MergeInfo mites side) = MergeInfo (mites ++ liftUnclosed (invert side) childMites) side where
  childMites = filter (not . contradictResult) $ (select side rightCombined leftCombined) env
  contradictResult mite = any (contradict mite) mites

liftUnclosed side childMites = childMites >>= \m -> case cxt m of
  Unclosed s _ | s == side -> withBase [m] $ [mite $ cxt m]
  _ -> []

ellipsisLeftVariants env = if null result then [] else mergeRight $ LS.removeDups result where
  result = rightCombined env >>= \m2 -> case cxt m2 of
    Ellipsis v (Just e2) -> leftCombined env >>= \m1 -> case ellipsisAnchor (cxt m1) of
      Just (anchorCxt, anchorVar, rest) -> let
        elided = suggestDoubleAnchorEllipsis env v anchorCxt e2
        in if null elided then []
           else withBase [m1,m2] [semV v P.EllipsisAnchor1 anchorVar, mite $ Clause v] ++ liftUnclosed LeftSide (leftCompatible env m1) ++ rest
                ++ xor (map (\(ClauseEllipsis v mites) -> [mite $ ConjEmphasizeable v] ++ mites) elided)
      _ -> []
    _ -> []

ellipsisAnchor :: Construction -> Maybe (Construction, Variable, [Mite])
ellipsisAnchor c = case c of
  VerbalModifier _ _ v -> Just (c, v, [])
  SemArgument _ _ v -> Just (c, v, [])
  _ | Just (arg@(Argument _ v), rest) <- asNoun c -> Just (arg, v, rest)
  _ -> Nothing

punctuationAware env (m1, m2) =
    let (left, right, base12) = mergeInfoHelpers m1 m2
        compatibleChildren side = select side (leftCompatible env m1) (rightCompatible env m2)
        liftUnclosedCompatible side = liftUnclosed side (compatibleChildren side)
        addUnclosed side v = let
          baseMite = select side m2 m1
          lifted = compatibleChildren side >>= \m -> case cxt m of
            Unclosed s vars | s == side -> withBase [m,baseMite] [mite $ Unclosed side (v:vars)]
            _ -> []
          in if null lifted then withBase [baseMite] [mite $ Unclosed side [v]] else lifted
        closeUnclosed side satisfied = (select side leftCombined rightCombined) env >>= \m -> case cxt m of
          Unclosed s vars | s == invert side -> withBase [m] $
            map (\v -> semS v (select side P.RightIsolated P.LeftIsolated) (if satisfied == Satisfied then "true" else "false")) vars
          _ -> []
    in case (cxt m1, cxt m2) of
      (NounPhrase head, CommaSurrounded True _ (NounAdjunct attr True var)) -> mergeLeft $
        base12 [semV head attr var] ++ liftUnclosedCompatible RightSide ++ closeUnclosed LeftSide Satisfied
      (CompHead attr head, CommaSurrounded True _ (Complement cp)) -> mergeLeft $
        base12 [semV head attr cp] ++ liftUnclosedCompatible RightSide ++ closeUnclosed LeftSide Satisfied
      (NounPhrase noun, CommaSurrounded True _ (RelativeClause agr2 cp)) -> leftCompatible env m1 >>= \m3 -> case cxt m3 of
        AdjHead _ _ agr1 | agree agr1 agr2 -> mergeLeft $
          withBase [m1,m2,m3] [semV noun P.Relative cp] ++ liftUnclosedCompatible RightSide
        _ -> []
      (CommaSurrounded _ closed (VerbalModifier attr True advP), _) | (cxt -> Verb verb):rest <- asVerb env m2 -> mergeRight $
        base12 ([semV verb attr advP] ++ rest)
        ++ closeUnclosed LeftSide (if closed then Satisfied else Unsatisfied)
        ++ liftUnclosedCompatible LeftSide
      (Verb verb, CommaSurrounded True _ (VerbalModifier attr True advP)) ->
        mergeLeft $ base12 [semV verb attr advP] ++ liftUnclosedCompatible RightSide ++ closeUnclosed LeftSide Satisfied

      (ComparativeHead head, CommaSurrounded True _ (ComparisonAnchor Satisfied v)) ->
        mergeLeft $ base12 [semV head P.Anchor v] ++ liftUnclosedCompatible RightSide
      (ConditionCompHead head, CommaSurrounded True _ (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (if cond=="if" then P.IfCondition else P.WhenCondition) cp] ++ liftUnclosedCompatible RightSide
      (Verb head, CommaSurrounded True _ (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (if cond=="if" then P.IfCondition else P.WhenCondition) cp] ++ liftUnclosedCompatible RightSide
      (CommaSurrounded _ True (ConditionComp cp cond _), Verb head) ->
        mergeRight $ base12 [semV head (if cond=="if" then P.IfCondition else P.WhenCondition) cp] ++ liftUnclosedCompatible LeftSide
      (Verb head, CommaSurrounded True _ (ReasonComp cp _)) ->
        mergeLeft $ base12 [semV head P.Reason cp] ++ liftUnclosedCompatible RightSide

      (SurroundingComma _, toWrap) | Just v <- getCommaSurroundableVar toWrap -> mergeLeft $
        base12 [mite $ CommaSurrounded True False toWrap] ++ withBase [m1] [semS v P.Isolation "comma", semS v P.LeftIsolated "true"]
        ++ addUnclosed RightSide v
      (toWrap, SurroundingComma _) | Just v <- getCommaSurroundableVar toWrap -> mergeRight $
        base12 [mite $ CommaSurrounded False True toWrap] ++ withBase [m2] [semS v P.Isolation "comma", semS v P.RightIsolated "true"]
        ++ addUnclosed LeftSide v
      (CommaSurrounded True False cxt, SurroundingComma _) ->
        mergeLeft $ base12 [mite $ CommaSurrounded True True cxt] ++ closeUnclosed LeftSide Satisfied

      (SurroundingDash _, toWrap@(Argument _ v)) -> mergeRight $
        base12 [mite $ DashSurrounded True False toWrap, semS v P.Isolation "dash", semS v P.LeftIsolated "true"] ++ addUnclosed RightSide v
      (DashSurrounded True False cxt, SurroundingDash _) ->
        mergeLeft $ base12 [mite $ DashSurrounded True True cxt] ++ closeUnclosed LeftSide Satisfied

      (QuestionVariants v kind, DashSurrounded True _ (Argument kind2 child)) | kind == kind2 ->
        mergeLeft $ base12 [semV v P.Variants child] ++ liftUnclosedCompatible RightSide
      (QuestionVariants v kind, CommaSurrounded True _ (Argument kind2 child)) | kind == kind2 ->
        mergeLeft $ base12 [semV v P.Variants child] ++ liftUnclosedCompatible RightSide

      (c, Word _ ".") | Just (cp, rest) <- asClause c ->
        mergeRight $ base12 [semS cp P.Dot "true", mite $ Sentence cp] ++ liftUnclosedCompatible LeftSide ++ closeUnclosed LeftSide Satisfied ++ rest
      (c, Word _ "!") | Just (cp, rest) <- asClause c ->
        mergeRight $ base12 [semS cp P.Exclamation_mark "true", mite $ Sentence cp] ++ liftUnclosedCompatible LeftSide ++ closeUnclosed LeftSide Satisfied ++ rest
      (Sentence cp, Word _ "\n") ->
        mergeRight $ base12 [semS cp P.ParagraphEnd "true"] ++ closeUnclosed LeftSide Satisfied
      (Argument Nom noun, Word _ "\n\n") ->
        right [semS noun P.SectionEnd "true", semS noun P.Clausal "true", semS noun P.SituationKind "object"]
      (TopLevelQuestion cp, Word _ "?") -> right [semS cp P.Question_mark "true", mite $ Sentence cp]

      (DirectSpeechHead head Nothing, Colon "directSpeech" v) ->
        mergeLeft $ base12 [mite $ DirectSpeechHead head $ Just v, semV head P.Message v] ++ closeUnclosed LeftSide Satisfied

      (DirectSpeechDash _, Sentence cp) ->
        mergeLeft $ base12 [mite $ Sentence cp, semS cp P.DirectSpeech "true"] ++ closeUnclosed RightSide Satisfied

      (Colon "elaboration" _, Clause cp) ->
        mergeLeft $ base12 [mite $ Elaboration cp]
        ++ closeUnclosed RightSide Satisfied ++ liftUnclosedCompatible RightSide

      (Ellipsis v Nothing, rightCxt) | Just (_, anchorVar, rest) <- ellipsisAnchor rightCxt ->
        left $ [mite $ Ellipsis v (Just rightCxt), semV v P.EllipsisAnchor2 anchorVar] ++ rest

      _ -> []

miteCxtPairs mites = map (\m -> (m, cxt m)) mites

questionableArguments env whContext = map (propagateUnclosed env) $ let
  (leftPairs, rightPairs) = (miteCxtPairs $ leftCombined env, miteCxtPairs $ rightCombined env)
  doInteract :: [(Mite, Construction)] -> [(Mite, Construction)] -> [MergeInfo]
  doInteract lp rp = do
    p1 <- lp
    p2 <- rp
    info@(MergeInfo mites side) <- interactQuestionable env lp rp whContext p1 p2
    let childMite = select side (fst p2) (fst p1)
    let liftedWh = (select side rightCompatible leftCompatible) env childMite >>= \m3 -> case cxt m3 of
          Wh _ var -> withBase [m3] [mite $ WhInSitu var]
          _ -> []
    return $ if whContext then info else MergeInfo (mites ++ liftedWh) side
  normalResults = doInteract leftPairs rightPairs
  hybridVariants headSide = select headSide rightCombined leftCombined env >>= \seqMite -> case cxt seqMite of
      Conjunction (SeqData {seqHybrid=True}) -> let
        headMites  = select headSide leftCombined    rightCombined  env
        childMites = select headSide rightCompatible leftCompatible env seqMite
        in map (\mites -> MergeInfo (withBase [seqMite] mites) headSide) (interactHybrid doInteract headSide headMites childMites)
      _ -> []
  in normalResults ++ hybridVariants LeftSide ++ hybridVariants RightSide

interactHybrid doInteract headSide headMites childMites = combinations where
  (headPairs, childPairs) = (miteCxtPairs headMites, miteCxtPairs childMites)
  interactConstituent childPairs = let
    resultInfos = select headSide (doInteract headPairs childPairs) (doInteract childPairs headPairs)
    in [mites | MergeInfo mites side <- resultInfos, side == headSide]
  unwrapHybrid childPairs combinations = let
    unwrappedLeft = childPairs >>= \(m, cc) -> case cc of SeqLeft c -> [(m, c)]; _ -> []
    unwrappedRight = childPairs >>= \(m, cc) -> case cc of SeqRight c -> [(m, c)]; _ -> []
    in
    if null unwrappedRight then [fromWhole ++ combo | combo <- combinations, fromWhole <- interactConstituent childPairs]
    else unwrapHybrid unwrappedLeft [fromRight ++ combo | combo <- combinations, fromRight <- interactConstituent unwrappedRight]
  combinations = unwrapHybrid childPairs [[]]

interactQuestionable env leftPairs rightPairs whContext (m1, c1) (m2, c2) =
    let (_, _, base12) = mergeInfoHelpers m1 m2
    in case (c1, c2) of
      (ArgHead _ kind1 relation head, _c) | Just (Argument kind2 arg, rest) <- asNoun _c, kind1 == kind2 ->
        mergeLeft $ base12 [semV head relation arg] ++ reflexive leftPairs rightPairs ++ existentials leftPairs rightPairs ++ rest
      (_c, ArgHead _ kind1 relation head) | Just (Argument kind2 arg, rest) <- asNoun _c, kind1 == kind2 ->
        mergeRight $ base12 [semV head relation arg] ++ reflexive rightPairs leftPairs ++ existentials rightPairs leftPairs ++ rest
      (SemArgHead _ kind1 head, SemArgument kind2 attr arg) | kind1 == kind2 ->
        mergeLeft $ base12 [mite $ SemArgHead Optional kind1 head, semV head attr arg] ++ reflexive leftPairs rightPairs ++ existentials leftPairs rightPairs
      (SemArgument kind2 attr arg, SemArgHead _ kind1 head) | kind1 == kind2 ->
        mergeRight $ base12 [mite $ SemArgHead Optional kind1 head, semV head attr arg] ++ reflexive rightPairs leftPairs ++ existentials rightPairs leftPairs

      (Argument Nom v1, NomHead agr1 v2 Unsatisfied) -> leftPairs >>= \case
        (m3, AdjHead v3 Nom agr2) | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) ->
          mergeRight $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []
      (NomHead agr1 v2 Unsatisfied, Argument Nom v1) -> rightPairs >>= \case
        (m3, AdjHead v3 Nom agr2) | agree agr1 agr2 && v1 == v3 && not (contradict m2 m3) ->
          mergeLeft $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []

      -- todo nom + nomHead/copulaHead duplication
      (Argument Nom v1, _c) | Just (cd, rest) <- asCopula _c -> leftPairs >>= \case
        (m3, AdjHead v3 Nom agr2) | agree (copAgr cd) agr2 && v1 == v3 && not (contradict m1 m3) ->
          mergeRight $ withBase [m1, m2, m3] $ completeCopula cd v1 ++ rest ++ (if whContext then [] else [mite $ Clause (copula cd)])
        _ -> []
      (_c, Argument Nom v2) | Just (cd, rest) <- asCopula _c, copKind cd /= AdjCopula && (copKind cd /= NomNPCopula || whContext) -> rightPairs >>= \case
        (m3, AdjHead v3 Nom agr2) | agree (copAgr cd) agr2 && v2 == v3 && not (contradict m2 m3) ->
          mergeLeft $ withBase [m1, m2, m3] $ completeCopula cd v2 ++ rest ++ (if whContext then [] else [mite $ Clause (copula cd)])
        _ -> []

      (Verb verb, VerbalModifier attr False advP) -> mergeLeft $ base12 [semV verb attr advP] ++ existentials leftPairs rightPairs
      (VerbalModifier attr needComma advP, _) | (cxt -> Verb verb):rest <- asVerb env m2 -> mergeRight $
        base12 ([semV verb attr advP] ++ rest
        ++ (if needComma && not whContext then [semS advP P.Isolation "comma", mite $ Unclosed LeftSide [advP]] else []))
        ++ existentials rightPairs leftPairs

      _ -> []

negationPropagation rightVar leftMites = leftMites >>= \m3 -> case cxt m3 of
  PendingNegation _ -> withBase [m3] [semS rightVar P.Negated "true", mite $ Negated rightVar]
  _ -> []

interactUnsorted env (m1, m2) = map (propagateUnclosed env) $
    let (left, right, base12) = mergeInfoHelpers m1 m2
    in case (cxt m1, cxt m2) of
      (Adj var2 attr adjCase agr1, c) | Just (AdjHead var nounCase agr2, rest) <- asAdjHead c, adjCase == nounCase && agree agr1 agr2 ->
        mergeRight $ base12 [semV var attr var2] ++ whPropagation (leftCompatible env m1) ++ negationPropagation var (leftCompatible env m1) ++ rest
      (c, Adj var2 attr adjCase agr1) | Just (AdjHead var nounCase agr2, rest) <- asAdjHead c, adjCase == nounCase && agree agr1 agr2 ->
        mergeLeft $ base12 [semV var attr var2] ++ whPropagation (rightCompatible env m2) ++ rest
      (c, ComparativeAdj agr1 attr var2) | Just (AdjHead var _ agr2, rest) <- asAdjHead c, agree agr1 agr2 ->
        mergeLeft $ base12 [semV var attr var2] ++ whPropagation (rightCompatible env m2) ++ rest

      (Possessive adjCase agr1 child, AdjHead _ nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        GenHead attrs -> mergeRight $
          withBase [m1,m2,m3] [semV h attr child | (attr, h) <- attrs]
          ++ Seq.pullThyself (leftCompatible env m1) ++ whPropagation (leftCompatible env m1)
        _ -> []
      (AdjHead _ nounCase agr2, Possessive adjCase agr1 child) | adjCase == nounCase && agree agr1 agr2 -> leftCompatible env m1 >>= \m3 -> case cxt m3 of
        GenHead attrs -> mergeLeft $
          withBase [m1,m2,m3] [semV h attr child | (attr, h) <- attrs]
          ++ Seq.pullThyself (leftCompatible env m1) ++ whPropagation (rightCompatible env m2)
        _ -> []
      (GenHead attrs, Argument Gen v2) -> left $ [semV v1 attr v2 | (attr, v1) <- attrs] ++ whPropagation (rightCompatible env m2)

      (AdjModifier attr agr1 caze modV, Adj adjVar adjAttr adjCase agr2) | caze == adjCase && agree agr1 agr2 ->
        left [semV adjVar attr modV, mite $ Adj adjVar adjAttr adjCase (commonAgr agr1 agr2)]

      (Relativizer wh, NomHead agr v2 Unsatisfied) -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        Clause cp -> mergeLeft $ withBase [m1,m2,m3] [mite $ Unify v2 wh, mite $ RelativeClause agr cp, semV cp P.Questioned wh]
        _ -> []
      -- todo relativizer + nomHead/copulaHead duplication
      (Relativizer wh, _c) | Just (cd, rest) <- asCopula _c, copKind cd /= NomNPCopula && not (agree (copAgr cd) A.n) ->
        left $ [mite $ Unify (copSubj cd) wh, mite $ RelativeClause (copAgr cd) (copula cd), semV (copula cd) P.Questioned wh] ++ rest ++ copulaSem cd
      (Relativizer wh, ArgHead _ Acc attr v2) -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        Clause cp -> mergeLeft $ withBase [m1,m2,m3] [semV v2 attr wh, mite $ RelativeClause A.empty cp, semV cp P.Questioned wh]
        _ -> []

      (ConjEmphasis attr _, ConjEmphasizeable head) -> right [semS head attr "true"]
      (AndEmphasis _, Argument _ head) -> right [semS head P.AndEmphasis "true", mite $ cxt m2]

      (Adverb v, _) | (cxt -> Verb head):rest <- asVerb env m2 -> right $ [mite $ Unify v head] ++ rest
      (Verb head, Adverb v) -> left [mite $ Unify v head]

      (NounPhrase head, NounAdjunct attr False var) -> left [semV head attr var]

      (Quantifier quantifierCase kind1 agr1 v1, Argument kind2 v2) | kind1 == kind2 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        AdjHead v3 kind3 agr2 | kind3 == kind1 && agree agr1 agr2 && v2 == v3 ->
          mergeLeft $ withBase [m1, m2, m3] ([semV v2 P.Quantifier v1] ++ synNoun quantifierCase agr1 (makeV v2 "")) ++ Seq.pullThyself (rightCompatible env m2)
        _ -> []

      (SemPreposition kind1 var1, Argument kind2 var2) | kind1 == kind2 -> left [mite $ Unify var1 var2]
      (PrepHead prep1 kind1 var1, c2) | Just (Argument kind2 var2, rest) <- asNoun c2, kind1 == kind2 ->
        let argMites = base12 ([mite $ Unify var1 var2]
                        ++ xorNonEmpty [[mite $ Argument (PP prep1 kind1) var1], adjunctMites, copulaVariants])
                       ++ whPropagation (rightCompatible env m2)
            adjunctMites = case (prep1, kind1) of
              ("k", Dat) -> semArg Direction P.Goal_to var2
              ("na", Acc) -> semArg Direction P.Goal_on var2
              ("na", Prep) -> [mite $ NounAdjunct P.Location_on False var2]
              ("po", Dat) -> xor [[mite $ VerbalModifier P.AccordingTo True var2],
                                  [mite $ NounAdjunct P.AccordingTo True var2],
                                  [mite $ VerbalModifier P.OptativeModality True var2]]
              ("ot", Gen) -> [mite $ VerbalModifier P.Reason False var2]
              ("s", Instr) -> xor [[mite $ NounAdjunct P.Companion False var2], [mite $ VerbalModifier P.Mood False var2]]
              ("s", Gen) -> xor [[mite $ NounAdjunct P.Source False var2], [mite $ VerbalModifier P.Source False var2]]
              ("u", Gen) -> [mite $ VerbalModifier P.Location_at False var2]
              ("v", Acc) -> semArg Direction P.Goal_in var2
              ("v", Prep) -> xor [[mite $ VerbalModifier P.Condition False var2],
                                  [mite $ VerbalModifier P.Location_in False var2]]
              _ -> []
            copulaCommon copulaType rel = copulaHead PPCopula A.empty copulaType rel Optional var1
            copulaVariants = case (prep1, kind1) of
              ("u", Gen) -> copulaCommon "copula" P.Owner
              ("na", Prep) -> copulaCommon "copula" P.Location_on
              ("o", Prep) -> xor [copulaCommon "copula_about" P.Arg2, copulaCommon "copula_talking_about" P.Arg2]
              _ -> []
            extra = Seq.pullThyself (rightCompatible env m2) ++ liftGen ++ rest
            liftGen = rightCompatible env m2 >>= \m3 -> case cxt m3 of
              GenHead {} -> withBase [m3] [mite $ cxt m3]
              _ -> []
        in mergeLeft (argMites ++ extra)

      (Verb head, Elaboration child) -> left [semV head P.Elaboration child, mite $ Unclosed RightSide [child]]

      (emphasized@(ParticleEmphasizeable _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
      (ComparativeEmphasis _, emphasized@(ComparativeAdj _ _ v)) -> right [semS v P.Emphasis "true", mite emphasized]
      (wh@(WhLeaf {}), Word _ "бишь") -> left [mite $ EmptyCxt wh]
      (wh@(WhLeaf {}), Word _ "это") -> left [mite $ EmptyCxt wh]

      (Verb v, Word _ "бы") -> left [semS v P.Irrealis "true"]
      (ModifierAdverb v1, AdverbModifiable v2) -> right [mite $ Unify v1 v2]

      (_, Tense v1 agr) | (cxt -> TenseHead _ v0):rest <- asTenseHead m1 agr -> left $  [mite $ Unify v0 v1] ++ rest
      (Tense v0 agr, _) | (cxt -> TenseHead _ v1):rest <- asTenseHead m2 agr -> right $ [mite $ Unify v0 v1] ++ rest

      (WhAsserter verb, Wh _ wh) -> right [mite $ ExistentialWh wh verb]

      (ConditionComp v0 s False, Clause cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]

      (ReasonComp v0 False, Clause cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]

      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left $ map mite wrapped
      
      (Conjunction    (SeqData {seqVar=v1, seqConj=",", seqHasLeft=False, seqHasRight=False}),
       Conjunction sd@(SeqData {seqVar=v2, seqHasComma=False, seqHasLeft=False, seqHasRight=True})) | seqConj sd /= "," ->
          right [mite $ Conjunction $ sd {seqHasComma=True, seqReady=True}, mite $ Unify v1 v2]

      (Quote _ False, word@(Word {})) -> left [mite $ QuotedWord word False]
      (QuotedWord word False, Quote _ True) -> left [mite $ QuotedWord word True]
      (NounPhrase noun, QuotedWord (Word _ word) _) -> left [semS noun P.Name word]

      (NegationModifier v1, Negated v) -> right [mite $ Unify v v1]
      (Negated v, NegationModifier v1) -> left [mite $ Unify v v1]

      (Word _ "не", Complement cp) -> right [semS cp P.ClauseNegated "true", mite $ Complement cp]
      (Word ne "не", Wh _ v) -> right [mite $ ExistentialWh v ne, semS v P.Negated "true"]
      (Word _ "не", c) | Just (v, rest) <- asNegateable c -> right $ xor [[semS v P.Negated "true", mite $ Negated v], [mite $ PendingNegation v]] ++ rest
      (Word _ "не", Verb v) -> let
        negateDirectObject = rightCombined env >>= \m3 -> case cxt m3 of
          ArgHead optional Acc attr v -> withBase [m1,m2,m3] [mite $ ArgHead optional Gen attr v]
          _ -> []
        in mergeRight $ base12 [semS v P.Negated "true", mite $ Negated v] ++ negateDirectObject

      (Complementizer cp1, Clause cp2) -> mergeLeft $ base12 [mite $ Unify cp1 cp2] ++ [mite $ Complement cp2]
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (FutureTense agr tense, ControlledInfinitive inf) -> right $ [mite $ Unify tense inf] ++ finiteClause agr True (makeV tense "")
      (RaisingVerb verb subj, Adj child _ Instr _) -> left [semV child P.Arg1 subj, semV verb P.Theme child] --todo raising agreement

      (ComparisonAnchor Unsatisfied _, Argument Nom v2) -> left [mite $ ComparisonAnchor Satisfied v2]
      (Comparativizer more, ShortAdj agr attr v) -> left [mite $ ComparativeAdj agr attr more, semT more "MORE", semV more P.Theme v, mite $ ComparativeHead more] -- todo synthetic comparative agreement

      _ -> []

reflexive headPairs childPairs = headPairs >>= \case
    (m1, ReflexiveTarget target) -> childPairs >>= \case
      (m2, ReflexiveReference ref) -> withBase [m1,m2] [semV ref P.Target target]
      _ -> []
    _ -> []

existentials headPairs childPairs = headPairs >>= \case
  (m1, ModalityInfinitive v) -> childPairs >>= \case
    (m2, ExistentialWh _ tensedVar) -> withBase [m1,m2] [semS v P.Clausal "true", mite $ Unify v tensedVar]
    _ -> []
  _ -> []

whPropagation childMites = childMites >>= \m3 -> case cxt m3 of
  Wh {} -> withBase [m3] [mite $ cxt m3]
  _ -> []
