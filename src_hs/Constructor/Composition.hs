module Constructor.Composition (interactNodes) where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Variable
import Constructor.Agreement
import Constructor.Tree
import Constructor.Util
import Constructor.LexiconUtils
import Constructor.InteractionEnv
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
            infos = fillers >>= \ info -> mergeLeft (mergeResult info ++ withBase [whMite, clauseMite] (whLinks cp whVar agr) ++ inSitus)
        in infos
    in case cxt whMite of
      Wh agr whVar -> rightCombined env >>= \clauseMite -> case cxt clauseMite of
        Clause cp -> fillGap cp whVar clauseMite agr
        ModalityInfinitive _ cp -> fillGap cp whVar clauseMite agr
        CopulaHead (CopulaData { copKind = kind, copCP = cp }) | kind /= NPCopula -> fillGap cp whVar clauseMite agr
        Argument Nom subj -> leftCompatible env whMite >>= \copulaMite -> case cxt copulaMite of
          CopulaHead (CopulaData { copAgr = agr, copCP = cp }) -> let
            fillers = filter (\info -> mergedHeadSide info == LeftSide) whBase
            infos = fillers >>= \ info -> mergeLeft (mergeResult info ++ withBase [whMite, clauseMite, copulaMite] (whLinks cp whVar agr))
            in infos
          _ -> []
        _ -> []
      _ -> []

whLinks cp whVar agr = [semV cp P.Questioned whVar, semT cp "situation"] ++ xor [[mite $ Complement cp], [mite $ RelativeClause agr cp], [mite $ TopLevelQuestion cp]]

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
    Ellipsis v Nothing rightCxt@(Just _) -> leftCombined env >>= \m1 -> case ellipsisAnchor (cxt m1) of
      Just anchor -> withBase [m1,m2] [mite $ Ellipsis v (Just $ cxt m1) rightCxt]
        ++ [semV v P.EllipsisAnchor1 anchor, mite $ Clause v]
        ++ liftUnclosed LeftSide (leftCompatible env m1)
      _ -> []
    _ -> []

ellipsisAnchor (VerbalModifier _ _ v) = Just v
ellipsisAnchor (Argument _ v) = Just v
ellipsisAnchor (SemArgument _ _ v) = Just v
ellipsisAnchor _ = Nothing

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
            optional $ [mite $ Closed vars]
            ++ map (\v -> semS v (select side P.LeftIsolated P.RightIsolated) (if satisfied == Satisfied then "true" else "false")) vars
          _ -> []
    in case (cxt m1, cxt m2) of
      (NounPhrase head, CommaSurrounded True _ (NounAdjunct attr True var)) -> mergeLeft $
        base12 [semV head attr var] ++ liftUnclosedCompatible RightSide ++ closeUnclosed LeftSide Satisfied
      (CompHead comp, CommaSurrounded True _ (Complement cp)) -> mergeLeft $
        base12 [mite $ Unify comp cp] ++ liftUnclosedCompatible RightSide ++ closeUnclosed LeftSide Satisfied
      (NounPhrase noun, CommaSurrounded True _ (RelativeClause agr2 cp)) -> leftCompatible env m1 >>= \m3 -> case cxt m3 of
        AdjHead _ _ agr1 | agree agr1 agr2 -> mergeLeft $
          withBase [m1,m2,m3] [semV noun P.Relative cp] ++ liftUnclosedCompatible RightSide
        _ -> []
      (CommaSurrounded _ closed (VerbalModifier attr True advP), Verb verb) -> mergeRight $
        base12 [semV verb attr advP]
        ++ closeUnclosed LeftSide (if closed then Satisfied else Unsatisfied)
        ++ liftUnclosedCompatible LeftSide
      --todo verbalModifier + copulaHead
      (CommaSurrounded _ closed (VerbalModifier attr True advP), CopulaHead cd) -> mergeRight $
        base12 [semV (copula cd) attr advP, mite $ CopulaHead (cd { copBound = True })]
        ++ closeUnclosed LeftSide (if closed then Satisfied else Unsatisfied)
        ++ liftUnclosedCompatible LeftSide
      (Verb verb, CommaSurrounded True _ (VerbalModifier attr True advP)) ->
        mergeLeft $ base12 [semV verb attr advP] ++ liftUnclosedCompatible RightSide ++ closeUnclosed LeftSide Satisfied

      (ConditionCompHead head, CommaSurrounded True _ (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (if cond=="if" then P.IfCondition else P.WhenCondition) cp] ++ liftUnclosedCompatible RightSide
      (Verb head, CommaSurrounded True _ (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (if cond=="if" then P.IfCondition else P.WhenCondition) cp] ++ liftUnclosedCompatible RightSide
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

      (QuestionVariants v kind, DashSurrounded True closed (Argument kind2 child)) | kind == kind2 ->
        mergeLeft $ base12 [semV v P.Variants child] ++ liftUnclosedCompatible RightSide
      (QuestionVariants v kind, CommaSurrounded True closed (Argument kind2 child)) | kind == kind2 ->
        mergeLeft $ base12 [semV v P.Variants child] ++ liftUnclosedCompatible RightSide

      (Clause cp, Word _ ".") ->
        mergeLeft $ base12 [semS cp P.Dot "true", mite $ Sentence cp] ++ closeUnclosed LeftSide Satisfied
      (Clause cp, Word _ "!") ->
        mergeLeft $ base12 [semS cp P.Exclamation_mark "true", mite $ Sentence cp] ++ closeUnclosed LeftSide Satisfied
      (Sentence cp, Word _ "\n") ->
        mergeRight $ base12 [semS cp P.ParagraphEnd "true"] ++ closeUnclosed LeftSide Satisfied
      (Argument Nom noun, Word cp "\n\n") ->
        right [semS cp P.SectionEnd "true", semT cp "situation", semS cp P.SituationKind "object", semV cp P.Content noun]
      (TopLevelQuestion cp, Word _ "?") -> left [semS cp P.Question_mark "true", mite $ Sentence cp]

      (DirectSpeechHead head Nothing, Colon "directSpeech" v) ->
        mergeLeft $ base12 [mite $ DirectSpeechHead head $ Just v, semV head P.Message v] ++ closeUnclosed LeftSide Satisfied

      (DirectSpeechDash v, Sentence cp) ->
        mergeLeft $ base12 [mite $ DirectSpeech cp, mite $ Sentence cp, semS cp P.DirectSpeech "true"] ++ closeUnclosed RightSide Satisfied

      (Colon "elaboration" _, Clause cp) ->
        mergeLeft $ base12 [mite $ Elaboration cp]
        ++ closeUnclosed RightSide Satisfied ++ liftUnclosedCompatible RightSide

      (Ellipsis v Nothing Nothing, rightCxt) | Just anchor <- ellipsisAnchor rightCxt ->
        left $ [mite $ Ellipsis v Nothing (Just rightCxt), semV v P.EllipsisAnchor2 anchor]

      _ -> []

questionableArguments env whContext = map (propagateUnclosed env) $ let
  leftPairs  = map (\m -> (m, cxt m)) $ leftCombined env
  rightPairs = map (\m -> (m, cxt m)) $ rightCombined env
  doInteract :: [(Mite, Construction)] -> [(Mite, Construction)] -> [MergeInfo]
  doInteract lp rp = do
    p1 <- lp
    p2 <- rp
    info@(MergeInfo mites side) <- interactQuestionable lp rp whContext p1 p2
    let childMite = select side (fst p2) (fst p1)
    let liftedWh = (select side rightCompatible leftCompatible) env childMite >>= \m3 -> case cxt m3 of
          Wh _ var -> withBase [m3] [mite $ WhInSitu var]
          _ -> []
    return $ if whContext then info else MergeInfo (mites ++ liftedWh) side
  normalResults = doInteract leftPairs rightPairs
  hybridVariants headSide = let
    childMites = (select headSide rightCombined leftCombined) env
    headPairs = select headSide leftPairs rightPairs
    interactConstituent childPairs = let
      resultInfos = select headSide (doInteract headPairs childPairs) (doInteract childPairs headPairs)
      in [mites | MergeInfo mites side <- resultInfos, side == headSide]
    unwrapHybrid childPairs combinations = let
      unwrappedLeft = childPairs >>= \(m, cc) -> case cc of SeqLeft c -> [(m, c)]; _ -> []
      unwrappedRight = childPairs >>= \(m, cc) -> case cc of SeqRight c -> [(m, c)]; _ -> []
      in
      if null unwrappedRight then [fromWhole ++ combo | combo <- combinations, fromWhole <- interactConstituent childPairs]
      else unwrapHybrid unwrappedLeft [fromRight ++ combo | combo <- combinations, fromRight <- interactConstituent unwrappedRight]
    combinations = unwrapHybrid (select headSide rightPairs leftPairs) [[]]
    in childMites >>= \seqMite -> case cxt seqMite of
      Conjunction (SeqData {seqHybrid=True}) ->
        map (\mites -> MergeInfo (withBase [seqMite] mites) headSide) combinations
      _ -> []
  in normalResults ++ hybridVariants LeftSide ++ hybridVariants RightSide

interactQuestionable leftPairs rightPairs whContext (m1, c1) (m2, c2) =
    let (left, right, base12) = mergeInfoHelpers m1 m2
    in case (c1, c2) of
      (ArgHead kind1 head, Argument kind2 arg) | kind1 == kind2 ->
        mergeLeft $ base12 (argVariants head arg leftPairs rightPairs) ++ reflexive leftPairs rightPairs ++ existentials leftPairs rightPairs
      (Argument kind2 arg, ArgHead kind1 head) | kind1 == kind2 ->
        mergeRight $ base12 (argVariants head arg rightPairs leftPairs) ++ reflexive rightPairs leftPairs ++ existentials rightPairs leftPairs
      (SemArgHead kind1 head, SemArgument kind2 arg _) | kind1 == kind2 ->
        mergeLeft $ base12 (argVariants head arg leftPairs rightPairs ++ optional [mite $ cxt m1]) ++ reflexive leftPairs rightPairs ++ existentials leftPairs rightPairs
      (SemArgument kind2 arg _, SemArgHead kind1 head) | kind1 == kind2 ->
        mergeRight $ base12 (argVariants head arg rightPairs leftPairs ++ optional [mite $ cxt m2]) ++ reflexive rightPairs leftPairs ++ existentials rightPairs leftPairs

      (Argument Nom v1, NomHead agr1 v2 Unsatisfied) -> leftPairs >>= \case
        (m3, AdjHead v3 Nom agr2) | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) ->
          mergeRight $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []
      (NomHead agr1 v2 Unsatisfied, Argument Nom v1) -> rightPairs >>= \case
        (m3, AdjHead v3 Nom agr2) | agree agr1 agr2 && v1 == v3 && not (contradict m2 m3) ->
          mergeLeft $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []

      -- todo nom + nomHead/copulaHead duplication
      (Argument Nom v1, CopulaHead (CopulaData { copSubj = subj, copAgr = agr, copula = v2, copCP = cp })) ->
        right $ [mite $ NomHead agr v1 Satisfied, mite $ Unify v1 subj] ++ (if whContext then [] else [mite $ Clause cp, mite $ Verb v2])
      (CopulaHead (CopulaData { copKind = kind, copAgr = agr, copSubj = subj, copula = v1, copCP = cp }), Argument Nom v2) | kind /= NPCopula || whContext ->
        left $ [mite $ NomHead agr v2 Satisfied, mite $ Unify v2 subj] ++ (if whContext then [] else [mite $ Clause cp, mite $ Verb v1])

      (Verb verb, VerbalModifier attr False advP) -> mergeLeft $ base12 [semV verb attr advP] ++ existentials leftPairs rightPairs
      (VerbalModifier attr needComma advP, Verb verb) -> mergeRight $
        base12 ([semV verb attr advP]
        ++ (if needComma && not whContext then [semS advP P.Isolation "comma", mite $ Unclosed LeftSide [advP]] else []))
        ++ existentials rightPairs leftPairs
      --todo remove verbalmod+(copula|verb) duplication
      (VerbalModifier attr needComma advP, CopulaHead cd) -> mergeRight $
        base12 ([semV (copula cd) attr advP, mite $ CopulaHead (cd { copBound = True })]
        ++ (if needComma && not whContext then [semS advP P.Isolation "comma", mite $ Unclosed LeftSide [advP]] else []))
        ++ existentials rightPairs leftPairs

      _ -> []

negationPropagation rightVar leftMites = leftMites >>= \m3 -> case cxt m3 of
  PendingNegation _ -> withBase [m3] [semS rightVar P.Negated "true", mite $ Negated rightVar]
  _ -> []

interactUnsorted env (m1, m2) = map (propagateUnclosed env) $
    let (left, right, base12) = mergeInfoHelpers m1 m2
    in case (cxt m1, cxt m2) of
      (Adj var2 attr adjCase agr1, AdjHead var nounCase agr2) | adjCase == nounCase && agree agr1 agr2 ->
        mergeRight $ base12 [semV var attr var2] ++ whPropagation m2 m1 (leftCompatible env m1) ++ negationPropagation var (leftCompatible env m1)
      (AdjHead var nounCase agr2, Adj var2 attr adjCase agr1) | adjCase == nounCase && agree agr1 agr2 ->
        mergeLeft $ base12 [semV var attr var2] ++ whPropagation m1 m2 (rightCompatible env m2)
      (CompositeAdj var2 attr adjCase agr1, AdjHead var nounCase agr2) | adjCase == nounCase && agree agr1 agr2 ->
        right [semV var attr var2]

      (Possessive adjCase agr1 child, AdjHead noun nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        GenHead h -> mergeRight $ withBase [m1,m2,m3] [mite $ Unify h child] ++ Seq.pullThyself (leftCompatible env m1) ++ whPropagation m1 m2 (leftCompatible env m1)
        _ -> []
      (GenHead v1, Argument Gen v2) -> left $ [mite $ Unify v1 v2] ++ whPropagation m1 m2 (rightCompatible env m2)

      (Relativizer wh, NomHead agr v2 Unsatisfied) -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        Clause cp -> mergeLeft $ withBase [m1,m2,m3] [mite $ Unify v2 wh, mite $ RelativeClause agr cp, semV cp P.Questioned wh]
        _ -> []
      -- todo relativizer + nomHead/copulaHead duplication
      (Relativizer wh, CopulaHead (CopulaData { copKind = kind, copAgr = agr, copSubj = v2, copCP = cp })) | kind /= NPCopula ->
        left $ [mite $ Unify v2 wh, mite $ RelativeClause agr cp, semV cp P.Questioned wh]
      (Relativizer wh, ArgHead Acc v2) -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        Clause cp -> mergeLeft $ withBase [m1,m2,m3] [mite $ Unify v2 wh, mite $ RelativeClause empty cp, semV cp P.Questioned wh]
        _ -> []

      (ConjEmphasis attr _, ConjEmphasizeable head) -> right [semS head attr "true"]

      (Adverb v, Verb head) -> right [mite $ Unify v head]
      --todo remove adverb+(copula|verb) duplication
      (Adverb head, CopulaHead cd) -> right [mite $ Unify head (copula cd), mite $ CopulaHead (cd { copBound = True })]

      (Verb head, Adverb v) -> left [mite $ Unify v head]

      (NounPhrase head, NounAdjunct attr False var) -> left [semV head attr var]

      (Quantifier kind1 agr1 v1, Argument kind2 v2) | kind1 == kind2 -> rightCompatible env m2 >>= \m3 -> case cxt m3 of
        AdjHead v3 kind3 agr2 | kind3 == kind1 && agree agr1 agr2 && v2 == v3 ->
          mergeLeft $ withBase [m1, m2, m3] [mite $ Unify v1 v2] ++ Seq.pullThyself (rightCompatible env m2)
        _ -> []

      (SemPreposition kind1 var1, Argument kind2 var2) | kind1 == kind2 -> left [mite $ Unify var1 var2]
      (PrepHead prep1 kind1 var1, Argument kind2 var2) | kind1 == kind2 ->
        let argMites = base12 ([mite $ Unify var1 var2]
                        ++ xorNonEmpty [[mite $ Argument (PP prep1 kind1) var1], adjunctMites, copulaVariants])
                       ++ whPropagation m1 m2 (rightCompatible env m2)
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
            v = makeV var1 "x"
            copulaCommon copulaType = copulaHead PPCopula empty copulaType False v
            copulaVariants = case (prep1, kind1) of
              ("u", Gen) -> copulaCommon "copula" ++ [semV (v "") P.Owner var1]
              ("na", Prep) -> copulaCommon "copula" ++ [semV (v "") P.Location_on var1]
              ("o", Prep) -> xor [copulaCommon "copula_about", copulaCommon "copula_talking_about"] ++ [semV (v "") P.Arg2 var1]
              _ -> []
            extra = Seq.pullThyself (rightCompatible env m2) ++ liftGen
            liftGen = rightCompatible env m2 >>= \m3 -> case cxt m3 of
              GenHead {} -> optional $ withBase [m3] [mite $ cxt m3]
              _ -> []
        in mergeLeft (argMites ++ extra)

      (Verb head, Elaboration child) -> left [semV head P.Elaboration child, mite $ Unclosed RightSide [child]]

      (emphasized@(ShortAdj _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
      (wh@(WhLeaf {}), Word _ "бишь") -> left [mite $ EmptyCxt wh]
      (wh@(WhLeaf {}), Word _ "это") -> left [mite $ EmptyCxt wh]

      (Verb v, Word _ "бы") -> left [semS v P.Irrealis "true"]
      (ModifierAdverb v1, AdverbModifiable v2) -> right [mite $ Unify v1 v2]

      (TenseHead v0, Tense v1) -> left [mite $ Unify v0 v1]
      (Tense v0, TenseHead v1) -> right [mite $ Unify v0 v1]

      (WhAsserter verb, Wh _ wh) -> right [mite $ ExistentialWh wh verb]

      (ConditionComp v0 s False, Clause cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]

      (ReasonComp v0 False, Clause cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]

      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left $ map mite wrapped
      
      (Conjunction    (SeqData {seqVar=v1, seqConj=",", seqHasLeft=False, seqHasRight=False}),
       Conjunction sd@(SeqData {seqVar=v2, seqConj="but", seqReady=False, seqHasRight=True})) ->
          right [mite $ Conjunction $ sd {seqReady=True}, mite $ Unify v1 v2]

      (Quote _ False, word@(Word {})) -> left [mite $ QuotedWord word False]
      (QuotedWord word False, Quote _ True) -> left [mite $ QuotedWord word True]
      (NounPhrase noun, QuotedWord (Word _ word) _) -> left [semS noun P.Name word]

      (Word _ "больше", Negated v) -> right [semS v P.Not_anymore "true"]
      (Negated v, Word _ "больше") -> left [semS v P.Not_anymore "true"]

      (Word _ "не", Complement cp) -> right [semS cp P.Negated "true", mite $ Complement cp]
      (Word ne "не", Wh _ v) -> right [mite $ ExistentialWh v ne, semS v P.Negated "true"]
      (Word _ "не", Negateable v) -> right $ xor [[semS v P.Negated "true", mite $ Negated v], [mite $ PendingNegation v]]
      (Word _ "не", Verb v) -> let
        negateDirectObject = rightCombined env >>= \m3 -> case cxt m3 of
          ArgHead Acc v -> let
            result = withBase [m1,m2,m3] [mite $ ArgHead Gen v] ++ colleagues
            colleagues = concat [withBase [m1,m2,m] [mite (cxt m)] | m <- rightCompatible env m2, contradict m m3]
            in result
          _ -> []
        in mergeRight $ base12 [semS v P.Negated "true", mite $ Negated v] ++ negateDirectObject

      (Word _ "тоже", Verb v) -> right [semS v P.Also "true"]
      (Complementizer cp1, Clause cp2) -> left [mite $ Unify cp1 cp2, mite $ Complement cp1]
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (FutureTense agr tense, ControlledInfinitive inf) -> right $ [mite $ Unify tense inf] ++ finiteClause agr True (makeV tense "")
      (RaisingVerb verb subj, Adj child _ Instr agr) -> left [semV child P.Arg1 subj, semV verb P.Theme child]
       
      _ -> []

argVariants headVar childVar headPairs childPairs = [mite $ Unify headVar childVar]

reflexive headPairs childPairs = headPairs >>= \case
    (m1, ReflexiveTarget target) -> childPairs >>= \case
      (m2, ReflexiveReference ref) -> withBase [m1,m2] [semV ref P.Target target]
      _ -> []
    _ -> []

existentials headPairs childPairs = headPairs >>= \case
  (m1, ModalityInfinitive v cp) -> childPairs >>= \case
    (m2, ExistentialWh whVar tensedVar) -> withBase [m1,m2] [semT cp "situation", mite $ Unify v tensedVar]
    _ -> []
  _ -> []

whPropagation headMite childMite childMites = childMites >>= \m3 -> case cxt m3 of
  Wh {} -> withBase [m3] [mite $ cxt m3]
  _ -> []
