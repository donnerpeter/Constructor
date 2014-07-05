module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Agreement
import Constructor.Tree
import Constructor.Util
import qualified Constructor.Seq as Seq

data MergeInfo = MergeInfo {mergeResult::[Mite], mergedHeadSide::Side} deriving (Show,Eq,Ord)
mergeLeft mites = [MergeInfo mites LeftSide]
mergeRight mites = [MergeInfo mites RightSide]

interactNodes:: Tree -> [Mite] -> [Mite] -> [MergeInfo]
interactNodes leftTree leftMites rightMites = if null whResults then noWh else whResults where

  seqVariants = map (propagateUnclosed leftMites rightMites) $ (if null seqRight then [] else mergeLeft seqRight) ++ (if null seqLeft then [] else mergeRight seqLeft)
  seqLeft = Seq.seqLeft leftTree leftMites rightMites
  seqRight = Seq.seqRight leftMites rightMites

  pairs = [(m1, m2) | m1 <- leftMites, isInteractive m1, m2 <- rightMites, isInteractive m2]
  questionable = pairs >>= questionableArguments leftMites rightMites
  nonQuestionable = (pairs >>= interactUnsorted leftMites rightMites) ++ (pairs >>= punctuationAware leftMites rightMites) ++ seqVariants

  noWh = questionable ++ nonQuestionable

  whResults = leftMites >>= \whMite -> let
    whIncompatible info = any (contradict whMite) (mergeResult info)
    fillGap cp whVar clauseMite =
        let fillers = filter (\info -> mergedHeadSide info == RightSide) questionable
            whLinks = withBase [whMite, clauseMite] $
              [semV cp "questioned" whVar, semT cp "question"] ++ xor [[mite $ Complement cp], [mite $ RelativeClause cp], [mite $ TopLevelQuestion cp]]
            infos = fillers >>= \ info -> mergeLeft (mergeResult info ++ whLinks)
        in infos ++ filter whIncompatible nonQuestionable
    in case cxt whMite of
      Wh whVar -> rightMites >>= \clauseMite -> case cxt clauseMite of
        Clause Interrogative cp -> fillGap cp whVar clauseMite
        ModalityInfinitive _ cp -> fillGap cp whVar clauseMite
        _ -> []
      _ -> []

isInteractive mite = case cxt mite of
  Sem {} -> False
  Unify {} -> False
  EmptyCxt {} -> False
  _ -> True

mergeInfoHelpers m1 m2 = ( \mites -> mergeLeft (base12 mites), \mites -> mergeRight (base12 mites), base12) where
  base12 = withBase [m1,m2]

propagateUnclosed leftMites rightMites info = info { mergeResult = mergeResult info ++ liftUnclosed childMites } where
  childMites = select (mergedHeadSide info) rightMites leftMites

liftUnclosed childMites = childMites >>= \m -> case cxt m of
  Unclosed RightSide _ -> withBase [m] $ [mite $ cxt m]
  _ -> []

punctuationAware leftMites rightMites (m1, m2) =
    let (left, right, base12) = mergeInfoHelpers m1 m2
        checkClosed closed v = xor $ map (\x -> [x]) $ liftUnclosed rightMites ++ (if closed then [] else base12 [mite $ Unclosed RightSide v])
    in case (cxt m1, cxt m2) of
      (AdjHead head _ _, CommaSurrounded True closed (NounAdjunct attr True var)) ->
        mergeLeft $ base12 [semV head attr var] ++ checkClosed closed var
      (CompHead comp, CommaSurrounded True closed (Complement cp)) ->
        mergeLeft $ base12 [mite $ Unify comp cp] ++ checkClosed closed cp
      (RelativeHead noun, CommaSurrounded True closed (RelativeClause cp)) ->
        mergeLeft $ base12 [semV noun "relative" cp] ++ checkClosed closed cp

      (CommaSurrounded _ _ (VerbalModifier attr True advP), Verb verb) -> right [semV verb attr advP]
      (Verb verb, CommaSurrounded True _ (VerbalModifier attr True advP)) -> left [semV verb attr advP]

      (ConditionCompHead head, CommaSurrounded True closed (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (cond++"Condition") cp] ++ checkClosed closed cp
      (Verb head, CommaSurrounded True closed (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (cond++"Condition") cp] ++ checkClosed closed cp
      (Verb head, CommaSurrounded True closed (ReasonComp cp _)) ->
        mergeLeft $ base12 [semV head "reason" cp] ++ checkClosed closed cp

      (SurroundingComma False _, toWrap) | isCommaSurroundable toWrap ->
        mergeLeft $ base12 [mite $ CommaSurrounded True False toWrap] ++ liftUnclosed rightMites
      (toWrap, SurroundingComma True _) | isCommaSurroundable toWrap -> right [mite $ CommaSurrounded False True toWrap]
      (CommaSurrounded True False cxt, SurroundingComma True _) -> left [mite $ CommaSurrounded True True cxt]

      (SurroundingDash False _, toWrap@(Argument {})) -> left [mite $ DashSurrounded True False toWrap]
      (DashSurrounded True False cxt, SurroundingDash True _) -> left [mite $ DashSurrounded True True cxt]

      (QuestionVariants v kind, DashSurrounded True closed (Argument kind2 child)) | kind == kind2 ->
        mergeLeft $ base12 [semV v "variants" child] ++ checkClosed closed child

      (Clause Declarative cp, Word _ ".") -> let
        closed = leftMites >>= \m -> case cxt m of
          Unclosed RightSide v -> withBase [m, m2] $ optional [mite $ Closed v]
          _ -> []
        in mergeLeft $ base12 [semS cp "dot" "true", mite $ Sentence cp] ++ closed
      (TopLevelQuestion cp, Word _ "?") -> left [semS cp "question_mark" "true", mite $ Sentence cp]

      (leftCxt@(VerbalModifier _ _ anchor), Ellipsis v Nothing rightCxt@(Just _)) ->
        right $ [mite $ Ellipsis v (Just leftCxt) rightCxt, semV v "ellipsisAnchor1" anchor]
                ++ (xor [[mite $ Clause Declarative v], [mite $ Clause Interrogative v]])
      (Ellipsis v leftCxt Nothing, rightCxt@(Argument _ anchor)) ->
        left $ [mite $ Ellipsis v leftCxt (Just rightCxt), semV v "ellipsisAnchor2" anchor]

      _ -> []

questionableArguments leftMites rightMites (m1, m2) = map (propagateUnclosed leftMites rightMites) $
    let (left, right, base12) = mergeInfoHelpers m1 m2
    in case (cxt m1, cxt m2) of
      (ArgHead kind1 head, Argument kind2 arg) | kind1 == kind2 -> left $ argVariants head arg leftMites rightMites
      (Argument kind2 arg, ArgHead kind1 head) | kind1 == kind2 -> right $ argVariants head arg rightMites leftMites

      (Argument Nom v1, NomHead agr1 v2 Unsatisfied) -> leftMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) ->
          mergeRight $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []
      (NomHead agr1 v2 Unsatisfied, Argument Nom v1) -> rightMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m2 m3) ->
          mergeLeft $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []

      (Verb verb, VerbalModifier attr False advP) -> left [semV verb attr advP]
      (VerbalModifier attr _ advP, Verb verb) -> right [semV verb attr advP]

      _ -> []

interactUnsorted leftMites rightMites (m1, m2) = map (propagateUnclosed leftMites rightMites) $
    let (left, right, base12) = mergeInfoHelpers m1 m2
    in case (cxt m1, cxt m2) of
      (Adj var2 adjCase agr1, AdjHead var nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> 
        right [mite $ Unify var var2]
      (AdjHead var nounCase agr2, Adj var2 adjCase agr1) | adjCase == nounCase && agree agr1 agr2 ->
        left [mite $ Unify var var2]
      (CompositeAdj var2 adjCase agr1, AdjHead var nounCase agr2) | adjCase == nounCase && agree agr1 agr2 ->
        right [semV var "components" var2]

      (Possessive adjCase agr1 child, AdjHead noun nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> rightMites >>= \m3 -> case cxt m3 of
        GenHead h -> mergeRight $ withBase [m1,m2,m3] $ [mite $ Unify h child] ++ Seq.pullThyself m1 leftMites
        _ -> []
      (GenHead v1, Argument Gen v2) -> left $ [mite $ Unify v1 v2] ++ whPropagation m1 m2 rightMites

      (ConjEmphasis attr _, Verb head) -> right [semS head attr "true"]

      (Adverb v, Verb head) -> right [mite $ Unify v head]
      (Verb head, Adverb v) -> left [mite $ Unify v head]
      (AdjHead head _ _, NounAdjunct attr False var) -> left [semV head attr var]

      (Quantifier kind1 agr1 v1, Argument kind2 v2) | kind1 == kind2 -> rightMites >>= \m3 -> case cxt m3 of
        AdjHead v3 kind3 agr2 | kind3 == kind1 && agree agr1 agr2 && v2 == v3 && not (contradict m2 m3) ->
          mergeLeft $ withBase [m1, m2, m3] [mite $ Unify v1 v2]
        _ -> []

      (SemPreposition kind1 var1, Argument kind2 var2) | kind1 == kind2 -> left [mite $ Unify var1 var2]
      (PrepHead prep1 kind1 var1, Argument kind2 var2) | kind1 == kind2 ->
        let argMites = leftMites >>= \m3 -> case cxt m3 of
              Argument (PP prep3 kind3) var3 | prep3 == prep1 && kind1 == kind3 ->
                withBase [m1,m2,m3] $ [mite $ Unify var1 var2, mite $ Argument (PP prep3 kind3) var3]
              Copula var3 -> withBase [m1,m2,m3] [mite $ Unify var1 var2]
              _ -> []
            adjunctMites = rightMites >>= \m3 -> case cxt m3 of
              PrepositionActivator prep3 kind3 _ innerCxt | prep3 == prep1 && kind3 == kind1 -> leftMites >>= \m4 -> case cxt m4 of
                ActivePreposition {} -> withBase [m1,m2,m3,m4] $ [mite innerCxt]
                _ -> []
              _ -> []
            extra = Seq.pullThyself m2 rightMites ++ Seq.liftArguments m2 rightMites ++ whPropagation m1 m2 rightMites
        in mergeLeft (argMites ++ adjunctMites ++ extra)
      (DirectSpeechHead head Nothing, Colon "directSpeech" v) -> left [mite $ DirectSpeechHead head $ Just v, semV head "message" v]
      --(DirectSpeechHead head (Just v), DirectSpeech v1) -> left [mite $ Unify v v1]
      (DirectSpeechDash v, Sentence cp) -> left [mite $ DirectSpeech cp, semS cp "directSpeech" "true"]
      (Colon "elaboration" _, Clause Declarative cp) -> left [mite $ Elaboration cp]
      (Verb head, Elaboration child) -> left [semV head "elaboration" child, mite $ Unclosed RightSide child]

      (emphasized@(ShortAdj _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
      (Verb v, Word _ "бы") -> left [semS v "irrealis" "true"]
      (Word _ "очень", adverb@(Adverb {})) -> right [mite $ adverb]
      
      (TenseHead v0, Tense v1) -> left [mite $ Unify v0 v1]
      (Tense v0, TenseHead v1) -> right [mite $ Unify v0 v1]

      (WhAsserter verb, Wh wh) -> right [mite $ ExistentialWh wh verb]

      (ConditionComp v0 s False, Clause Declarative cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]

      (ReasonComp v0 False, Clause Declarative cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]

      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left $ map mite wrapped
      
      (Conjunction (SeqData {seqVar=v1, seqConj=",", seqHasLeft=False, seqRightVar=Nothing}), Conjunction sd@(SeqData {seqVar=v2, seqConj="but", seqReady=False})) ->
          right [mite $ Conjunction $ sd {seqReady=True}, mite $ Unify v1 v2]

      (Quote _ False, word@(Word {})) -> left [mite $ QuotedWord word False]
      (QuotedWord word False, Quote _ True) -> left [mite $ QuotedWord word True]
      (AdjHead noun _ _, QuotedWord (Word _ word) _) -> left [semS noun "name" word]

      (Word _ "больше", Negated v) -> right [semS v "not_anymore" "true"]
      (Negated v, Word _ "больше") -> left [semS v "not_anymore" "true"]

      (Word _ "не", Complement cp) -> right [semS cp "negated" "true", mite $ Complement cp]
      (Word ne "не", Wh v) -> right [mite $ ExistentialWh v ne, semS v "negated" "true"]
      (Word _ "не", Verb v) -> let
        negateDirectObject = rightMites >>= \m3 -> case cxt m3 of
          ArgHead Acc v -> let
            result = withBase [m1,m2,m3] [mite $ ArgHead Gen v] ++ colleagues
            colleagues = concat [withBase [m1,m2,m] [mite (cxt m)] | m <- rightMites, not (contradict m m2), contradict m m3]
            in result
          _ -> []
        in mergeRight $ base12 [semS v "negated" "true", mite $ Negated v] ++ negateDirectObject

      (Word _ "тоже", Verb v) -> right [semS v "also" "true"]
      (Complementizer cp1, Clause Declarative cp2) -> left [mite $ Unify cp1 cp2, mite $ Complement cp1]
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (RaisingVerb verb subj, Raiseable agr child) -> left [semV child "arg1" subj, semV verb "theme" child]
       
      _ -> []

argVariants headVar childVar headMites childMites = [mite $ Unify headVar childVar] ++ reflexive ++ existentials where
  reflexive = headMites >>= \m1 -> case cxt m1 of
    ReflexiveTarget target -> childMites >>= \m2 -> case cxt m2 of
      ReflexiveReference ref -> withBase [m1,m2] [semV ref "target" target]
      _ -> []
    _ -> []
  existentials = headMites >>= \m1 -> case cxt m1 of
    ModalityInfinitive v cp -> childMites >>= \m2 -> case cxt m2 of
      ExistentialWh whVar tensedVar -> withBase [m1,m2] [semT cp "fact", mite $ Unify v tensedVar]
      _ -> []
    _ -> []

whPropagation headMite childMite childMites = childMites >>= \m3 -> case cxt m3 of
  Wh {} -> withBase [headMite, childMite, m3] [mite $ cxt m3]
  _ -> []
