module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Variable
import Constructor.Agreement
import Constructor.Tree
import Constructor.Util
import Constructor.Lexicon
import qualified Constructor.Seq as Seq
import qualified Constructor.LinkedSet as LS

data MergeInfo = MergeInfo {mergeResult::[Mite], mergedHeadSide::Side} deriving (Show,Eq,Ord)
mergeLeft mites = [MergeInfo mites LeftSide]
mergeRight mites = [MergeInfo mites RightSide]

interactNodes:: Tree -> [Mite] -> [Mite] -> [MergeInfo]
interactNodes leftTree leftMites rightMites = {-traceIt ("    interact") $ -}if null whResults then noWh else whResults where

  seqVariants = map (propagateUnclosed leftMites rightMites) $ (if null seqRight then [] else mergeLeft seqRight) ++ (if null seqLeft then [] else mergeRight seqLeft)
  seqLeft = Seq.seqLeft leftTree leftMites rightMites
  seqRight = Seq.seqRight leftMites rightMites

  pairs = [(m1, m2) | m1 <- leftMites, isInteractive m1, m2 <- rightMites, isInteractive m2]
  questionable whContext = pairs >>= questionableArguments leftMites rightMites whContext
  nonQuestionable = (pairs >>= interactUnsorted leftMites rightMites)
                 ++ (pairs >>= punctuationAware leftMites rightMites)
                 ++ seqVariants
                 ++ ellipsisLeftVariants leftMites rightMites
  noWh = questionable False ++ nonQuestionable

  whResults = leftMites >>= \whMite -> let
    whIncompatible info = any (contradict whMite) (mergeResult info)
    fillGap cp whVar clauseMite =
        let fillers = filter (\info -> mergedHeadSide info == RightSide) $ questionable True
            whLinks = withBase [whMite, clauseMite] $
              [semV cp "questioned" whVar, semT cp "situation"] ++ xor [[mite $ Complement cp], [mite $ RelativeClause cp], [mite $ TopLevelQuestion cp]]
            infos = fillers >>= \ info -> mergeLeft (mergeResult info ++ whLinks)
        in infos ++ filter whIncompatible nonQuestionable
    in case cxt whMite of
      Wh whVar -> rightMites >>= \clauseMite -> case cxt clauseMite of
        Clause cp -> fillGap cp whVar clauseMite
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

propagateUnclosed leftMites rightMites (MergeInfo mites side) = MergeInfo (mites ++ liftUnclosed (invert side) childMites) side where
  childMites = filter (not . contradictResult) $ select side rightMites leftMites
  contradictResult mite = any (contradict mite) mites

liftUnclosed side childMites = childMites >>= \m -> case cxt m of
  Unclosed s _ | s == side -> withBase [m] $ [mite $ cxt m]
  _ -> []

ellipsisLeftVariants leftMites rightMites = if null result then [] else mergeRight $ LS.removeDups result where
  result = rightMites >>= \m2 -> case cxt m2 of
    Ellipsis v Nothing rightCxt@(Just _) -> leftMites >>= \m1 -> case ellipsisAnchor (cxt m1) of
      Just anchor -> withBase [m1,m2] [mite $ Ellipsis v (Just $ cxt m1) rightCxt]
        ++ [semV v "ellipsisAnchor1" anchor, mite $ Clause v]
        ++ liftUnclosed LeftSide (filter (not . contradict m1) leftMites)
      _ -> []
    _ -> []

ellipsisAnchor (VerbalModifier _ _ v) = Just v
ellipsisAnchor (Argument _ v) = Just v
ellipsisAnchor (SemArgument _ _ v) = Just v
ellipsisAnchor (Adj v _ _) = Just v
ellipsisAnchor _ = Nothing

punctuationAware leftMites rightMites (m1, m2) =
    let (left, right, base12) = mergeInfoHelpers m1 m2
        compatibleChildren side = filter (not. contradict (select side m1 m2)) $ select side leftMites rightMites
        liftUnclosedCompatible side = liftUnclosed side (compatibleChildren side)
        addUnclosed side v = let
          baseMite = select side m2 m1
          lifted = compatibleChildren side >>= \m -> case cxt m of
            Unclosed s vars | s == side -> withBase [m,baseMite] [mite $ Unclosed side (v:vars)]
            _ -> []
          in if null lifted then withBase [baseMite] [mite $ Unclosed side [v]] else lifted
        closeUnclosed side satisfied = (select side leftMites rightMites) >>= \m -> case cxt m of
          Unclosed s vars | s == invert side -> withBase [m, m2] $
            optional $ [mite $ Closed vars]
            ++ (if satisfied == Satisfied then map (\v -> semS v (select side "left" "right" ++ "Isolated") "true") vars else [])
          _ -> []
    in case (cxt m1, cxt m2) of
      (AdjHead head _ _, CommaSurrounded True _ (NounAdjunct attr True var)) -> mergeLeft $
        base12 [semV head attr var] ++ liftUnclosedCompatible RightSide
      (CompHead comp, CommaSurrounded True _ (Complement cp)) -> mergeLeft $
        base12 [mite $ Unify comp cp] ++ liftUnclosedCompatible RightSide
      (RelativeHead noun, CommaSurrounded True _ (RelativeClause cp)) -> mergeLeft $
        base12 [semV noun "relative" cp] ++ liftUnclosedCompatible RightSide

      (CommaSurrounded _ closed (VerbalModifier attr True advP), Verb verb) -> mergeRight $
        base12 [semV verb attr advP]
        ++ closeUnclosed LeftSide (if closed then Satisfied else Unsatisfied)
        ++ liftUnclosedCompatible LeftSide
      (Verb verb, CommaSurrounded True _ (VerbalModifier attr True advP)) ->
        mergeLeft $ base12 [semV verb attr advP] ++ liftUnclosedCompatible RightSide

      (ConditionCompHead head, CommaSurrounded True _ (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (cond++"Condition") cp] ++ liftUnclosedCompatible RightSide
      (Verb head, CommaSurrounded True _ (ConditionComp cp cond _)) ->
        mergeLeft $ base12 [semV head (cond++"Condition") cp] ++ liftUnclosedCompatible RightSide
      (Verb head, CommaSurrounded True _ (ReasonComp cp _)) ->
        mergeLeft $ base12 [semV head "reason" cp] ++ liftUnclosedCompatible RightSide

      (SurroundingComma _, toWrap) | Just v <- getCommaSurroundableVar toWrap -> mergeLeft $
        base12 [mite $ CommaSurrounded True False toWrap, semS v "isolation" "comma", semS v "leftIsolated" "true"]
        ++ addUnclosed RightSide v
      (toWrap, SurroundingComma _) | Just v <- getCommaSurroundableVar toWrap -> mergeRight $
        base12 [mite $ CommaSurrounded False True toWrap, semS v "isolation" "comma", semS v "rightIsolated" "true"]
        ++ addUnclosed LeftSide v
      (CommaSurrounded True False cxt, SurroundingComma _) ->
        mergeLeft $ base12 [mite $ CommaSurrounded True True cxt] ++ closeUnclosed LeftSide Satisfied

      (SurroundingDash _, toWrap@(Argument _ v)) -> mergeRight $
        base12 [mite $ DashSurrounded True False toWrap, semS v "isolation" "dash", semS v "leftIsolated" "true"] ++ addUnclosed RightSide v
      (DashSurrounded True False cxt, SurroundingDash _) ->
        mergeLeft $ base12 [mite $ DashSurrounded True True cxt] ++ closeUnclosed LeftSide Satisfied

      (QuestionVariants v kind, DashSurrounded True closed (Argument kind2 child)) | kind == kind2 ->
        mergeLeft $ base12 [semV v "variants" child] ++ liftUnclosedCompatible RightSide
      (QuestionVariants v kind, CommaSurrounded True closed (Argument kind2 child)) | kind == kind2 ->
        mergeLeft $ base12 [semV v "variants" child] ++ liftUnclosedCompatible RightSide

      (Clause cp, Word _ ".") ->
        mergeLeft $ base12 [semS cp "dot" "true", mite $ Sentence cp] ++ closeUnclosed LeftSide Satisfied
      (TopLevelQuestion cp, Word _ "?") -> left [semS cp "question_mark" "true", mite $ Sentence cp]

      (DirectSpeechHead head Nothing, Colon "directSpeech" v) ->
        mergeLeft $ base12 [mite $ DirectSpeechHead head $ Just v, semV head "message" v] ++ closeUnclosed LeftSide Satisfied

      (DirectSpeechDash v, Sentence cp) ->
        mergeLeft $ base12 [mite $ DirectSpeech cp, semS cp "directSpeech" "true"] ++ closeUnclosed RightSide Satisfied

      (Ellipsis v Nothing Nothing, rightCxt) | Just anchor <- ellipsisAnchor rightCxt ->
        left $ [mite $ Ellipsis v Nothing (Just rightCxt), semV v "ellipsisAnchor2" anchor]

      _ -> []

questionableArguments leftMites rightMites whContext (m1, m2) = map (propagateUnclosed leftMites rightMites) $
    let (left, right, base12) = mergeInfoHelpers m1 m2
    in case (cxt m1, cxt m2) of
      (ArgHead kind1 head, Argument kind2 arg) | kind1 == kind2 -> left $ argVariants head arg leftMites rightMites
      (Argument kind2 arg, ArgHead kind1 head) | kind1 == kind2 -> right $ argVariants head arg rightMites leftMites
      (SemArgHead kind1 head, SemArgument kind2 arg _) | kind1 == kind2 -> left $ argVariants head arg leftMites rightMites
      (SemArgument kind2 arg _, SemArgHead kind1 head) | kind1 == kind2 -> right $ argVariants head arg rightMites leftMites

      (Argument Nom v1, NomHead agr1 v2 Unsatisfied) -> leftMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) ->
          mergeRight $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []
      (NomHead agr1 v2 Unsatisfied, Argument Nom v1) -> rightMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m2 m3) ->
          mergeLeft $ withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]
        _ -> []

      (Verb verb, VerbalModifier attr False advP) -> left $ [semV verb attr advP] ++ existentials leftMites rightMites
      (VerbalModifier attr needComma advP, Verb verb) -> right $
        [semV verb attr advP]
        ++ (if needComma && not whContext then [semS advP "isolation" "comma", mite $ Unclosed LeftSide [advP]] else [])
        ++ existentials rightMites leftMites

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
        GenHead h -> mergeRight $ withBase [m1,m2,m3] $ [mite $ Unify h child] ++ Seq.pullThyself m1 leftMites ++ whPropagation m1 m2 leftMites
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
                withBase [m1,m2,m3] $ [mite $ Unify var1 var2] ++ xor [[mite $ Argument (PP prep3 kind3) var3], adjunctMites]
              Copula var3 | not (contradict m1 m3) -> withBase [m1,m2,m3] [mite $ Unify var1 var2]
              _ -> []
            adjunctMites = case (prep1, kind1) of
              ("k", Dat) -> semArg Direction "goal_to" var2
              ("po", Dat) -> xor [[mite $ VerbalModifier "accordingTo" True var2],
                                  [mite $ NounAdjunct "accordingTo" True var2],
                                  [mite $ VerbalModifier "optativeModality" True var2]]
              ("s", Instr) -> [mite $ VerbalModifier "mood" False var2]
              ("s", Gen) -> xor [[mite $ NounAdjunct "source" False var2], [mite $ VerbalModifier "source" False var2]]
              ("v", Acc) -> semArg Direction "goal_in" var2
              ("v", Prep) -> [mite $ VerbalModifier "condition" False var2]
              ("na", Acc) -> semArg Direction "goal_on" var2
              ("na", Prep) -> [mite $ NounAdjunct "location" False var2]
              _ -> []
            extra = Seq.pullThyself m2 rightMites ++ Seq.liftArguments m2 rightMites ++ whPropagation m1 m2 rightMites
        in mergeLeft (argMites ++ extra)

      (Colon "elaboration" _, Clause cp) -> left [mite $ Elaboration cp]
      (Verb head, Elaboration child) -> left [semV head "elaboration" child, mite $ Unclosed RightSide [child]]

      (emphasized@(ShortAdj _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
      (Verb v, Word _ "бы") -> left [semS v "irrealis" "true"]
      (Word _ "очень", adverb@(Adverb {})) -> right [mite $ adverb]
      
      (TenseHead v0, Tense v1) -> left [mite $ Unify v0 v1]
      (Tense v0, TenseHead v1) -> right [mite $ Unify v0 v1]

      (WhAsserter verb, Wh wh) -> right [mite $ ExistentialWh wh verb]

      (ConditionComp v0 s False, Clause cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]

      (ReasonComp v0 False, Clause cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]

      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left $ map mite wrapped
      
      (Conjunction    (SeqData {seqVar=v1, seqConj=",", seqHasLeft=False, seqRightVar=Nothing}),
       Conjunction sd@(SeqData {seqVar=v2, seqConj="but", seqReady=False, seqRightVar=Just _})) ->
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
      (Complementizer cp1, Clause cp2) -> left [mite $ Unify cp1 cp2, mite $ Complement cp1]
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (RaisingVerb verb subj, Raiseable agr child) -> left [semV child "arg1" subj, semV verb "theme" child]
       
      _ -> []

argVariants headVar childVar headMites childMites = [mite $ Unify headVar childVar] ++ reflexive ++ existentials headMites childMites where
  reflexive = headMites >>= \m1 -> case cxt m1 of
    ReflexiveTarget target -> childMites >>= \m2 -> case cxt m2 of
      ReflexiveReference ref -> withBase [m1,m2] [semV ref "target" target]
      _ -> []
    _ -> []

existentials headMites childMites = headMites >>= \m1 -> case cxt m1 of
  ModalityInfinitive v cp -> childMites >>= \m2 -> case cxt m2 of
    ExistentialWh whVar tensedVar -> withBase [m1,m2] [semT cp "situation", mite $ Unify v tensedVar]
    _ -> []
  _ -> []

whPropagation headMite childMite childMites = childMites >>= \m3 -> case cxt m3 of
  Wh {} -> withBase [headMite, childMite, m3] [mite $ cxt m3]
  _ -> []
