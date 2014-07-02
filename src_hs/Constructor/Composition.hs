module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Agreement
import Constructor.Tree
import Constructor.Util
import qualified Constructor.Seq as Seq

data MergeInfo = MergeInfo {mergeResult::[Mite], mergedHeadSide::Side} deriving (Show,Eq,Ord)

interactNodes:: Tree -> [Mite] -> [Mite] -> [MergeInfo]
interactNodes leftTree leftMites rightMites = (if null whResults then noWh else whResults) >>= propagateBorder where
  noWh = interactNodesNoWh leftTree leftMites rightMites
  propagateBorder (MergeInfo mites side) = let
    parentBorders = []
    in [MergeInfo { mergeResult = mites ++ parentBorders, mergedHeadSide = side }]
  whResults = leftMites >>= \whMite -> let
    whIncompatible info = any (contradict whMite) (mergeResult info)
    fillGap cp whVar clauseMite =
        let fillers = filter (\info -> mergedHeadSide info == RightSide) noWh
            whLinks = withBase [whMite, clauseMite] $
              [semV cp "questioned" whVar, semT cp "question"] ++ xor [[mite $ Complement cp], [mite $ RelativeClause cp], [mite $ TopLevelQuestion cp]]
            infos = map (\ info -> MergeInfo (mergeResult info ++ whLinks) LeftSide) fillers
        in infos ++ filter whIncompatible noWh
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

interactNodesNoWh leftTree leftMites rightMites = pairVariants ++ seqVariants where
  leftInteractive = filter isInteractive leftMites
  rightInteractive = filter isInteractive rightMites
  pairVariants = concat [interactPair m1 m2 | m1 <- leftInteractive, m2 <- rightInteractive]
  interactPair m1 m2 =
    let left mites = [MergeInfo (withBase [m1, m2] mites) LeftSide]
        right mites = [MergeInfo (withBase [m1, m2] mites) RightSide]
    in case (cxt m1, cxt m2) of
      (Adj var2 adjCase agr1, AdjHead var nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> 
        right [mite $ Unify var var2]
      (AdjHead var nounCase agr2, Adj var2 adjCase agr1) | adjCase == nounCase && agree agr1 agr2 ->
        left [mite $ Unify var var2]
      (CompositeAdj var2 adjCase agr1, AdjHead var nounCase agr2) | adjCase == nounCase && agree agr1 agr2 ->
        right [semV var "components" var2]

      (Possessive adjCase agr1 child, AdjHead noun nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> rightMites >>= \m3 -> case cxt m3 of
        GenHead h -> [MergeInfo (withBase [m1,m2,m3] $ [mite $ Unify h child] ++ Seq.pullThyself m1 leftMites) RightSide]
        _ -> []
      (GenHead v1, Argument Gen v2) -> left $ [mite $ Unify v1 v2] ++ whPropagation m1 m2 rightMites

      (Argument Nom v1, NomHead agr1 v2 Unsatisfied) -> leftMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) -> 
          [MergeInfo (withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]) RightSide]
        _ -> []
      (NomHead agr1 v2 Unsatisfied, Argument Nom v1) -> rightMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m2 m3) ->
          [MergeInfo (withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 Satisfied]) LeftSide]
        _ -> []

      (ConjEmphasis attr _, Verb head) -> right [semS head attr "true"]

      (Adverb v, Verb head) -> right [mite $ Unify v head]
      (Verb head, Adverb v) -> left [mite $ Unify v head]
      (AdjHead head _ _, NounAdjunct attr False var) -> left [semV head attr var]
      (AdjHead head _ _, CommaSurrounded True _ (NounAdjunct attr True var)) -> left [semV head attr var]

      (Quantifier kind1 agr1 v1, Argument kind2 v2) | kind1 == kind2 -> rightMites >>= \m3 -> case cxt m3 of
        AdjHead v3 kind3 agr2 | kind3 == kind1 && agree agr1 agr2 && v2 == v3 && not (contradict m2 m3) ->
          [MergeInfo (withBase [m1, m2, m3] [mite $ Unify v1 v2]) LeftSide]
        _ -> []

      (ArgHead kind1 head, Argument kind2 arg) | kind1 == kind2 -> left $ argVariants head arg leftMites rightMites
      (Argument kind2 arg, ArgHead kind1 head) | kind1 == kind2 -> right $ argVariants head arg rightMites leftMites

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
        in [MergeInfo (argMites ++ adjunctMites ++ extra) LeftSide]
      (DirectSpeechHead head Nothing, Colon "directSpeech" v) -> left [mite $ DirectSpeechHead head $ Just v, semV head "message" v]
      --(DirectSpeechHead head (Just v), DirectSpeech v1) -> left [mite $ Unify v v1]
      (DirectSpeechDash v, Sentence cp) -> left [mite $ DirectSpeech cp, semS cp "directSpeech" "true"]
      (Colon "elaboration" _, Clause Declarative cp) -> left [mite $ Elaboration cp]
      (Verb head, Elaboration child) -> left [semV head "elaboration" child, mite $ Unclosed (cxt m2)]
      (CompHead comp, CommaSurrounded True _ (Complement cp)) -> left [mite $ Unify comp cp]
      (RelativeHead noun, CommaSurrounded True _ (RelativeClause cp)) -> left [semV noun "relative" cp]
      
      (CommaSurrounded _ _ (VerbalModifier attr True advP), Verb verb) -> right [semV verb attr advP]
      (Verb verb, CommaSurrounded True _ (VerbalModifier attr True advP)) -> left [semV verb attr advP]
      (Verb verb, VerbalModifier attr False advP) -> left [semV verb attr advP]
      (VerbalModifier attr _ advP, Verb verb) -> right [semV verb attr advP]

      (QuestionVariants v kind, DashSurrounded True closed (Argument kind2 child)) | kind == kind2 ->
        left $ [semV v "variants" child] ++ (if closed then [] else [mite $ Unclosed $ cxt m2])
      (emphasized@(ShortAdj _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
      (Verb v, Word _ "бы") -> left [semS v "irrealis" "true"]
      (Word _ "очень", adverb@(Adverb {})) -> right [mite $ adverb]
      
      (TenseHead v0, Tense v1) -> left [mite $ Unify v0 v1]
      (Tense v0, TenseHead v1) -> right [mite $ Unify v0 v1]

      (WhAsserter verb, Wh wh) -> right [mite $ ExistentialWh wh verb]

      (ConditionComp v0 s False, Clause Declarative cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]
      (ConditionCompHead head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]
      (Verb head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]

      (ReasonComp v0 False, Clause Declarative cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]
      (Verb head, CommaSurrounded True _ (ReasonComp cp _)) -> left [semV head "reason" cp]
      
      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left $ map mite wrapped
      
      (Clause Declarative cp, Word _ ".") -> let
        closed = edgeTrees RightSide leftTree >>= headMites >>= \m -> case cxt m of
          Unclosed c -> withBase [m, m2] $ optional [mite $ Closed c]
          _ -> []
        in [MergeInfo (withBase [m1,m2] [semS cp "dot" "true", mite $ Sentence cp] ++ closed) LeftSide]
      (TopLevelQuestion cp, Word _ "?") -> left [semS cp "question_mark" "true", mite $ Sentence cp]
      (Conjunction (SeqData {seqVar=v1, seqConj=",", seqHasLeft=False, seqRightVar=Nothing}), Conjunction sd@(SeqData {seqVar=v2, seqConj="but", seqReady=False})) ->
          right [mite $ Conjunction $ sd {seqReady=True}, mite $ Unify v1 v2]

      (SurroundingComma False _, toWrap) | isCommaSurroundable toWrap -> left [mite $ CommaSurrounded True False toWrap]
      (toWrap, SurroundingComma True _) | isCommaSurroundable toWrap -> right [mite $ CommaSurrounded False True toWrap]
      (CommaSurrounded True False cxt, SurroundingComma True _) -> left [mite $ CommaSurrounded True True cxt]

      (SurroundingDash False _, toWrap@(Argument {})) -> left [mite $ DashSurrounded True False toWrap]
      (DashSurrounded True False cxt, SurroundingDash True _) -> left [mite $ DashSurrounded True True cxt]

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
        in [MergeInfo (withBase [m1,m2] [semS v "negated" "true", mite $ Negated v] ++ negateDirectObject) RightSide]

      (Word _ "тоже", Verb v) -> right [semS v "also" "true"]
      (Complementizer cp1, Clause Declarative cp2) -> left [mite $ Unify cp1 cp2, mite $ Complement cp1]
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (RaisingVerb verb subj, Raiseable agr child) -> left [semV child "arg1" subj, semV verb "theme" child]
       
      (leftCxt@(VerbalModifier _ _ anchor), Ellipsis v Nothing rightCxt@(Just _)) -> right $ [mite $ Ellipsis v (Just leftCxt) rightCxt, semV v "ellipsisAnchor1" anchor]
        ++ (xor [[mite $ Clause Declarative v], [mite $ Clause Interrogative v]])
      (Ellipsis v leftCxt Nothing, rightCxt@(Argument _ anchor)) -> left $ [mite $ Ellipsis v leftCxt (Just rightCxt), semV v "ellipsisAnchor2" anchor]
      _ -> []
  seqVariants = (if null seqRight then [] else [MergeInfo seqRight LeftSide]) ++ (if null seqLeft then [] else [MergeInfo seqLeft RightSide])
  seqLeft = Seq.seqLeft leftTree leftMites rightMites
  seqRight = Seq.seqRight leftMites rightMites

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
