module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions
import Constructor.Mite
import Constructor.Agreement
import Constructor.Tree
import Constructor.Util
import qualified Constructor.Seq as Seq

data MergeInfo = MergeInfo {mergeResult::[Mite], mergedHeadSide::Side} deriving (Show,Eq,Ord)

interactNodes:: Tree -> [Mite] -> [Mite] -> [MergeInfo]
interactNodes leftTree leftMites rightMites = if null whResults then noWh else whResults where
  noWh = interactNodesNoWh leftTree leftMites rightMites
  whResults = leftMites >>= \whMite -> case cxt whMite of
    Wh wh cp -> rightMites >>= \rightMite1 -> case cxt rightMite1 of
      Clause Interrogative cp2 ->
        let fillers = filter (\info -> mergedHeadSide info == RightSide) noWh
            whLinks = withBase [whMite, rightMite1] $
              [mite $ Unify cp cp2, semV cp "questioned" wh] ++ xor [[mite $ Complement cp], [mite $ RelativeClause cp]]
            infos = map (\ info -> MergeInfo (mergeResult info ++ whLinks) LeftSide) fillers
            whIncompatible info = any (contradict whMite) (mergeResult info)
        in infos ++ filter whIncompatible noWh
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
        GenHead h -> [MergeInfo [(mite $ Unify h child) {baseMites=[m1,m2,m3]}] RightSide]
        _ -> []
      (GenHead v1, Argument Gen v2) -> left [mite $ Unify v1 v2] 

      (Argument Nom v1, NomHead agr1 v2 False) -> leftMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) -> 
          [MergeInfo (withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 True]) RightSide]
        _ -> []
      (NomHead agr1 v2 False, Argument Nom v1) -> rightMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) -> 
          [MergeInfo (withBase [m1, m2, m3] [mite $ Unify v1 v2, mite $ NomHead (commonAgr agr1 agr2) v2 True]) LeftSide]
        _ -> []

      (ConjEmphasis attr _, Verb head) -> right [semS head attr "true"]

      (Adverb attr val, Verb head) -> right [semS head attr val]
      (Verb head, Adverb attr val) -> left [semS head attr val]
      (AdjHead head _ _, NounAdjunct var) -> left [mite $ Unify head var]

      (ArgHead kind1 var1, Argument kind2 var2) | kind1 == kind2 -> left $ argVariants var1 var2 leftMites rightMites
      (Argument kind2 var2, ArgHead kind1 var1) | kind1 == kind2 -> right $ argVariants var1 var2 rightMites leftMites

      (PrepHead prep1 kind1 var1, Argument kind2 var2) | kind1 == kind2 ->
        let argMites = leftMites >>= \m3 -> case cxt m3 of
              Argument (PP prep3 kind3) var3 | prep3 == prep1 && kind1 == kind3  -> withBase [m1,m2,m3] [mite $ Unify var1 var2, mite $ Argument (PP prep3 kind3) var3]
              Copula var3 -> withBase [m1,m2,m3] [mite $ Unify var1 var2]
              _ -> []
            adjunctMites = rightMites >>= \m3 -> case cxt m3 of
              PrepositionActivator prep3 kind3 cxts | prep3 == prep1 && kind3 == kind1 -> leftMites >>= \m4 -> case cxt m4 of
                ActivePreposition {} -> withBase [m1,m2,m3,m4] $ map mite cxts
                _ -> []
              _ -> []
        in [MergeInfo (argMites ++ adjunctMites) LeftSide]
      (DirectSpeechHead head Nothing, Colon "directSpeech" v) -> left [mite $ DirectSpeechHead head $ Just v, semV head "message" v]
      --(DirectSpeechHead head (Just v), DirectSpeech v1) -> left [mite $ Unify v v1]
      (DirectSpeechDash v, Sentence cp) -> left [mite $ DirectSpeech cp, semS cp "directSpeech" "true"]
      (Colon "elaboration" _, Clause _ cp) -> left [mite $ Elaboration cp]
      (Verb head, Elaboration child) -> left [semV head "elaboration" child, mite $ Unclosed (cxt m2)]
      (CompHead comp, CommaSurrounded True _ (Complement cp)) -> left [mite $ Unify comp cp]
      (RelativeHead noun, CommaSurrounded True _ (RelativeClause cp)) -> left [semV noun "relative" cp]
      
      (CommaSurrounded _ True (VerbalModifier attr True advP), Verb verb) -> right [semV verb attr advP]
      (Verb verb, CommaSurrounded True _ (VerbalModifier attr True advP)) -> left [semV verb attr advP]
      (Verb verb, VerbalModifier attr False advP) -> left [semV verb attr advP]
      (VerbalModifier attr False advP, Verb verb) -> right [semV verb attr advP]
      
      (QuestionVariants (Just v) Nothing, QuestionVariants Nothing (Just s)) -> left [mite $ QuestionVariants (Just v) (Just s)]
      (QuestionVariants (Just v) (Just _), Argument Nom child) -> left [semV v "variants" child]
      (emphasized@(ShortAdj _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
      (Verb v, Word _ "бы") -> left [semS v "irrealis" "true"]
      (Word _ "очень", adverb@(Adverb attr val)) -> right [mite $ adverb]
      
      (Copula v0, CopulaTense v1) -> left [mite $ Unify v0 v1]
      (CopulaTense v0, Copula v1) -> right [mite $ Unify v0 v1]
      (CopulaTense v1, ModalityInfinitive v2) -> right [mite $ Unify v1 v2]
      
      (ConditionComp v0 s False, Clause Declarative cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]
      (ConditionCompHead head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]
      (Verb head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]

      (ReasonComp v0 False, Clause Declarative cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]
      (Verb head, CommaSurrounded True _ (ReasonComp cp _)) -> left [semV head "reason" cp]
      
      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left $ map mite wrapped
      
      (Clause Declarative cp, Word _ ".") -> left [semS cp "dot" "true", mite $ Sentence cp]
      (Conjunction (SeqData {seqVar=v1, seqConj=",", seqHasLeft=False, seqHasRight=False}), Conjunction sd@(SeqData {seqVar=v2, seqConj="but", seqReady=False})) ->
          right [mite $ Conjunction $ sd {seqReady=True}, mite $ Unify v1 v2]
      (SurroundingComma False _, toWrap) | isCommaSurroundable toWrap -> left [mite $ CommaSurrounded True False toWrap]
      (toWrap, SurroundingComma True _) | isCommaSurroundable toWrap -> right [mite $ CommaSurrounded False True toWrap]
      (CommaSurrounded True False cxt, SurroundingComma True _) -> left [mite $ CommaSurrounded True True cxt]
      (Quote _ False, word@(Word {})) -> left [mite $ QuotedWord word False]
      (QuotedWord word False, Quote _ True) -> left [mite $ QuotedWord word True]
      (AdjHead noun _ _, QuotedWord (Word _ word) _) -> left [semS noun "name" word]
      (Word _ "не", Complement cp) -> right [semS cp "negated" "true", mite $ Complement cp]
      (Word _ "не", Verb v) -> right [semS v "negated" "true"]
      (Word _ "тоже", Verb v) -> right [semS v "also" "true"]
      (Complementizer cp1, Clause Declarative cp2) -> left [mite $ Unify cp1 cp2, mite $ Complement cp1]
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (RaisingVerb verb subj, Raiseable agr child) -> left [semV child "arg1" subj, semV verb "theme" child]
       
      -- todo take all ellipsis anchor alternatives into account
      (leftCxt@(VerbalModifier _ False _), Ellipsis v Nothing rightCxt) -> right [mite $ Ellipsis v (Just leftCxt) rightCxt]
      (Ellipsis v leftCxt Nothing, rightCxt@(Argument _ _)) -> left [mite $ Ellipsis v leftCxt (Just rightCxt)]
      _ -> []
  seqVariants = (if null seqRight then [] else [MergeInfo seqRight LeftSide]) ++ (if null seqLeft then [] else [MergeInfo seqLeft RightSide])
  seqLeft = Seq.seqLeft leftTree leftMites rightMites
  seqRight = Seq.seqRight leftMites rightMites

argVariants v1 v2 headMites childMites = [mite $ Unify v1 v2] ++ reflexive where
  reflexive = headMites >>= \m1 -> case cxt m1 of
    ReflexiveTarget target -> childMites >>= \m2 -> case cxt m2 of
      ReflexiveReference ref -> withBase [m1,m2] [semV ref "target" target]
      _ -> []
    _ -> []