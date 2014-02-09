module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions
import Constructor.Agreement
import Constructor.Tree
import qualified Constructor.Seq as Seq
import Debug.Trace

data MergeInfo = MergeInfo {mergeResult::[Mite], leftHeadedMerge::Bool} deriving (Show)

interactNodes:: Tree -> [Mite] -> [Mite] -> [MergeInfo]
interactNodes leftTree leftMites rightMites = if null whResults then noWh else whResults where
  noWh = interactNodesNoWh leftTree leftMites rightMites
  whResults = leftMites >>= \leftMite1 -> case cxt leftMite1 of
    Wh wh cp -> rightMites >>= \rightMite1 -> case cxt rightMite1 of
      Question cp2 verb ->
        let fillers = filter (not . leftHeadedMerge) noWh
            whLinks = withBase [leftMite1, rightMite1] [mite $ Unify cp cp2, semV cp "questioned" wh]
            infos = map (\ info -> MergeInfo (mergeResult info ++ whLinks) True) fillers
        in infos
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
    let left mites = [MergeInfo (withBase [m1, m2] mites) True]
        right mites = [MergeInfo (withBase [m1, m2] mites) False]
    in case (cxt m1, cxt m2) of
      (Adj _ adjCase agr1 property value, AdjHead var nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> 
        right [semS var property value]
      (AdjHead var nounCase agr2, Adj _ adjCase agr1 property value) | adjCase == nounCase && agree agr1 agr2 -> 
        left [semS var property value]
      (Possessive adjCase agr1 child, AdjHead noun nounCase agr2) | adjCase == nounCase && agree agr1 agr2 -> rightMites >>= \m3 -> case cxt m3 of
        GenHead h -> [MergeInfo [(mite $ Unify h child) {baseMites=[m1,m2,m3]}] False]
        _ -> []
      (GenHead v1, Argument Gen v2) -> left [mite $ Unify v1 v2] 
      (Argument Nom v1, NomHead agr1 v2) -> leftMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) -> 
          [MergeInfo (withBase [m1, m2, m3] [mite $ Unify v1 v2]) False]
        _ -> []
      (NomHead agr1 v2, Argument Nom v1) -> rightMites >>= \m3 -> case cxt m3 of
        AdjHead v3 Nom agr2 | agree agr1 agr2 && v1 == v3 && not (contradict m1 m3) -> 
          [MergeInfo (withBase [m1, m2, m3] [mite $ Unify v1 v2]) True]
        _ -> []
      (Adverb attr val, Verb head) -> right [semS head attr val]
      (ArgHead kind1 var1, Argument kind2 var2) | kind1 == kind2 -> left [mite $ Unify var1 var2]
      (Argument kind2 var2, ArgHead kind1 var1) | kind1 == kind2 -> right [mite $ Unify var1 var2]
      (PrepHead prep1 kind1 var1, Argument kind2 var2) | kind1 == kind2 ->
        let argMites = leftMites >>= \m3 -> case cxt m3 of
              Argument (PP prep3 kind3) var3 | prep3 == prep1 && kind1 == kind3  -> withBase [m1,m2,m3] [mite $ Unify var1 var2, mite $ Argument (PP prep3 kind3) var3]
              PrepCopula var3 | var1 == var3  -> withBase [m1,m2,m3] [mite $ Unify var1 var2]
              _ -> []
            adjunctMites = rightMites >>= \m3 -> case cxt m3 of
              PrepositionActivator prep3 kind3 cxts | prep3 == prep1 && kind3 == kind1 -> leftMites >>= \m4 -> case cxt m4 of
                ActivePreposition {} -> withBase [m1,m2,m3,m4] $ map mite cxts
                _ -> []
              _ -> []
        in [MergeInfo (argMites ++ adjunctMites) True]      
      (DirectSpeechHead head Nothing, Colon "directSpeech" v) -> left [mite $ DirectSpeechHead head $ Just v, semV head "message" v]
      --(DirectSpeechHead head (Just v), DirectSpeech v1) -> left [mite $ Unify v v1]
      (DirectSpeechDash v, Clause TopLevel cp) -> left [mite $ DirectSpeech cp, semS cp "directSpeech" "true"]
      (Colon "elaboration" _, Clause Subordinate cp) -> left [mite $ Elaboration cp]
      (Verb head, Elaboration child) -> left [semV head "elaboration" child, mite $ Unclosed (cxt m2)]
      (CompHead comp, CommaSurrounded True _ (Wh _ cp)) -> left [mite $ Unify comp cp]
      (CompHead comp, CommaSurrounded True _ (Complementizer cp)) -> left [mite $ Unify comp cp]
      (AdjHead noun _ _, CommaSurrounded True _ (Wh _ cp)) -> left [semV noun "relative" cp]
      
      (CommaSurrounded _ True (VerbalModifier attr True advP), Verb verb) -> right [semV verb attr advP]
      (Verb verb, VerbalModifier attr False advP) -> left [semV verb attr advP]
      (VerbalModifier attr False advP, Verb verb) -> right [semV verb attr advP]
      
      (QuestionVariants (Just v) Nothing, QuestionVariants Nothing (Just s)) -> left [mite $ QuestionVariants (Just v) (Just s)]
      (QuestionVariants (Just v) (Just _), Argument Nom child) -> left [semV v "variants" child]
      (emphasized@(ShortAdj _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
      
      (Copula v0, CopulaTense v1) -> left [mite $ Unify v0 v1]
      (CopulaTense v1, ModalityInfinitive v2) -> right [mite $ Unify v1 v2]
      
      (ConditionComp v0 s False, Clause Subordinate cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]
      (ConditionCompHead head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]
      (Verb head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]

      (ReasonComp v0 False, Clause Subordinate cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]
      (Verb head, CommaSurrounded True _ (ReasonComp cp _)) -> left [semV head "reason" cp]
      
      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left wrapped
      
      (Clause TopLevel cp, Word _ ".") -> left [semS cp "dot" "true"]
      (Conjunction v ",", Word _ "а") -> right [mite $ Conjunction v "but", semS v "conj" "but"]
      (SurroundingComma False _, toWrap) | isCommaSurroundable toWrap -> left [mite $ CommaSurrounded True False toWrap]
      (toWrap, SurroundingComma True _) | isCommaSurroundable toWrap -> right [mite $ CommaSurrounded False True toWrap]
      (CommaSurrounded True False cxt, SurroundingComma True _) -> left [mite $ CommaSurrounded True True cxt]
      (Quote _ False, word@(Word {})) -> left [mite $ QuotedWord word False]
      (QuotedWord word False, Quote _ True) -> left [mite $ QuotedWord word True]
      (AdjHead noun _ _, QuotedWord (Word _ word) _) -> left [semS noun "name" word]
      (Word _ "не", Verb v) -> right [semS v "negated" "true"]
      (Word _ "тоже", Verb v) -> right [semS v "also" "true"]
      (Complementizer cp1, Fact cp2) -> rightMites >>= \m3 -> case cxt m3 of
         Clause Subordinate cp3 | cp3 == cp2 ->
           [MergeInfo [(mite $ Unify cp1 cp2) { baseMites = [m1, m2, m3]}] True]
         _ -> []
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (RaisingVerb verb subj, Raiseable agr child) -> left [semV child "arg1" subj, semV verb "theme" child]
       
      (leftCxt@(VerbalModifier _ False _), Ellipsis v Nothing rightCxt) -> right [mite $ Ellipsis v (Just leftCxt) rightCxt]
      (Ellipsis v leftCxt Nothing, rightCxt@(Argument _ _)) -> left [mite $ Ellipsis v leftCxt (Just rightCxt)]
      _ -> []
  seqVariants = (if null seqRight then [] else [MergeInfo seqRight True]) ++ (if null seqLeft then [] else [MergeInfo seqLeft False])
  seqLeft = Seq.seqLeft leftTree leftMites rightMites
  seqRight = Seq.seqRight leftMites rightMites
