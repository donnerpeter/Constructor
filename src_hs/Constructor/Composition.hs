module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions

data MergeInfo = MergeInfo {mergeResult::[Mite], leftHeadedMerge::Bool, mergedMites::[Mite]} deriving (Show)

left mites = (mites, True)
right mites = (mites, False)

interactNodes:: [Mite] -> [Mite] -> [MergeInfo]
interactNodes leftMites rightMites =
  pairResults ++ compoundResults where
  pairResults = concat [
      [
        MergeInfo mergeResult leftHeaded [leftMite, rightMite] 
          | (mergeResult, leftHeaded) <- interactMites (cxt leftMite) (cxt rightMite)] 
      | leftMite <- leftMites, rightMite <- rightMites
    ]
  compoundResults = leftMites >>= \leftMite1 -> case cxt leftMite1 of
    Elaboration head -> rightMites >>= \rightMite1 -> case cxt rightMite1 of
      Fact cp -> rightMites >>= \rightMite2 -> case cxt rightMite2 of
        SubordinateClause cp2 | cp == cp2 -> [MergeInfo [semV head "elaboration" cp] True [leftMite1, rightMite1, rightMite2]]
        _ -> []
      _ -> []
    _ -> []

interactMites:: Construction -> Construction -> [([Mite], Bool)]
interactMites leftMite rightMite = case (leftMite, rightMite) of
  (Adj _ adjCase property value, AdjHead var nounCase) | adjCase == nounCase -> [right [semS var property value]]
  (Noun child Nom, FiniteVerb head) -> [right [semV head "arg1" child]]
  (Adverb attr val, FiniteVerb head) -> [right [semS head attr val]]
  (ArgHead kind1 var1, Argument kind2 var2) | kind1 == kind2 -> [left [mite $ Unify var1 var2]]
  (FiniteVerb head, Word _ ":") -> [left [mite $ Elaboration head]]
  (CompHead head, Word _ ",") -> [left [mite $ CompComma head]]
  (CompComma head, Wh _ cp) -> [left [mite $ Unify head cp]]
  (Wh wh cp2, Question cp verb) -> [left [mite $ Unify cp cp2, semV verb "arg1" wh, semV cp "questioned" wh]]
  (ComeScalarly verb, ScalarAdverb order _) -> [left [semS verb "order" order]]
  (QuestionVariants (Just v) Nothing, QuestionVariants Nothing (Just s)) -> [left [mite $ QuestionVariants (Just v) (Just s)]]
  (QuestionVariants (Just v) (Just _), Noun child Nom) -> [left [semV v "variants" child]]
  (Conjunction v _, Noun child Nom) -> [left [semV v "member2" child, mite $ SeqRight v]]
  (Noun child Nom, SeqRight v) -> [right [semV v "member1" child, mite $ SeqFull v, mite $ Noun v Nom]]
  _ -> []
