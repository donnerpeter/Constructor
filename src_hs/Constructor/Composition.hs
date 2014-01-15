module Constructor.Composition (interactMites, MergeInfo(..)) where
import Constructor.Constructions

data MergeInfo = MergeInfo {mergedMites::[Mite], satisfied::[Construction], leftHeadedMerge::Bool} deriving (Show)

left mites = MergeInfo mites [] True
right mites = MergeInfo mites [] False

interactMites:: Construction -> Construction -> [MergeInfo]
interactMites leftMite rightMite = case (leftMite, rightMite) of
  (Adj _ adjCase property value, AdjHead var nounCase) | adjCase == nounCase -> [right [semS var property value]]
  (Noun child Nom, FiniteVerb head) -> [right [semV head "arg1" child]]
  (Adverb attr val, FiniteVerb head) -> [right [semS head attr val]]
  (ArgHead kind1 var1, Argument kind2 var2) | kind1 == kind2 -> [left [mite $ Unify var1 var2]]
  (FiniteVerb head, Word _ ":") -> [left [Mite (Elaboration head) False]]
  (Elaboration head, FiniteVerb child) -> [left [semV head "elaboration" child]]
  (CompHead head, Word _ ",") -> [left [mite $ CompComma head]]
  (CompComma head, Wh _ cp) -> [left [mite $ Unify head cp]]
  (Wh wh cp, FiniteVerb verb) -> [left [semV cp "content" verb, semV verb "arg1" wh]]
  (ComeScalarly verb, ScalarAdverb order _) -> [left [semS verb "order" order, semT verb "COME_SCALARLY"]]
  (QuestionVariants (Just v) Nothing, QuestionVariants Nothing (Just s)) -> [left [mite $ QuestionVariants (Just v) (Just s)]]
  (QuestionVariants (Just v) (Just _), Noun child Nom) -> [left [semV v "variants" child]]
  _ -> []
