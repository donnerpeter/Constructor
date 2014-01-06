module Constructor.Composition where
import Constructor.Constructions

data MergeInfo = MergeInfo {mergedMites::[Mite], satisfied::[Construction], leftHeadedMerge::Bool} deriving (Show)

interactMites:: Construction -> Construction -> [MergeInfo]
interactMites left right = case (left, right) of
  (Adj adjCase property value, Noun var nounCase) | adjCase == nounCase ->
    [MergeInfo [semS var property value] [left] False]
  (Noun child Nom, FiniteVerb head) -> 
    [MergeInfo [semV head "arg1" child] [left] False]
  (Adverb attr val, FiniteVerb head) ->
    [MergeInfo [semS head attr val] [] False]
  (ArgHead kind1 var1, Argument kind2 var2) | kind1 == kind2 ->
    [MergeInfo [mite $ Unify var1 var2] [] True]
  (FiniteVerb head, Word _ ":") ->
    [MergeInfo [Mite (Elaboration head) False] [] True]
  (Elaboration head, FiniteVerb child) ->
    [MergeInfo [semV head "elaboration" child] [] True]
  _ -> []
