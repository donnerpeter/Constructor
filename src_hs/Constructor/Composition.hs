module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions
import Debug.Trace
import Data.Maybe

data MergeInfo = MergeInfo {mergeResult::[Mite], leftHeadedMerge::Bool} deriving (Show)

left mites = [(mites, True)]
right mites = [(mites, False)]

interactNodes:: [Mite] -> [Mite] -> [MergeInfo]
interactNodes leftMites rightMites = if null whResults then noWh else whResults where
  noWh = interactNodesNoWh leftMites rightMites
  whResults = leftMites >>= \leftMite1 -> case cxt leftMite1 of
    Wh wh cp -> rightMites >>= \rightMite1 -> case cxt rightMite1 of
      Question cp2 verb ->
        let fillers = filter (not . leftHeadedMerge) noWh
            whLinks = map (\mite -> mite { baseMites = [leftMite1, rightMite1] }) [mite $ Unify cp cp2, semV cp "questioned" wh]
            infos = map (\ info -> MergeInfo (mergeResult info ++ whLinks) True) fillers
        in infos
      _ -> []
    _ -> []

interactNodesNoWh leftMites rightMites =
  pairResults ++ compoundResults where
  pairResults = [ MergeInfo (map (\mite -> mite { baseMites = [leftMite, rightMite] }) mergeResult) leftHeaded | 
                            leftMite <- leftMites, 
                            rightMite <- rightMites, 
                            (mergeResult, leftHeaded) <- interactMites (cxt leftMite) (cxt rightMite)]
  compoundResults = leftMites >>= \leftMite1 -> case cxt leftMite1 of
    Elaboration head -> rightMites >>= \rightMite1 -> case cxt rightMite1 of
      Fact cp -> rightMites >>= \rightMite2 -> case cxt rightMite2 of
        SubordinateClause cp2 | cp == cp2 ->
          [MergeInfo [(semV head "elaboration" cp) { baseMites = [leftMite1, rightMite1, rightMite2]}] True]
        _ -> []
      _ -> []
    Conjunction seqV _ -> rightMites >>= \rightMite1 -> case cxt rightMite1 of
      TopLevelClause child ->
        let unhappy = filter (not . happy) rightMites
            wrapped = [(mite $ ElidedArgHead $ cxt m) {baseMites=[m]} | m <- unhappy]
        in
        [MergeInfo ([(semV seqV "member2" child) {baseMites = [leftMite1, rightMite1]}, (mite $ SeqRight seqV CP) {baseMites = [leftMite1, rightMite1]}] ++ wrapped) True]
      _ -> []
    TopLevelClause child -> rightMites >>= \rightMite1 -> case cxt rightMite1 of
      SeqRight seqV CP ->
        let unifications = concat [unifyMissingArgument mite1 mite2 | happyLeft <- filter happy leftMites, 
                                                                      mite1 <- baseMites happyLeft, 
                                                                      mite2 <- rightMites]
            unifyMissingArgument mite1 mite2 = case (cxt mite1, cxt mite2) of
              (NomHead v1, ElidedArgHead (NomHead v2)) -> [(mite $ Unify v1 v2) {baseMites=[mite1, mite2]}]
              _ -> []
        in
        [MergeInfo ([(semV seqV "member1" child) {baseMites = [leftMite1, rightMite1]}] ++ unifications) False]
      _ -> []
    _ -> []

interactMites:: Construction -> Construction -> [([Mite], Bool)]
interactMites leftMite rightMite = case (leftMite, rightMite) of
  (Adj _ adjCase property value, AdjHead var nounCase) | adjCase == nounCase -> right [semS var property value]
  (Possessive adjCase child, AdjHead noun nounCase) | adjCase == nounCase -> right [semV noun "arg1" child]
  (Argument Nom v1, NomHead v2) -> right [mite $ Unify v1 v2]
  (NomHead v2, Argument Nom v1) -> left [mite $ Unify v1 v2]
  (Adverb attr val, Verb head) -> right [semS head attr val]
  (ArgHead kind1 var1, Argument kind2 var2) | kind1 == kind2 -> left [mite $ Unify var1 var2]
  (Argument kind2 var2, ArgHead kind1 var1) | kind1 == kind2 -> right [mite $ Unify var1 var2]
  (PrepHead kind1 var1, Argument kind2 var2) | kind1 == kind2 -> left [mite $ Unify var1 var2]
  (Verb head, Word _ ":") -> left [mite $ Elaboration head]
  (CompHead comp, CommaSurrounded (Wh _ cp)) -> left [mite $ Unify comp cp]
  (ComeScalarly verb, ScalarAdverb order _) -> left [semS verb "order" order]
  (QuestionVariants (Just v) Nothing, QuestionVariants Nothing (Just s)) -> left [mite $ QuestionVariants (Just v) (Just s)]
  (QuestionVariants (Just v) (Just _), Argument Nom child) -> left [semV v "variants" child]
  (Conjunction v _, Argument Nom child) -> left [semV v "member2" child, mite $ SeqRight v Nom]
  (Conjunction v _, Possessive caze child) -> left [semV v "member2" child, mite $ SeqRight v (PossKind caze)]
  (Argument Nom child, SeqRight v Nom) -> right [semV v "member1" child, mite $ SeqFull v, mite $ Argument Nom v]
  (Possessive caze child, SeqRight v (PossKind caze2)) | caze == caze2 -> right [semV v "member1" child, mite $ SeqFull v, mite $ Possessive caze v]
  (emphasized@(ShortAdj _), Word _ "же") -> left [mite $ EmptyCxt emphasized]
  (Copula v0, CopulaTense v1) -> left [mite $ Unify v0 v1]
  (ConditionComp v0 s False, SubordinateClause cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]
  (Word _ ",", condComp@(ConditionComp cp s True)) -> left [mite $ CommaSurrounded condComp]
  (Word _ ",", comp@(Wh _ _)) -> left [mite $ CommaSurrounded comp]
  (Word _ "не", Verb v) -> right [semS v "negated" "true"]
  (Word _ "тоже", Verb v) -> right [semS v "also" "true"]
  (Verb head, CommaSurrounded (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]
  (Control slave, Infinitive inf) -> left [mite $ Unify slave inf]
  _ -> []
