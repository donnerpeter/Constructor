module Constructor.Composition (interactNodes, MergeInfo(..)) where
import Constructor.Constructions
import Constructor.Agreement
import Constructor.Variable
import Constructor.Tree
import Debug.Trace
import Data.Maybe
import Data.List
import Data.Data
import qualified Data.Map as Map
import qualified Constructor.LinkedSet as LS

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

withBase base mites = map (\m -> m {baseMites=base}) mites

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
      (DirectSpeechDash v, TopLevelClause cp) -> left [mite $ DirectSpeech cp, semS cp "directSpeech" "true"]
      (Colon "elaboration" _, SubordinateClause cp) -> left [mite $ Elaboration cp]
      (Verb head, Elaboration child) -> left [semV head "elaboration" child]
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
      
      (ConditionComp v0 s False, SubordinateClause cp) -> left [mite $ Unify v0 cp, mite $ ConditionComp v0 s True]
      (ConditionCompHead head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]
      (Verb head, CommaSurrounded True _ (ConditionComp cp cond _)) -> left [semV head (cond++"Condition") cp]

      (ReasonComp v0 False, SubordinateClause cp) -> left [mite $ Unify v0 cp, mite $ ReasonComp v0 True]
      (Verb head, CommaSurrounded True _ (ReasonComp cp _)) -> left [semV head "reason" cp]
      
      (TwoWordCxt s1 True wrapped _, TwoWordCxt s2 False _ _) | s1 == s2 -> left wrapped
      
      (TopLevelClause cp, Word _ ".") -> left [semS cp "dot" "true"]
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
         SubordinateClause cp3 | cp3 == cp2 ->
           [MergeInfo [(mite $ Unify cp1 cp2) { baseMites = [m1, m2, m3]}] True]
         _ -> []
      (Control slave, ControlledInfinitive inf) -> left [mite $ Unify slave inf]
      (RaisingVerb verb subj, Raiseable agr child) -> left [semV child "arg1" subj, semV verb "theme" child]
       
      (leftCxt@(VerbalModifier _ False _), Ellipsis v Nothing rightCxt) -> right [mite $ Ellipsis v (Just leftCxt) rightCxt]
      (Ellipsis v leftCxt Nothing, rightCxt@(Argument _ _)) -> left [mite $ Ellipsis v leftCxt (Just rightCxt)]
      _ -> []
  seqVariants = (if null seqRight then [] else [MergeInfo seqRight True]) ++ (if null seqLeft then [] else [MergeInfo seqLeft False])
  hasSeqFull = flip any rightMites $ \mite -> case cxt mite of SeqFull {} -> True; _ -> False
  seqRight = leftMites >>= \m1 -> case cxt m1 of
    Conjunction v _ -> if hasSeqFull then [] else rightMites >>= \m2 -> case cxt m2 of
      Argument kind child -> withBase [m1,m2] [semV v "member2" child, mite $ SeqRight v kind]
      Possessive caze agr child -> withBase [m1,m2] [semV v "member2" child, mite $ SeqRight v (PossKind caze agr)]
      TopLevelClause child ->                         
        let unhappy = filter elideable $ filter (not . happy) rightMites
            wrapped = [(mite $ ElidedArgHead $ cxt m) {baseMites = [m,m1,m2]} | m <- unhappy]
            elideable mite = case cxt mite of
              NomHead {} -> True
              _ -> False
            emptyCounterparts = rightMites >>= \m -> case cxt m of
              EmptyCxt w | any (contradict m) wrapped -> [(mite $ cxt m) {baseMites = [m,m1,m2]}]
              _ -> []
        in
          withBase [m1, m2] [semV v "member2" child, mite $ SeqRight v CP] ++ wrapped ++ emptyCounterparts
      Ellipsis child _ _ -> withBase [m1,m2] [semV v "member2" child, mite $ SeqRight v CP]
      _ -> []
    _ -> []      
  seqLeft = concat [interactSeqLeft m1 m2 | m1 <- leftInteractive, m2 <- rightInteractive]
  interactSeqLeft m1 m2 = case (cxt m1, cxt m2) of
    (Argument kind child, SeqRight v kind2) | kind == kind2 -> withBase [m1,m2] [semV v "member1" child, mite $ SeqFull v, mite $ Argument kind v]
    (Possessive caze1 agr1 child, SeqRight v (PossKind caze2 agr2)) | caze1 == caze2 && agree agr1 agr2 -> 
        withBase [m1,m2] [semV v "member1" child, mite $ SeqFull v, mite $ Possessive caze1 (commonAgr agr1 agr2) v]
    (TopLevelClause child, SeqRight seqV CP) ->
       let unifications = concat [unifyMissingArgument mite1 mite2 | mite1 <- unhappyLeft ++ happyBases, 
                                                                     mite2 <- rightMites]
           happyLeft = filter happy leftMites
           happyBases = LS.removeDups $ concat $ map baseMites happyLeft
           unhappyLeft = filter (not . happy) leftMites
           unifyMissingArgument aux1 aux2 = case (cxt aux1, cxt aux2) of
             (NomHead agr1 v1, ElidedArgHead (NomHead agr2 v2)) | agree agr1 agr2 -> withBase [m1,m2,aux1,aux2] [mite $ Unify v1 v2]
             _ -> []
           ellipsisVariants = baseMites m2 >>= \m3 -> case cxt m3 of
             Ellipsis ellipsisVar (Just e1) (Just e2) -> processEllipsis ellipsisVar e1 e2 leftTree 
             _ -> []
       in
         traceShow ellipsisVariants $ withBase [m1, m2] [semV seqV "member1" child, mite $ TopLevelClause seqV] ++ unifications
    _ -> []

data AnchorMapping = AnchorMapping {-original-} Mite Variable {-anchor-} Construction Variable

checkOriginal ::  Construction -> Mite -> Maybe AnchorMapping
checkOriginal anchor candidate = case (cxt candidate, anchor) of
  (VerbalModifier a1 False v1, VerbalModifier a2 False v2) | a1 == a2 -> Just $ AnchorMapping candidate v1 anchor v2
  (Argument kind1 v1, Argument kind2 v2) | kind1 == kind2 -> Just $ AnchorMapping candidate v1 anchor v2
  _ -> Nothing

findOriginals mites anchor = catMaybes $ map (checkOriginal anchor) mites 

processEllipsis :: Variable -> Construction -> Construction -> Tree -> [[Mite]]
processEllipsis ellipsisVar@(Variable varIndex _) e1 e2 prevTree = let
  allMites = allTreeMites prevTree
  mappings = catMaybes [mapConstructions mapping1 mapping2 | mapping1 <- findOriginals allMites e1, mapping2 <- findOriginals allMites e2]
  mapConstructions :: AnchorMapping -> AnchorMapping -> Maybe [Mite]
  mapConstructions (AnchorMapping mo1 vo1 a1 va1) (AnchorMapping mo2 vo2 a2 va2) = let
    o1 = cxt mo1
    o2 = cxt mo2
    mapVariable _v = if _v == vo1 then va1 else if _v == vo2 then va2 else Variable varIndex ("_" ++ show _v)
    mapMite m = withBase [m,mo1,mo2] $ case cxt m of
      Unify _v1 _v2 -> [mite $ Unify (mapVariable _v1) (mapVariable _v2)]
      Sem _v1 attr (StrValue s) -> [mite $ Sem (mapVariable _v1) attr (StrValue s)]
      Sem _v1 attr (VarValue _v2)  -> [mite $ Sem (mapVariable _v1) attr (VarValue $ mapVariable _v2)]
      _ -> []
    walkTree :: Tree -> ([Mite], [Construction])
    walkTree tree = let
      ownMites = mites tree
      ownCxts = map cxt ownMites
      ownMapped = ownMites >>= mapMite 
      (leftMapped, leftCovered) = if isBranch tree then walkTree (fromJust $ left tree) else ([], [])
      (rightMapped, rightCovered) = if isBranch tree then walkTree (fromJust $ right tree) else ([], [])
      in
      if elem o1 ownCxts then ([], [o1])
      else if elem o2 ownCxts then ([], [o2])
      else (leftMapped++ownMapped++rightMapped, leftCovered++rightCovered)
    (mapped, covered) = walkTree prevTree
    in if covered == [o1,o2] then Just mapped else Nothing
  in mappings