module Constructor.LexiconUtils where
import Constructor.CopulaData
import Constructor.Mite
import Constructor.Variable
import qualified Constructor.Agreement as A
import Constructor.Agreement (Gender(..))
import qualified Constructor.SemanticProperties as P

nounSg caze gender typ v = pronoun caze (A.Agr (Just gender) (Just A.Sg) (Just 3)) typ v
nounPl caze typ v = pronoun caze (A.Agr Nothing (Just A.Pl) (Just 3)) typ v
pronoun caze agr typ v = synNoun caze agr v ++ [semT (v "") typ] ++ rusGender agr (v "")

synNoun caze agr v = [mite $ Argument caze (v ""), mite $ AdjHead (v "") caze agr, mite $ NounPhrase (v "")]

rusGender agr v = case A.gender agr of
  Just g -> [semS v P.RusGender (show g)]
  _ -> []

rusPerson agr v = case A.person agr of
  Just g -> [semS v P.RusPerson (show g)]
  _ -> []

rusNumber agr v = case A.number agr of
  Just g -> [semS v P.RusNumber (show g)]
  _ -> []

preposition prep nounArg v = [mite $ PrepHead prep nounArg (v "")]
semPreposition nounArg attr v = [mite $ SemPreposition nounArg (v "noun"), semV (v "") attr (v "noun")]
finVerb typ time agr v = [semT (v "") typ, semS (v "") P.Time time] ++ finiteClause agr True v
raisingVerb typ time agr v = [semT (v "") typ, semS (v "") P.Time time, mite $ RaisingVerb (v "") (v "arg1")] ++ finiteClause agr False v
imperativeVerb typ agr v = [semT (v "") typ, semS (v "") P.Imperative "true"] ++ finiteClause agr True v
finiteClause agr withSemSubject v =
                     [mite $ NomHead agr (v "arg1") Unsatisfied, mite $ ReflexiveTarget (v "arg1")] ++
                     (if withSemSubject then [semV (v "") P.Arg1 (v "arg1")] else []) ++
                     rusNumber agr (v "arg1") ++ rusGender agr (v "arg1") ++ rusPerson agr (v "arg1") ++
                     clause v

copulaHead kind agr copulaType rel tenseRequired var =
  [mite $ CopulaHead (CopulaData kind agr var False copulaType rel), mite $ TenseHead tenseRequired (makeV var "cop" "")]

copulaSem cd = if (copBound cd) then [] else common ++ specific where
  common = [semV (copula cd) P.Arg1 (copSubj cd), semT (copula cd) (copType cd), semS (copula cd) P.Clausal "true"]
  ph = makeV (copVar cd) "cop_ph" ""
  specific = case copKind cd of
    NomNPCopula -> [semV (copula cd) (copAttr cd) (copVar cd)]
    InstrNPCopula -> [semV (copula cd) (copAttr cd) (copVar cd)]
    _ -> [semV (copula cd) P.Arg2 ph, semV ph (copAttr cd) (copVar cd), semT ph "placeholder", semV ph P.Target (copSubj cd)]

clause v = [mite $ Verb (v ""), semS (v "") P.Clausal "true", mite $ Clause (v ""), mite $ ConjEmphasizeable (v "")]

infinitive typ v =
  [semT (v "") typ] ++
  xor [[mite $ ControlledInfinitive $ v ""],
       [mite $ ModalityInfinitive (v "x"), semT (v "x") "modality", semV (v "x") P.Theme (v ""), semS (v "x") P.Clausal "true", mite $ Verb (v "x"), mite $ ConjEmphasizeable (v "x"), mite $ TenseHead Optional (v "x")] ++ arg Dat P.Arg1 v,
       semArg Direction P.Goal_action (v "")]
arg argType relation v = [mite $ ArgHead Optional argType relation (v "")]
obligatoryArg argType relation v = [mite $ ArgHead Obligatory argType relation (v "")]
compHead attr v = [mite $ CompHead attr (v "")]

semArg argType relation childVar = [mite $ SemArgument argType relation childVar]

whWord agr v = [mite $ Wh agr (v ""), semT (v "") "wh", mite $ WhLeaf (v "")]
caseWhWord kind agr v = whWord agr v ++ [mite $ QuestionVariants (v "") kind, mite $ AdjHead (v "") kind agr, mite $ Argument kind (v "")]
negatedWh v = [semT (v "") "wh", semS (v "") P.Negated "true", mite $ Negated (v ""), mite $ NegativePronoun (v "")]
animate v = [semS (v "") P.Animate "true"]

adj caze agr attr value v = adjLike value (Adj (v "") attr caze agr) v

possessive caze agr value v = adjLike value (Possessive caze agr (v "")) v

adjLike value adjLikeCxt v = [semT v0 value, mite $ AdverbModifiable v0, mite adjLikeCxt]
  where v0 = v ""

shortAdj agr attr value v = [semT v0 value, mite $ AdverbModifiable v0, mite $ ShortAdj agr attr v0]
  where v0 = v ""

comparativeAdj attr value v =
  [semT v0 value, semT more "MORE", semV more P.Theme v0, mite $ ComparativeAdj A.empty attr more, mite $ AdverbModifiable more]
  ++ xor [arg Gen P.Anchor (makeV v0 "more"), [mite $ ComparativeHead more]]
  where v0 = v ""; more = v "more"

adjWh caze agr attr v = [mite $ Adj (v "") attr caze agr, semT (v "") "wh", mite $ Wh agr (v "")]

perfectBackground typ v = [mite $ Verb (v ""), semT (v "") typ, mite $ VerbalModifier P.PerfectBackground True (v ""), mite $ ConjEmphasizeable (v "")]
sAdverb attr value v = [mite $ Adverb (v "verb"), semS (v "verb") attr value]
modifierAdverb value v = [mite $ ModifierAdverb (v "head"), semS (v "head") P.ModifierAdverb value]
adverb attr value v = [mite $ VerbalModifier attr False (v ""), semT (v "") value, mite $ AdverbModifiable (v "")]
genHead attr v = [mite $ GenHead [(attr, v "")]]
directObject v = arg Acc P.Arg2 v

conjunction v0 conj ready = [mite $ Conjunction $ SeqData v0 conj ready False False False False, semT v0 "seq"] ++ (if conj == "," then [] else [semS v0 P.Conj $ semConj conj]) where
  semConj s = if s == "i" then "and" else if s == "ili" then "or" else if s == "a" || s == "no" then "but" else s


numQuantifier ownCase childCase childAgr v = [mite $ Quantifier ownCase childCase childAgr (v "")]

number word v = xor (concat [nounAlternatives caze ++ [quantifierAlternative caze] | caze <- [Nom, Gen, Acc]]) where
  nounAlternatives caze = [pronoun caze A.sg3 word v ++ [semS (v "") P.Number "true"]]
  quantifierAlternative caze = numQuantifier caze (quantifierChildCase caze word) (quantifierChildAgr word) v ++ [semT (v "") word, semS (v "") P.Number "true"]

wordNumber caze typ v = xor [nounSg caze Masc typ v, numQuantifier caze (quantifierChildCase caze typ) (quantifierChildAgr typ) v ++ [semT (v "") typ]]
quantifierChildCase caze typ = if typ /= "1" && (caze == Nom || caze == Acc) then Gen else caze
quantifierChildAgr typ = if typ `elem` ["1","2","3","4"] then A.sg3 else A.pl3

go_args v = [mite $ SemArgHead Optional Direction (v "")]

