module Constructor.LexiconUtils where
import Constructor.Constructions
import Constructor.CopulaData
import Constructor.Mite
import Constructor.Variable
import Constructor.Util
import qualified Constructor.Agreement as A
import Constructor.Agreement (Gender(..))
import Data.Char (ord, chr)
import Data.Maybe
import Data.List
import qualified Constructor.SemanticProperties as P

nounSg caze gender typ v = pronoun caze (A.Agr (Just gender) (Just A.Sg) (Just 3)) typ v
nounPl caze typ v = pronoun caze (A.Agr Nothing (Just A.Pl) (Just 3)) typ v
pronoun caze agr typ v = synNoun caze agr v ++ [semT (v "") typ] ++ rusGender agr (v "")

synNoun caze agr v = synNounPhrase caze agr v ++ [mite $ Argument caze (v "")]
synNounPhrase caze agr v = [mite $ AdjHead (v "") caze agr, mite $ NounPhrase (v ""), mite $ Negateable (v "")]

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
finiteClause agr withSemSubject v =
                     [mite $ NomHead agr (v "arg1") Unsatisfied] ++ [mite $ ReflexiveTarget (v "arg1")] ++
                     (if withSemSubject then [semV (v "") P.Arg1 (v "arg1")] else []) ++
                     rusNumber agr (v "arg1") ++ rusGender agr (v "arg1") ++ rusPerson agr (v "arg1") ++
                     clause v

copulaHead kind agr copulaType rel tenseRequired var =
  [mite $ CopulaHead (CopulaData kind agr var False copulaType rel), mite $ TenseHead tenseRequired (makeV var "cop" "")]

copulaSem cd = if (copBound cd) then [] else common ++ specific where
  common = [semV (copula cd) P.Arg1 (copSubj cd), semT (copula cd) (copType cd), semV (copCP cd) P.Content (copula cd), semT (copCP cd) "situation"]
  ph = makeV (copVar cd) "cop_ph" ""
  specific = 
    if copKind cd == NPCopula then [semV (copula cd) (copAttr cd) (copVar cd)]
    else [semV (copula cd) P.Arg2 ph, semV ph (copAttr cd) (copVar cd), semT ph "placeholder", semV ph P.Target (copSubj cd)]

clause v = [mite $ Verb (v ""), semV (v "cp") P.Content (v ""), semT (v "cp") "situation", mite $ Clause (v "cp"), mite $ ConjEmphasizeable (v "")]

infinitive typ v =
  [semT (v "") typ] ++
  xor [[mite $ ControlledInfinitive $ v ""],
       [mite $ ModalityInfinitive (v "x") (v "cp"), semT (v "x") "modality", semV (v "x") P.Theme (v ""), semV (v "cp") P.Content (v "x"), mite $ Verb (v "x"), mite $ ConjEmphasizeable (v "x"), mite $ TenseHead Optional (v "x")] ++ optional (arg Dat P.Arg1 v),
       semArg Direction P.Goal_action (v "")]
arg argType relation v = [mite $ ArgHead argType relation (v "")]
compHead attr v = [mite $ CompHead attr (v "")]

semArg argType relation childVar@(Variable index s) = let headVar = Variable index (s ++ "_head") in
  [mite $ SemArgument argType headVar childVar, semV headVar relation childVar]

whWord agr v = [mite $ Wh agr (v ""), semT (v "") "wh", mite $ WhLeaf (v "")]
caseWhWord kind agr v = whWord agr v ++ [mite $ QuestionVariants (v "") kind, mite $ AdjHead (v "") kind agr, mite $ Argument kind (v "")]
negatedWh v = [semT (v "") "wh", semS (v "") P.Negated "true", mite $ Negated (v ""), mite $ NegativePronoun (v "")]
animate v = [semS (v "") P.Animate "true"]

adj caze agr attr value v = adjLike caze agr attr value (Adj (v "") attr caze agr) v

possessive caze agr value v = adjLike caze agr P.Arg1 value (Possessive caze agr (v "")) v

adjLike caze agr attr value adjLikeCxt v =
  [semT v0 value, mite $ AdverbModifiable v0] ++ xor [adjVariant, nounVariant]
  where
  v0 = v ""
  adjVariant = [mite $ Negateable v0, mite adjLikeCxt]
  nounVariant = rusNumber agr (v "noun") ++ synNoun caze agr (makeV v0 "noun") ++ [mite $ Argument caze (v "noun"), semS (v "noun") P.Elided "true", mite $ Handicap (v "noun")] ++ semLink (v "noun")
  semLink nounV = [semV nounV attr v0]

shortAdj agr attr value v =
  [semT v0 value, mite $ AdverbModifiable v0] ++ copulaHead AdjCopula agr "copula" attr Optional v0 ++ [mite $ Negateable v0]
  where v0 = v ""

comparativeAdj attr value v =
  [semT v0 value, semT (v "more") "MORE", semV (v "more") P.Theme v0, mite $ Negateable (v "more"), mite $ ComparativeAdj (v "more"), mite $ AdverbModifiable (v "more")]
  ++ copulaHead AdjCopula A.empty "copula" attr Optional (v "more")
  ++ optional (xor [arg Gen P.Anchor (makeV v0 "more"), [mite $ ComparativeHead (v "more")]])
  where v0 = v ""

adjWh caze agr attr v = [mite $ Adj (v "") attr caze agr, semT (v "") "wh", mite $ Wh agr (v "")]

perfectBackground typ v = [mite $ Verb (v ""), semT (v "") typ, mite $ VerbalModifier P.PerfectBackground True (v ""), mite $ ConjEmphasizeable (v "")]
sAdverb attr value v = [mite $ Adverb (v "verb"), semS (v "verb") attr value]
modifierAdverb value v = [mite $ ModifierAdverb (v "head"), semS (v "head") P.ModifierAdverb value]
adverb attr value v = [mite $ VerbalModifier attr False (v ""), semT (v "") value, mite $ AdverbModifiable (v "")]
genHead attr v = optional [mite $ GenHead (v "gen"), semV (v "") attr (v "gen")]
directObject v = arg Acc P.Arg2 v

conjunction v0 conj ready = [mite $ Conjunction $ SeqData v0 conj ready False False False, semT v0 "seq"] ++ (if conj == "," then [] else [semS v0 P.Conj $ semConj conj]) where
  semConj s = if s == "i" then "and" else if s == "ili" then "or" else if s == "a" || s == "no" then "but" else s


numQuantifier ownCase childCase childAgr v = [mite $ Quantifier ownCase childCase childAgr (v "")]

number word v = xor (concat [nounAlternatives caze ++ [quantifierAlternative caze] | caze <- [Nom, Gen, Acc]]) where
  nounAlternatives caze = [pronoun caze A.sg3 word v ++ [semS (v "") P.Number "true"]]
  quantifierAlternative caze = numQuantifier caze (quantifierChildCase caze word) (quantifierChildAgr word) v ++ [semT (v "") word, semS (v "") P.Number "true"]

wordNumber caze typ v = xor [nounSg caze Masc typ v, numQuantifier caze (quantifierChildCase caze typ) (quantifierChildAgr typ) v ++ [semT (v "") typ]]
quantifierChildCase caze typ = if typ /= "1" && (caze == Nom || caze == Acc) then Gen else caze
quantifierChildAgr typ = if typ `elem` ["1","2","3","4"] then A.sg3 else A.pl3

go_args v = [mite $ SemArgHead Optional Direction (v "")]

