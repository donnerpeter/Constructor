module Constructor.LexiconUtils where
import Constructor.Constructions
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

synNoun caze agr v = synNounPhrase caze agr v ++ argOrCopula caze agr v
synNounPhrase caze agr v = [mite $ AdjHead (v "") caze agr, mite $ NounPhrase (v ""), mite $ Negateable (v "")]

argOrCopula caze agr v =
  if caze == Nom then xor [argRole, npCopulaHead False]
  else if caze == Instr then xor [argRole, npCopulaHead True]
  else argRole
  where
  argRole = [mite $ Argument caze (v "")]
  npCopulaHead instr = copulaHead NPCopula agr "copula" instr cv ++ [semV (cv "") P.Arg2 (v "")] ++
    (if instr then [semS (cv "") P.ProfessionCopula "true", mite $ ConjEmphasizeable (cv "")] else [])
  cv = modifyV v 'x'

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

copulaHead kind agr copulaType tenseRequired v =
  [mite $ CopulaHead (CopulaData kind agr (v "arg1") v0 cp False)] ++
   (if tenseRequired then tenseHead else optional tenseHead) ++
   [semV v0 P.Arg1 (v "arg1"), semT v0 copulaType, semV cp P.Content v0, semT cp "situation"] where
  v0 = v ""
  cp = v "cp"
  tenseHead = [mite $ TenseHead v0]

clause v = [mite $ Verb (v ""), semV (v "cp") P.Content (v ""), semT (v "cp") "situation", mite $ Clause (v "cp"), mite $ ConjEmphasizeable (v "")]

infinitive typ v =
  [semT (v "") typ] ++
  xor [[mite $ ControlledInfinitive $ v ""],
       [mite $ ModalityInfinitive (v "x") (v "cp"), semT (v "x") "modality", semV (v "x") P.Theme (v ""), semV (v "cp") P.Content (v "x"), mite $ Verb (v "x"), mite $ ConjEmphasizeable (v "x")] ++ optional [mite $ TenseHead (v "x")] ++ optional (arg Dat P.Arg1 v),
       semArg Direction P.Goal_action (v "")]
arg argType relation v = [mite $ ArgHead argType (v $ decapitalize $ show relation), semV (v "") relation (v $ decapitalize $ show relation)]
compHead attr v = [mite $ CompHead (v "comp"), semV (v "") attr (v "comp")]

semArg argType relation childVar@(Variable index s) = let headVar = Variable index (s ++ "_head") in
  [mite $ SemArgument argType headVar childVar, semV headVar relation childVar]

whWord agr v = [mite $ Wh agr (v ""), semT (v "") "wh", mite $ WhLeaf (v "")]
caseWhWord kind agr v = whWord agr v ++ [mite $ QuestionVariants (v "") kind, mite $ AdjHead (v "") kind agr] ++ argOrCopula kind agr v
negatedWh v = [semT (v "") "wh", semS (v "") P.Negated "true", mite $ Negated (v ""), mite $ NegativePronoun (v "")]
animate v = [semS (v "") P.Animate "true"]

adj caze agr attr value v =
  [semT v0 value] ++ (if caze == Nom || caze == Instr then xor [adjVariant, adjCopulaHead, nounVariant] else xor [adjVariant, nounVariant])
  where
  v0 = v ""
  adjVariant = [mite $ Negateable v0, mite $ Adj v0 attr caze agr]
  adjCopulaHead = copulaHead NPCopula agr "copula" False (makeV v0 "cop") ++ [mite $ Negateable v0] ++ semLink (v "cop")
  nounVariant = rusNumber agr (v "noun") ++ synNounPhrase caze agr (makeV v0 "noun") ++ [mite $ Argument caze (v "noun"), semS (v "noun") P.Elided "true", mite $ Handicap (v "noun")] ++ semLink (v "noun")
  semLink nounV = [semV nounV attr v0]

shortAdj agr attr value v =
  [semT v0 value, semV (v "cop") attr v0] ++ copulaHead NPCopula agr "copula" False (makeV v0 "cop") ++ [mite $ Negateable (v "")]
  where v0 = v ""

comparativeAdj agr attr value v =
  [semT v0 value, semV (v "cop") attr (v "more"), semT (v "more") "MORE", semV (v "more") P.Theme v0]
  ++ copulaHead NPCopula agr "copula" False (makeV v0 "cop") ++ [mite $ Negateable (v ""), mite $ ComparativeAdj (v "more")]
  where v0 = v ""

orNomCopula plainMites caze agr v = if caze == Nom then xor [plainMites, adjCopulaHead] else plainMites where
  adjCopulaHead = copulaHead NPCopula agr "copula" False v

adjWh caze agr attr v = [mite $ Adj (v "adj") attr caze agr, semT (v "adj") "wh", mite $ Wh agr (v "adj")]

perfectBackground typ v = [mite $ Verb (v ""), semT (v "") typ, mite $ VerbalModifier P.PerfectBackground True (v ""), mite $ ConjEmphasizeable (v "")]
sAdverb attr value v = [mite $ Adverb (v "verb"), semS (v "verb") attr value]
modifierAdverb value v = [mite $ ModifierAdverb (v "head"), semS (v "head") P.ModifierAdverb value]
adverb attr value v = [mite $ VerbalModifier attr False (v ""), semT (v "") value, mite $ AdverbModifiable (v "")]
genHead attr v = optional [mite $ GenHead (v "gen"), semV (v "") attr (v "gen")]
directObject v = arg Acc P.Arg2 v
conjunction v0 conj ready = [mite $ Conjunction $ SeqData v0 conj ready False False False, semT v0 "seq"] ++ (if conj == "," then [] else [semS v0 P.Conj conj])

modifyV v c = \s -> v $ c:s
makeV (Variable index oldS) suffix = \s -> Variable index (oldS ++ suffix ++ s)

whatComesNext v = [mite $ ArgHead ScalarAdverb (v "scalar"),
  semV (v "") P.Arg2 (v "arg2"),
  semT (v "arg2") "situation", semV (v "arg2") P.Questioned (v "wh"), semV (v "arg2") P.Content (v "comes"),
  semT (v "comes") "COME_SCALARLY", semV (v "comes") P.Order (v "scalar"),
  semV (v "comes") P.Arg1 (v "wh"), semT (v "wh") "wh"]
numQuantifier ownCase childCase childAgr v = synNoun ownCase childAgr v ++ [semV (v "") P.Quantifier (v "q"), mite $ Quantifier childCase childAgr (v "")]
number word v = xor (concat [nounAlternatives caze ++ [quantifierAlternative caze] | caze <- [Nom, Gen, Acc]]) where
  nounAlternatives caze = [nounSg caze gender word v  ++ [semS (v "") P.Number "true"] | gender <- [Masc, Neu]]
  quantifierAlternative caze = numQuantifier caze (quantifierChildCase caze word) (quantifierChildAgr word) v ++ [semT (v "q") word, semS (v "q") P.Number "true"]
wordNumber caze typ v = xor [nounSg caze Masc typ v, numQuantifier caze (quantifierChildCase caze typ) (quantifierChildAgr typ) v ++ [semT (v "q") typ]]
quantifierChildCase caze typ = if typ /= "1" && (caze == Nom || caze == Acc) then Gen else caze
quantifierChildAgr typ = if typ `elem` ["1","2","3","4"] then A.sg else A.pl

go_args v = optional [mite $ SemArgHead Direction (v "")]

