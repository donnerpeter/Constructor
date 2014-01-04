import Data.Char (ord, chr, toLower)
import qualified Data.Set as Set
import Data.Maybe
import Data.List

data Variable = Variable Int String
instance Show Variable where show (Variable i s) = "V"++(show i)++s

data SemValue = StrValue String | VarValue Variable
instance Show SemValue where
  show (StrValue s) = s
  show (VarValue v) = show v

data ArgKind = Nom | Acc | Gen | Dat | Instr | Prep | SInstr deriving (Show, Eq)
data Construction = Word Variable String
                  | Sem Variable String SemValue
                  | Unify Variable Variable
                  | Adj ArgKind String String
                  | Noun Variable ArgKind
                  | FiniteVerb Variable
                  | ArgHead ArgKind Variable
                  | Argument ArgKind Variable 
                  | Adverb String String
                  | Elaboration Variable
                  deriving (Show)
data Mite = Mite { cxt :: Construction, happy :: Bool }
instance Show Mite where
  show (Mite {cxt=c, happy=h}) = (if h then "" else "!")++(show c)
  
isHappy (Noun {}) = False
isHappy (Adj {}) = False
isHappy (Adverb {}) = False
isHappy (ArgHead {}) = False
isHappy (Argument {}) = False
isHappy _ = True

mite cxt = Mite cxt $ isHappy cxt
  
semS var prop value = mite $ Sem var prop (StrValue value)
semV var prop value = mite $ Sem var prop (VarValue value)
semT var _type = semS var "type" _type

wordMites :: String -> Int -> [Mite]  
wordMites word index =
  let v = \i -> Variable index $ [chr (i+(ord 'a'))] in
  case word of
  "вдруг" -> [mite $ Adverb "manner" "SUDDENLY"]
  "забыл" -> [mite $ FiniteVerb (v 0), semS (v 0) "type" "FORGET", semS (v 0) "time" "PAST"]
  "мной" -> [mite $ Noun (v 0) Instr, mite $ Argument Instr (v 0), semT (v 0) "ME"]
  "удивительный" -> [mite $ Adj Nom "property" "AMAZING"]
  "случай" -> [mite $ Noun (v 0) Nom, semS (v 0) "type" "THING"]
  "случился" -> [mite $ FiniteVerb (v 0), mite $ ArgHead SInstr (v 1), semS (v 0) "type" "HAPPEN", semS (v 0) "time" "PAST", semV (v 0) "experiencer" (v 1)]
  "со" -> [mite $ Argument SInstr (v 0), mite $ ArgHead Instr (v 0)]
  "я" -> [mite $ Noun (v 0) Nom, semT (v 0) "ME"]
  _ -> [mite $ Word (v 0) word]
  
data Tree = Tree {mites::[Mite], left::Maybe Tree, right::Maybe Tree, leftHeaded::Bool}
instance Show Tree where
  show tree =
    let inner = \tree prefix ->
          let myLine = prefix ++ (Data.List.intercalate ", " [show m | m <- headMites tree])++"\n" in
          case rightSubTree tree of
            Just r -> (inner r (" "++prefix)) ++ myLine
            Nothing -> myLine
    in "\n" ++ inner tree ""

data MergeInfo = MergeInfo {mergedMites::[Mite], satisfied::Set.Set Construction, leftHeadedMerge::Bool} deriving (Show)

interactMites:: Construction -> Construction -> [MergeInfo]
interactMites left right = case (left, right) of
  (Adj adjCase property value, Noun var nounCase) | adjCase == nounCase ->
    [MergeInfo [semS var property value] (Set.singleton left) False]
  (Noun child Nom, FiniteVerb head) -> 
    [MergeInfo [semV head "arg1" child] (Set.singleton left) False]
  (Adverb attr val, FiniteVerb head) ->
    [MergeInfo [semS head attr val] Set.empty False]
  (ArgHead kind1 var1, Argument kind2 var2) | kind1 == kind2 ->
    [MergeInfo [mite $ Unify var1 var2] Set.empty True]
  (FiniteVerb head, Word _ ":") ->
    [MergeInfo [Mite (Elaboration head) False] Set.empty True]
  (Elaboration head, FiniteVerb child) ->
    [MergeInfo [semV head "elaboration" child] Set.empty True]
  _ -> []
  
headMites tree =
  if isNothing (left tree) then mites tree
  else (mites tree)++(headMites $ fromJust $ (if leftHeaded tree then left tree else right tree))

rightSubTree tree =
  if isNothing (left tree) then Nothing
  else if leftHeaded tree then right tree
  else Nothing
  
isBranch tree = isJust (left tree)
isDirectedBranch tree isLeftBranch = isBranch tree && leftHeaded tree == isLeftBranch

createEdges leftTree rightTree leftMites rightMites =
  let mergeInfos = concat [interactMites (cxt leftMite) (cxt rightMite) | leftMite <- leftMites, rightMite <- rightMites] in
  [Tree mergedMites (Just leftTree) (Just rightTree) leftHeadedMerge | MergeInfo mergedMites _ leftHeadedMerge <- mergeInfos]

integrateSubTree leftTree rightTree toLeft subResult =
  let leftLeft = fromJust $ left leftTree
      rightRight = fromJust $ right rightTree
      newEdges = \ subTree -> createEdges leftLeft subTree 
                              (headMites (if toLeft then leftLeft else subTree)) 
                              (headMites (if toLeft then subTree else rightRight))
  in
  case subResult of
    Left subTree ->
      if isDirectedBranch subTree (not toLeft)
      then if toLeft then [Right (leftLeft, subTree)] else [Right (subTree, rightRight)]
      else [Left tree | tree <- newEdges subTree]
    Right (x1, x2) ->
      let subTree = if toLeft then x1 else x2
          another = if toLeft then x2 else x1
      in
      if isDirectedBranch subTree (not toLeft)
      then []
      else [if toLeft then Right (tree, another) else Right (another, tree) | tree <- newEdges subTree]
  
optimize:: Tree -> Tree -> Bool -> Bool -> Bool -> [Either Tree (Tree, Tree)]
optimize leftTree rightTree digLeft digRight useOwnMites =
  let ownResults = if useOwnMites 
                   then [Left tree | tree <- createEdges leftTree rightTree (headMites leftTree) (headMites rightTree)]
                   else []
      leftSubResults = if digLeft && isBranch leftTree
                       then optimize (fromJust $ right leftTree) rightTree True False $ isDirectedBranch leftTree True 
                       else []
      rightSubResults = if digRight && isBranch rightTree 
                        then optimize leftTree (fromJust $ left rightTree) False True $ isDirectedBranch rightTree False
                        else []
      dugLeft = concat $ map (integrateSubTree leftTree rightTree True) leftSubResults
      dugRight = concat $ map (integrateSubTree leftTree rightTree False) rightSubResults
      in ownResults ++ dugLeft ++ dugRight

mergeTrees:: [Tree] -> [Tree]
mergeTrees state@(rightTree:leftTree:rest) =
  let treeCandidates = optimize leftTree rightTree True True True
      singleCandidates = filter (\ x -> case x of Left _ -> True; Right _ -> False) treeCandidates
  in case singleCandidates of
    Left first:_ -> mergeTrees (first:rest)
    _ -> state
mergeTrees trees = trees

addMites:: [Tree] -> [Mite] -> [Tree]
addMites state mites = mergeTrees $ (Tree mites Nothing Nothing True):state

tokenize s = map (\x -> map toLower x) $
             filter (\token -> length token > 0) $
             reverse $
             fst $
             foldl processChar ([], "") (s++" ") where
  processChar = \(tokens, current) char ->
    case char of
      ' ' -> (current:tokens, "")
      ':' -> (":":current:tokens, "")
      _ -> (tokens, current++[char])

parse:: String -> [Tree]
parse s =
  let tokens = tokenize s
      pair = foldl (\(state, index) word -> (addMites state (wordMites word index), index + 1)) ([], 1) tokens
  in
    fst pair

main = do putStrLn (show (tokenize "я: Тыы"))
