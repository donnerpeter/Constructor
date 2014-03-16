module Constructor.Tests.Testing where

import Test.HUnit
import Constructor.EnglishGenerator
import Constructor.Parser
import Constructor.ParsingState
import Constructor.Sense as Sense
import Debug.Trace

translateTest src target = TestLabel src $ TestCase $
  let trees = parse src
      sense = Sense.makeSense $ activeStateMites trees
  in
  assertEqual (show sense ++ "\n\n" ++ (show trees)) target (generate sense)
