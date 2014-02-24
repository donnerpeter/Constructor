module Constructor.Tests.Testing where

import Test.HUnit
import Constructor.Sense
import Constructor.EnglishGenerator
import Constructor.Parser
import Debug.Trace

translateTest src target = TestLabel src $ TestCase $
  let trees = parse src in
  assertEqual ((show $ makeSense trees) ++ "\n\n" ++ (show trees)) target (generate $ makeSense trees) 
