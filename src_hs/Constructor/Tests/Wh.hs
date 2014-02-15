module Constructor.Tests.Wh where
import Constructor.Tests.Testing
import Test.HUnit

whTests = [TestLabel "wh tests" $ TestList [
  --  what do they think on this matter
  translateTest "Что они думают по этому поводу?"
                "What do they think on this matter?"
  ,
  --  whose jaw did she break
  translateTest "Кому она сломала челюсть?"
                "Whose jaw did she break?"
  ,
  --  what did she break
  translateTest "Что она ему сломала?"
                "What did she break?"
  ,
  --  who did he ask?
  translateTest "Кого он спросил?"
                "Who did he ask?"
  ,
  --  what did he ask the neighbors about
  translateTest "О чём он спросил соседей?"
                "What did he ask his neighbors about?"
  ,
  --  what comes first 7 or 8
  translateTest "Что идёт раньше - 7 или 8?"
                "What comes first - 7 or 8?"
  ]]
