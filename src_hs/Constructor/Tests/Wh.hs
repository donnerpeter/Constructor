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
  -- todo нечего
  -- translateTest "Делать нам было нечего"
  --               "We had nothing to do"
  -- ,
  -- translateTest "Нам было нечего делать"
  --               "We had nothing to do"
  -- ,
  -- translateTest "Нам нечего было делать"
  --               "We had nothing to do"
  -- ,
  -- translateTest "Делать нам нечего"
  --               "We have nothing to do"
  -- ,
  -- translateTest "Нам есть что делать"
  --               "We have something to do"
  -- ,
  -- translateTest "Нам было что делать"
  --               "We had something to do"
  -- ,
  -- translateTest "Нам будет что делать"
  --               "We'll have something to do"
  -- ,
  -- translateTest "Мы ничего не делали"
  --               "We did nothing"
  -- ,
  -- translateTest "Некому танцевать"
  --               "There's nobody to dance"
  -- ,
  -- translateTest "Мне нечего сказать"
  --               "I have nothing to say"
  -- ,
  -- translateTest "Мне есть что сказать"
  --               "I have something to say"
  -- ,
  -- translateTest "Мне не к кому идти"
  --               "I have nobody to go to"
  -- ,
  -- translateTest "Мне некуда идти"
  --               "I have nowhere to go"
  -- ,
  -- translateTest "Мне некого больше любить"
  --               "I have nobody to love anymore"
  -- ,
  -- translateTest "Меня некому больше любить"
  --               "There's nobody to love me anymore"
  -- ,
  -- translateTest "Меня никто больше не любит"
  --               "Nobody loves me anymore"
  -- ,
  -- translateTest "Мы никуда не идём"
  --               "We go nowhere"
  ]]
