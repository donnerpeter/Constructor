module Constructor.Tests.Wh where
import Constructor.Tests.Testing
import Test.HUnit

whTests = [TestLabel "wh tests" $ TestList [
  translateTest "Что они думают по этому поводу?"
                "What do they think on this matter?"
  ,
  translateTest "Кому она сломала челюсть?"
                "Whose jaw did she break?"
  ,
  translateTest "Что она ему сломала?"
                "What did she break?"
  ,
  translateTest "Кого он спросил?"
                "Who did he ask?"
  ,
  translateTest "О чём он спросил соседей?"
                "What did he ask his neighbors about?"
  ,
  translateTest "Что идёт раньше - 7 или 8?"
                "What comes first - 7 or 8?"
  ,
  translateTest "Делать нам было нечего"
                "We had nothing to do"
  ,
  translateTest "Нам было нечего делать"
                "We had nothing to do"
  ,
  translateTest "Нам нечего было делать"
                "We had nothing to do"
  ,
  translateTest "Делать нам нечего"
                "We have nothing to do"
--  ,
  -- translateTest "Нам есть что делать"
  --               "We have something to do"
  -- ,
  -- translateTest "Нам было что делать"
  --               "We had something to do"
  -- ,
  -- translateTest "Нам будет что делать"
  --               "We'll have something to do"
  ,
  translateTest "Мы ничего не делали"
                "We did nothing"
  -- ,
  -- translateTest "Некому танцевать"
  --               "There's nobody to dance"
  ,
  translateTest "Мне нечего сказать"
                "I have nothing to say"
  -- ,
  -- translateTest "Мне есть что сказать"
  --               "I have something to say"
  -- ,
  -- translateTest "Мне не к кому идти"
  --               "I have nobody to go to"
  -- ,
  -- translateTest "Мне некуда идти"
  --               "I have nowhere to go"
  ,
  translateTest "Куда мне идти?"
                "Where should I go?"
  -- ,
  -- translateTest "Куда они пошли?"
  --               "Where did they go?"
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
  -- todo нечего
  ,
  translateTest "Я забыл, о чём он спросил соседей."
                "I forgot what he asked his neighbors about."
--  ,
--todo  translateTest "О чём я?"
--                "What was I talking about?"
--  ,
--todo  translateTest "Я увидел дом, где они сидят"
--                "I saw the house where they were sitting"
--  ,
--todo  translateTest "Я увидел дом, в котором они сидят"
--                "I saw the house which they were sitting in"
--  ,
--todo  translateTest "Я увидел дом, у которого они сидят"
--                "I saw the house which they were sitting next to"
--  ,
--todo  translateTest "Я спросил про дом, что он увидел"
--                "What do they think on this matter?"
--  ,
--todo  translateTest "Кому он велел помочь?"
--                "What do they think on this matter?"
--  ,
--todo  translateTest "Кому он велел ей помочь?"
--                "What do they think on this matter?"
--  ,
--todo  translateTest "Кому он велел помочь, ей?"
--                "What do they think on this matter?"
--  ,
  ]]
