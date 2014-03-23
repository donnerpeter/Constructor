module Constructor.Tests.RandomStuff where
import Constructor.Tests.Testing
import Test.HUnit

randomStuffTests = [TestLabel "random stuff tests" $ TestList [
  --  I'm already on the corner of Basseinaya and Znamenskaya streets
  translateTest "Я уже на углу Бассейной и Знаменской улицы"
                "I'm already on the corner of Basseinaya and Znamenskaya streets"
  ,
  -- IHaveMelon
  translateTest "У меня есть арбуз."
                "I have a watermelon."
  ,
  -- IHadMelon
  translateTest "У меня был арбуз."
                "I had a watermelon."
  ,
  -- VasyaHadMelon
  translateTest "У Васи был арбуз."
                "Vasya had a watermelon."
  ,
  --  I'm on the corner
  translateTest "Я на углу"
                "I'm on the corner"
  ]]
