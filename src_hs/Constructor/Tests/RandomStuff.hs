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
--  ,
--todo  translateTest "Он увидел их семью"
--                "To drink tea is to live long"
--  ,
--todo  translateTest "Он увидел их своими глазами"
--                "To drink tea is to live long"
--  ,
--todo  translateTest "Он увидел их семью своими глазами"
--                "To drink tea is to live long"
--  ,
--todo  translateTest "Он велел ей помочь"
--                "To drink tea is to live long"
--  ,
--todo  translateTest "Он велел помочь ей"
--                "To drink tea is to live long"
--  ,
--todo  translateTest "Идет дождь"
--                "It's raining"
--  ,
--todo  translateTest "Идет снег"
--                "It's snowing"
--  ,
--todo  translateTest "Курить - здоровью вредить"
--                "To smoke is to damage one's health"
--  ,
--todo  translateTest "Чай пить - долго жить"
--                "To drink tea is to live long"
--  ,
--todo  translateTest "Он увидел их"
--                "To drink tea is to live long"
--  ,
  ]]
