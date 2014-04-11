module Constructor.Tests.RandomStuff where
import Constructor.Tests.Testing
import Test.HUnit

randomStuffTests = [TestLabel "random stuff tests" $ TestList [
  translateTest "Я уже на углу Бассейной и Знаменской улицы"
                "I'm already on the corner of Basseinaya and Znamenskaya streets"
  ,
  translateTest "У меня есть арбуз."
                "I have a watermelon."
  ,
  translateTest "У меня был арбуз."
                "I had a watermelon."
  ,
  translateTest "У Васи был арбуз."
                "Vasya had a watermelon."
  ,
  translateTest "Я на углу"
                "I'm on the corner"
  ,
  translateTest "Они пошли домой"
                "They went home"
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
