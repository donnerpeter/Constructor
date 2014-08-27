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
  ,
  translateTest "Он увидел их семью"
                "He saw their family"
  ,
  translateTest "Он увидел их своими глазами"
                "He saw them with his own eyes"
  ,
  translateTest "Он печатал семью пальцами"
                "He typed using seven of his fingers"
--  ,
--todo  translateTest "Он печатал всеми десятью пальцами"
--                "He typed using all ten of his fingers"
  ,
  translateTest "Он печатал всеми пальцами"
                "He typed using all of his fingers"
  ,
  translateTest "Он увидел их семью своими глазами"
                "He saw their family with his own eyes"
--  ,
--todo  translateTest "Он увидел их всеми семью своими глазами"
--                "He saw them with all of his seven eyes"
--  ,
--todo  translateTest "Он велел им ей помочь"
--                "He ordered them to help her"
--  ,
--todo  translateTest "Он велел им помочь ей"
--                "He ordered them to help her"
--  ,
--todo  translateTest "Он велел ей помочь"
--                "He ordered her to help"
--  ,
--todo  translateTest "Он велел помочь ей"
--                "He ordered to help her"
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
  ,
  translateTest "Он увидел их"
                "He saw them"
  ,
  translateTest "Он увидел их и их семью"
                "He saw them and their family"
  ]]
