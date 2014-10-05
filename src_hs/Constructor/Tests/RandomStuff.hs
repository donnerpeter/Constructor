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
  ,
  translateTest "Он печатал всеми десятью пальцами"
                "He typed using all ten of his fingers"
  ,
  translateTest "Он печатал всеми пальцами"
                "He typed using all of his fingers"
  ,
  translateTest "Он увидел их семью своими глазами"
                "He saw their family with his own eyes"
  ,
  translateTest "Он увидел их всеми семью своими глазами"
                "He saw them with all seven of his eyes"
  ,
  translateTest "Он велел им ей помочь"
                "He ordered them to help her"
  ,
  translateTest "Он велел им помочь ей"
                "He ordered them to help her"
  ,
  translateTest "Он велел ей помочь"
                "He ordered to help her"
  ,
  translateTest "Он велел помочь ей"
                "He ordered to help her"
  ,
  translateTest "Все и всё знают"
                "Everybody knows everything"
--  ,
--todo  translateTest "Знают все и всё"
--                "Everybody knows everything"
  ,
  translateTest "Все всё знают"
                "Everybody knows everything"
  ,
  translateTest "Все знают всё"
                "Everybody knows everything"
  ,
  translateTest "Никто не знает ничего"
                "Nobody knows anything"
--  ,
--todo  translateTest "Никто и ничего не знает"
--                "Nobody knows anything"
--  ,
--todo  translateTest "Не знает никто и ничего"
--                "Nobody knows anything"
--  ,
--todo  translateTest "Кассирша, что подвигала носом, идёт в сад"
--                "The cashier that moved her nose goes to the garden"
--  ,
--todo  translateTest "Идет дождь. Дождь шёл. Дождь шел. Дождь будет идти."
--                "It's raining. It was raining. It was raining. It will be raining."
--  ,
--todo  translateTest "Идет снег"
--                "It's snowing"
--  ,
--todo  translateTest "Курить - здоровью вредить"
--                "To smoke is to damage the health"
--  ,
--todo  translateTest "Курить вредно"
--                "Smoking is harmful"
--  ,
--todo  translateTest "Курить - это вредно"
--                "Smoking is harmful"
--  ,
--todo  translateTest "Курение - это вредно"
--                "Smoking is harmful"
--  ,
--todo  translateTest "Курение вредно"
--                "Smoking is harmful"
--  ,
--todo  translateTest "Чай пить - долго жить"
--                "To drink tea means to live long"
--  ,
--todo  translateTest "Это арбуз. Он зелёный."
--                "This is a water melon. It's green."
--  ,
--todo  translateTest "Она кассирша и работает в магазине."
--                "She is a cashier and works in a store."
  ,
  translateTest "Он увидел их"
                "He saw them"
  ,
  translateTest "Он увидел их и их семью"
                "He saw them and their family"
  ]]
