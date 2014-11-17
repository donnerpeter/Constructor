module Constructor.Tests.RandomStuff where
import Constructor.Tests.Testing

randomStuffTests = [
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
  translateTest "Идет дождь. Дождь шёл. Дождь шел. Дождь будет идти."
                "It's raining. It was raining. It was raining. It'll be raining."
  ,
  translateTest "Идет снег"
                "It's snowing"
  ,
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
  translateTest "Он увидел их"
                "He saw them"
  ,
  translateTest "Он увидел их и их семью"
                "He saw them and their family"
  ,
  translateTest "Сестра кассира пошла в сад"
                "The cashier's sister went to the garden"
  ]
