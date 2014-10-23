module Constructor.Tests.Copula where
import Constructor.Tests.Testing
import Test.HUnit

copulaTests = [TestLabel "copula tests" $ TestList [
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
--todo  translateTest "О чём я?"
--                "What was I talking about?"
--  ,
--todo  translateTest "О чём бишь я?"
--                "What was I talking about?"
--  ,
--todo  translateTest "О чём это бишь я?"
--                "What was I talking about?"
--  ,
--todo  translateTest "О чём это я?"
--                "What was I talking about?"
--  ,
  translateTest "О чём эта книга?"
                "What is this book about?"
  ,
  translateTest "О чём была та книга?"
                "What was that book about?"
--  ,
--todo  translateTest "Что это?"
--                "What's this?"
--  ,
--todo  translateTest "Что это было?"
--                "What was this?"
--  ,
--todo  translateTest "Куда он?"
--                "Where is he going?"
  ,
  translateTest "Эта книга о Васе."
                "This book is about Vasya."
--  ,
--todo  translateTest "Это арбуз. Он зелёный."
--                "This is a water melon. It's green."
--  ,
--todo  translateTest "Она кассирша и работает в магазине."
--                "She is a cashier and works in a store."
  ]]
