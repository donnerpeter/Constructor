module Constructor.Tests.Copula where
import Constructor.Tests.Testing

copulaTests = [
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
  translateTest "О чём я?"
                "What was I talking about?"
  ,
  translateTest "О ком я? Я о Васе."
                "Who was I talking about? I was talking about Vasya."
  ,
  translateTest "О чём бишь я?"
                "What was I talking about?"
  ,
  translateTest "О чём это бишь я?"
                "What was I talking about?"
  ,
  translateTest "О чём это я?"
                "What was I talking about?"
  ,
  translateTest "О чём это?"
                "What is this about?"
  ,
  translateTest "О чём это было?"
                "What was this about?"
  ,
  translateTest "О чем это было?"
                "What was this about?"
  ,
  translateTest "О чём эта книга?"
                "What is this book about?"
  ,
  translateTest "О чём была та книга?"
                "What was that book about?"
  ,
  translateTest "Что это?"
                "What is this?"
  ,
  translateTest "Что это было?"
                "What was this?"
--  ,
--todo  translateTest "Куда он? Он домой."
--                "Where is he going? He's going home."
  ,
  translateTest "Кто он? Он кассир."
                "Who is he? He's a cashier."
  ,
  translateTest "Кто Вася? Вася кассир."
                "Who is Vasya? Vasya is a cashier."
--  ,
--todo  translateTest "Кто такой Вася? Вася - это кассир."
--                "Who is Vasya? Vasya is the cashier."
--  ,
--todo  translateTest "Кто здесь кассир? Кассир - Вася."
--                "Who is the cashier here? Vasya is the cashier."
--  ,
--todo  translateTest "Я не кассир, кассир не я."
--                "I'm not a cashier, the cashier is not me."
  ,
  translateTest "Сегодня Вася кассир."
                "Vasya is a cashier today."
  ,
  translateTest "Вася сегодня кассир."
                "Vasya is a cashier today."
  ,
  translateTest "Кассир сегодня Вася."
                "The cashier is Vasya today."
  ,
  translateTest "Вася сегодня, кстати, кассир."
                "By the way, Vasya is a cashier today."
  ,
  translateTest "Кстати, Вася сегодня кассир."
                "By the way, Vasya is a cashier today."
  ,
  translateTest "Вася, кстати, сегодня кассир."
                "By the way, Vasya is a cashier today."
--  ,
--todo  translateTest "Вася - кассир."
--                "Vasya is a cashier."
--  ,
--todo  translateTest "Вася - кассир, а не продавец."
--                "Vasya is a cashier and not a salesman."
--  ,
--todo  translateTest "Вася не кассир, а продавец."
--                "Vasya is a not a cashier, but a salesman."
--  ,
--todo  translateTest "Маша не кассир, а продавец."
--                "Masha is a not a cashier, but a saleswoman."
--  ,
--todo  translateTest "Вася сегодня кассир, а завтра - продавец."
--                "Today Vasya is a cashier and tomorrow - a salesman."
--  ,
--todo  translateTest "Вася сегодня кассир, а завтра будет продавцом."
--                "Today Vasya is a cashier and tomorrow he'll be a salesman."
--  ,
--todo  translateTest "Вася вчера был кассиром, а сегодня уже продавец."
--                "Yesterday Vasya was a cashier and today he's already a salesman."
--  ,
--todo  translateTest "Вчера Василий был кассир, сегодня он уж продавец, а завтра будет бригадир. Такой вот он у нас хитрец!"
--                "Yesterday Vassily was a cashier, today he's already a salesman, and tomorrow he'll be a brigadier. What a cunning fellow he is!"
--  ,
--todo  translateTest "Каков хитрец!"
--                "What a cunning fellow!"
--  ,
--todo  translateTest "Eй уж замуж невтерпёж."
--                "She can't wait to become married."
  ,
  translateTest "Эта книга о Васе."
                "This book is about Vasya."
--  ,
--todo  translateTest "Это арбуз. Он зелёный."
--                "This is a water melon. It's green."
  ,
 translateTest "Она кассирша и работает в магазине."
                "She's a cashier and works in a store."
--  ,
--todo  translateTest "Кто кассирша и работает в магазине?"
--                "Who is a cashier and works in a store?"
--  ,
--todo  translateTest "Кто тут кассирша и работает в магазине?"
--                "Who here is a cashier and works in a store?"
--  ,
--todo  translateTest "Человек человеку волк."
--                "Man is to man a wolf."
--  ,
--todo  translateTest "Человек человеку друг, товарищ и брат."
--                "Man is to man a friend, a comrade, and a brother."
  ]
