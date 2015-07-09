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
  ,
  translateTest "Куда он? Он домой."
                "Where is he going? He's going home."
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
  ,
  translateTest "Я не кассир, кассир не я."
                "I'm not a cashier, the cashier is not me."
  ,
  translateTest "Сегодня Вася кассир."
                "Today Vasya is a cashier."
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
  ,
  translateTest "Вася не кассир, а продавец. Маша не кассир, а продавец."
                "Vasya is not a cashier, but a salesman. Masha is not a cashier, but a saleswoman."
  ,
  translateTest "Вася был не кассиром, а продавцом."
                "Vasya was not a cashier, but a salesman."
--  ,
--todo  translateTest "Вася сегодня кассир, а завтра - продавец."
--                "Today Vasya is a cashier and tomorrow - a salesman."
  ,
  translateTest "Вася сегодня кассир, а завтра будет продавцом."
                "Today Vasya is a cashier, and tomorrow he'll be a salesman."
  ,
  translateTest "Вася вчера был кассиром, а сегодня уже продавец."
                "Yesterday Vasya was a cashier, and today he's already a salesman."
--  ,
--todo  translateTest "Вася вчера был кассиром, а сегодня - продавец."
--                "Yesterday Vasya was a cashier and today he's a salesman."
  ,
  translateTest "Вася только вчера приехал и уже продавец."
                "Vasya arrived just yesterday and he's already a salesman."
  ,
  translateTest "Вася только вчера приехал, а уже продавец."
                "Vasya arrived just yesterday, but he's already a salesman."
  ,
  translateTest "Он у нас хитрец!"
                "He's a cunning person!"
  ,
  translateTest "Он у нас такой хитрец!"
                "We have such a cunning person!"
  ,
  translateTest "Он такой хитрец!"
                "He's such a cunning person!"
  ,
  translateTest "Вчера Василий был кассир, сегодня он уж продавец, а завтра будет бригадир. Такой вот он у нас хитрец!"
                "Yesterday Vassily was a cashier, today he's already a salesman, and tomorrow he'll be a brigadier. What a cunning person we have here!"
  ,
  translateTest "Сегодня я ещё кассир, а завтра буду продавцом."
                "Today I'm still a cashier, and tomorrow I'll be a salesman."
  ,
  translateTest "Каков хитрец!"
                "What a cunning person!"
--  ,
--todo  translateTest "Eй уж замуж невтерпёж."
--                "She can't wait to become married."
  ,
  translateTest "Эта книга о Васе."
                "This book is about Vasya."
  ,
  translateTest "Это арбуз. Он зелёный."
                "This is a watermelon. It's green."
  ,
  translateTest "Это зелёный арбуз."
                "This is a green watermelon."
  ,
  translateTest "Это не зелёный арбуз."
                "This is not a green watermelon."
  ,
  translateTest "Этот арбуз зелёный."
                "This watermelon is green."
  ,
  translateTest "Этот арбуз зелёный, а тот арбуз не зелёный."
                "This watermelon is green, and that watermelon is not green."
  ,
  translateTest "Этот помидор не зелёный, а красный."
                "This tomato is not green, but red."
  ,
  translateTest "Этот помидор зелёный, а не красный."
                "This tomato is green and not red."
  ,
  translateTest "Это не зелёный свет, а красный."
                "This is not a green light, but a red one."
  ,
  translateTest "Это не зелёный, а красный свет."
                "This is not a green, but a red light."
  ,
  translateTest "Это не зелёный арбуз, а красный помидор."
                "This is not a green watermelon, but a red tomato."
  ,
  translateTest "Это зелёный арбуз, а не красный помидор."
                "This is a green watermelon and not a red tomato."
  ,
  translateTest "Она кассирша и работает в магазине."
                "She's a cashier and works in a store."
  ,
  translateTest "Кто это был? Кем он был? А кем была она?"
                "Who was this? What was he? And what was she?"
--  ,
--todo  translateTest "Кто или что это? Кто или что это было?"
--                "Who or what is this? Who or what was this?"
  ,
  translateTest "Где это? Где это было? Где было это?"
                "Where is this? Where was this? Where was this?"
--  ,
--todo  translateTest "Где и когда это было?"
--                "Where and when was this?"
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
  ,
  translateTest "Вы умны, а я умнее."
                "You're smart, but I'm smarter."
  ,
  translateTest "Вы умны, а я ещё умнее."
                "You're smart, but I'm even smarter."
  ,
  translateTest "Вы умны и быстры, а я умнее и быстрее."
                "You're smart and fast, but I'm smarter and faster."
  ,
  translateTest "Вы не умны, а только быстры."
                "You're not smart, but only fast."
  ,
  translateTest "Вы не умны, а всего лишь быстры."
                "You're not smart, but only fast."
  ,
  translateTest "Вы не умны, а лишь быстры."
                "You're not smart, but only fast."
  ,
  translateTest "Вы не умны, а просто быстрее кассира."
                "You're not smart, but just faster than the cashier."
  ,
  translateTest "Вы не умны, а умнее."
                "You're not smart, but smarter."
  ,
  translateTest "Вы не умнее, а быстрее."
                "You're not smarter, but faster."
  ,
  translateTest "Вы не просто умная, а умнее кассирши."
                "You're not just smart, but smarter than the cashier."
  ,
  translateTest "Вы не просто умны, а умнее кассирши."
                "You're not just smart, but smarter than the cashier."
  ,
  translateTest "Вы не умнее его."
                "You're not smarter than him."
  ,
  translateTest "Вы неумны."
                "You're not smart."
  ,
  translateTest "Вы умнее кассирши."
                "You're smarter than the cashier."
  ,
  translateTest "Вы более умны, чем кассир."
                "You're smarter than the cashier."
  ,
  translateTest "Вы умнее кассира, но не умнее кассирши."
                "You're smarter than the male cashier, but not smarter than the female cashier."
  ,
  translateTest "Вы умнее и быстрее кассира, но не кассирши."
                "You're smarter and faster than the male cashier, but not the female cashier."
  ,
  translateTest "Вы умнее, чем кассирша."
                "You're smarter than the cashier."
  ,
  translateTest "Кассир умён, но кассирша умнее."
                "The male cashier is smart, but the female cashier is smarter."
--  ,
--todo  translateTest "Кассир умён, а кассирша - нет."
--                "The male cashier is smart, but the female cashier is not."
--  ,
--todo  translateTest "Кассирша умна, а кассир - не очень."
--                "The female cashier is smart, but the male cashier is not so smart."
--  ,
--todo  translateTest "Кассир был умный, кассирша - не очень."
--                "The male cashier was smart, but the female cashier wasn't so smart."
  ,
  translateTest "Кассирша была умной. Кассир тоже будет умным."
                "The female cashier was smart. The male cashier will also be smart."
--  ,
--todo  translateTest "Чем кассирша лучше, чем кассир?"
--                "Why is the female cashier better than the male cashier?"
  ,
  translateTest "Кассирша лучше, чем кассир."
                "The female cashier is better than the male cashier."
  ,
  translateTest "Моя семья больше вашей."
                "My family is larger than yours."
  ,
  translateTest "Семья кассира больше семьи кассирши."
                "The male cashier's family is larger than the female cashier's family."
  ,
  translateTest "Семья кассира не больше семьи кассирши."
                "The male cashier's family is not larger than the female cashier's family."
  ,
  translateTest "Семья кассира будет не больше семьи кассирши."
                "The male cashier's family will be not larger than the female cashier's family."
  ,
  translateTest "Семья кассира не будет больше семьи кассирши."
                "The male cashier's family will not be larger than the female cashier's family."
  ,
  translateTest "Семья кассира не была больше семьи кассирши."
                "The male cashier's family wasn't larger than the female cashier's family."
  ,
  translateTest "Я и ты такие разные, ты и я такие разные."
                "Me and you are so different, you and I are so different."
--  ,
--todo  translateTest "Такие мы разные."
--                "That's how different we are."
--  ,
--todo  translateTest "Мы такие же умные, как они."
--                "We are as smart as them."
--  ,
--todo  translateTest "Он не такой умный, как я думал."
--                "He's not as smart as I thought."
--  ,
--todo  translateTest "Он не такой умный, как Вася."
--                "He's not as smart as Vasya."
--  ,
--todo  translateTest "Она такая умная, что мне даже страшно."
--                "She's so smart that I'm even scared."
--  ,
--todo  translateTest "Она настолько умная, что мне даже страшно."
--                "She's so smart that I'm even scared."
--  ,
--todo  translateTest "Она умна настолько, что даже мне страшно."
--                "She's so smart that even I'm scared."
  ,
  translateTest "Я и ты очень разные."
                "Me and you are very different."
--  ,
--todo  translateTest "Я согласен с тобой."
--                "I agree with you."
--  ,
--todo  translateTest "Я более чем согласен с тобой."
--                "I more than agree with you."
--  ,
--todo  translateTest "Эта квартира ваша. А та квартира не ваша."
--                "This apartment is yours. And that apartment is not yours."
--  ,
--todo  translateTest "Арбуз мой!"
--                "This watermelon is mine!"
  ]
