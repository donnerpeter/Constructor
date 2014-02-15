module Constructor.Tests.Sonnet where
import Constructor.Tests.Testing
import Test.HUnit

sonnetTests = [
  translateTest "Удивительный случай случился со мной: я вдруг забыл, что идет раньше - 7 или 8" 
                "An amazing thing happened to me today, I suddenly forgot what comes first - 7 or 8",
  translateTest "Я отправился к соседям и спросил их, что они думают по этому поводу" 
                "I went to my neighbors and asked them about their opinion on this matter"
                ,
  translateTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета." 
                "Great was their and my amazement, when they suddenly discovered, that they couldn't recall the counting order."
                ,
  translateTest "1, 2, 3, 4, 5 и 6 помнят, а дальше забыли." 
                "They remember 1, 2, 3, 4, 5 and 6, but forgot what comes next."
                ,
  translateTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета. \
                        \1, 2, 3, 4, 5 и 6 помнят, а дальше забыли."
                "Great was their and my amazement, when they suddenly discovered, that they couldn't recall the counting order. \
                        \They remembered 1, 2, 3, 4, 5 and 6, but forgot what comes next."
                ,
  translateTest "Мы все пошли в коммерческий магазин \"Гастроном\", что на углу Знаменской и Бассейной улицы, и спросили кассиршу о нашем недоумении."
                "We all went to a commercial grocery store, the one that's on the corner of Znamenskaya and Basseinaya streets to consult a cashier on our predicament."
                ,
  translateTest "Кассирша грустно улыбнулась, вынула изо рта маленький молоточек и, слегка подвигав носом, сказала:"
                "The cashier gave us a sad smile, took a small hammer out of her mouth, and moving her nose slightly back and forth, she said:"
                ,
  translateTest "- По-моему, семь идет после восьми в том случае, когда восемь идет после семи."
                "- In my opinion, a seven comes after an eight, only if an eight comes after a seven."
                ,
  translateTest "Кассирша грустно улыбнулась, вынула изо рта маленький молоточек и, слегка подвигав носом, сказала:\n\
                \- По-моему, семь идет после восьми в том случае, когда восемь идет после семи."
                "The cashier gave us a sad smile, took a small hammer out of her mouth, and moving her nose slightly back and forth, she said:\n\
                \- In my opinion, a seven comes after an eight, only if an eight comes after a seven."
                ,
  translateTest "Мы поблагодарили кассиршу и с радостью выбежали из магазина."
                "We thanked the cashier and ran cheerfully out of the store."
                ,
  translateTest "Но тут, вдумываясь в слова кассирши, мы опять приуныли, так как ее слова показались нам лишенными всякого смысла."
                "But there, thinking carefully about cashier's words, we got sad again because her words were void of any meaning."
                ,
  translateTest "Что нам было делать?"
                "What were we supposed to do?"
                ,
  translateTest "Мы пошли в Летний сад и стали там считать деревья."
                "We went to the Summer Garden and started counting trees."
                ,
  translateTest "Но дойдя в счете до 6-ти, мы остановились и начали спорить: по мнению одних дальше следовало 7, по мнению других - 8"
                "But reaching a six in count, we stopped and started arguing: in the opinion of some, a 7 went next; but in opinion of others an 8 did"
                ,
  translateTest "Мы спорили бы очень долго, но, по счастию, тут со скамейки свалился какой-то ребенок и сломал себе обе челюсти."
                "We were arguing for a long time, when by some sheer luck, a child fell off a bench and broke both of his jaws."
                ,
  translateTest "Это отвлекло нас от нашего спора"
                "That distracted us from our argument"
                ,
  translateTest "А потом мы разошлись по домам."
                "And then we all went home."
  ]
