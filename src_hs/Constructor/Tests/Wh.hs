module Constructor.Tests.Wh where
import Constructor.Tests.Testing
import Test.HUnit

whTests = [TestLabel "wh tests" $ TestList [
  translateTest "Что они думают по этому поводу?"
                "What do they think on this matter?"
  ,
  translateTest "Кому она сломала челюсть?"
                "Whose jaw did she break?"
  ,
  translateTest "Что она ему сломала?"
                "What did she break?"
  ,
  translateTest "Кого он спросил?"
                "Who did he ask?"
  ,
  translateTest "О чём он спросил соседей?"
                "What did he ask his neighbors about?"
  ,
  translateTest "Что идёт раньше - 7 или 8?"
                "What comes first - 7 or 8?"
  ,
  translateTest "Делать нам было нечего"
                "We had nothing to do"
  ,
  translateTest "Нам было нечего делать"
                "We had nothing to do"
  ,
  translateTest "Нам нечего было делать"
                "We had nothing to do"
  ,
  translateTest "Делать нам нечего"
                "We have nothing to do"
  ,
  translateTest "Делать нечего"
                "There's nothing to do"
  ,
  translateTest "Делать было нечего"
                "There was nothing to do"
  ,
  translateTest "Нам есть что делать"
                "We have something to do"
  ,
  translateTest "Нам было что делать"
                "We had something to do"
  ,
  translateTest "Нам будет что делать"
                "We'll have something to do"
  ,
  translateTest "Нам будет нечего делать"
                "We'll have nothing to do"
  ,
  translateTest "Мы ничего не делали"
                "We did nothing"
  ,
  translateTest "Некому танцевать"
                "There's nobody to dance"
  ,
  translateTest "Мне нечего сказать"
                "I have nothing to say"
  ,
  translateTest "Мне есть что сказать"
                "I have something to say"
  ,
  translateTest "Мне есть что сказать ему"
                "I have something to tell him"
  ,
  translateTest "Ему есть что сказать мне"
                "He has something to tell me"
  ,
  translateTest "К кому нам идти?"
                "Who should we go to?"
-- todo nonprojective есть что ,
--  translateTest "Есть мне что сказать"
--                "I have something to say"
  ,
  translateTest "Мне не к кому идти"
                "I have nobody to go to"
  ,
  translateTest "Мне есть к кому идти"
                "I have somebody to go to"
  ,
  translateTest "Мне некуда идти"
                "I have nowhere to go"
  ,
  translateTest "Куда мне идти?"
                "Where should I go?"
  ,
  translateTest "Куда они пошли?"
                "Where did they go?"
  ,
  translateTest "Мне некого любить"
                "I have nobody to love"
  ,
  translateTest "Мне некого больше любить"
                "I have nobody to love anymore"
--  ,
-- todo nonprojective больше не
--  translateTest "Больше мне некого любить"
--               "I have nobody to love anymore"
  ,
  translateTest "Мне больше некого любить"
                "I have nobody to love anymore"
--  ,
--  translateTest "Мне некого любить больше"
--               "I have nobody to love anymore"
  ,
  translateTest "Меня некому больше любить"
                "There's nobody to love me anymore"
  ,
  translateTest "Меня никто больше не любит"
                "Nobody loves me anymore"
  ,
  translateTest "Она меня больше не любит"
                "She doesn't love me anymore"
  ,
  translateTest "Она больше меня не любит"
                "She doesn't love me anymore"
  ,
  translateTest "Мы никуда не идём"
                "We go nowhere"
  ,
  translateTest "Я забыл, о чём он спросил соседей."
                "I forgot what he asked his neighbors about."
--  ,
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
--todo  translateTest "О чём эта книга?"
--                "What is this book about?"
--  ,
--todo  translateTest "О чём была та книга?"
--                "What was that book about?"
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
  translateTest "Куда он шёл?"
                "Where was he going?"
  ,
  translateTest "Когда он увидел дом?"
                "When did he see the house?"
--  ,
--todo  translateTest "Каково это — курить?"
--                "What is it like to smoke?"
--  ,
--todo  translateTest "Каково его мнение?"
--                "What's his opinion?"
  ,
  translateTest "Я увидел дом, где они сидят"
                "I saw the house where they were sitting"
  ,
  translateTest "Я увидел дом, в котором они сидят"
                "I saw the house that they were sitting in"
--  ,
--todo  translateTest "Я увидел дом, у которого они сидят"
--                "I saw the house that they were sitting next to"
--  ,
--todo  translateTest "Я спросил про дом, что он увидел"
--                "I asked about that house that he saw"
  ,
  translateTest "Где они сидят?"
                "Where are they sitting?"
--  ,
--todo  translateTest "В каком доме они сидят?"
--                "Which house are they sitting in?"
--  ,
--todo  translateTest "В котором доме они сидят?"
--                "Which house are they sitting in?"
--  ,
--todo  translateTest "У какого дома они сидят?"
--                "Which house are they sitting at?"
--  ,
--todo  translateTest "У которого дома они сидят?"
--                "Which house are they sitting at?"
--  ,
--todo  translateTest "Какой дом он увидел?"
--                "Which house did he see?"
  ,
  translateTest "Кому он велел помочь?"
                "Who did he order to help?"
  ,
  translateTest "Кому он велел ей помочь?"
                "Who did he order to help her?"
  ,
  translateTest "Кому он велел помочь, ей?"
                "Who did he order to help - her?"
--  ,
--todo  translateTest "Кто кому велел помочь?"
--                "Who ordered to help whom?"
  ,
  translateTest "Что он подарил Васе?"
                "What did he give to Vasya?"
  ,
  translateTest "Что он подарил?"
                "What did he give?"
  ,
  translateTest "Кому он подарил книгу?"
                "Who did he give the book to?"
  ,
  translateTest "Кому он подарил книгу Васи?"
                "Who did he give Vasya's book to?"
--  ,
--todo  translateTest "Что и кому он подарил?"
--                "What and to whom did he give?"
--  ,
--todo  translateTest "Кто, что и кому подарил?"
--                "Who gave what and to whom?"
--  ,
--todo  translateTest "Кто подарил что кому?"
--                "Who gave what to whom?"
--  ,
--todo  translateTest "Кто подарил что и кому?"
--                "Who gave what and to whom?"
--  ,
  ]]
