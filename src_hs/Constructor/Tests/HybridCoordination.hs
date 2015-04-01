module Constructor.Tests.HybridCoordination where
import Constructor.Tests.Testing

hybridCoordinationTests = [
  translateTest "Все и всё знают"
                "Everybody knows everything"
  ,
  translateTest "Знают все и всё"
                "Everybody knows everything"
  ,
  translateTest "Знают всё и все"
                "Everybody knows everything"
  ,
  translateTest "Все всё знают"
                "Everybody knows everything"
  ,
  translateTest "Все знают всё"
                "Everybody knows everything"
  ,
  translateTest "Никто не знает ничего"
                "Nobody knows anything"
  ,
  translateTest "Никто и ничего не знает"
                "Nobody knows anything"
  ,
  translateTest "Не знает никто и ничего"
                "Nobody knows anything"
  ,
  translateTest "Что и кому он дал?"
                "What did he give, and to whom?"
  ,
  translateTest "Кто и что дал?"
                "Who gave what?"
  ,
  translateTest "Кто, кому и что дал?"
                "Who gave what, and to whom?"
  ,
  translateTest "Кто, что и кому дал?"
                "Who gave what, and to whom?"
  ,
  translateTest "Кто дал что кому?"
                "Who gave what to whom?"
  ,
  translateTest "Кто дал что и кому?"
                "Who gave what, and to whom?"
--  ,
--todo  translateTest "Где и когда это случилось?"
--                "Where and when did happen?"
--  ,
--todo  translateTest "Он не дурак, а умнее тебя."
--                "He's not a fool, but smarter than you."
--  ,
--todo  translateTest "И Василий, и бежит - офигеть!"
--                "Vassily, and running - wow!"
  ]
