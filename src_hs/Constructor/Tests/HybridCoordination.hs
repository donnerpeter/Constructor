module Constructor.Tests.HybridCoordination where
import Constructor.Tests.Testing
import Test.HUnit

hybridCoordinationTests = [TestLabel "hybrid coordination tests" $ TestList [
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
  ]]