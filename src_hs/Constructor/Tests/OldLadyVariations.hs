module Constructor.Tests.OldLadyVariations where
import Constructor.Tests.Testing

oldLadyVariationTests = [
  translateTest "Один старик от чрезмерного любопытства вывалился из окна, упал и разбился."
                "Because of his excessive curiosity, an old man fell out of the window and smashed into the ground."
                ,
  translateTest "Окна соседей были закрыты."
                "The neighbors' windows were closed."
                ,
  translateTest "Другая старуха стала смотреть вниз на разбившуюся старуху."
                "Another old lady started staring down at the smashed old lady."
                ,
  translateTest "Другая старуха смотрит вниз на разбившуюся."
                "Another old lady stares down at the one who is smashed."
                ,
  translateTest "Из окна высунулась другая старуха."
                "Another old lady leaned out of the window."
                ,
  translateTest "Из окна вывалилась третья старуха. Потом четвертая."
                "The third old lady fell out of the window. Then the fourth did."
                ,
  translateTest "Потом из окна вывалилась третья старуха. Потом четвертая."
                "Then the third old lady fell out of the window. Then the fourth did."
                ,
  translateTest "Из окна вывалилась старуха. Потом другая."
                "The old lady fell out of the window. Then another one did."
                ,
  translateTest "Из окна вывалился старик. Потом старуха."
                "The old man fell out of the window. Then the old lady did."
  ]