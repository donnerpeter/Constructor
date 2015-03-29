module Constructor.Tests.OldLadies where
import Constructor.Tests.Testing

oldLadyTests = [
  translateTest "Одна старуха от чрезмерного любопытства вывалилась из окна, упала и разбилась."
                "Because of her excessive curiosity, an old lady fell out of the window and smashed into the ground."
                ,
  translateTest "Из окна высунулась другая старуха и стала смотреть вниз на разбившуюся, но от чрезмерного любопытства тоже вывалилась из окна, упала и разбилась."
                "Another old lady looked out of the window, staring down at the one who was smashed, but out of her excessive curiosity she also fell out of the window and smashed into the ground."
                ,
  translateTest "Потом из окна вывалилась третья старуха, потом четвертая, потом пятая."
                "Then the third old lady fell out of the window, then the fourth did, then the fifth."
                ,
  --todo full old ladies
  translateTest "Когда вывалилась шестая старуха, мне надоело смотреть на них, и я пошел на Мальцевский рынок, где, говорят, одному слепому подарили вязаную шаль."
                "When the sixth old lady fell out, I got bored watching them and went to Maltsev market where, they say, someone gave a woven shawl to a blind."
  ]

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
--                ,
--todo  translateTest "Он мне надоел. И кассирша мне тоже надоела."
--                "I've had enough of him. And I've had enough of the cashier, too."
--                ,
--todo  translateTest "Говорят, что здесь одному подарили шаль."
--                "They say that someone here gave a shawl to one."
--                ,
--todo  translateTest "Они говорят, что подарили слепому шаль."
--                "They say that they gave a shawl to a blind."
  ]