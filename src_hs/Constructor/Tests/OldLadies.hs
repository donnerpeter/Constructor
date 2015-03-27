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
  translateTest "Когда вывалилась шестая старуха, мне надоело смотреть на них, и я пошел на Мальцевский рынок."
                "When the sixth old lady fell out, I got bored watching them and went to Maltsev market."
  ]