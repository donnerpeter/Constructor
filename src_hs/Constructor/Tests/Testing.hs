module Constructor.Tests.Testing where

data TranslateTest = TranslateTest { src:: String, target:: String, timeLimit:: Int }

translateTest src target = textTranslateTest 100 src target

textTranslateTest time src target = TranslateTest src target time

