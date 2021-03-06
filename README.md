This is a deterministic parser of a subset of Russian equipped with deep semantic representation/inference and human-like translation into English.
Text samples are mostly based on Daniil Kharms' [Sonnet](sonnet_text.txt) short story, hence their strangeness.
The source code is in Haskell.

## Examples

### wh-questions

    translateTest "Что они думают по этому поводу?"
                  "What do they think on this matter?"

    translateTest "Что идёт раньше - 7 или 8?"
                  "What comes first - 7 or 8?"

    translateTest "Кому он велел ей помочь?"
                  "Who did he order to help her?"

    translateTest "Кто кому велел помочь?"
                  "Who ordered whom to help?"

    translateTest "Кто это был? Кем он был? А кем была она?"
                  "Who was this? What was he? And what was she?"

### Subordinate clauses

    translateTest "Я вдруг забыл, что идет дальше."
                  "I suddenly forgot what comes next."

    -- wh-subject/complementizer ambiguity resolved semantically
    translateTest "Она забыла, что идет после Васи."
                  "She forgot, that she went after Vasya."

    translateTest "Я спросил их, что они думают по этому поводу"
                  "I asked them about their opinion on this matter"

### Infinitival clauses

    translateTest "Что нам было делать?"
                  "What were we supposed to do?"

    translateTest "Ему есть что сказать мне"
                  "He has something to tell me"

    translateTest "Некому танцевать"
                  "There's nobody to dance"

### Copula

    translateTest "Я на углу"
                  "I'm on the corner"

    -- copula in a subordinate clause
    translateTest "Мы пошли в магазин \"Гастроном\", что на углу, и спросили кассиршу о нашем недоумении"
                  "We went to a grocery store, the one that's on the corner to consult a cashier on our predicament"

    -- overt copula verb "is"
    translateTest "У меня есть арбуз."
                  "I have a watermelon."

    -- a poem with subject ellipsis
    translateTest "Вчера Василий был кассир, сегодня он уж продавец, а завтра будет бригадир. Такой вот он у нас хитрец!"
                  "Yesterday Vassily was a cashier, today he's already a salesman, and tomorrow he'll be a brigadier. What a cunning person we have here!"

### Speech, direct and indirect

    translateTest "Кассирша сказала, подвигав носом:\n- Семь идет после восьми."
                  "The cashier said, moving her nose back and forth:\n- A seven comes after an eight."

    translateTest "Кассирша сказала, подвигав носом, что, по ее мнению, семь идет после восьми в том случае, когда восемь идет после семи"
                  "The cashier said, moving her nose back and forth, that in her opinion, a seven comes after an eight, only if an eight comes after a seven"

### Reflexive pronouns

    translateTest "Ребенок сломал себе обе челюсти и 8 пальцев"
                  "The child broke both of his jaws and 8 fingers"

    translateTest "Он увидел их семью своими глазами"
                  "He saw their family with his own eyes"

### Coordination with argument ellipsis

    -- elided word is both genitive dependent and wh
    translateTest "Я забыл, по словам и мнению кого семь идет после восьми."
                  "I forgot according to and in the opinion of whom a seven comes after an eight."

    translateTest "1, 2, 3, 4, 5 и 6 помнят, а дальше забыли."
                  "They remember 1, 2, 3, 4, 5 and 6, but forgot what comes next."

### Verbal ellipsis

    translateTest "Но дойдя в счете до 6-ти, мы остановились и начали спорить: по мнению одних дальше следовало 7, по мнению других - 8"
                  "But reaching a six in count, we stopped and started arguing: in the opinion of some, a 7 went next; but in opinion of others an 8 did"

    -- non-elided parts of different grammatical categories + ellipsis in another sentence
    translateTest "Глупый кассир идет на работу. А умные - гулять."
                  "The stupid cashier goes to the work. And the smart ones - for a walk."

    -- non-marked ellipsis
    translateTest "Потом из окна вывалилась третья старуха, потом четвертая, потом пятая."
                  "Then the third old lady fell out of the window, then the fourth did, then the fifth."

### Hybrid coordination (aka coordination of unlikes)

    translateTest "Все и всё знают"
                  "Everybody knows everything"

    translateTest "Кто, что и кому дал?"
                  "Who gave what, and to whom?"

### Other coordination

    -- noun coordinated with an adjective (acting as another noun)
    translateTest "Я люблю капусту, но не жареную."
                  "I like cabbage, but not roasted."

### English generation challenges

    -- translate present tense as past
    translateTest "Каково же было их и мое удивление, когда они вдруг обнаружили, что тоже не могут вспомнить порядок счета. \
                          \1, 2, 3, 4, 5 и 6 помнят, а дальше забыли."
                  "Great was their and my amazement, when they suddenly discovered, that they couldn't recall the counting order. \
                          \They remembered 1, 2, 3, 4, 5 and 6, but forgot what comes next."

    -- Resolve a pronoun reference and choose the correct English equivalent
    translateTest "Это арбуз. Он зелёный."
                  "This is a watermelon. It's green."

    -- Gender-specific translation
    translateTest "Вася не кассир, а продавец. Маша не кассир, а продавец."
                  "Vasya is not a cashier, but a salesman. Masha is not a cashier, but a saleswoman."

    -- Specify gender explicitly for contrasted entities
    translateTest "Кассирша была умной. Кассир тоже будет умным."
                  "The female cashier was smart. The male cashier will also be smart."

## Notes

The goal of this project is to explore how computers can do natural language parsing in a more or less human-like way:
serial left-to-right incremental parsing with pruning of unlikely parse variants. Semantics is used to assess variant's likelihood.
To ensure that the semantic representation built by parser is sufficient, a separate generator produces an English traslation
 based solely on the text meaning. The translation is the "gold standard": it has been produced by a real human before.

The grammar formalism used is inspired by HPSG and Construction Grammar. The intermediate semantic representation is frame-based.
As syntactic parsing is the main focus of this project, everything else (morphology, semantics, English generator) is mocked up by design.

[IntelliJ IDEA](http://www.jetbrains.com/idea/) project files are included. The tests can be run using './runghc.sh' from 'src_hs' subdirectory.

[Setting up Cabal with sandbox](http://stackoverflow.com/questions/21199189/setting-up-yesod-on-ubuntu-13-10)