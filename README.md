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

### Subordinate clauses

    translateTest "Я вдруг забыл, что идет дальше."
                  "I suddenly forgot what comes next."

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

    translateTest "Мы пошли в магазин \"Гастроном\", что на углу, и спросили кассиршу о нашем недоумении"
                  "We went to a grocery store, the one that's on the corner to consult a cashier on our predicament"

    translateTest "У меня есть арбуз."
                  "I have a watermelon."

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

    translateTest "Я забыл, по словам и мнению кого семь идет после восьми."
                  "I forgot according to and in the opinion of whom a seven comes after an eight."

    translateTest "1, 2, 3, 4, 5 и 6 помнят, а дальше забыли."
                  "They remember 1, 2, 3, 4, 5 and 6, but forgot what comes next."

### Clause coordination with verbal ellipsis

    translateTest "Но дойдя в счете до 6-ти, мы остановились и начали спорить: по мнению одних дальше следовало 7, по мнению других - 8"
                  "But reaching a six in count, we stopped and started arguing: in the opinion of some, a 7 went next; but in opinion of others an 8 did"

    translateTest "Глупый кассир идет на работу, а умный - гулять"
                  "The stupid cashier goes to the work, but the smart one - for a walk"

### Hybrid coordination (aka coordination of unlikes)

    translateTest "Все и всё знают"
                  "Everybody knows everything"

    translateTest "Кто, что и кому дал?"
                  "Who gave what, and to whom?"

## Notes

The goal of this project is to explore how computers can do natural language parsing in a more or less human-like way:
serial left-to-right incremental parsing with pruning of unlikely parse variants. Semantics is used to assess variant's likelihood.
To ensure that the semantic representation built by parser is sufficient, a separate generator produces an English traslation
 based solely on the text meaning. The translation is the "gold standard": it has been produced by a real human before.

The grammar formalism used is inspired by HPSG and Construction Grammar. The intermediate semantic representation is frame-based.
As syntactic parsing is the main focus of this project, everything else (morphology, semantics, English generator) is mocked up by design.

[IntelliJ IDEA](http://www.jetbrains.com/idea/) project files are included. The tests can be run using './runghc.sh' from 'src_hs' subdirectory.

[Setting up Cabal with sandbox](http://stackoverflow.com/questions/21199189/setting-up-yesod-on-ubuntu-13-10)