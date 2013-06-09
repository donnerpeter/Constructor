package cons4.enrichment

import cons4.constructions.*
import cons4.*

fun handleWord(w: String): List<Mite> {
  val v = Vars()
  val v0 = v[0]

  if (Util.parseNumber(w) != null) {
    return multiXor(listOf(noun(v, nom, w), noun(v, gen, w), noun(v, acc, w))) + sem(v0, "number" to "true")
  }

  return when (w) {
    "было" -> l(phrase(v0, "verb"), nom("head" to v0, "noun" to v[1].lv)) + sem(v0, "time" to "PAST")
    "вдруг" -> l(phrase(v0.lv, "verb")) + sem(v0, "manner" to "SUDDENLY")
    "вспомнить" -> l(phrase(v0, "infinitive")) + sem.t(v0, "RECALL") + control("slave" to v0) + accArg(v)
    "думают" -> finiteVerb(v, "PRESENT", "THINK", agrNumber="pl", agrPerson=3) + arg(v, poDat, "topic") + accArg(v)
    "же" -> l(phrase("head" to v0.lv, "last" to true))
    "забыл" -> finiteVerb(v, "PAST", "FORGET", agrGender="m", agrNumber="sg") + accArg(v).xor(arg(v, comp, "arg2", "comp"))
    "забыли" -> finiteVerb(v, "PAST", "FORGET", agrNumber="pl") + accArg(v).xor(arg(v, comp, "arg2", "comp"))
    "и" -> l(seq("conj" to "and", "seqVar" to v0, "left" to v[1], "right" to v[2]))
    "идет" -> finiteVerb(v, "PRESENT", agrNumber="sg", agrPerson=3) + l(comeScalarly("head" to v0))
    "или" -> l(seq("conj" to "or", "seqVar" to v0, "left" to v[1], "right" to v[2]))
    "их" -> pronoun(v, acc, "THEY").xor(l(possessive("possessor" to v0), sem.t(v0, "THEY")))
    "к" -> preposition(v, kDat, dat)
    "каково" -> sem(v0, "type" to "degree", "arg1" to v[1], "arg2" to v[2]) + sem.t(v[2], "wh") + l(nom("head" to v0.lv, "noun" to v[1].lv), phrase(v0.lv, "verb"), phrase(v[2], "shortAdj"))
    "когда" -> l(conditionComp("comp" to v0), phrase(v0.lv, "verb"))
    "мной" -> pronoun(v, instr, "ME")
    "могут" -> finiteVerb(v, "PAST", "CAN", agrPerson=3, agrNumber="pl") + arg(v, control, "theme", "slave")
    "мое" -> l(possessive("possessor" to v0), sem.t(v0, "ME"))
    "не" -> l(phrase(v0.lv, "verb"), sem(v0, "negated", "true"))
    "обнаружили" -> finiteVerb(v, "PAST", "DISCOVER", agrNumber="pl") + arg(v, comp, "theme", "comp")
    "они" -> pronoun(v, nom, "THEY")
    "отправился" -> finiteVerb(v, "PAST", "GO_OFF", agrGender="m", agrNumber="sg") + arg(v, kDat, "goal")
    "по" -> preposition(v, poDat, dat)
    "поводу" -> noun(v, dat, "MATTER")
    "помнят" -> finiteVerb(v, "PRESENT", "REMEMBER", agrNumber="pl") + accArg(v)
    "порядок" -> noun(v, acc, "ORDER") + arg(v, gen, "criterion")
    "раньше" -> l(comeScalarly("head" to v0.lv, "order" to "EARLIER"))
    "случай" -> noun(v, nom, "THING")
    "случился" -> finiteVerb(v, "PAST", "HAPPEN", agrGender="m", agrNumber="sg") + arg(v, sInstr, "experiencer")
    "со" -> preposition(v, sInstr, instr)
    "соседям" -> noun(v, dat, "NEIGHBOURS")
    "счета" -> noun(v, gen, "COUNTING")
    "спросил" -> finiteVerb(v, "PAST", "ASK", agrGender="m", agrNumber="sg") + accArg(v) + arg(v, comp, "question", "comp")
    "тоже" -> l(phrase(v0.lv, "verb"), sem(v0, "also", "true"))
    "удивительный" -> adj(v, nom, "property", "AMAZING")
    "удивление" -> noun(v, nom, "AMAZE")
    "что" -> l(clauseType("clauseParent" to v[2])) +
           (pronoun(v, nom, "wh", "agrGender" to "n", "agrNumber" to "sg", "agrPerson" to 3).xor(pronoun(v, acc, "wh")) +
           l(question("head" to v[2], "questioned" to v0))).xor(l(complementizer("head" to v[2])))
    "этому" -> adj(v, dat, "determiner", "THIS")
    "я" -> pronoun(v, nom, "ME")
    "," -> l(phrase("head" to v0.lv, "kind" to "verb", "last" to true), sentence("verb" to v[1].lv)) +
           multiXor(listOf(
                   l(comp("comp" to v[2]), clauseType("clauseParent" to v[2].lv, "head" to v0)),
                   l(conditionComp("head" to v0))//,
                   //l(seq("conj" to ",", "seqVar" to v0, "left" to v[3], "right" to v[4]))
           )) +
           l(semSectionEnd("id" to v0))
    ":" -> l(semSectionEnd("id" to v0), phrase("head" to v0.lv, "kind" to "verb", "last" to true), elaboration("head" to v0, "elaboration" to v[1].lv, "first" to true), sem(v0, "elaboration", v[1]))
    "-" -> l(questionVariants("variants" to v0, "dummyHead" to v[1]), semSectionEnd("id" to v0))
    "." -> l(sentence("head" to v0, "verb" to v[1].lv), sem(v[1], "dot", "true"), semSectionEnd("id" to v0))
    else -> l()
  }
}

