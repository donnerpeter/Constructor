import Constructor.Tree
import Constructor.ParsingState
import Constructor.Lexicon
import Constructor.Parser
import Constructor.EnglishGenerator
import Constructor.Tests.Sonnet

toTranslate = [sonnetText,
               "Кассир сказал, что семь, по его мнению, идет после восьми в том случае, когда восемь идет после семи",
               "Кассирша сказала, подвигав носом, что, по ее мнению, семь идет после восьми в том случае, когда восемь идет после семи"]

main = putStrLn $ show $ map translate toTranslate