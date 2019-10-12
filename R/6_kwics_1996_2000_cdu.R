## load libraries

library("polmineR")
library("magrittr")
library("data.table")

use("GermaParl")


## get dictionaries

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"',
        '"[Dd]oppel.*" "[Ss]taat.*"', '"Abstammungs(recht|prinzip).*"')

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
        '"[Dd]oppel.*" "[Ss]taat.*"', '"Doppelpa(ss|ß).*"',
        '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"')


## get CDU/CSU partition (coi)

coi_cdu <- partition("GERMAPARL",
                   parliamentary_group = "CDU/CSU",
                   year  = 1996:2000,
                   interjection= F,
                   encoding = "latin1",
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_cdu, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 183 times '".*[Ss]taatsbürger.*"' (freq = 0.00004003041)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_cdu, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## Most times the noun Staatsbürger(schaft|schafts|schaftsrecht|schaftsrecht(e)s|schaftskunde) is used in some times it is the prefix DDR- used, only 9 times Staatsbürgerschaft is used as derived form of a adjevctive.
## In various contexts the 2-gram (doppelt.*|deutsche.*|türkische.*) Staatsbürgerschaft is used.
## In the context of Staatsbürger as individuals or discourse collectives the word standing before is mostly personal pronoun, preposition, indefinite article or an adjective.


## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_cdu, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE","APPRART", "ART", "PPOSAT")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)


## The plot shows the significant context words
## doppelte, doppelten, eine, generelle, Uniform, deutsche,
## die, generellen, deutscher, (Beifall - OCR error), ausländische,
## verzichtet (2x), Zulassung, Integration, alte, der, Einführung, DDR, das 


## count '"[Dd]oppelstaat.*"'

count(coi_cdu, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 30 times '"[Dd]oppelstaat.*"' (freq = 0.000006562363)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_cdu, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## Most times the noun Doppelstaatler(in|n) is used. Likewise the noun Doopelstaat(sangehörigkeit|sbürgerschaft|lichkeit) is used.
## Words standing before the node are personal pronouns, prepositions, indefinite articles, or adjectives and quantifiers.


## cooccurrences '"[Dd]oppelstaat.*"'

cooccurrences(coi_cdu, query = '"[Dd]oppelstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPR","APPRART", "ART", "PPOSAT", "PRELS", "PDS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, der, die, den, Zahl


## count '"[Mm]ehrstaat.*"'

count(coi_cdu, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 12 times '"[Mm]ehrstaat.*"' (freq = 0.000002624945)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_cdu, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## Only two forms exist. The noun Mehrstaat(igkeit|lichkeit) are both six times used.
## Words standing before the node are prepositions and articles.


## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_cdu, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## der, die, Vermeidung
## If we include additional characters one can see that "" is highly significant, coming from citations


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_cdu, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 278 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00006081123)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_cdu, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## Only nouns exist. The noun Staats(angehörigkeit|angehorigkeitsrecht(s|es)) are used the most. In some cases the prefix Kinder- is used.
## Very rare is the use of Drittstaatsangehörig(keit|e) and the reference to dual citizenship with both prefixes Doppel- und Regeldoppel-.
## Words standing before the node are prepositions, personal pronouns, (indefinite) articles, adjectives, cardinals and phrasal finalising punctuation.


## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_cdu, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPR","APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## doppelte, doppelten, deutsche, deutschen, Reform,
## ob, Hinnahme, regelmäßige, Einführung, behalten, generelle
## regelmäßigen, zu, Eltern, Integration, verleihen, Staatsangehörigkeit, verliehen, Geburt, behalten
## If we include additional characters one can see that mostly articles are highly significant.


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_cdu, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 125 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.00002734318)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_cdu, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## The 2-gram doppelte(n) Staats(angehörigkeit(en)|bürgerschaft(en)) are used.
## Very rare is the use of Drittstaatsangehörig(keit|e) and the reference to dual citizenship with both prefixes Doppel- und Regeldoppel-.
## Words standing before the node are prepositions, personal pronouns, (indefinite) articles, adjectives, and nouns.


## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_cdu, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPR","APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words


## count '"Doppelpa(ss|ß).*"'

count(coi_cdu, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 24 times '"Doppelpa(ss|ß).*"' (freq = 0.00006081123)


## kwic '"Doppelpa(ss|ß).*"'

kwic(coi_cdu, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T)

## Only nouns exist. The noun Doppelpaß are used most times. In one case the sufix -entscheidung is used.
## Very rare is the use of Doppelpass(es).
## Words standing before the node are (indefinite) articles, nouns, and adjectives.


## cooccurrences '"Doppelpa(ss|ß).*"'

cooccurrences(coi_cdu, query = '"Doppelpa(ss|ß).*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## den, den, der, die, der, haben, den, die


## count '"[Oo]ptionspflicht.*"'

count(coi_cdu, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 1 times '"[Oo]ptionspflicht.*"' (freq = 0.0000002187454)


## kwic '"Doppelpa(ss|ß).*"'

kwic(coi_cdu, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T)

## Only one node exists from 1996 to 2000 for CDU/CSU.
## The word beforehand is a adjective.

## cooccurrences can not be executed for a single case


## count '"[Oo]ptionszwang.*"'

count(coi_cdu, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 0 times '"[Oo]ptionszwang.*"'
