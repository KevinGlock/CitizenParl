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


## get GREENS partition (coi)

coi_greens <- partition("GERMAPARL",
                   parliamentary_group = "GRUENE",
                   year  = 1996:2000,
                   interjection= F,
                   encoding = "latin1",
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_greens, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## GREENS count 240 times '".*[Ss]taatsbürger.*"' (freq = 0.0001091318)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_greens, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## 5 times as an adjective, two times as an named entity, in all other times as noun. Also like the CDU/CSU most times as Staatsbürger(schaft(s)|recht(s)).
## 9 times as Doppelstaatsbürger(n|schaft)
## The GREENS does very often refer to doppelte Staatsbürgerschaft
## Most often the wort beforehand the node is an adjective, (undefined) article, ful stop, noun, and prepositions.

## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_greens, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE","APPRART", "ART", "PPOSAT", "CARD")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)


## The plot shows the significant context words
## doppelte, doppelten, deutsche, Uniform, Hinnahme, (Beifall), Reform, ganz, Entlassung,
## Privileg, doppelter, Geburt, die, deutscher, Staatsbürger, Rechtsanspruch, bekommen, sein, geben, beim


## count '"[Dd]oppelstaat.*"'

count(coi_greens, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## GREENS count 12 times '"[Dd]oppelstaat.*"' (freq = 0.000005456589)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_greens, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## 8 times Doppelstaatsbürger(n), one time Doppelstaatlichkeit, two times Doppelstaatler(n), single time Doppelstaatsbürgerschaften
## preposition, article, adverb, quantifier, personal pronoun


## cooccurrences '"[Dd]oppelstaat.*"'

cooccurrences(coi_greens, query = '"[Dd]oppelstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(ll >= 11.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "NN", "APPRART", "ART", "PPER", "CARD")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (caused by the low number of counts the criterion is reduced to minimum of one count in cells and a higher significance (from 10.83 to 11.83))
## Pflichten, ganz, Rechte, richten, polemisiert, soli, Sicherheitsrisiko, klarmachen (2x), (BÜNDINS), Nationalmannschaft, Leitung, Nr, erwerben

## count '"[Mm]ehrstaat.*"'

count(coi_greens, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## GREENS count 11 times '"[Mm]ehrstaat.*"' (freq = 0.000005001873)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_greens, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## two times Mehrstaatigkeit, 9 times Merhstaatlichkeit
## preposition, (undefined) article


## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_greens, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1)%>%
  subset(ll >= 11.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "NN", "APPRART", "ART", "PPER", "CARD")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (caused by the low number of counts the criterion is reduced to minimum of one count in cells and a higher significance (from 10.83 to 11.83))
## Hinnahme, eine, Vermeidung, Grundsatz, soli, Lösungswege, hinnimmt, bewährte, Vorliegen


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_greens, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## GREENS count 139 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00006320549)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_greens, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## Staatsangehörigkeit(s|srecht(e|es)), Zugehörigkeit only two times with prefix Kinder-, doppelte only as adjective not as prefix; one count for Schnupper-, EU-
## (undefined) article, preposition, adjective, personal pronoun

## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_greens, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "PPER", "APPR","APPRART", "ART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## Reform, eine des, das, neues, Beifall, deutsche, europäisches, zu, bekommen, modernes,
## reformieren, bekommen, Ausländer, bekommen, machen, Kinder, ein, Mit, neue

## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_greens, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## GREENS count 70 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.0000318301)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_greens, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## no Staatszugehörigkeit, only Staatsangehörigkeit and -bürgerschaft
## significant word form beforehand are (undefined) article, prepositions, personal pronoun, nouns

## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_greens, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "ART", "APPR", "APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## Hinnahme, eine, Privileg, die, bekommen (3x), der, Menschen


## count '"Doppelpa(ss|ß)"' --> .* is excluded because of wrong context

count(coi_greens, query = '"Doppelpa(ss|ß)"', regex = T, cqp = T, feq = T)

## GREENS count 4 times '"Doppelpa(ss|ß)"' (freq = 0.000001818863)


## kwic '"Doppelpa(ss|ß)"'

kwic(coi_greens, query = '"Doppelpa(ss|ß)"', regex = T, cqp = T)

## only four times Doppelpaß with article --> no cooccurrences


## count '"[Oo]ptionspflicht.*"'

count(coi_greens, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## GREENS count 0 times '"[Oo]ptionspflicht.*"'


## count '"[Oo]ptionszwang.*"'

count(coi_greens, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## GREENS count 0 times '"[Oo]ptionszwang.*"'
