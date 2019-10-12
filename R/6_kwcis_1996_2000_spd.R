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


## get SPD partition (coi)

coi_spd <- partition("GERMAPARL",
                   parliamentary_group = "SPD",
                   year  = 1996:2000,
                   interjection= F,
                   encoding = "latin1",
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_spd, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## SPD count 165 times '".*[Ss]taatsbürger.*"' (freq = 0.00003692142)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_spd, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## most often Staatsbürger(schaft), and Staatsbürgerschaftsrecht no -rechtes, only -rechts; 11 times as an adjective
## adjective, (undefined) articles, conjunctions, pronoun


## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_spd, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPRART", "ART", "PPOSAT", "PPER")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  dotplot(cex = 0.8)


## The plot shows the significant context words
## Uniform, eine, doppelte doppelten, deutsche, Soldaten, des, Reform, Einbürgerung, Geburt,
## Staatbürgerschaft, Beifall, deutschen, einen, die, Bundeswehr, der, das, machen

## count '"[Dd]oppelstaat.*"'

count(coi_spd, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## SPD count 16 times '"[Dd]oppelstaat.*"' (freq = 0.000003580259)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_spd, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## 4x Doppelstaatlichkeit, no -igkeit; 9x Doppelstaatler, 2x -angehörigkeit, 1x -bürgerschaft
## article, preposition, nouns, quantifier

## cooccurrences '"[Dd]oppelstaat.*"'

cooccurrences(coi_spd, query = '"[Dd]oppelstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(ll >= 11.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR", "APPRART", "ART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (lower count and higher sig)
## eine, Doppelstaatlichkeit, Hinnahme, eingebürgerte, bestimmten, anstellen,
## Notwendigkeit, eingewandt, anstellen, Mehrstaatlichkeit, Streitpunkt, behalten, beträchtliche, Loyalität


## count '"[Mm]ehrstaat.*"'

count(coi_spd, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## SPD count 30 times '"[Mm]ehrstaat.*"' (freq = 0.000006712985)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_spd, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## -staatlichkeit and -staatigkeit are approximitly equivalent in counts; -staatigkeit exists 5 times more
## preposition and article

## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_spd, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR", "APPRART", "ART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## Hinnahme, hingenommen, von, unter, die


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_spd, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## SPD count 221 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00004945232)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_spd, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## -monopol; Kinder-, Doppel- (2x), Dritt-, -gesetzgebung, -regelung, staatsangehörigkeitsrechtliche (2x)
## Most often the word is used as noun; with personal recurrence (Der Staatsangehörige) only 23 times from 221
## (undefined) articles, adjectives, nouns, 

## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_spd, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR", "APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## Reform, doppelte, doppelten, Hinnahme, deutshe, Beifall, Einbürgerun, Geburt, ausländischer, modernes,
## Freitag, umfassende, deutschen, Neuregelung, schaffen, deutscher, bei, Schwierigkeiten, eigenen, zu


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_spd, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## SPD count 51 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.00001141207)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_spd, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## one times -zugehörigkeit, most often -angehörigkeit and -bürgerschaft approximitly equivalently often.
## nouns, articles, some times adjectives

## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_spd, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## Hinnahme, Einbürgerung, eine, generelle, gegen, Thema, unter, zum

## count '"Doppelpa(ss|ß).*"'

count(coi_spd, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## SPD count 1 times '"Doppelpa(ss|ß).*"' (freq = 0.000000237662)


## kwic '"Doppelpa(ss|ß).*"'

kwic(coi_spd, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, right = 20, left = 20, interjection = T)

## no adequate result


## count '"[Oo]ptionspflicht.*"'

count(coi_spd, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## SPD count 0 times '"[Oo]ptionspflicht.*"'


## count '"[Oo]ptionszwang.*"'

count(coi_spd, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## SPD count 0 times '"[Oo]ptionszwang.*"'
