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


## get LEFTS partition (coi)

coi_lefts <- partition("GERMAPARL",
                   parliamentary_group = c("PDS", "LINKE", "LINKE/PDS"),
                   year  = 1996:2000,
                   interjection= F,
                   encoding = "latin1",
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_lefts, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## LEFTS count 101 times '".*[Ss]taatsbürger.*"' (freq = 0.00006477589)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_lefts, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## 13 times Staatsbürger(s|n), some 80 counts for Staasbürgerschaft, whereby some 30 refer to -srecht(s), -frage, - gesetz
## also adjective forms occur
## (undefined) articles, adjectives, some pronouns

## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_lefts, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPRART", "ART", "PPER", "PRELS")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)


## The plot shows the significant context words
## doppelten, der, die, doppelte, Uniform, deutsche, dem, Reform, bzw (2x),
## erhalten das erhalten, Soldaten, erhalten, bzw


## count '"[Dd]oppelstaat.*"'

count(coi_lefts, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## LEFTS count 0 times '"[Dd]oppelstaat.*"'


## count '"[Mm]ehrstaat.*"'

count(coi_lefts, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## LEFTS count 4 times '"[Mm]ehrstaat.*"' (freq = 0.000002565382)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_lefts, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## only -staatigkeit
## only articles

## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_lefts, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART", "ART", "KON")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 11.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (low counts!!)
## Möglichkeit, Verengung, Türkinnen, Doppelpaß, eröffnen, Kreis 

## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_lefts, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## LEFTS count 74 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00004745957)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_lefts, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## (undefined) articles, adjectives, conjunctions


## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_lefts, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART", "ART", "KON")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## deutsche, doppelte, Geschlecht, bekommen, bzw (2x), bekommen, deutschen, bekommen, die, zu


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_lefts, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## LEFTS count 28 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.00001795767)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_lefts, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## mostly Staatsbürgerschaft lesser -angehörigkeit
## (undefined) articles


## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_lefts, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, eine, der, die, eine


## count '"Doppelpa(ss|ß).*"'

count(coi_lefts, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## LEFTS count 7 times '"Doppelpa(ss|ß).*"' (freq = 0.0000004489418)


## kwic '"Doppelpa(ss|ß).*"'

kwic(coi_lefts, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T)

## articles only; no .*


## cooccurrences '"Doppelpa(ss|ß)"'

cooccurrences(coi_lefts, query = '"Doppelpa(ss|ß)"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## den (3x)

## count '"[Oo]ptionspflicht.*"'

count(coi_lefts, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## LEFTS count 0 times '"[Oo]ptionspflicht.*"'


## count '"[Oo]ptionszwang.*"'

count(coi_lefts, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## LEFTS count 0 times '"[Oo]ptionszwang.*"'
