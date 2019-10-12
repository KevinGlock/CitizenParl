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


## get FDP partition (coi)

coi_fdp <- partition("GERMAPARL",
                   parliamentary_group = "FDP",
                   year  = 1996:2000,
                   interjection= F,
                   encoding = "latin1",
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_fdp, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## FDP count 79 times '".*[Ss]taatsbürger.*"' (freq = 0.00004109983)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_fdp, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## mostly Staatsbürgerschaft, around a dozen times -schaftsrecht and Doppel-, no adjective form
## preposition, (undefined) article, adjectives, pronouns

## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_fdp, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE","APPRART", "ART", "PPOSAT", "PTKZU", "PPER", "PDS", "PDAT")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  dotplot(cex = 0.8)


## The plot shows the significant context words
## die, der, eine, doppelte, Uniform, Die, deutsche, deutschen, die, das


## count '"[Dd]oppelstaat.*"'

count(coi_fdp, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## FDP count 10 times '"[Dd]oppelstaat.*"' (freq = 0.00000520251)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_fdp, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## only Doppelstaatsbürgerschaft
## adjectives, (undefined) articles

## cooccurrences '"[Dd]oppelstaat.*"'

cooccurrences(coi_fdp, query = '"[Dd]oppelstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(ll >= 11.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART", "ART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (lower count, higher sig)
## flächendeckender, Doppelstaatsbürgerschaft, eine, generellen, 
## erleichterte, rechtspolitischen, merkwürdige, verloren, Füße, um


## count '"[Mm]ehrstaat.*"'

count(coi_fdp, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## FDP count 15 times '"[Mm]ehrstaat.*"' (freq = 0.000007803765)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_fdp, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## mostly -staatigkeit, also -staatlichkeit, one time Mehrstaatsangehörigkeit
## (undefined) article, preposition

## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_fdp, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## der, Vermeidung, der, die, die


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_fdp, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## FDP count 163 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00008480092)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_fdp, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## mostly -keit; often -rechts, no adjective; Mehrfach-, Mehrstaats-, EU-
## (undefined) articles, personal pronouns, adjectives, conjunctions


## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_fdp, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPR","APPRART", "KON")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## doppelte, Modernisierung, deutsche, Beifall, Reform, modernes,
## entscheiden, entlassen, Regelfall, Eltern, entlassen, als, zu, bekommen, bisherige, zugunsten,
## Aufgabe, entscheiden, bisherigen, bekommen


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_fdp, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## FDP count 32 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.00001664803)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_fdp, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## mostly -angehörigkeit, lower frequently -bürgerschaft
## (undefined) articles, adjectives

## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_fdp, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## eine, Regelfall, als, um


## count '"Doppelpa(ss|ß).*"'

count(coi_fdp, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## FDP count 13 times '"Doppelpa(ss|ß).*"' (freq = 0.000006763263)


## kwic '"Doppelpa(ss|ß).*"'

kwic(coi_fdp, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T)

## articles and some time adjectives


## cooccurrences '"Doppelpa(ss|ß).*"'

cooccurrences(coi_fdp, query = '"Doppelpa(ss|ß).*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## den, der, den, der, den


## count '"[Oo]ptionspflicht.*"'

count(coi_fdp, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## FDP count 1 times '"[Oo]ptionspflicht.*"' (freq = 0.000000520251)


## kwic '"[Oo]ptionspflicht.*"'

kwic(coi_fdp, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T)

## eine Optionspflicht


## count '"[Oo]ptionszwang.*"'

count(coi_fdp, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## FDP count 0 times '"[Oo]ptionszwang.*"'
