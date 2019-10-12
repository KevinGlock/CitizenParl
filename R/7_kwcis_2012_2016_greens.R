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
                     parliamentary_group ="GRUENE",
                     year  = 2012:2016,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_greens, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## GREENS count 143 times '".*[Ss]taatsbürger.*"' (freq = 0.0000461265)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_greens, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## 


## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_greens, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE","APPRART", "ART", "PPOSAT")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)


## The plot shows the significant context words


## count '"[Dd]oppelstaat.*"'

count(coi_greens, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## GREENS count 17 times '"[Dd]oppelstaat.*"' (freq = 0.00000548357)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_greens, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## 1x Doppelstaatsbürgerschaftsabkommen, 2x -staatigkeit, 3x -staatlichkeit, 9x Doppelstaatler
## adjectives, articles, nouns, pronouns

## cooccurrences '"[Dd]oppelstaat.*"'

cooccurrences(coi_greens, query = '"[Dd]oppelstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART", "ART", "PPER", "PPOSAT", "PRELS", "PDS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, der, die
## articles, verbs, personal pronouns

## count '"[Mm]ehrstaat.*"'

count(coi_greens, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## GREENS count 21 times '"[Mm]ehrstaat.*"' (freq = 0.000006773822)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_greens, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## 19x -staatigkeit, 2x -staatlichkeit


## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_greens, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART", "ART", "PPER", "PPOSAT", "PRELS", "PDS")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## der, die, Hinnahme, der, akzeptieren, akzeptieren, die, bei


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_greens, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## GREENS count 134 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00004322344)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_greens, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## 


## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_greens, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPR","APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_greens, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## GREENS count 47 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.00001516046)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_greens, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## 13x -angehörigkeit, 34x -bürgerschaft
## articles

## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_greens, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "APPR", "APPRART", "ART", "PPOSAT", "PPER", "PRELS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, eine, der, die, gegen, das


## count '"Doppelpa(ss|ß).*"'

count(coi_greens, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## GREENS count 8 times '"Doppelpa(ss|ß).*"' (freq = 0.000002580504)
## (undefined) articles

## kwic '"Doppelpass.*"'

kwic(coi_greens, query = '"Doppelpass.*"', regex = T, cqp = T)


## count '"[Oo]ptionspflicht.*"'

count(coi_greens, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## GREENS count 64 times '"[Oo]ptionspflicht.*"' (freq = 0.00002064403)


## kwic '"[Oo]ptionspflicht.*"'

kwic(coi_greens, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T)

## 8x as adjective -pflichtig, 6x Optionspflichtige (as a person),
## 1x -abschaffungsgesetz, 3x -verlängerungsgesetz (two as a first word part truncation ..- und ...gesetz)
## 1x -debatte, 45x Optionspflicht
## articles, adjectives, nouns, quantifiers

## cooccurrences '"[Oo]ptionspflicht.*"'

cooccurrences(coi_greens, query = '"[Oo]ptionspflicht.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "NN", "CARD", "APPR", "TRUNC", "APPRART", "ART", "PPOSAT", "PRELS", "PDS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, der, die, Die, Abschaffung, die, 000, das, der


## count '"[Oo]ptionszwang.*"'

count(coi_greens, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## GREENS count 24 times '"[Oo]ptionszwang.*"' (freq = 0.000007741511)


## kwic '"[Oo]ptionszwang.*"'

kwic(coi_greens, query = '"Optionszwang.*"', regex = T, cqp = T)

## (undefined) articles, prepositions


## cooccurrences '"Optionszwang.*"'

cooccurrences(coi_greens, query = '"Optionszwang.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "APPR", "APPRART", "ART", "PPOSAT", "PRELS", "PDS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## der, die, den, der, Der, den, die, Der, des


f1 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f2 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f3 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f4 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f5 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f6 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f7 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f8 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f9 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f10 <- features(coi_greens, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)