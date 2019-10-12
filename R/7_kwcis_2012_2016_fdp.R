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
                     year  = 2012:2016,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_fdp, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## FDP count 15 times '".*[Ss]taatsbürger.*"' (freq = 0.00001284941)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_fdp, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## 3x -bürgerschaft, 1x adjective staatsbürgerlicher, 1x -kundeunterricht, 10x Staatsbürger(n)
## adjectives, articles

## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_fdp, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "PPER", "APPRART", "ART", "PPOSAT")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## namibische, deutsche, fairem, knüpft, mehrfachen, bitten, namibischen, erleichterten, eine, doppelten, doppelte


## count '"[Dd]oppelstaat.*"'

count(coi_fdp, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## FDP count 1 times '"[Dd]oppelstaat.*"' (freq = 0.0000008566272)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_fdp, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## die Doppelstaatsangehörigkeit


## count '"[Mm]ehrstaat.*"'

count(coi_fdp, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## FDP count 2 times '"[Mm]ehrstaat.*"' (freq = 0.000001713254)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_fdp, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## both 1x Mehrstaatlichkeit and Mehrstaatsangehörigkeit
## citation and an article


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_fdp, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## FDP count 51 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00004368799)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_fdp, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## 5x Drittstaatsangehörige(r|n), 2x Mehrfachstaatsangehöigkeit(en), 1 x Mehrstaatsangehörigkeit
## no -gesetz, no -zugehörigkeit, 6x -recht(s), 9x Staatsangehörige(n), 27 x Staatsangehörigkeit, 1x Herkunftsstaatsangehörigkeit
## (undefined) articles, prepositions, pronouns, adjectives

## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_fdp, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "ART", "PPER", "PPOSAT", "APPR", "APPRART", "PRELS", "PDS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, der, die, eine, doppelten, doppelte, den, deutsche, das, die, deutschen


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_fdp, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## FDP count 12 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.00001027953)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_fdp, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## one -bürgerschaft, 11 -angehörigkeit
## (undefined) articles


## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_fdp, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR", "APPRART", "ART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 11.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## reden, stoppt, reden, Hinnahme, vermehrte, eine, erlauben


## count '"Doppelpa(ss|ß).*"'

count(coi_fdp, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## FDP count 0 times '"Doppelpa(ss|ß).*"'


## count '"[Oo]ptionspflicht.*"'

count(coi_fdp, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## FDP count 3 times '"[Oo]ptionspflicht.*"' (freq = 0.000002569882)


## kwic '"Optionspflicht"'

kwic(coi_fdp, query = '"Optionspflicht"', regex = T, cqp = T)

## articles


## count '"[Oo]ptionszwang.*"'

count(coi_fdp, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## FDP count 1 times '"[Oo]ptionszwang.*"' (freq = 0.0000008566272)


## kwic '"Optionszwang"'

kwic(coi_fdp, query = '"Optionszwang"', regex = T, cqp = T)

## den...abschaffen

f1 <- features(coi_fdp, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f2 <- features(coi_fdp, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f3 <- features(coi_fdp, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f4 <- features(coi_fdp, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f5 <- features(coi_fdp, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f6 <- features(coi_fdp, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f7 <- features(coi_fdp, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f8 <- features(coi_greens, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f9 <- features(coi_greens, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f10 <- features(coi_lefts, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)