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
                     year  = 2012:2016,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_spd, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## SPD count 143 times '".*[Ss]taatsbürger.*"' (freq = 0.00003031603)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_spd, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## 


## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_spd, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE","APPRART", "ART", "PPOSAT")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)


## The plot shows the significant context words


## count '"[Dd]oppelstaat.*"'

count(coi_spd, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## SPD count 4 times '"[Dd]oppelstaat.*"' (freq = 0.000000840007)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_spd, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## both 2x -staatlichkeit, -staatler


## count '"[Mm]ehrstaat.*"'

count(coi_spd, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## SPD count 27 times '"[Mm]ehrstaat.*"' (freq = 0.000005724005)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_spd, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## 19x -staatigkeit, 7x -staatlichkeit, 1x Mehrstaatler
## prepositions, articles

## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_spd, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "PDS", "PPOSAT", "PPER", "APPR", "APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, generelle, akzeptiert, akzeptiert, von


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_spd, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## SPD count 148 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00003137603)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_spd, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## 


## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_spd, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPR","APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_spd, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## SPD count 40 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.000008480007)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_spd, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## 24x -bürgerschaft, 16x -angehörigkeit
## articles, adjectives, pronouns

## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_spd, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "ART", "APPR","APPRART", "PPOSAT", "PDAT", "PDS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, eine, den, die


## count '"Doppelpa(ss|ß).*"'

count(coi_spd, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## SPD count 8 times '"Doppelpass.*"' (freq = 0.000001696001)


## kwic '"Doppelpass.*"'

kwic(coi_spd, query = '"Doppelpass.*"', regex = T, cqp = T)

## 1x -kampagne, 7x -pass
### pronouns, articles


## count '"[Oo]ptionspflicht.*"'

count(coi_spd, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## SPD count 44 times '"[Oo]ptionspflicht.*"' (freq = 0.000009328008)


## kwic '"[Oo]ptionspflicht.*"'

kwic(coi_spd, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T)

## 1x -pflichtig (adjective), all other -pflicht (nouns)
## articles

## cooccurrences '"[Oo]ptionspflicht.*"'

cooccurrences(coi_spd, query = '"[Oo]ptionspflicht.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, der, die, Abschaffung, mit, mit, Die, haben, abgeschafft, die, der, das


## count '"[Oo]ptionszwang.*"'

count(coi_spd, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## SPD count 13 times '"[Oo]ptionszwang.*"' (freq = 0.000002756002)


## kwic '"[Oo]ptionszwang.*"'

kwic(coi_spd, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T)

## -zwang(s|es)
## articles

## cooccurrences '"[Oo]ptionszwang.*"'

cooccurrences(coi_spd, query = '"[Oo]ptionszwang.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## der, der, des, der, wird


f1 <- features(coi_cdu, coi_spd, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f2 <- features(coi_cdu, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f3 <- features(coi_cdu, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f4 <- features(coi_cdu, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f5 <- features(coi_spd, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f6 <- features(coi_spd, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f7 <- features(coi_spd, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f8 <- features(coi_greens, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f9 <- features(coi_greens, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f10 <- features(coi_lefts, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)