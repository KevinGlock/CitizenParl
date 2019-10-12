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
                     year  = 2012:2016,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_lefts, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## LEFTS count 107 times '".*[Ss]taatsbürger.*"' (freq = 0.00003657429)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_lefts, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## 1x DDR-, mostly -schaft, 12x -recht(s), 1x -schaftsfrage, 1x Options-, often Staatsbürger(s|in|innen)
## adjectives and articles

## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_lefts, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE", "APPRART", "ART", "PPOSAT")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## doppelte, deutsche, eine, Steuerpflicht, binden, doppelten,
## Beifall, die, des, deutschen, Demokratie, gilt


## count '"[Dd]oppelstaat.*"'

count(coi_lefts, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## LEFTS count 4 times '"[Dd]oppelstaat.*"' (freq = 0.000001367263)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_lefts, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## 2x -staatler, 1x -staatlerquote, 1x -sangehörigkeit


## count '"[Mm]ehrstaat.*"'

count(coi_lefts, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## LEFTS count 13 times '"[Mm]ehrstaat.*"' (freq = 0.000004443606)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_lefts, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## 3x -staatslichkeit, all other -staatigkeit
## articles, prepositions

## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_lefts, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "PDS", "PPOSAT", "APPR", "APPRART")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, bei, in


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_lefts, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## LEFTS count 110 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00003759974)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_lefts, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## most often -angehörigkeit and -angehörige(n), 6x -gesetz(es), 12x -recht, 8x Dritt-..angehörige(n|r) (one as adjective)
## -angehörigkeitsmodell, 1x Herkunfts-..angehörigkeit
## adjectives, quantifiers, articles, preposition
## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_lefts, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "ART", "PPER", "PPOSAT", "CARD", "APPR","APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## türkischen, türkische, türkischer, deutsche, doppelte, Beifall, bei, Eltern, zu, deutschen

## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_lefts, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## LEFTS count 39 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.00001333082)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_lefts, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## 7x -angehörigkeit, 32x -bürgerschaft
## (undefined) articles, pronouns, nouns

## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_lefts, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "PDS", "PPOSAT", "PIS", "APPR", "APPRART")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, eine, Beifall, eine, gegen


## count '"Doppelpa(ss|ß).*"'

count(coi_lefts, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## LEFTS count 8 times '"Doppelpa(ss|ß).*"' (freq = 0.000002734527)


## kwic '"Doppelpass.*"'

kwic(coi_lefts, query = '"Doppelpass.*"', regex = T, cqp = T)

## (undefined) articles, personal pronoun, adjective


## count '"[Oo]ptionspflicht.*"'

count(coi_lefts, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## LEFTS count 38 times '"[Oo]ptionspflicht.*"' (freq = 0.000012989)
## no declination


## kwic '"Optionspflicht"'

kwic(coi_lefts, query = '"Optionspflicht"', regex = T, cqp = T)

## articles


## cooccurrences '"[Oo]ptionspflicht.*"'

cooccurrences(coi_lefts, query = '"[Oo]ptionspflicht.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## der, die, der, Abschaffung, die, Die, eine, bei, eine, der


## count '"[Oo]ptionszwang.*"'

count(coi_lefts, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## LEFTS count 8 times '"[Oo]ptionszwang.*"' (freq = 0.000002734527)


## kwic '"Optionszwang.*"'

kwic(coi_lefts, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T)

## articles, 3x -zwangs


f1 <- features(coi_lefts, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f2 <- features(coi_lefts, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f3 <- features(coi_lefts, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f4 <- features(coi_lefts, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f5 <- features(coi_lefts, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f6 <- features(coi_lefts, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f7 <- features(coi_lefts, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f8 <- features(coi_greens, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f9 <- features(coi_greens, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f10 <- features(coi_lefts, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)