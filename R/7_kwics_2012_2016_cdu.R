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
                     year  = 2012:2016,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))


## count '".*[Ss]taatsbürger.*"'

count(coi_cdu, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 227 times '".*[Ss]taatsbürger.*"' (freq = 0.0000345022)


## kwic '".*[Ss]taatsbürger.*"'

kwic(coi_cdu, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, interjection = F)

## 
## adjectives, (undefined) articels, (personal) pronouns, (referring and comparing) conjunctions, nouns, additional symbols (XY), quantifiers

## cooccurrences '".*[Ss]taatsbürger.*"'

cooccurrences(coi_cdu, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPRART", "ART", "PPOS", "PPOSAT", "PPER", "KON", "XY")) %>%  
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## doppelte, deutsche, eine, Uniform, deutschen, behalten, Beifall,
## entscheiden, als, Erlangung, Verleihung, Staatsbürgerschaft,
## deutscher, muss, noch, türkische, doppelten, behalten wie, muss

## count '"[Dd]oppelstaat.*"'

count(coi_cdu, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 11 times '"[Dd]oppelstaat.*"' (freq = 0.000001671913)


## kwic '"[Dd]oppelstaat.*"'

kwic(coi_cdu, query = '"[Dd]oppelstaat.*"', regex = T, cqp = T, interjection = F)

## Doppelstaatl(er(n)|ligkeit), -angehörigkeitsverträge, -bürgerschaft


## cooccurrences '"[Dd]oppelstaat.*"'

cooccurrences(coi_cdu, query = '"[Dd]oppelstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(ll >= 11.83) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "--")) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (lower count, higher sig)
## die, in, Staatsangehörigkeit, Der, den, die, mit, Staatsbürgerschaft, deutsche,
## der, mit, bei, denen, Selin, bringt, ausschöpfen, Konfliktfall, Staatsangehörigkeitsgesetz,
## hochkomplexe, welche


## count '"[Mm]ehrstaat.*"'

count(coi_cdu, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 23 times '"[Mm]ehrstaat.*"' (freq = 0.000003495817)


## kwic '"[Mm]ehrstaat.*"'

kwic(coi_cdu, query = '"[Mm]ehrstaat.*"', regex = T, cqp = T)

## mostly -staatigkeit
## (undefined) articles, preposition, personal pronoun


## cooccurrences '"[Mm]ehrstaat.*"'

cooccurrences(coi_cdu, query = '"[Mm]ehrstaat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR","APPRART", "ART", "PPOSAT")) %>%
  subset(ll >= 11.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (lower count, higher sig)
## zuzulassen, generelle, eine, die, Ausnahme, Loyalitätskonflikten, muss, Optionszwang,
## vermeiden, Aufweichen, Grundsatz, akzeptieren, verankern, Staatsangehörigkeitsrechts,
## Ermöglichung, Abstriche


## count '".*[Ss]taats(an|zu)gehörig.*"'

count(coi_cdu, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 299 times '".*[Ss]taats(an|zu)gehörig.*"' (freq = 0.00004544563)


## kwic '".*[Ss]taats(an|zu)gehörig.*"'

kwic(coi_cdu, query = '".*[Ss]taats(an|zu)gehörig.*"', regex = T, cqp = T)

## less frequently refering to -gesetz(es), mostly -angehörigkeit(en|srecht(s)|sverträge|behörde), 3x -zugehörigkeit, 1x Doppel.*verträge
## (undefined) articles, preposition, adjectives, quantifiers, personal pronouns, $,

## cooccurrences '".*[Ss]taats(an|zu)gehörig.*"'

cooccurrences(coi_cdu, query = '".*[Ss]taats(an|zu)gehörig.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR", "APPRART", "PPER", "PPOSAT", "CARD")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## deutsche, eine, deutschen, entschieden, doppelten,
## doppelte, türkische, entscheiden, türkischer,
## zu, Özcan, türksichen, Erwerb, behalten, ganz,
## Staatsangehörigen, ob, bzw, britische, bzw


## count '"[Dd]oppel.*" "[Ss]taat.*"'

count(coi_cdu, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 53 times '"[Dd]oppel.*" "[Ss]taat.*"' (freq = 0.000008055579)


## kwic '"[Dd]oppel.*" "[Ss]taat.*"'

kwic(coi_cdu, query = '"[Dd]oppel.*" "[Ss]taat.*"', regex = T, cqp = T)

## no -zugehörigkeit, mostly -bürgerschaft
## (undefined) articles, prepositions

## cooccurrences '"[Dd]oppel.*" "[Ss]taat.*"'

cooccurrences(coi_cdu, query = '"[Dd]oppel.*" "[Ss]taat.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR", "APPRART", "ART", "PPOSAT", "PDS")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words
## die, der, eine, die, sagen, zu


## count '"Doppelpa(ss|ß).*"'

count(coi_cdu, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 8 times '"Doppelpa(ss|ß).*"' (freq = 0.000001215936)


## kwic '"Doppelpa(ss|ß).*"'

kwic(coi_cdu, query = '"Doppelpa(ss|ß).*"', regex = T, cqp = T)

## only Doppelpass
## articles


## cooccurrences '"Doppelpass"'

cooccurrences(coi_cdu, query = '"Doppelpass"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 1) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 11.83) %>%
  dotplot(cex = 0.8)

## The plot shows the significant context words (lower counts, higher sig)
## der, generellen, der, zunehmend, laxe,
## Nirgendwo, einen, einen, ein, Nirgendwo, taktische, Özcan,
## verschwindet, Özcan, mobilen, wählen, in, globalisierten, Welt, den


## count '"[Oo]ptionspflicht.*"'

count(coi_cdu, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 52 times '"[Oo]ptionspflicht.*"' (freq = 0.000007903587)

## kwic '"[Oo]ptionspflicht.*"'

kwic(coi_cdu, query = '"[Oo]ptionspflicht.*"', regex = T, cqp = T)

## some -pflichtigen (12x), mostly -plicht, a few adjective forms
## artiles and quantifiers

## cooccurrences '"[Oo]ptionspflicht.*"'

cooccurrences(coi_cdu, query = '"[Oo]ptionspflicht.*"', # vary the query
              cqp = T,
              regex = T) %>%
  subset(count_coi >= 4) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "APPR", "APPRART", "ART", "CARD")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  subset(ll >= 10.83) %>%
  dotplot(cex = 0.8)

## befreit, eine, befreit, die, Abschaffung, 90, der, für


## count '"[Oo]ptionszwang.*"'

count(coi_cdu, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T, feq = T)

## CDU/CSU count 4 times '"[Oo]ptionszwang.*"' (freq = 0.0000006079682)

## kwic '"[Oo]ptionszwang.*"'

kwic(coi_cdu, query = '"[Oo]ptionszwang.*"', regex = T, cqp = T)

## 1x -zwangs
## articles


f1 <- features(coi_cdu, coi_cdu, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f2 <- features(coi_cdu, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f3 <- features(coi_cdu, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f4 <- features(coi_cdu, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f5 <- features(coi_cdu, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f6 <- features(coi_cdu, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f7 <- features(coi_cdu, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f8 <- features(coi_greens, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f9 <- features(coi_greens, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f10 <- features(coi_lefts, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)