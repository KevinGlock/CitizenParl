## cooccurrence network of subsets

##                        actors                                                         issues
## 1            [Dd]oppelstaat.*                                              .*[Aa]us.bürger.*
## 2              [Mm]ehrstaat.*                                              .*[Ee]in.bürger.*
## 3         .*[Ss]taatsbürger.*                                               Doppelpa(ss|ß).*
## 4 .*[Ss]taats(an|zu)gehörig.* '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"'
## 5             [Ss]taatenlos.*                                                       Pa(ss|ß)
## 6                                                                        [Oo]ptionspflicht.*
## 7                                                                          [Oo]ptionszwang.*
##                                                           norm
## 1                                                 Blutsrecht.*
## 2                                     Geburts(recht|prinzip).*
## 3 '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"'
## 4                                 Abstammungs(recht|prinzip).*


## load libraries

library("polmineR")
library("magrittr")
library("data.table")
library("tm")

use("GermaParl")


## get collocations from coi and ref


## coi right parties

coi_r <- partition("GERMAPARL",
                   party = c(
                     "CDU", "CSU"),
                   year  = 1996:2016, # vary the year
                   interjection= F,
                   encoding = "latin1",
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government")) # %>% # use the pipe
  cooccurrences(query = '"[Dd]oppel.*" ".*[Ss]taatsbürger.*"', # vary the query
                cqp = TRUE,
                regex = T,) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP")) %>% # use pos tagging
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(", "bzw")) %>%
  dotplot(cex = 0.8)
  

## ref left parties

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   year = 1996:2016,
                   interjection= F,
                   encoding = "latin1",
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government")) # %>%  # use the pipe
  cooccurrences(query = '"[Dd]oppel.*" ".*[Ss]taatsbürger.*"', # vary the query
                cqp = TRUE,
                regex = T,) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP")) %>% # "NN", "NE"
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)


## with query
  
naturalisation <- '".*[Ee]in.*bürger.*"'

cooccurrences(coi_r, query = naturalisation, regex = T, cqp = T, p_attribute = c("word", "pos")) %>%
  subset(count_coi >= 5) %>% 
  subset(ll >= 11.83) %>%
  subset(pos %in% c("NN", "ADJA", "VVFIN", "NE")) %>%
  dotplot(cex = 0.8)


