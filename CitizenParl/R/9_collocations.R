## cooccurrence network of subsets

##                actors                     issues
## 1     [Aa]us.bürger.*        [Oo]ptionspflicht.*
## 2    [Dd]oppelstaat.*          [Oo]ptionszwang.*
## 3     [Ee]in.bürger.* Doppelpa(ss|ß).*|"Pa(ss|ß)
## 4      [Mm]ehrstaat.*                           
## 5     [Ss]taatenlos.*                           
## 6 .*[Ss]taatsbürger.*                           
##                               norm
## 1                     Blutsrecht.*
## 2           Geburts(recht|prinzip)
## 3 [Ii]us (soli/sangruini/domicili)
## 4      .*[Ss]taats(an|zu)gehörig.*
## 5     Abstammungs(recht|prinzip).*


## load libraries

library("polmineR")
library("magrittr")
library("data.table")
library("networkD3")
library("igraph")
library("DT")
library("RColorBrewer")
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
  
natural_query <- '".*[Ee]in.*bürger.*"'

cooccurrences(coi_r, query = natural_query, regex = T, cqp = T, p_attribute = c("word", "pos")) %>%
  subset(count_coi >= 5) %>% 
  subset(ll >= 11.83) %>%
  subset(pos %in% c("NN", "ADJA", "VVFIN", "NE")) %>%
  dotplot(cex = 0.8)


