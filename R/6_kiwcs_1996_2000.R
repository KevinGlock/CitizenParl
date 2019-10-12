## load libraries

library("polmineR")
library("magrittr")
library("data.table")

use("GermaParl")


## get partitions by combined party (groups)


## cdu/csu

coi_cdu <- partition("GERMAPARL",
                     party = c(
                       "CDU", "CSU"),
                     year  = 1996:2000,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))

count(coi_cdu, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

cooccurrences(coi_cdu, query = '".*[Ss]taatsbürger.*"', # vary the query
              cqp = TRUE,
              regex = T,) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE")) %>%
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

kwic(coi_cdu, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T)


## spd

coi_spd <- partition("GERMAPARL",
                     party = "SPD",
                     year = 1996:2000,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))

count(coi_spd, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

cooccurrences(coi_spd, query = '".*[Ss]taatsbürger.*"', # vary the query
                cqp = TRUE,
                regex = T,) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE")) %>%
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

kwic(coi_spd, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T)


## greens

coi_greens <- partition("GERMAPARL",
                        party = "GRUENE",
                        year = 1996:2000,
                        interjection= F,
                        encoding = "latin1",
                        p_attribute = c("word", "pos"),
                        role = c("mp", "government"))

count(coi_greens, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

cooccurrences(coi_greens, query = '".*[Ss]taatsbürger.*"', # vary the query
                cqp = TRUE,
                regex = T,) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE")) %>%
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

kwic(coi_greens, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T)


## fdp

coi_fdp <- partition("GERMAPARL",
                     party = "FDP",
                     year = 1996:2000,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))

count(coi_fdp, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

cooccurrences(coi_fdp, query = '".*[Ss]taatsbürger.*"', # vary the query
                cqp = TRUE,
                regex = T,) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE")) %>%
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

kwic(coi_fdp, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T)


## lefts

coi_lefts <- partition("GERMAPARL",
                       party = c("LINKE", "PDS"),
                       year = 1996:2000,
                       interjection= F,
                       encoding = "latin1",
                       p_attribute = c("word", "pos"),
                       role = "mp")

count(coi_lefts, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

cooccurrences(coi_lefts, query = '".*[Ss]taatsbürger.*"', # vary the query
                cqp = TRUE,
                regex = T,) %>%
  subset(count_coi >= 4) %>%
  subset(ll >= 10.83) %>%
  subset(pos %in% c("ADJA", "VVFIN", "VVINF", "VVIZU", "VVPP", "NN", "NE")) %>%
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8)

kwic(coi_lefts, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T)


f1 <- features(coi_cdu, coi_spd, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

#f1 <- features(x = coi_cdu, y = coi_spd, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83) %>%
 # subset(pos %in% c("ADJA", "VVFIN", "NN")) %>% #  "VVINF", "VVIZU", "VVPP", "NE"
  #subset(!tolower(word) %in% tm::stopwords("de")) %>%
  #subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>% subset(rank_ll <= 100)

#view(f1)

#head(f1)

f2 <- features(coi_cdu, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f3 <- features(coi_cdu, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f4 <- features(coi_cdu, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f5 <- features(coi_spd, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f6 <- features(coi_spd, coi_greens, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f7 <- features(coi_spd, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f8 <- features(coi_greens, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f9 <- features(coi_greens, coi_lefts, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

f10 <- features(coi_lefts, coi_fdp, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

view(f1)
