## 5-grams


## load libraries

library("polmineR")
library("magrittr")
library("data.table")

use("GermaParl")


## get partitions of interest with ngrams

coi_r <- partition("GERMAPARL",
                   party = c(
                     "CDU", "CSU"),
                   interjection= F,
                   p_attribute = c("word", "pos"), # enrich with word and part-of-speech annotation
                   role = c("mp", "government"))

right_5grams <- partition(coi_r, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 5, p_attribute = c("word", "pos"))

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_5grams <- partition(coi_l, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 5, p_attribute = c("word", "pos"))

dt4 <- features(right_5grams, left_5grams, included = TRUE) %>% 
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt44 <- subset(dt4, dt4[["3_pos"]] == "NN") #5-grams

dt44[,"3_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)] # formatting table; you can add more columns

View(dt44)

## Wer	PWS	unser	PPOSAT	Gastrecht	mißbraucht	VVPP	,	$,	4	-2