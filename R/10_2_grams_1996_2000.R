## 2-grams


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

right_2grams <- partition(coi_r, year = 1996:2000) %>% # time span of interest
  partition(interjection = F) %>%
  polmineR::ngrams(n = 2, p_attribute = c("word", "pos")) # change n for n-grams

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_2grams <- partition(coi_l, year = 1996:2000) %>%
  partition(interjection = F) %>%
  polmineR::ngrams(n = 2, p_attribute = c("word", "pos"))


## build ngram feature object to compare subsets and convert it to data table

dt1 <- features(right_2grams, left_2grams, included = TRUE) %>%
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table()


## fill the data table with specific columns

dt11 <- subset(dt1, dt1[["1_pos"]] == "ADJA") %>% subset(.[["2_pos"]] == "NN") #%>% subset(.[["2_word"]] %in% "StaatsbÃ¼rgerschaft") # 2-gram ADJA-NN

dt11[,"1_pos" := NULL][,"2_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)]

View(dt11)


## doppelten	Staatsangehörigkeit	29	-12
## andere 	Staatsangehörigkeit	7	-5
## ausländischen	Staatsangehörigkeit	6	-3
## deutschen	Staatsangehörigkeit	13	-2
## deutsche	Staatsangehörigkeit	38	8
## neue	Asylrecht	5	-4
## nationalen	Recht	4	-3
## rechtlichen	Möglichkeiten	14	-3
## engster	Abstimmung	4
## deutscher	Abstammung	3	-2
## nationaler	Solidarität	7	-6