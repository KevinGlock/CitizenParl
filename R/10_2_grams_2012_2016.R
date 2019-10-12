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

right_2grams <- partition(coi_r, year = 2012:2016) %>% # time span of interest
  partition(interjection = F) %>%
  polmineR::ngrams(n = 2, p_attribute = c("word", "pos")) # change n for n-grams

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_2grams <- partition(coi_l, year = 2012:2016) %>%
  partition(interjection = F) %>%
  polmineR::ngrams(n = 2, p_attribute = c("word", "pos"))


## build ngram feature object to compare subsets and convert it to data table

dt1 <- features(right_2grams, left_2grams, included = TRUE) %>%
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table()


## fill the data table with specific columns

dt11 <- subset(dt1, dt1[["1_pos"]] == "ADJA") %>% subset(.[["2_pos"]] == "NN") #%>% subset(.[["2_word"]] %in% "StaatsbÃ¼rgerschaft") # 2-gram ADJA-NN

dt11[,"1_pos" := NULL][,"2_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)]

View(dt11)

## deutschen	Staatsbürgerschaft	27	-24
## deutschen	Staatsangehörigkeit	19	-11
## deutsche	Staatsangehörigkeit	53	-14
## eigenen	Staatsbürger	4	-3
## deutschen	Staatsbürgern	6	-3
## doppelter	Staatsbürgerschaft	3	-2
## kulturelle	Identität	6	-5
## europäische	Identität	5	-2
## schnellere	Asylverfahren	4	-2
## europäische	Asylpolitik	3	-2
## deutschen	Asylrecht	3	-2
## abgelehnten	Asylbewerbern	3	-2
## europäische	Asylsystem	7	-4
## unbegründete	Asylanträge	4	-3
## abgelehnten	Asylbewerber	4	-3
## abgelehnte	Asylbewerber	9	-6
## europäischen	Asylsystem	6	-5
## abgelehnter	Asylbewerber	6	-5

