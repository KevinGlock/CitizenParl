## 4-grams


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

right_4grams <- partition(coi_r, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 4, p_attribute = c("word", "pos"))

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_4grams <- partition(coi_l, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 4, p_attribute = c("word", "pos")) # change n for n-grams

dt3 <- features(right_ngrams, left_ngrams, included = TRUE) %>% 
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt33 <- subset(dt3, dt3[["4_pos"]] == "NN") # 4-grams

dt33[,"4_pos" := NULL][, "chisquare" := round(chisquare, 2)] # format of table

View(dt33)

## der	ART	regelmäßigen	ADJA	doppelten	ADJA	Staatsangehörigkeit	5	-4
## daß	KOUS	die	ART	doppelte	ADJA	Staatsangehörigkeit	3	-2
## eine	ART	Reform	NN	des	ART	Staatsangehörigkeitsrechts	3	-2
## 	,	$,	die	ART	doppelte	ADJA	Staatsangehörigkeit	4	-2
## der	ART	Integration	NN	ausländischer	ADJA	Mitbürger	4	-3
## 