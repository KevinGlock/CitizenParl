## 3-grams


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

right_3grams <- partition(coi_r, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 3, p_attribute = c("word", "pos"))

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_3grams <- partition(coi_l, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 3, p_attribute = c("word", "pos"))

dt2 <- features(right_3grams, left_3grams, included = TRUE) %>%
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt22 <- subset(dt2, dt2[["3_pos"]] == "NN")# "ADJA", "ART"

dt22[,"3_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)] # format of table

View(dt22)

## eine	ART	andere	ADJA	Staatsangehörigkeit	6	-5
## einer	ART	doppelten	ADJA	Staatsangehörigkeit	5	-4
## regelmäßigen	ADJA	doppelten	ADJA	Staatsangehörigkeit	5	-4
## die	ART	doppelte	ADJA	Staatsangehörigkeit	23	-2
## Frage	NN	der	ART	Staatsangehörigkeit	3	-2
## die	ART	deutsche	ADJA	Staatsangehörigkeit	35	4
## generelle	ADJA	doppelte	ADJA	Staatsangehörigkeit	4	-2
## der	ART	ausländischen	ADJA	Staatsangehörigkeit	5	-2

## generellen	ADJA	doppelten	ADJA	Staatsbürgerschaft	3	-2
## mit	APPR	der	ART	Staatsbürgerschaft	3	-2

## und	KON	den	ART	Nationalstaaten	3	-2

## lebenden	ADJA	ausländischen	ADJA	Mitbürger	6	-4
## unserer	PPOSAT	ausländischen	ADJA	Mitbürger	3	-2
## für	APPR	deutsche	ADJA	Staatsbürger	3	-2
## Bürger	NN	unseres	PPOSAT	Landes	25	4

## Zahl	NN	der	ART	Einbürgerungen	4	-2
