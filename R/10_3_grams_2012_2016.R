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

right_3grams <- partition(coi_r, year = 2012:2016) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 3, p_attribute = c("word", "pos"))

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_3grams <- partition(coi_l, year = 2012:2016) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 3, p_attribute = c("word", "pos"))

dt2 <- features(right_3grams, left_3grams, included = TRUE) %>%
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt22 <- subset(dt2, dt2[["3_pos"]] == "NN")# "ADJA", "ART"

dt22[,"3_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)] # format of table

View(dt22)

## der	ART	deutschen	ADJA	Staatsbürgerschaft	20	-17
## die	ART	deutsche	ADJA	Staatsangehörigkeit	48	-16
## der	ART	deutschen	ADJA	Staatsangehörigkeit	18	-10
## von	APPR	der	ART	Staatsangehörigkeit	5	-4
## Die	ART	deutsche	ADJA	Staatsangehörigkeit	4	-3
## ,	$,	die	ART	Staatsangehörigkeit	3	-2
## Staatsbürgers	NN	in	APPR	Uniform	7	-3

## ,	$,	die	ART	Mehrstaatigkeit	3	-2

## Migration	NN	und	KON	Flüchtlinge	144	-47
## Bundesamt	NN	für	APPR	Migration	105	-35
## der	ART	illegalen	ADJA	Migration	7	-6
## die	ART	illegale	ADJA	Migration	7	-6
## Personen	NN	mit	APPR	Migrationshintergrund	9	-7
## und	KON	ohne	APPR	Migrationshintergrund	8	-6
## das	ART	Thema	NN	Migration	5	-4
## Flüchtlinge	NN	und	KON	Migranten	4	-3
## Bundesamtes	NN	für	APPR	Migration	28	-6
## Bundesamts	NN	für	APPR	Migration	8	-4
## .	$.	Die	ART	Migranten	3	-2
## Bildung	NN	und	KON	Migration	3	-2
## Thema	NN	``	CARD	Migration	3	-2
## Unternehmer	NN	mit	APPR	Migrationshintergrund	3	-2
## Migranten	NN	und	KON	Flüchtlingen	4	-2
## im	APPRART	Bereich	NN	Migration	4	-2

## Aussetzung	NN	des	ART	Familiennachzugs	10	-7
## Einschränkung	NN	des	ART	Familiennachzugs	4	-2

## von	APPR	der	ART	Optionspflicht	9	-5
## Aufhebung	NN	der	ART	Residenzpflicht	4	-2
## Lockerung	NN	der	ART	Residenzpflicht	4	-2

## Entziehung	NN	des	ART	Passes	7	-6

## für	APPR	die	ART	Abschiebung	3	-2

