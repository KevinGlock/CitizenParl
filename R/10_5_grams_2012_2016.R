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

right_5grams <- partition(coi_r, year = 2012:2016) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 5, p_attribute = c("word", "pos"))

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_5grams <- partition(coi_l, year = 2012:2016) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 5, p_attribute = c("word", "pos"))

dt4 <- features(right_5grams, left_5grams, included = TRUE) %>% 
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt44 <- subset(dt4, dt4[["3_pos"]] == "NN") # 5-grams

dt44[,"3_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)] # formatting table; you can add more columns

View(dt44)

## der	ART	deutschen	ADJA	Staatsangehörigkeit	und	KON	der	ART	3	-2

## .	$.	Das	ART	Bundesamt	für	APPR	Migration	NN	22	-19
## dass	KOUS	das	ART	Bundesamt	für	APPR	Migration	NN	8	-7
## Bundesamt	NN	für	APPR	Migration	und	KON	Flüchtlinge	NN	104	-35
## Mitarbeiter	NN	des	ART	Bundesamtes	für	APPR	Migration	NN	4	-3
## Bundesamtes	NN	für	APPR	Migration	und	KON	Flüchtlinge	NN	28	-6
## Bundesamts	NN	für	APPR	Migration	und	KON	Flüchtlinge	NN	8	-4
## Migration	NN	und	KON	Flüchtlinge	,	$,	BAMF	NE	3	-2

## Bundesamt	NN	für	APPR	Migration	und	KON	Flüchtlinge	NN	104	-35
## bei	APPR	der	ART	Bewältigung	der	ART	Flüchtlingskrise	NN	6	-5
## für	APPR	die	ART	Versorgung	der	ART	Flüchtlinge	NN	5	-4
## ,	$,	die	ART	Zahl	der	ART	Flüchtlinge	NN	7	-5
## Bundesamtes	NN	für	APPR	Migration	und	KON	Flüchtlinge	NN	28	-6
## Bundesamts	NN	für	APPR	Migration	und	KON	Flüchtlinge	NN	8	-4
## Integration	NN	der	ART	Flüchtlinge	in	APPR	den	ART	5	-3
## dass	KOUS	die	ART	Zahl	der	ART	Flüchtlinge	NN	5	-3
## .	$.	Die	ART	Integration	der	ART	Flüchtlinge	NN	3	-2
## Aufnahme	NN	von	APPR	Flüchtlingen	und	KON	Asylbewerbern	NN	3	-2
## Migration	NN	und	KON	Flüchtlinge	,	$,	BAMF	NE	3	-2
## Prozent	NN	der	ART	Flüchtlinge	aus	APPR	dem	ART	3	-2
## auch	ADV	im	APPRART	Interesse	der	ART	Flüchtlinge	NN	3	-2
## bekämpfen	VVINF	--	APPRART	Flüchtlinge	reintegrieren	VVFIN	``	CARD	4	-2
## in	APPR	den	ART	Flüchtlingslagern	in	APPR	Jordanien	NE	4	-2
## bei	APPR	der	ART	Unterbringung	von	APPR	Flüchtlingen	NN	5	-2
## für	APPR	die	ART	Integration	der	ART	Flüchtlinge	NN	5	-2

## Sonderinitiative	NN	``	CARD	Fluchtursachen	bekämpfen	VVINF	--	APPRART	6	-5
## auf	APPR	der	ART	Flucht	.	$.	Das	PDS	7	-5
## Beitrag	NN	zur	APPRART	Bekämpfung	von	APPR	Fluchtursachen	NN	4	-3
## auf	APPR	der	ART	Flucht	.	$.	Der	ART	4	-3
## für	APPR	die	ART	Opfer	von	APPR	Flucht	NN	3	-2
## der	ART	Stiftung	NN	Flucht	,	$,	Vertreibung	NN	6	-2
