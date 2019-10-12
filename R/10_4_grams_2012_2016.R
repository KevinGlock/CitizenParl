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

right_4grams <- partition(coi_r, year = 2012:2016) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 4, p_attribute = c("word", "pos"))

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   p_attribute = c("word", "pos"),
                   role = c("mp", "government"))

left_4grams <- partition(coi_l, year = 2012:2016) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 4, p_attribute = c("word", "pos")) # change n for n-grams

dt3 <- features(right_4grams, left_4grams, included = TRUE) %>% 
  subset(count_coi >= 1) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt33 <- subset(dt3, dt3[["3_pos"]] == "NN") # 4-grams

dt33[,"4_pos" := NULL][, "chisquare" := round(chisquare, 2)] # format of table

View(dt33)

## der	ART	deutschen	ADJA	Staatsangehörigkeit	NN	und	6	-5
## die	ART	deutsche	ADJA	Staatsangehörigkeit	NN	entschieden	6	-5
## die	ART	deutsche	ADJA	Staatsangehörigkeit	NN	zu	8	-6

## von	APPR	der	ART	Optionspflicht	NN	befreit	6	-4

## Bundesamt	NN	für	APPR	Migration	NN	und	104	-35
## Migration	NN	und	KON	Flüchtlinge	NN	im	5	-4
## Migration	NN	und	KON	Flüchtlinge	NN	in	5	-4
## Migration	NN	und	KON	Flüchtlinge	NN	hat	6	-4
## Bundesamtes	NN	für	APPR	Migration	NN	und	28	-6
## Bundesamts	NN	für	APPR	Migration	NN	und	8	-4
## Migration	NN	und	KON	Flüchtlinge	NN	,	19	-4
## Migration	NN	und	KON	Flüchtlinge	NN	ist	3	-2
## Migration	NN	und	KON	Flüchtlinge	NN	.	9	-3

## auf	APPR	der	ART	Flucht	NN	oder	4	-3
## Flucht	NN	und	KON	Vertreibung	NN	,	7	-4
## Sonderinitiative	NN	``	CARD	Fluchtursachen	NN	bekämpfen	11	-4
## Bekämpfung	NN	von	APPR	Fluchtursachen	NN	in	3	-2
## Fluchtursachen	NN	vor	APPR	Ort	NN	bekämpfen	3	-2
## Ursachen	NN	von	APPR	Flucht	NN	,	3	-2

## Aussetzung	NN	des	ART	Familiennachzugs	NN	für	4	-3
## auf	APPR	der	ART	Flucht	NN	und	7	-3
## Bekämpfung	NN	der	ART	Fluchtursachen	NN	,	4	-2
## Bekämpfung	NN	der	ART	Fluchtursachen	NN	und	4	-2
## der	ART	Stiftung	NN	Flucht	NN	,	6	-2
## Bekämpfung	NN	von	APPR	Fluchtursachen	NN	.	8	-2

## 000	CARD	syrische	ADJA	Flüchtlinge	NN	aufgenommen	6	-5
## Migration	NN	und	KON	Flüchtlinge	NN	im	5	-4
## Migration	NN	und	KON	Flüchtlinge	NN	in	5	-4
## Versorgung	NN	der	ART	Flüchtlinge	NN	in	5	-4
## Integration	NN	der	ART	Flüchtlinge	NN	in	9	-6
## bekämpfen	VVINF	--	APPRART	Flüchtlinge	NN	reintegrieren	8	-5
## 000	CARD	syrische	ADJA	Flüchtlinge	NN	in	4	-3
## Migration	NN	und	KON	Flüchtlinge	NN	hat	6	-4
## in	APPR	den	ART	Flüchtlingslagern	NN	.	8	-4
## in	APPR	den	ART	Flüchtlingslagern	NN	in	10	-4
## Migration	NN	und	KON	Flüchtlinge	NN	,	19	-4
## Bewältigung	NN	der	ART	Flüchtlingskrise	NN	.	6	-3
## der	ART	Genfer	ADJA	Flüchtlingskonvention	NN	.	6	-3
## über	APPR	das	ART	Thema	NN	Flüchtlinge	6	-3
## Asylbewerbern	NN	und	KON	Flüchtlingen	NN	in	3	-2
## Bewältigung	NN	der	ART	Flüchtlingssituation	NN	.	3	-2
## Flüchtlinge	NN	und	KON	Asylbewerber	NN	,	3	-2
## Flüchtlingen	NN	und	KON	Asylbewerbern	NN	,	3	-2
## Migration	NN	und	KON	Flüchtlinge	NN	ist	3	-2
## die	ART	meisten	PIDAT	Flüchtlinge	NN	in	3	-2
## um	APPR	die	ART	Flüchtlinge	NN	in	3	-2
## über	APPR	2	CARD	Millionen	NN	Flüchtlinge	3	-2
## Migration	NN	und	KON	Flüchtlinge	NN	.	9	-3
## in	APPR	den	ART	Flüchtlingslagern	NN	,	5	-2
## mit	APPR	den	ART	Flüchtlingen	NN	.	5	-2
## bekämpfen	VVINF	,	$,	Flüchtlinge	NN	reintegrieren	6	-2