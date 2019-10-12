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
  subset(count_coi >= 5) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table()


## fill the data table with specific columns

dt2 <- subset(dt1, dt1[["1_pos"]] == "ART") %>% subset(.[["2_pos"]] == "NN") #%>% subset(.[["2_word"]] %in% "Staatsbürgerschaft") # 2-gram ADJA-NN

dt2[,"1_pos" := NULL][,"2_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)]

View(dt2)


## 3-grams

right_3grams <- partition(coi_r, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 3, p_attribute = c("word", "pos"))

left_3grams <- partition(coi_l, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 3, p_attribute = c("word", "pos"))

dt2 <- features(right_3grams, left_3grams, included = TRUE) %>%
  subset(count_coi >= 5) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt2 <- subset(dt2, dt2[["1_pos"]] == "ADJA") %>% subset(.[["2_pos"]] == "ADJA") %>% subset(.[["3_pos"]] == "NN")# %>% subset(.[["3_word"]] %in% "Staatsangehörigkeit") #3-gram "NN", "NE", "ADJA", "CARD"
  
dt2[,"1_pos" := NULL][,"2_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)] # format of table

View(dt2)

------------------------------------------------------------------------
## 4-grams

right_ngrams <- partition(coi_r, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 4, p_attribute = c("word", "pos"))

left_ngrams <- partition(coi_l, year = 1996:2000) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 4, p_attribute = c("word", "pos")) # change n for n-grams

dt3 <- features(right_ngrams, left_ngrams, included = TRUE) %>% 
  subset(count_coi >= 5) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt31 <- subset(dt3, dt3[["1_pos"]] == "ADJA") %>% subset(.[["2_pos"]] == "NN") #%>% subset(.[["3_pos"]] == "PRELS") #%>% subset(.[["4_pos"]] == "NN") # 4-gram "ART ; NN/ADJA" "APPR; ART/NN/NE/PIAT/PDAT/ADJA" "ADJA ; KON/NN" "$[ ; KON"

dt31[,"1_pos" := NULL][,"2_pos" := NULL][,"3_pos" := NULL][,"4_pos" := NULL][, "chisquare" := round(chisquare, 2)] # format of table

View(dt3)

------------------------------------------------------------------------
## 5-grams

right_5grams <- partition(coi_r, year = c(1996:2000, 2012:2016)) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 5, p_attribute = c("word", "pos"))

left_5grams <- partition(coi_l, year = c(1996:2000, 2012:2016)) %>%
  partition(interjection = FALSE) %>%
  polmineR::ngrams(n = 5, p_attribute = c("word", "pos"))

dt4 <- features(right_5grams, left_5grams, included = TRUE) %>% 
  subset(count_coi >= 5) %>% subset(chisquare >= 10.83) %>% data.table::as.data.table() # convert to data.table

dt4 <- subset(dt4, dt4[["1_pos"]] == "ART") %>% subset(.[["2_pos"]] == "NN") %>% subset(.[["3_pos"]] == "ADJA") #5-grams; you can copy the subset pipe and add more positional attritbues auf type "pos"

dt4[,"1_pos" := NULL][,"2_pos" := NULL][,"3_pos" := NULL][,"exp_coi" := round(exp_coi, 2)][, "chisquare" := round(chisquare, 2)] # formatting table; you can add more columns

View(dt4)


