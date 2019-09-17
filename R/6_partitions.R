## partition

#Es lassen sich beliebig viele S-Attribute kombinieren, diese sind in der partition()-Funktion durch Kommata getrennt. 

## laod libraries

library("polmineR")
library("magrittr")
use("GermaParl")


## corpus of interests by parties and liberal/conservative position regarding national/transnational citizenship concepts

coi_r <- partition("GERMAPARL",
    party = c(
      "CDU", "CSU"),
    interjection= F,
    encoding = "latin1",
    p_attribute = "word",
    role = c("mp", "government")
  )

coi_l <- partition("GERMAPARL",
    party = c(
      "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
    interjection= F,
    encoding = "latin1",
    p_attribute = "word",
    role = c("mp", "government")
  )





-------------------------------------------------------------------------------------------------
## works very well
  
kwic(coi_r, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T)

count(coi_r, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)

cooccurrences(coi_r, query = '".*[Ee]in.*bürger.*"', regex = T, cqp = T) %>%
dotplot()

features(coi_r, coi_l, method = "ll") %>% subset(count_coi >= 5) %>% subset(ll >= 10.83)

#"[Dd]oppel.*" .*[Aa]us.*bürger.*


rp <- partition(
  "GERMAPARL",
  parliamentary_group = "CDU/CSU", interjection = F,
  p_attribute = "word"
)

lp <- partition("GERMAPARL",
                parliamentary_group = c("SPD", "GRUENE"), interjection = F, p_attribute = "word")

terms_rp <- features(x = lp, y = rp, included = TRUE)
top100 <- subset(terms_rp, rank_chisquare <= 100)
head(top100)


---------------------------------------------------------------------------------------------
## redeanteile

"GERMAPARL" %>% partition(year = 2008) %>% count(query = "Finanzmarktkrise")
##               query count       freq
## 1: Finanzmarktkrise   213 4.1998e-05


## redeanteile

merkel2008 <- partition(bt2008min, speaker = "Angela Merkel")
for (day in s_attributes(merkel2008, s_attribute = "date")){
  dt <- partition(merkel2008, date = day) %>% count(query = "Finanzmarktkrise")
  cat(sprintf("%s -> N Finanzkrise = %s\n", day, dt[["count"]]))
}
## 2008-04-24 -> N Finanzkrise = 0
## 2008-06-19 -> N Finanzkrise = 0
## 2008-09-17 -> N Finanzkrise = 0
## 2008-10-7 -> N Finanzkrise = 5
## 2008-10-15 -> N Finanzkrise = 0
## 2008-11-26 -> N Finanzkrise = 2
## 2008-12-4 -> N Finanzkrise = 1


------------------------------------------------------------------------------------
# Als “corpus of interest” (coi) legen wir zunächst eine partition für 2015/16 an,
# wobei wir Zwischenrufe aus der Analyse ausschließen.

coi <- partition("GERMAPARL", year = 2016, interjection = FALSE)

# Als Referenzkorpus wählen wir die vorangegangenen Jahre.
# Natürlich ist relevant, wie groß wir den Untersuchungszeitraum stricken.
# In diesem Fall wählen wir den Zeitraum ab 2002, d.h. die Phase nach dem 11. September.

ref <- partition("GERMAPARL", year = 2002:2015, interjection = FALSE)

# Nun fehlen noch die Zählungen der Worthäufigkeiten. Dies erreicht die enrich()-Methode.

coi <- enrich(coi, p_attribute = "word")
ref <- enrich(ref, p_attribute = "word")


coi <- partition("GERMAPARL", year = 2016, interjection = FALSE) %>% enrich(p_attribute = "word")

# Es ist allerdings auch möglich, das Argument p_attribute direkt beim Aufruf von partition() anzugeben
# und also gleich “in einem” Rutsch die Zählung durchzuführen.

coi <- partition("GERMAPARL", year = 2016, interjection = FALSE, p_attribute = "word")
ref <- partition("GERMAPARL", year = 2002:2015, interjection = FALSE, p_attribute = "word")

# Wesentlich für die Durchführung der Feature-Extraktion ist, dass Zählungen verglichen werden.
# Insofern ist wichtig, dass die partition-Objekte auch count-Objekte sehen.
# Das sehen Sie wie folgt.

is(coi)

## [1] "plpr_partition" "partition"      "count"          "subcorpus"     
## [5] "textstat"

---------------------------------------------------------------------------------------------------
pb <- partition_bundle("GERMAPARL", s_attribute = "date", progress = TRUE) # ~ 7 secs 
pb2 <- partition_bundle(pb, s_attribute = "agenda_item", prefix = "agenda_item", progress = TRUE) # ~ 25 secs

dict <- c("Migration", "Zuwanderung", "Einwanderung", "Asyl", "Flucht", "Flüchtlinge")
dict_cnt <- count(pb2, query = dict, p_attribute = "word", progress = TRUE) # ~ 3 secs

# at this stage, you might want to have a look at histograms to determine threshold

dict_cnt_sub <- dict_cnt[TOTAL > 10]

pb_mig <- pb2[[ dict_cnt_sub[["partition"]] ]]

# to see whether we like the result

ai_mig_2016_12_16 <- partition_bundle(pb_mig[["2016-12-16_agenda_item_2"]], s_attribute = "speaker")

speech_frank_schwabe <- ai_mig_2016_12_16[["Frank Schwabe"]]
library(magrittr)
html(speech_frank_schwabe) %>% highlight(list(yellow = dict))
