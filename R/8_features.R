## features


# Mithilfe von Keywordextraktion werden die Merkmale von zwei Objekten,
# beispielsweise wird eine Partition (Korpus von Interesse)
# und eine weitere Partition (Referenzkorpus)verglichen. 

# Die Methode beinhaltet keinen voreingestellten Filter nach der Auftretenshäufigkeit,
# damit Nutzer*innen sich über Selektionsschritte im Klaren bleiben. 

# Selten auftretendes Vokabular muss mit der subset()-Methode aus der Analyse ausgeschlossen werden.


## partition

# Es lassen sich beliebig viele S-Attribute kombinieren,
# diese sind in der partition()-Funktion durch Kommata getrennt. 


## laod libraries

library("polmineR")
library("magrittr")
use("GermaParl")


## corpus of interests by parties and liberal/conservative position
## regarding national/transnational citizenship concepts

coi_r <- partition("GERMAPARL",
                   party = c(
                     "CDU", "CSU"),
                   interjection= F,
                   encoding = c("latin1")
) %>% count(p_attribute = c("word", "pos", "lemma"))

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   encoding = c("latin1")
) %>% count(p_attribute = c("word", "pos", "lemma"))




## exclude lower frequent words and irrelevant lexical entities

terms_to_drop <- c(tm::stopwords("de"), "--", "``", "[", "]", ")", "(", "dass", "2016", "2017", "2015", "Vielen", "Dank", "Sehr", "Meine", "sehr", "geehrte", "verehrten", "verehrte", "geehrten", "Damen", "und", "Herren")

f <- features(coi_r, coi_l, included = TRUE) %>% subset(count_coi >= 5) %>% subset(chisquare >= 10.83)

f <- subset(f, !word %in% terms_to_drop)

f2 <- features(coi_l, coi_r, included = TRUE) %>% subset(count_coi >= 5) %>% subset(chisquare >= 10.83)

f2 <- subset(f2, !word %in% terms_to_drop)

view(f2)

# Wir nehmen Standard-Filterschritte vor.
# Den Filter mit der Part-of-Speech-Annotation gestalten wir etwas weniger restriktiv als zuvor:
# Wir behalten als Inhaltsworte Nomen (“NN”), Adjektive (“ADJA”) und Verben (“VVFIN”).

features_rl <- f %>%
  subset(count_coi >= 5) %>%
  subset(chisquare >= 10.83) %>%
  subset(pos %in% c("NN", "ADJA", "VVFIN"))

view(features_rl)

features_lr <- f2 %>%
  subset(count_coi >= 5) %>%
  subset(chisquare >= 10.83) %>%
  subset(pos %in% c("NN", "ADJA", "VVFIN"))

view(features_lr)

-------------------------------------------------------------------------------------------------------------
## word cloud for first sight; next step n_grams and cooccurrence network in the sense of citizenship aspects
-------------------------------------------------------------------------------------------------------------

wordcloud::wordcloud(
  words = features_rl[["word"]][1:50],
  freq = features_rl[["count_coi"]][1:59],
  colors = rep(RColorBrewer::brewer.pal(7, "Dark2"), times = 7),
  random.color = TRUE
  )

wordcloud::wordcloud(
  words = features_lr[["word"]][1:50],
  freq = features_lr[["count_coi"]][1:59],
  colors = rep(RColorBrewer::brewer.pal(7, "Dark2"), times = 7),
  random.color = TRUE
)

