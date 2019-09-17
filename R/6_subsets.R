## subset all parliamentary_groups

# The aim is to subset all parties per left and right political positioning to analyse the subset´s collocations of search words from the dicitionary.
# In doing so, the anaylsis targets in investigating the lingustical contextualisation of those nodes referring to citizenship to unveil possible convergence or divergence between contextualisations.


#Es lassen sich beliebig viele S-Attribute kombinieren, diese sind in der partition()-Funktion durch Kommata getrennt. 


library("polmineR")
use("GermaParl")


coi_r <- corpus(
  "GERMAPARL") %>%
  subset(
    party = c(
      "CDU", "CSU"),
    interjection= F
    )

coi_r_spb <- split(coi_r, s_attribute = "speaker") %>% split(coi_r, s_attribute = "date") ## subcorpus_bundle with 185 MB all speaker and dates are splitted

coi_l <- corpus(
  "GERMAPARL") %>%
  subset(
    party = c(
      "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
    interjection= F
    )

coi_l_spb <- split(coi_l, s_attribute = "speaker") %>% split(coi_l, s_attribute = "date")

kwic(coi_r,
     query = '".*[Aa]us.*bürger.*"',
     regex = T,
     cqp = T,
     s_attribute = "speaker" ## you can vary the s_attribute
     )

kwic(coi_l,
     query = '".*[Aa]us.*bürger.*"',
     regex = T,
     cqp = T,
     s_attribute = "party"
     )

cooccurrences(coi_r, query = '"Abstammungs(recht|prinzip).*"', left = 5, right = 5) %>% 
  subset(ll >= 10.83) %>%
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8, main = 'Kookkurrenzen zu "..."')

cooccurrences(coi_l, query = '"Abstammungs(recht|prinzip).*"', left = 5, right = 5) %>% 
  subset(ll >= 10.83)  %>%
  subset(!tolower(word) %in% tm::stopwords("de")) %>%
  subset(!word %in% c("''", ",", "``", ":", "[", "]", ")", "(")) %>%
  dotplot(cex = 0.8, main = 'Kookkurrenzen zu "..."')

count(coi_r, query = '".*[Ss]taatsbürger.*"', regex = T, cqp = T, feq = T)
