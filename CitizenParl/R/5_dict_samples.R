## getting the dictionary samples

# The following workflow creates two partitions from the GermaParl corpus,
# subseted by parties ideological position (left/right or progressive/conservative)
# regarding issuses of national and transnational citizenship.


## load libraries

library("polmineR")
library("magrittr")
library("data.table")


## for conservative parties

coi_r <- partition("GERMAPARL",
                   party = c(
                     "CDU", "CSU"),
                   interjection= F,
                   encoding = c("UTF-8", "latin1"),
                   p_attribute = c("word", "lemma"),
                   role = c("mp", "government")
)

pb_r <- partition_bundle(coi_r, s_attribute = "date")

nested_r <- lapply(
  pb_r@objects,
  function(x) partition_bundle(x, s_attribute = "agenda_item", verbose = F)
)

debates_r <- flatten(nested_r)

names(debates_r) <- paste(
  blapply(debates_r, function(x) s_attributes(x, "date")),
  blapply(debates_r, function(x) name(x)), 
  sep = "_"
)

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us"', '"Abstammungs(recht|prinzip).*"')


dt_r <- count(debates_r, query = q1, regex = T, cqp = T) %>% setorderv(cols = "TOTAL", order = -1L)

debates_citizen_r <- debates_r[[ subset(dt_r, TOTAL >= 25)[["partition"]] ]]

debates_citizen_r[[26]] %>% read() %>% highlight(lightgreen = q1) # vary 1 to 26

warnings()


## for progressive parties

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   encoding = c("UTF-8", "latin1"),
                   p_attribute = c("word", "lemma"),
                   role = c("mp", "government")
)


pb_l <- partition_bundle(coi_l, s_attribute = "date")

nested_l <- lapply(
  pb_l@objects,
  function(x) partition_bundle(x, s_attribute = "agenda_item", verbose = F)
)

debates_l <- flatten(nested_l)

names(debates_l) <- paste(
  blapply(debates_l, function(x) s_attributes(x, "date")),
  blapply(debates_l, function(x) name(x)), 
  sep = "_"
)

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us"', '"Abstammungs(recht|prinzip).*"')

dt_l <- count(debates_l, query = q1, regex = T, cqp = T) %>% setorderv(cols = "TOTAL", order = -1L)

debates_citizen_l <- debates_l[[ subset(dt_l, TOTAL >= 25)[["partition"]] ]]

debates_citizen_l[[40]] %>% read() # vary 1 to 40

warnings()


