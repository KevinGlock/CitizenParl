## getting the dictionary samples

# The following workflow creates two partitions from the GermaParl corpus,
# subseted by parties ideological position (left/right or progressive/conservative)
# regarding issuses of national and transnational citizenship.


## load libraries

library("polmineR")
library("magrittr")
library("data.table")

use("GermaParl")

devtools::install_github("PolMine/polmineR", ref = "dev")

## for conservative parties

coi <- partition("GERMAPARL",
                   year = c(1996:2000, 2012:2016),
                   interjection= F,
                   encoding = "UTF-8",
                   p_attribute = c("word", "lemma"),
                   role = c("mp", "government"))

pb <- partition_bundle(coi, s_attribute = "date")

nested <- lapply(pb@objects,
                 function(x) partition_bundle(x,
                               s_attribute = "agenda_item",
                               verbose = F
                               )
                 )

debates <- flatten(nested)

names(debates) <- paste(blapply(debates,
                                function(x) s_attributes(x, "date")),
                        blapply(debates,
                                function(x) name(x)),
                        sep = "_"
                        )

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"',
        '"[Dd]oppel.*" "[Ss]taat.*"', '"Abstammungs(recht|prinzip).*"')

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
        '"[Dd]oppel.*" "[Ss]taat.*"', '"Doppelpa(ss|ß).*"',
        '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"')

q3 <- c('".*[Aa]syl.*"', '".*[Ff]l(u|ü)cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '"[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*sch(ie|ob).*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied(elt|ler).*"',
        '"[Vv]is(a|um).*"', '"Identitätsfeststellung"', '"Rückführung.*"', '".*[Aa]usländ.*"',
        '".*[Rr]usslanddeutsch.*"', '"[Aa]ufenthalt.*"', '"Rückübernahme.*"', '"Ehegattennachzug"', '"Duldung.*"',
        '"Residenzpflicht"', '"Regelanfrage"', '"Vertreibung"', '".*Vertr(ie|ei)b.*"', '"AZR"', '"Aufnahme.*"')

q4 <- c(q1, q3)

dt <- count(debates,
            query = q4,
            regex = T,
            fill = T,
            cqp = T
            ) %>% setorderv(cols = "TOTAL",
                                   order = -1L
                                   )

show(dt)

debates_citizen <- debates[[ subset(dt, TOTAL >= 25)[["partition"]] ]]

q1_regex <- gsub('^\\"(.*?)\\"$', '\\1', q4)

debates_citizen[[47]] %>% read() %>% highlight(green = q1_regex, regex = T) # vary 1st to 47th debate on citizenship

warnings()






-----------------------------------------------------------------------------------------------
## for progressive parties

coi_l <- partition("GERMAPARL",
                   party = c(
                     "SPD", "GRUENE", "FDP", "LINKE", "PDS"),
                   interjection= F,
                   encoding = c("UTF-8", "latin1"),
                   p_attribute = c("word", "lemma"),
                   role = c("mp", "government")
)


pb_l <- partition_bundle(coi_lefts, s_attribute = "date")

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

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbÃ¼rger.*"',
        '".*[Ss]taats(an|zu)gehÃ¶rig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bÃ¼rger.*"',
        '"[Ee]in.*bÃ¼rger.*"', '"Doppelpa(ss|ÃŸ).*"', '"Pa(ss|ÃŸ)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us"', '"Abstammungs(recht|prinzip).*"')

dt_l <- count(debates_l, query = q1, regex = T, fill = T, cqp = T) %>% setorderv(cols = "TOTAL", order = -1L)

debates_citizen_l <- debates_l[[ subset(dt_l, TOTAL >= 25)[["partition"]] ]]

debates_citizen_l[[40]] %>% read() %>% highlight(green = q4, regex = T) # vary 1 to 40

warnings()
-------------------------------------------------------------------------------------------------------------------

