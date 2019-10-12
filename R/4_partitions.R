## partitions

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


coi_spd <- partition("GERMAPARL",
                     party = "SPD",
                     year = 1996:2016,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))

coi_greens <- partition("GERMAPARL",
                        party = "GRUENE",
                        year = 1996:2016,
                        interjection= F,
                        encoding = "latin1",
                        p_attribute = c("word", "pos"),
                        role = c("mp", "government"))

coi_fdp <- partition("GERMAPARL",
                     party = "FDP",
                     year = 1996:2016,
                     interjection= F,
                     encoding = "latin1",
                     p_attribute = c("word", "pos"),
                     role = c("mp", "government"))

coi_lefts <- partition("GERMAPARL",
                       party = c("LINKE", "PDS"),
                       year = 1996:2016,
                       interjection= F,
                       encoding = "latin1",
                       p_attribute = c("word", "pos"),
                       role = "mp")