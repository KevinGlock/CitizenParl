## kwic analyses of search words from the dictionary

##                actors

## 1   .*[Aa]us.bürger.*
## 2    [Dd]oppelstaat.*
## 3   .*[Ee]in.bürger.*
## 4      [Mm]ehrstaat.*
## 5     [Ss]taatenlos.*
## 6 .*[Ss]taatsbürger.*


## load libraries

library(xts)
library(data.table)
library(magrittr)
library(polmineR)
use("GermaParl")


## .*[Aa]us.bürger.*

kwic(
  "GERMAPARL",
  query = '".*[Aa]us.*bürger.*"',
  left = 5, right = 5, cqp = TRUE,
  s_attributes = c(
	  "lp", "date", "role", "parliamentary_group"),
  interjection = F)

highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## [Dd]oppelstaat.*

kwic(
  "GERMAPARL",
      query = '"[Dd]oppelstaat.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## .*[Ee]in.bürger.*

kwic(
  "GERMAPARL",
      query = '".*[Ee]in.bürger.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## [Mm]ehrstaat.*

kwic(
  "GERMAPARL",
      query = '"[Mm]ehrstaat.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## [Ss]taatenlos.*

kwic(
  "GERMAPARL",
      query = '"[Ss]taatenlos.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## .*[Ss]taatsbürger.*

kwic(
  "GERMAPARL",
      query = '".*[Ss]taatsbürger.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


##                issues

## 1 [Oo]ptionspflicht.*
## 2   [Oo]ptionszwang.*
## 3    Doppelpa(ss|ß).*
## 4            Pa(ss|ß)


## [Oo]ptionspflicht.*

kwic(
  "GERMAPARL",
      query = '"[Oo]ptionspflicht.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## [Oo]ptionszwang.*

kwic(
  "GERMAPARL",
      query = '"[Oo]ptionszwang.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## Doppelpa(ss|ß).*

kwic(
  "GERMAPARL",
      query = '"Doppelpa(ss|ß).*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## Pa(ss|ß)

kwic(
  "GERMAPARL",
      query = '"Pa(ss|ß)"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


##                               norm


## 1                     Blutsrecht.*
## 2           Geburts(recht|prinzip)
## 3 [Ii]us (soli/sangruini
## 4      .*[Ss]taats(an|zu)gehörig.*
## 5     Abstammungs(recht|prinzip).*


## Blutsrecht.*

kwic(
  "GERMAPARL",
      query = '"Blutsrecht.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## Geburts(recht|prinzip).*

kwic(
  "GERMAPARL",
      query = '"Geburts(recht|prinzip).*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## '"[Ii]us" "soli"', '"[Ii]us" "sanguinis"', '"[Jj]us" "soli"', '"[Jj]us" "sanguinis"'

kwic(
  "GERMAPARL",
      query = c('"[Ii]us" "soli"', '"[Ii]us" "sanguinis"', '"[Jj]us" "soli"', '"[Jj]us" "sanguinis"'),
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)


read()


## .*[Ss]taats(an|zu)gehörig.*


kwic(
  "GERMAPARL",
      query = '".*[Ss]taats(an|zu)gehörig.*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()


## Abstammungs(recht|prinzip).*


kwic(
  "GERMAPARL",
      query = '"Abstammungs(recht|prinzip).*"',
      left = 25, right = 25, cqp = TRUE,
      s_attributes = c(
        "lp", "date", "role", "parliamentary_group"),
      interjection = FALSE) %>%
highlight(lightgreen = c("[Dd]oppel.*", ".*[Ss]taat.*", ".*[Rr]echt.*"), regex = T)

read()

