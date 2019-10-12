## time series analysis with dictionary


## Given keywords from the dictionary will be analysed regarding the salience effect of immigration on speech frequencies.
## In comparing the immigration rate statistics with the time series first assumptions can be formulated and the frequency of tokens can further correlated.
## The immigration rate statistics come from the Federal Statitical Office of Germany.


## load libraries

library(xts)
library(polmineR)
library(magrittr)

use("GermaParl")


## relative frequencies

##                        actors                                                         issues
## 1            [Dd]oppelstaat.*                                              .*[Aa]us.bürger.*
## 2              [Mm]ehrstaat.*                                              .*[Ee]in.bürger.*
## 3         .*[Ss]taatsbürger.*                                               Doppelpa(ss|ß).*
## 4 .*[Ss]taats(an|zu)gehörig.* '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"'
## 5             [Ss]taatenlos.*                                                       Pa(ss|ß)
## 6                                                                        [Oo]ptionspflicht.*
## 7                                                                          [Oo]ptionszwang.*
##                                                           norm
## 1                                                 Blutsrecht.*
## 2                                     Geburts(recht|prinzip).*
## 3 '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"'
## 4                                 Abstammungs(recht|prinzip).*


## overall dispersion

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"',
        '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
        '"Abstammungs(recht|prinzip).*"')

dis1 <- dispersion("GERMAPARL",
  query = q1,
  cqp = T,
  s_attribute = c("year","parliamentary_group"),
  freq = T
  )


# time series plot (overall)

ts1 <- xts(x = dis1[,
                  c(
                    "CDU/CSU", "SPD", "GRUENE",
                    "FDP", "LINKE")],
          order.by = as.Date(
            sprintf(
              "%s-01-01",
              dis1[["year"]]
              )
          )
)

plot.xts(
  ts1,
  col = c(
    "black", "red", "green",
    "yellow", "darkred"),
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = 'overall')


## dual citizenship dispersion


## [Dd]oppelstaat.*
## [Mm]ehrstaat.*
## .*[Ss]taatsbürger.*
## '"[Dd]oppelt.*" [] "[Ss]taat.*"'
## '"[Dd]oppel.*" "[Ss]taat.*"'
## Doppelstaats(an|zu)gehörig.*
## Doppelpa(ss|ß).*
## [Oo]ptionspflicht.*
## [Oo]ptionszwang.*


dis2 <- dispersion(
  "GERMAPARL",
  query = c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbÃ¼rger.*"',
    '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
    '"Doppelstaats(an|zu)gehÃ¶rig.*"', '"Doppelpa(ss|ÃŸ).*"', '"[Oo]ptionspflicht.*"',
    '"[Oo]ptionszwang.*"'),
  cqp = T,
  s_attribute = c("year", "parliamentary_group"),
  freq = T
)


## time series plot of dual citizenship

ts2 <- xts(x = dis2[,
                   c(
                     "CDU/CSU", "SPD", "GRUENE",
                     "FDP", "LINKE", "PDS")
                   ],
          order.by = as.Date(
            sprintf("%s-01-01",
                    dis2[["year"]])
          )
)

plot.xts(
  ts2,
  multi.panel = F,
  col = c(
    "black", "red", "green",
    "yellow", "darkred", "darkred"),
  lwd = 3,
  yaxs = "r",
  main = 'dual citizenship')


## per date

par(mar = c(4,2,2,2))

ts1 <- xts(x = dis1[, ..p1], order.by = as.Date(dis1[["date"]]))

plot.xts(
  ts1,
  col = col1,
  multi.panel = F,
  lwd = 2,
  yaxs = "r",
  main = 'overall')

ts2 <- xts(x = dis2[, ..p2], order.by = as.Date(dis2[["date"]]))

plot.xts(
  ts2,
  col = col2,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = 'overall')


## overall

par(mar = c(4,2,2,2))

dis3 <- dispersion("GERMAPARL",
                   query = q1,
                   cqp = T,
                   s_attribute = "date",
                   freq  =F)

dis3 <- dis3[!is.na(as.Date(dis3[["date"]]))]

ts3 <- xts(x = dis3[["freq"]], order.by = as.Date(dis3[["date"]]))

plot.xts(
  ts3,
  col = "black",
  multi.panel = F,
  lwd = 2,
  yaxs = "r",
  main = 'overall')


head(dis3)
