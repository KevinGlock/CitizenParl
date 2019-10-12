## different time series aggregates


## load library

library(lubridate)
library(polmineR)
library(magrittr)
library(xts)

use("GermaParl")


## dispersions

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"',
        '"[Dd]oppel.*" "[Ss]taat.*"', '"Abstammungs(recht|prinzip).*"')

q2 <- c('".*[Aa]syl.*"', '".*[Ff]l(u|ü)cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '"[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*sch(ie|ob).*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied(elt|ler).*"',
        '"[Vv]is(a|um).*"', '"Identitätsfeststellung"', '"Rückführung.*"', '".*[Aa]usländ.*"',
        '".*[Rr]usslanddeutsch.*"', '"[Aa]ufenthalt.*"', '"Rückübernahme.*"', '"Ehegattennachzug"', '"Duldung.*"',
        '"Residenzpflicht"', '"Regelanfrage"', '"Vertreibung"', '".*Vertr(ie|ei)b.*"', '"AZR"', '"Aufnahme.*"')

q3 <- c(q1, q2)


## Foreigners´ Policy dispersion

dis1 <- dispersion("GERMAPARL",
                   query =  q3,
                   cqp = T,
                   freq = T,
                   s_attribute = c("year", "party")
                   )

## per year

ts1 <- xts(x = dis1[,c("CDU", "CSU", "FDP", "GRUENE", "SPD", "LINKE", "PDS")],
          order.by = as.Date(sprintf("%s-01-01", dis1[["year"]]
                                     )
                             )
          )

ts1_year <- aggregate(ts1, as.Date(sprintf("%s-01-01",
                                           gsub("^(\\d{4})-.*?$", "\\1",
                                                index(ts1)
                                                )
                                           )
                                   )
                      )

plot(ts1_year, main = 'Foreigners´ Policy per year') # simple panel

plot.xts(
  ts1,
  multi.panel = F,
  col = c("black",
          "blue",
          "yellow",
          "green",
          "firebrick",
          "darkred",
          "darkred"),
  lwd = 3,
  yaxs = "r",
  main = 'Foreigners´ Policy per year'
  )


## citizenship dispersion

dis3 <- dispersion("GERMAPARL",
                   query =  q1,
                   cqp = T,
                   freq = T,
                   s_attribute = c("year", "party")
                   )

## per year

ts3 <- xts(x = dis3[,c("CDU", "CSU", "FDP", "GRUENE", "SPD", "LINKE", "PDS")],
           order.by = as.Date(sprintf("%s-01-01", dis3[["year"]]
                                      )
                              )
           )

ts3_year <- aggregate(ts3, as.Date(sprintf("%s-01-01",
                                           gsub("^(\\d{4})-.*?$", "\\1",
                                                index(ts3)
                                                )
                                           )
                                   )
                      )

plot(ts3_year, main = 'Citizenship per year') # simple panel

plot.xts(
  ts3,
  multi.panel = F,
  col = c("black",
          "blue",
          "yellow",
          "green",
          "firebrick",
          "darkred",
          "darkred"),
  lwd = 3,
  yaxs = "r",
  main = 'Citizenship per year'
)


## Foreigners´ Policy dispersion


par(mar = c(4,2,2,2))

dis2 <- dispersion("GERMAPARL",
                   query =  q3,
                   cqp = T,
                   freq =T,
                   s_attribute = c("date")
                   )


## per date

dis2 <- dis2[!is.na(as.Date(dis2[["date"]]
                            )
                    )
             ]

ts2_date <- xts(x = dis2[["freq"]],
                order.by = as.Date(dis2[["date"]]
                                   )
                )

plot(ts2_date, main = 'Foreigners´ Policy per date') # simple aggregated plot

View(dis2[, date])


## citizenship dispersion

par(mar = c(4,2,2,2))

dis4 <- dispersion("GERMAPARL",
                   query =  q1,
                   cqp = T,
                   freq =T,
                   s_attribute = c("date")
                   )


## per date

dis4 <- dis4[!is.na(as.Date(dis4[["date"]]
                            )
                    )
             ]

ts3_date <- xts(x = dis4[["freq"]],
                order.by = as.Date(dis4[["date"]]
                                   )
                )

plot(ts3_date, main = 'Citizenship per date') # simple aggregated plot

View(dis4[, date])
