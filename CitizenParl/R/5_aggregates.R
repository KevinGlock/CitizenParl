## different time series aggregates

##                actors                     issues
## 1     [Aa]us.bürger.*        [Oo]ptionspflicht.*
## 2    [Dd]oppelstaat.*          [Oo]ptionszwang.*
## 3     [Ee]in.bürger.* Doppelpa(ss|ß).*|"Pa(ss|ß)
## 4      [Mm]ehrstaat.*                           
## 5     [Ss]taatenlos.*                           
## 6 .*[Ss]taatsbürger.*                           
##                               norm
## 1                     Blutsrecht.*
## 2           Geburts(recht|prinzip)
## 3 [Ii]us (soli/sangruini/domicili)
## 4      .*[Ss]taats(an|zu)gehörig.*
## 5     Abstammungs(recht|prinzip).*

## load library

library(lubridate)
library(polmineR)
library(magrittr)

use("GermaParl")


## dispersion

citizendisp5 <- dispersion("GERMAPARL", query =  '"[Dd]oppelt.*" "[Ss]taatsb?rger.*"', cqp = TRUE, s_attribute = c("year", "party"))

ts2 <- xts(x = citizendisp5[,c("CDU", "CSU", "FDP", "GRUENE", "SPD")],
          order.by = as.Date(sprintf("%s-01-01", citizendisp5[["year"]]))
          )

head(ts2)

plot.xts(
  ts2,
  multi.panel = TRUE,
  col = c("black",
          "blue",
          "yellow",
          "green",
          "red"),
  lwd = 2,
  yaxs = "r"
  )

par(mar = c(4,2,2,2))
citizendisp6 <- dispersion("GERMAPARL", query =  '"[Dd]oppelt.*" "[Ss]taatsb?rger.*"', cqp = TRUE, s_attribute = c("date"))

citizendisp6 <- citizendisp6[!is.na(as.Date(citizendisp6[["date"]]))]
ts3 <- xts(x = citizendisp6[["count"]], order.by = as.Date(citizendisp6[["date"]]))
plot(ts3)


## get different aggregates

ts2_week <- aggregate(ts2, {a <- lubridate::ymd(paste(lubridate::year(index(ts2)), 1, 1, sep = "-")); lubridate::week(a) <- lubridate::week(index(ts2)); a})

ts2_month <- aggregate(ts2, as.Date(as.yearmon(index(ts2))))

ts2_qtr <- aggregate(ts2, as.Date(as.yearqtr(index(ts2))))

ts2_year <- aggregate(ts2, as.Date(sprintf("%s-01-01", gsub("^(\\d{4})-.*?$", "\\1", index(ts2)))))

par(mfrow = c(2,2), mar = c(2,2,3,1))

plot(as.xts(ts2_week), main = "Aggregation: Woche", col = c("black",
          "blue",
          "yellow",
          "green",
          "red"))

plot(as.xts(ts2_month), main = "Aggregation: Monat", col = c("black",
          "blue",
          "yellow",
          "green",
          "red"))

plot(as.xts(ts2_qtr), main = "Aggregation: Quartal", col = c("black",
          "blue",
          "yellow",
          "green",
          "red"))

plot(as.xts(ts2_year), main = "Aggregation: Jahr", col = c("black",
          "blue",
          "yellow",
          "green",
          "red"))


par(mar = c(4,2,2,2))
twodimdis2 <- dispersion("GERMAPARL", query =  c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
                                                 '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
                                                 '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
                                                 '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
                                                 '"[Ii]us"', '"Abstammungs(recht|prinzip).*"'), cqp = TRUE, freq = T, s_attribute = c("date"))

twodimdis2 <- twodimdis2[!is.na(as.Date(twodimdis2[["date"]]))]
ts3 <- xts(x = twodimdis2[["freq"]], order.by = as.Date(twodimdis2[["date"]]))
plot(ts3)