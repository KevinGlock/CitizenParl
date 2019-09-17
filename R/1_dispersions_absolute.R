## time series analysis with dictionary

# Given keywords from the dictionary will be analysed regarding the salience effect of immigration on speech frequencies.
# In comparing the immigration rate statistics with the time series first assumptions can be formulated and the frequency of tokens can further correlated.
# The immigration rate statistics come from the Federal Statitical Office of Germany.


## load libraries


library(xts)
library(polmineR)
library(magrittr)
use("GermaParl")


## absolute frequencies

## overall

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
  '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
  '"[Ii]us"', '"Abstammungs(recht|prinzip).*"')

dis1 <- dispersion("GERMAPARL",
  query = q1,
  cqp = TRUE,
  s_attribute = c("year","parliamentary_group"),
  freq = F
  )

# time series plot (overall)

p1 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col1 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts1 <- xts(x = dis1[, ..p1],
          order.by = as.Date(
            sprintf(
              "%s-01-01",
              dis1[["year"]]
              )
          )
)

plot.xts(
  ts1,
  col = col1,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  legend.names = p1,
  legend.loc = "top",
  type = "o",
  main = '"[Dd]oppelstaat.*"| "[Mm]ehrstaat.*"| ".*[Ss]taatsbürger.*"| ".*[Ss]taats(an|zu)gehörig.*"|
  "[Ss]taatenlos.*"| "[Aa]us.bürger.*"| "[Ee]in.bürger.*"| "Doppelpa(ss|ß).*"| "Pa(ss|ß)"| "[Oo]ptionspflicht.*"|
  "[Oo]ptionszwang.*"| "Blutsrecht.*"| "Geburts(recht|prinzip)"| "[Ii]us"| "Abstammungs(recht|prinzip).*"')

head(dis1)

## dual citizenship

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
  '"Doppelstaats(an|zu)gehörig.*"', '"Doppelpa(ss|ß).*"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"')

dis2 <- dispersion(
  "GERMAPARL",
  query = q2,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of dual citizenship

p2 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col2 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")
  
ts2 <- xts(x = dis2[, ..p2],
          order.by = as.Date(
            sprintf("%s-01-01",
                    dis2[["year"]])
          )
)

plot.xts(
  ts2,
  col = col2,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  legend.names = p2,
  legend.loc = "top",
  type = "o",
  main = '"[Dd]oppelstaat.*" | "[Mm]ehrstaat.*" | ".*[Ss]taatsbürger.*" | "[Dd]oppelt.*" [] "[Ss]taat.*"|
  "[Dd]oppel.*" "[Ss]taat.*" | "Doppelstaats(an|zu)gehörig.*" | "Doppelpa(ss|ß).*" |
  "[Oo]ptionspflicht.*" | "[Oo]ptionszwang.*"')

head(dis2)





## absolute frequencies

## overall

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us"', '"Abstammungs(recht|prinzip).*"')

dis1 <- dispersion(coi_r,
                   query = q1,
                   cqp = TRUE,
                   s_attribute = c("date", "party"),
                   freq = F
)

dis1 <- dis1[!is.na(as.Date(dis1[["date"]]))]

dis2 <- dispersion(coi_l,
                   query = q1,
                   cqp = TRUE,
                   s_attribute = c("date", "party"),
                   freq = F
)

dis2 <- dis2[!is.na(as.Date(dis2[["date"]]))]

head(dis2)


# time series plot (overall)

p1 <- c("CDU", "CSU")

col1 <- c("black", "blue")

p2 <- c("FDP", "GRUENE", "SPD")  # "LINKE", "PDS"

col2 <- c("yellow", "green", "red") # "darkred", "darkred"

library(xts)


## per year

ts1 <- xts(x = order.by = as.Date(
               sprintf(
                 "%s-01-01",
                 dis1[["year"]]
               )
             )
)

ts2 <- xts(x = dis2[, ..p2],
           order.by = as.Date(
             sprintf(
               "%s-01-01",
               dis2[["year"]]
             )
           )
)

plot.xts(
  ts1,
  col = col1,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  legend.names = p1,
  legend.loc = "top",
  type = "o",
  main = '"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
  '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
  '"[Ii]us"', '"Abstammungs(recht|prinzip).*""')


## per date

par(mar = c(4,2,2,2))

ts1 <- xts(x = dis1[, ..p1], order.by = as.Date(dis1[["date"]]))

plot.xts(
  ts1,
  col = col1,
  multi.panel = F,
  lwd = 2,
  yaxs = "r",
  legend.names = p1,
  legend.loc = "top",
  type = "o",
  main = '"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
  '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
  '"[Ii]us"', '"Abstammungs(recht|prinzip).*""')

ts2 <- xts(x = dis2[, ..p2], order.by = as.Date(dis2[["date"]]))

plot.xts(
  ts2,
  col = col2,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  legend.names = p2,
  legend.loc = "top",
  type = "o",
  main = '"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
  '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
  '"[Ii]us"', '"Abstammungs(recht|prinzip).*""')


## overall

par(mar = c(4,2,2,2))

dis3 <- dispersion("GERMAPARL",
                   query = q1,
                   cqp = TRUE,
                   s_attribute = "date",
                   freq  =F
)

dis3 <- dis3[!is.na(as.Date(dis3[["date"]]))]

ts3 <- xts(x = dis3[["freq"]], order.by = as.Date(dis3[["date"]]))

plot.xts(
  ts3,
  col = "black",
  multi.panel = F,
  lwd = 2,
  yaxs = "r",
  main = '"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
  '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
  '"[Ii]us"', '"Abstammungs(recht|prinzip).*"')


head(dis3)
