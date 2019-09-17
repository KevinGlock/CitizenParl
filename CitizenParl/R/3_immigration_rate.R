
# total immigration to Germany from 1991 and 2016

# from 1991 until 1995 obout one million
# decreased after 1995 and increased since 2012 a second time above one million (data from Statista 2019).


## load library

library("polmineR")
library("data.table")
library("xts")

use("GermaParl")

## get immigration rate statistics

stat <-read.csv(file= "C:/Users/admin/stats_citizen.csv",
                header=TRUE,
                sep = ";"
                )

immigr <- c(stat[,1:2])


## convert it to a barplot

immigr_year <- as.Date.yearmon(immigr$year)

immigr_immigr <- as.numeric(immigr$immigration)

ts1 <- xts(x = immigr_immigr,
           order.by = immigr_year)

plot_immigr_year <- plot.xts(ts1,
                       col = "darkblue",
                       lwd = 3,
                       yaxis.left = T,
                       yaxis.right = F,
                       type = "o",
                      main = "total immigration per year"
                      )

plot(plot_immigr_year)


## absolute frequencies

## overall

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"', '"Abstammungs(recht|prinzip).*"')

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
        '"Doppelstaats(an|zu)gehörig.*"', '"Doppelpa(ss|ß).*"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"')

q_add <- c('".*[Aa]syl.*"', '".*[Ff]l(u|ü)cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '".*[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*sch(ie|ob).*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied(elt|ler).*"',
        '".*[Rr]usslanddeutsch.*"', '"[Aa]ufenthalt.*"')

before01 <- c('".*[Aa]us.*sied(elt|ler).*"', '".*[Rr]usslanddeutsch.*"')

mig <- c('".*[Ee]in.*wander.*"', '".*[Mm]igra.*"')

after01 <- c('".*[Aa]syl.*"', '".*[Ff]l(u|ü)cht.*"')

natural <- c('".*[Ee]in.*bürger.*"')

deport <- c('".*[Aa]b.*sch(ie|ob).*"', '".*[Aa]us.*bürger.*"', '"Rückführung"')

q3 <- c(q1, q2, q_add)

dis1 <- dispersion("GERMAPARL",
                   query = q1, ## vary q1 and q2
                   cqp = T,
                   s_attribute = c("year", "parliamentary_group"),
                   freq = T ## vary freq = T or F
)

# time series plot (overall)

p1 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col1 <- c("black", "firebrick1", "green", "yellow", "red3", "red3", "blue")

ts2 <- xts(x = dis1[, ..p1],
           order.by = as.Date(
             sprintf(
               "%s-01-01",
               dis1[["year"]]
             )
           )
)

plot(ts2) # total immigration rate 

plot.xts(
  ts2,
  col = col1,
  multi.panel = F,
  lwd = 3,
  yaxis.left =  T,
  yaxis.right = F,
  legend.names = p1,
  legend.loc = "top",
  type = "l",
  main = 'Foreigner´s Policy relative frequencies by parliamentary parties and year') # , dual citizenship (q2), overall Foreigner´s Policy (q3
