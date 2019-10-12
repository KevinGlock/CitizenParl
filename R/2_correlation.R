## correlation tests

## load libraries

library(polmineR)
library(Hmisc)
library(stats)

use("GermaParl")


## frequencies

## overall

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"',
        '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
        '"Abstammungs(recht|prinzip).*"')

dis1 <- dispersion("GERMAPARL",
                   query = q1,
                   cqp = TRUE,
                   s_attribute = c("year"),
                   freq = T
)

show(dis1)


## create vector with immigration statistics

x <- c("959691", "840633", "802456", "874023",
       "841158", "879217", "842543", "768975",
       "780175", "707352", "661855", "680766",
       "682146", "721014", "798282", "958299",
       "1080936", "1226493", "1464724",
       "2136954", "1865122") # year from 1996 to 2016 

x <- as.numeric(x)


## create vector with absolute freq

y <- c("463", "622", "651","1392", "235",
       "161","131", "343", "126", "295",
       "272", "265", "613", "372", "787",
       "615", "341", "581", "1052", "327",
       "606") # total freq from 1996 to 2016  

y <- as.numeric(y)


## create vector with relative freq

y_freq <- c("1.033329e-04", "1.406597e-04", "1.847405e-04",
            "2.719914e-04", "4.666697e-05", "3.128931e-05",
            "2.959171e-05", "7.477335e-05", "2.620541e-05",
            "1.070702e-04", "5.503650e-05", "5.865048e-05",
            "1.208675e-04", "8.828025e-05", "1.315053e-04",
            "9.848706e-05", "5.524530e-05", "1.312345e-04",
            "1.946703e-04", "6.303296e-05", "1.333563e-04")  # relative freq of citizenship from 1996 to 2016

y_freq <- as.numeric(y_freq)


## combine them (column bind)

stat <- cbind(x, y)

stat_freq <- cbind(x, y_freq)


## corrrelation test

rcorr(x, y_freq, type = "pearson") # vary pearson and spearman

## r = .12 p is .5953 >=> 59.53 % probability of no linear bivariate relation
## between the raising of total immigration rate and raising citizenship related speeches.

## rho = .21 (.36)

## data for 1991 to 1995 is missing the immigration rate was higher,
## plus, in 2016 are not the whole year included.
## The corpus must be merged for those years to check whether the corr is higher or lower.


## frequencies


## dual citizenship

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
  '"[Dd]oppel.*" "[Ss]taat.*"', '"Doppelpa(ss|ß).*"',
  '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"')

dis2 <- dispersion("GERMAPARL",
                   query = q2,
                   cqp = TRUE,
                   s_attribute = c("year"),
                   freq = T
)

show(dis2)


## create a vector with absolute freq

y_dual <- c("48", "92", "122", "313",
            "18", "5", "7", "59", "5",
            "30", "2", "12", "35", "45",
            "83", "84", "60", "119",
            "360", "37", "117") # total freq from 1996 to 2016

y_dual <- as.numeric(y_dual)


## create a vector with relative freq

y_dual_freq <- c("1.071270e-05", "2.080498e-05", "3.462112e-05",
                 "6.115900e-05", "3.574491e-06", "9.717178e-07",
                 "1.581236e-06", "1.286189e-05", "1.039897e-06",
                 "1.088849e-05", "4.046801e-07", "2.655871e-06",
                 "6.901080e-06", "1.067906e-05", "1.386905e-05",
                 "1.345189e-05", "9.720580e-06", "2.687935e-05",
                 "6.661720e-05", "7.132170e-06", "2.574702e-05") # relative freq of dual citizenship from 1996 to 2016

y_dual_freq <- as.numeric(y_dual_freq)


## combine them (column bind)

stat_dual <- cbind(x, y_dual)

stat_dual_freq <- cbind(x, y_dual_freq)


## corrrelation test

rcorr(stat_dual_freq, type = "spearman")

## corr is .28 (.2215) >=> 30.71 % of linear bivariate relation
## between the raising of total immigration rate and raising dual citizenship related speeches.

## rho = .4 (.0694)


## total correlation of Foreigners´ Policy

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"',
        '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
        '"Abstammungs(recht|prinzip).*"')

q2 <- c('".*[Aa]syl.*"', '".*[Ff]l(u|ü)cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '"[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*sch(ie|ob).*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied(elt|ler).*"',
        '"[Vv]is(a|um).*"', '"Identitätsfeststellung"', '"Rückführung.*"', '".*[Aa]usländ.*"',
        '".*[Rr]usslanddeutsch.*"', '"[Aa]ufenthalt.*"', '"Rückübernahme.*"', '"Ehegattennachzug"', '"Duldung.*"',
        '"Residenzpflicht"', '"Regelanfrage"', '"Vertreibung"', '".*Vertr(ie|ei)b.*"', '"AZR"', '"Aufnahme.*"')

q3 <- c(q1, q2)

dis1 <- dispersion("GERMAPARL",
                   query = q2,
                   cqp = T,
                   s_attribute = "year",
                   freq = T ## vary freq = T or F
)

show(dis1)


## create vector of absolute freq

y_total_count <- c("5771",
                   "5600",
                   "5236",
                   "6362",
                   "6272",
                   "5616",
                   "3637",
                   "5047",
                   "5152",
                   "2539",
                   "4355",
                   "5006",
                   "5626",
                   "5506",
                   "6243",
                   "7794",
                   "6881",
                   "5902",
                   "8115",
                   "12742", 
                   "9706") # absolute freq of total Foreigners´ Policy from 1996 to 2016

y_total_count <- as.numeric(y_total_count)


## create vector of relative freq

y_total_freq <- c("0.0012879787",
                  "0.0012663899",
                  "0.0014858702",
                  "0.0012431103",
                  "0.0012455115",
                  "0.0010914334",
                  "0.0008215652",
                  "0.0011002364",
                  "0.0010715100",
                  "0.0009215292",
                  "0.0008811910",
                  "0.0011079408",
                  "0.0011092993",
                  "0.0013066426",
                  "0.0010431864",
                  "0.0012481434",
                  "0.0011147886",
                  "0.0013331252",
                  "0.0015016627",
                  "0.0024561651",
                  "0.0021359020") # relative freq of total Foreigners´ Policy from 1996 to 2016


y_total_freq <- as.numeric(y_total_freq)


## combine them (column bind)

stat_total_count <- cbind(x, y_total_count)

stat_total_freq <- cbind(x, y_total_freq)


## corrrelation test

rcorr(stat_total_freq, type = "pearson")

## corr is .9 >=>  % of linear bivariate relation
## between the raising of total immigration rate and raising dual citizenship related speeches.

## rho = .64 (.0019)

## percentages of discourse variance explained by immigration rate

## get R²

cor(stat_freq, method = "pearson")^2*100

## 1.37 %

cor(stat_dual_freq, method = "pearson")^2*100

## r^2 7.76 %

cor(stat_total_freq, method = "pearson")^2*100

## r^2 81.15 %