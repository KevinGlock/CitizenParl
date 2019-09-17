## correlation test

## load libraries

library(xts)
library(polmineR)
library(magrittr)
library(Hmisc)
library(stats)
use("GermaParl")


## frequencies

## overall

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
  '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
  '"[Ii]us"', '"Abstammungs(recht|prinzip).*"')

dis1 <- dispersion("GERMAPARL",
                   query = q1,
                   cqp = TRUE,
                   s_attribute = c("year"),
                   freq = T # vary freq T or F
)

head(dis1)


## create vector with immigration statistics

x <- c("959691", "840633", "802456", "874023",
       "841158", "879217", "842543", "768975",
       "780175", "707352", "661855", "680766",
       "682146", "721014", "798282", "958299",
       "1080936", "1226493", "1464724",
       "2136954", "1865122") # year from 1996 to 2016 

x <- as.numeric(x)

y <- c("428", "558", "546", "1248", "222", "159",
       "136", "315", "123", "272", "270", "257",
       "593", "361", "764", "576", "330", "536",
       "952", "320", "560") # total freq from 1996 to 2016  

y <- as.numeric(y)

y_freq <- c("9.552156e-05", "1.261867e-04", "1.549437e-04", "2.438544e-04",
            "4.408539e-05", "3.090062e-05", "3.072116e-05", "6.866940e-05",
            "2.558147e-05", "9.872231e-05", "5.463182e-05", "5.687990e-05",
            "1.169240e-04", "8.566981e-05", "1.276621e-04", "9.224154e-05",
            "5.346319e-05", "1.210700e-04", "1.761655e-04", "6.168363e-05", "1.232336e-04") # relative freq from 1996 to 2016

y_freq <- as.numeric(y_freq)

stat <- cbind(x, y)

stat_freq <- cbind(x, y_freq)


## corrrelation test

rcorr(x, y_freq, type = "pearson") # p is .5953 >=> 59.53 % probability of no linear bivariate relation between the raising of total immigration rate and raising citizenship related speeches

## data for 1991 to 1995 is missing the immigration rate was higher, plus, in 2016 are not the whole year included. The corpus must be merged for those years to check whether the corr is higher or lower.


## frequencies


## dual citizenship

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
  '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
  '"Doppelstaats(an|zu)gehörig.*"', '"Doppelpa(ss|ß).*"', '"[Oo]ptionspflicht.*"',
  '"[Oo]ptionszwang.*"')

dis2 <- dispersion("GERMAPARL",
                   query = q2,
                   cqp = TRUE,
                   s_attribute = c("year"),
                   freq = T  # vary T or F
)

show(dis2)

  
## take vector with immigration statistics
  
x <- c("959691", "840633", "802456", "874023",
         "841158", "879217", "842543", "768975",
         "780175", "707352", "661855", "680766",
         "682146", "721014", "798282", "958299",
         "1080936", "1226493", "1464724",
         "2136954", "1865122") # year from 1996 to 2016

x <- as.numeric(x)

y_dual <- c("170", "277", "329", "594", "134", "62",
       "56", "148", "64", "112", "78", "64",
       "166", "163", "263", "225", "140", "264",
       "593", "133", "309") # total freq from 1996 to 2016

y_dual <- as.numeric(y_dual)

y_dual_freq <- c("3.794081e-05", "6.264107e-05", "9.336350e-05", "1.160653e-04", "2.661010e-05",
            "1.204930e-05", "1.264989e-05", "3.226372e-05", "1.331068e-05", "4.065036e-05",
            "1.578252e-05", "1.416464e-05", "3.273084e-05", "3.868194e-05", "4.394650e-05",
            "3.603185e-05", "2.268135e-05", "5.963149e-05", "1.097333e-04", "2.563726e-05",
            "6.799853e-05") # relative freq from 1996 to 2016

y_dual_freq <- as.numeric(y_dual_freq)

stat_dual <- cbind(x, y_dual)

stat_dual_freq <- cbind(x, y_dual_freq)


## corrrelation test

rcorr(stat_dual_freq, type = "pearson") # corr is .3071 >=> 30.71 % of linear bivariate relation between the raising of total immigration rate and raising dual citizenship related speeches


## total correlation

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"', '"Abstammungs(recht|prinzip).*"')

q2 <- c('".*[Aa]syl.*"', '".*[Ff]l(u|ü)cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '".*[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*sch(ie|ob).*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied(elt|ler).*"', '".*[Rr]usslanddeutsch.*"', '"[Aa]ufenthalt.*"')

q3 <- c(q1, q2)

dis1 <- dispersion("GERMAPARL",
                   query = q3,
                   cqp = T,
                   s_attribute = "year",
                   freq = T ## vary freq = T or F
)

show(dis1)

x <- c("959691", "840633", "802456", "874023",
       "841158", "879217", "842543", "768975",
       "780175", "707352", "661855", "680766",
       "682146", "721014", "798282", "958299",
       "1080936", "1226493", "1464724",
       "2136954", "1865122") # year from 1996 to 2016

x <- as.numeric(x)

y_total_freq <- c("0.0010779652",
                  "0.0010671596",
                  "0.0012520358",
                  "0.0011411135",
                  "0.0009980773",
                  "0.0009085561",
                  "0.0006738325",
                  "0.0009288899",
                  "0.0007861622",
                  "0.0008238957",
                  "0.0007767835",
                  "0.0009704995",
                  "0.0010385139",
                  "0.0011500164",
                  "0.0009349075",
                  "0.0011237134",
                  "0.0009799965",
                  "0.0011325466",
                  "0.0014733503",
                  "0.0022718853",
                  "0.0020205906") # relative freq from 1996 to 2016

y_total_freq <- as.numeric(y_total_freq)

stat_total_freq <- cbind(x, y_total_freq)


## percentage of variance explained test

rcorr(stat_total_freq, type = "pearson")

cor(stat_freq, method = "pearson")^2*100

cor(stat_dual_freq, method = "pearson")^2*100

cor(stat_total_freq, method = "pearson")^2*100


## time series analysis

## Holt-Winters test to look for trend modelling

HoltWinters(x, alpha = NULL, beta = NULL, gamma = F) # trend for immigration rate is highly predicted by former values and prediction by more weighting present values

HoltWinters(y_freq, alpha = NULL, beta = NULL, gamma = F) # trend predition for citizenship aspects on present values and prediction by more weighting former values

HoltWinters(y_dual_freq, alpha = NULL, beta = NULL, gamma = F) # trend predition for dual citizenship aspects on present values and prediction by more weighting former values

HoltWinters(y_total_freq, alpha = NULL, beta = NULL, gamma = F) #  # trend predition for total on present values and more prediction by more weighting former values


## load library for using smoothing average to plot the trends

library("TTR")


## citizen freq

smooth_average_x <- SMA(x, n=1)

plot.ts(smooth_average_x) # immigration AR

smooth_average_y_freq <- SMA(y_freq, n=1)

plot.ts(smooth_average_y_freq) # citizen freq MA process

smooth_average_y_dual <- SMA(y_dual_freq, n=1)

plot.ts(smooth_average_y_dual) # dual citizen freq MA process

smooth_average_y_total <- SMA(y_total_freq, n=1) # general Foreigner´s policy AR process

plot.ts(smooth_average_y_total)


## get them smooth by higher p simple moving average with order n = lag

smooth_average_x <- SMA(x, n= 10)

plot.ts(smooth_average_x)

smooth_average_y_total <- SMA(y_total_freq, n= 10)

plot.ts(smooth_average_y_total)


## now the trend is obvious.
## Immigration and total speech frequencies of Foreigner´s Policy dictionary
## are trend correlated over time but there are restrictions given for citizenship aspects.


## dual citizen only

smooth_average_y_dual <- SMA(y_dual_freq, n=3)

plot.ts(smooth_average_y_dual)


## citizen only

smooth_average_y_freq <- SMA(y_freq, n=10)

plot.ts(smooth_average_y_freq)


## excluding citizen aspects

smooth_average_y_foreign <- SMA(y_foreign_freq, n=10)

plot.ts(smooth_average_y_foreign)
