## time-series analysis


## load libraries

library(xts)
library(polmineR)
library(magrittr)
library(Hmisc)
library(stats)

use("GermaParl")


## create vector with immigration statistics

immig <- c("959691", "840633", "802456", "874023",
       "841158", "879217", "842543", "768975",
       "780175", "707352", "661855", "680766",
       "682146", "721014", "798282", "958299",
       "1080936", "1226493", "1464724",
       "2136954", "1865122") # year from 1996 to 2016 

immig <- as.numeric(immig)


q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taats(an|zu)gehörig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Doppelpa(ss|ß).*"', '"Pa(ss|ß)"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburts(recht|prinzip)"',
        '"[Ii]us" "(soli|sanguinis)"', '"[Jj]us" "(soli|sanguinis)"',
        '"[Dd]oppelt.*" [] "[Ss]taat.*"', '"[Dd]oppel.*" "[Ss]taat.*"',
        '"Abstammungs(recht|prinzip).*"') # citizenship

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
        '"[Dd]oppel.*" "[Ss]taat.*"', '"Doppelpa(ss|ß).*"',
        '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"') # dual citizenship

q4 <- c('".*[Aa]syl.*"', '".*[Ff]l(u|ü)cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '".*[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*sch(ie|ob).*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied(elt|ler).*"',
        '"[Vv]is(a|um).*"', '"Identitätsfeststellung"', '"Rückführung.*"',
        '".*[Rr]usslanddeutsch.*"', '"[Aa]ufenthalt.*"') # other policy fields than citizenship issues

q3 <- c(q1, q4) # total Foreigners´ Policy

dis1 <- dispersion("GERMAPARL",
                   query = q3, # vary query q1, q2, q3, q4
                   cqp = TRUE,
                   s_attribute = c("year"),
                   freq = T # vary freq T or F
)

show(dis1)


## simple plots

xts(x = immig,
    order.by = as.Date(
      sprintf(
        "%s-01-01",
        dis1[["year"]]
      )
    )
) %>% plot.xts(yaxis.right = F)

xts(x = dis1[, freq],
    order.by = as.Date(
      sprintf(
        "%s-01-01",
        dis1[["year"]]
      )
    )
) %>% plot.xts(yaxis.right = F)


## time series analysis

y_freq <- c("1.033329e-04", "1.406597e-04", "1.847405e-04",
            "2.719914e-04", "4.666697e-05", "3.128931e-05",
            "2.959171e-05", "7.477335e-05", "2.620541e-05",
            "1.070702e-04", "5.503650e-05", "5.865048e-05",
            "1.208675e-04", "8.828025e-05", "1.315053e-04",
            "9.848706e-05", "5.524530e-05", "1.312345e-04",
            "1.946703e-04", "6.303296e-05", "1.333563e-04")

y_freq <- as.numeric(y_freq)

y_dual_freq <- c("1.071270e-05", "2.080498e-05", "3.462112e-05",
                 "6.115900e-05", "3.574491e-06", "9.717178e-07",
                 "1.581236e-06", "1.286189e-05", "1.039897e-06",
                 "1.088849e-05", "4.046801e-07", "2.655871e-06",
                 "6.901080e-06", "1.067906e-05", "1.386905e-05",
                 "1.345189e-05", "9.720580e-06", "2.687935e-05",
                 "6.661720e-05", "7.132170e-06", "2.574702e-05")

y_dual_freq <- as.numeric(y_dual_freq)

y_total_freq <- c("0.0011321983", "0.0011720890",
                  "0.0013207104", "0.0011934797",
                  "0.0010334251", "0.0009312943",
                  "0.0006910002", "0.0009598456",
                  "0.0010205550", "0.0009139073",
                  "0.0008134071", "0.0009908611",
                  "0.0010803148", "0.0012057848",
                  "0.0009902166", "0.0011815245",
                  "0.0010459344", "0.0012757525",
                  "0.0015307152", "0.0023148710",
                  "0.0021002523")

y_total_freq <- as.numeric(y_total_freq)


## Holt-Winters test to look for trend modelling

HoltWinters(immig, alpha = NULL, beta = NULL, gamma = F) # trend for immigration rate is highly predicted by former values and prediction by more weighting present values

HoltWinters(y_freq, alpha = NULL, beta = NULL, gamma = F) # trend predition for citizenship aspects on present values and prediction by more weighting former values

HoltWinters(y_dual_freq, alpha = NULL, beta = NULL, gamma = F) # trend predition for dual citizenship aspects on present values and prediction by more weighting former values

HoltWinters(y_total_freq, alpha = NULL, beta = NULL, gamma = F) #  # trend predition for total on present values and more prediction by more weighting former values


## load library for using smoothing average to plot the trends

library("TTR")


## citizen freq

smooth_average_immig <- SMA(immig, n=1)

plot.ts(smooth_average_immig) # immigration

smooth_average_y_freq <- SMA(y_freq, n=1)

plot.ts(smooth_average_y_freq) # citizen freq process

smooth_average_y_dual <- SMA(y_dual_freq, n=1)

plot.ts(smooth_average_y_dual) # dual citizen freq process

smooth_average_y_total <- SMA(y_total_freq, n=1) # general Foreigners´ policy process

plot.ts(smooth_average_y_total)


## get them smooth by higher p simple moving average with order n = lag

smooth_average_x <- SMA(x, n= 10)

plot.ts(smooth_average_x)

smooth_average_y_total <- SMA(y_total_freq, n= 10)

plot.ts(smooth_average_y_total)


## now the trend is obvious.
## Immigration and total speech frequencies of Foreigners´ Policy dictionary
## are trend correlated over time but there are restrictions given for citizenship aspects.


## dual citizen only

smooth_average_y_dual <- SMA(y_dual_freq, n=10)

plot.ts(smooth_average_y_dual)


## citizen only

smooth_average_y_freq <- SMA(y_freq, n=10)

plot.ts(smooth_average_y_freq)

