## single query dispersions


## 1    [Aa]us.*bürger.*
## 2    [Dd]oppelstaat.*
## 3    [Ee]in.*bürger.*
## 4      [Mm]ehrstaat.*
## 5     [Ss]taatenlos.*
## 6 .*[Ss]taatsbürger.*


## single query dispersions of [Aa]us.*bürger.*
  
q1 <- c('"[Aa]us.*bürger.*"')

dis1 <- dispersion(
  "GERMAPARL",
  query = q1,
  cqp = TRUE,
  s_attribute = c(
	"lp", "date", "role", "parliamentary_group"),
  freq = F
)


# time series plot of [Aa]us.*bürger.*

p1 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "V1")

col1 <- c("black", "red", "green", "yellow", "darkred", "blue")

ts1 <- xts(x = dis1[, ..p1],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis1[["year"]])
           )
)

plot.xts(
  ts1,
  col = col1,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '[Aa]us.*bürger.*')

head(dis1)


## single query dispersions of [Dd]oppelstaat.*


q2 <- c('"[Dd]oppelstaat.*"')

dis2 <- dispersion(
  "GERMAPARL",
  query = q2,
  cqp = TRUE,
  s_attribute = c(
	"lp", "date", "role", "parliamentary_group"),
  freq = F
)


# time series plot of [Dd]oppelstaat.*

p2 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "V1")

col2 <- c("black", "red", "green", "yellow", "darkred", "blue")

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
  main = '[Dd]oppelstaat.*')

head(dis2)


## single query dispersions of [Ee]in.*bürger.*


q3 <- c('"[Ee]in.*bürger.*"')

dis3 <- dispersion(
  "GERMAPARL",
  query = q3,
  cqp = TRUE,
  s_attribute = c(
	"lp", "date", "role", "parliamentary_group"),
  freq = F
)


# time series plot of [Ee]in.*bürger.*

p3 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col3 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts3 <- xts(x = dis3[, ..p3],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis3[["year"]])
           )
)

plot.xts(
  ts3,
  col = col3,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '[Ee]in.*bürger.*')

head(dis3)


## single query dispersions of [Mm]ehrstaat.*


q4 <- c('"[Mm]ehrstaat.*"')

dis4 <- dispersion(
  "GERMAPARL",
  query = q4,
  cqp = TRUE,
  s_attribute = c(
	"lp", "date", "role", "parliamentary_group"),
  freq = F
)


# time series plot of [Mm]ehrstaat.*

p4 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col4 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts4 <- xts(x = dis4[, ..p4],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis4[["year"]])
           )
)

plot.xts(
  ts4,
  col = col4,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '[Mm]ehrstaat.*')

head(dis4)


## single query dispersions of [Ss]taatenlos.*


q5 <- c('"[Ss]taatenlos.*"')

dis5 <- dispersion(
  "GERMAPARL",
  query = q5,
  cqp = TRUE,
  s_attribute = c(
	"lp", "date", "role", "parliamentary_group"),
  freq = F
)


# time series plot of [Ss]taatenlos.*

p5 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col5 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts5 <- xts(x = dis5[, ..p5],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis5[["year"]])
           )
)

plot.xts(
  ts5,
  col = col5,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '[Ss]taatenlos.*')

head(dis5)


## single query dispersions of .*[Ss]taatsbürger.*


q6 <- c('".*[Ss]taatsbürger.*"')

dis6 <- dispersion(
  "GERMAPARL",
  query = q6,
  cqp = TRUE,
  s_attribute = c(
	"lp", "date", "role", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Ss]taatsbürger.*

p6 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col6 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts6 <- xts(x = dis6[, ..p6],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis6[["year"]])
           )
)

plot.xts(
  ts6,
  col = col6,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '.*[Ss]taatsbürger.*')

head(dis6)


## 1          [Oo]ptionspflicht.*
## 2            [Oo]ptionszwang.*
## 3             Doppelpa(ss|ß).*
## 4                     Pa(ss|ß)


## single query dispersions of [Oo]ptionspflicht.*

q7 <- c('"[Oo]ptionspflicht.*"')

dis7 <- dispersion(
  "GERMAPARL",
  query = q7,
  cqp = TRUE,
  s_attribute = c("date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of [Oo]ptionspflicht.*

p7 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "V1")

col7 <- c("black", "red", "green", "yellow", "darkred", "blue")

ts7 <- xts(x = dis7[, ..p7],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis7[["year"]])
           )
)

plot.xts(
  ts7,
  col = col7,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '[Oo]ptionspflicht.*')

head(dis7)


## single query dispersions of [Oo]ptionszwang.*
  
q8 <- c('"[Oo]ptionszwang.*"')

dis8 <- dispersion(
  "GERMAPARL",
  query = q8,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of [Oo]ptionszwang.*

p8 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "V1")

col8 <- c("black", "red", "green", "yellow", "darkred", "blue")

ts8 <- xts(x = dis8[, ..p8],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis8[["year"]])
           )
)

plot.xts(
  ts8,
  col = col8,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '[Oo]ptionszwang.*')

head(dis8)


## single query dispersions of Doppelpa(ss|ß).*

q9 <- c('"Doppelpa(ss|ß).*"')

dis9 <- dispersion(
  "GERMAPARL",
  query = q9,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of Doppelpa(ss|ß).*

p9 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col9 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts9 <- xts(x = dis9[, ..p9],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis9[["year"]])
           )
)

plot.xts(
  ts9,
  col = col9,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = 'Doppelpa(ss|ß).*')

head(dis9)


## single query dispersions of Pa(ss|ß)

q10 <- c('"Pa(ss|ß)"')

dis10 <- dispersion(
  "GERMAPARL",
  query = q10,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of Pa(ss|ß)

p10 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col10 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts10 <- xts(x = dis10[, ..p10],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis10[["year"]])
           )
)

plot.xts(
  ts10,
  col = col10,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = 'Pa(ss|ß)')

head(dis10)


## 1                 Blutsrecht.*
## 2       Geburts(recht|prinzip)
## 3   [Ii]us [] (soli|sangruini)
## 4  .*[Ss]taats(an|zu)gehörig.*
## 5 Abstammungs(recht|prinzip).*


## single query dispersions of Blutsrecht.*

q11 <- c('"Blutsrecht.*"')

dis11 <- dispersion(
  "GERMAPARL",
  query = q11,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of Blutsrecht.*

p11 <- c("GRUENE", "LINKE", "PDS", "V1")

col11 <- c("green", "darkred", "darkred", "blue")

ts11 <- xts(x = dis11[, ..p11],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis11[["year"]])
           )
)

plot.xts(
  ts11,
  col = col11,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '.Blutsrecht.*')

head(dis11)


## single query dispersions of Geburts(recht|prinzip)

q12 <- c('"Geburts(recht|prinzip).*"')

dis12 <- dispersion(
  "GERMAPARL",
  query = q12,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of Geburts(recht|prinzip).*

p12 <- c("CDU/CSU", "SPD", "GRUENE", "FDP")

col12 <- c("black", "red", "green", "yellow")

ts12 <- xts(x = dis12[, ..p12],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis12[["year"]])
           )
)

plot.xts(
  ts12,
  col = col12,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = 'Geburts(recht|prinzip).*')

head(dis12)


## single query dispersions of [Ii]us [] (soli|sangruini)

q13 <- c('"[Ii]us" "soli"', '"[Ii]us" "sanguinis"', '"[Jj]us" "soli"', '"[Jj]us"  "sanguinis"')

dis13 <- dispersion(
  "GERMAPARL",
  query = q13,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of [Ii]us [] (soli|sangruini)

p13 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col13 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts13 <- xts(x = dis13[, ..p13],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis13[["year"]])
           )
)

plot.xts(
  ts13,
  col = col13,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '[Ii]us [] (soli|sangruini)')

head(dis13)


## single query dispersions of .*[Ss]taats(an|zu)gehörig.*

q14 <- c('".*[Ss]taats(an|zu)gehörig.*"')

dis14 <- dispersion(
  "GERMAPARL",
  query = q14,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Ss]taats(an|zu)gehörig.*

p14 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col14 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts14 <- xts(x = dis14[, ..p14],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis14[["year"]])
           )
)

plot.xts(
  ts14,
  col = col14,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = '.*[Ss]taats(an|zu)gehörig.*')

head(dis14)


## single query dispersions of Abstammungs(recht|prinzip).*

q15 <- c('"Abstammungs(recht|prinzip).*"')

dis15 <- dispersion(
  "GERMAPARL",
  query = q15,
  cqp = TRUE,
  s_attribute = c(date", "year", "parliamentary_group"),
  freq = F
)


# time series plot of Abstammungs(recht|prinzip).*

p15 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col15 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts15 <- xts(x = dis15[, ..p15],
           order.by = as.Date(
             sprintf("%s-01-01",
                     dis15[["year"]])
           )
)

plot.xts(
  ts15,
  col = col15,
  multi.panel = F,
  lwd = 3,
  yaxs = "r",
  main = 'Abstammungs(recht|prinzip).*')

head(dis15)