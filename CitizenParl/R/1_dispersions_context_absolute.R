## single query dispersions of '".*[Aa]uss.*ied.*"', '"[Rr]usslanddeutsch.*"', '"[Hh]eimatvertrieb.*"'

q2 <- c('".*[Aa]uss.*ied.*"', '"[Rr]usslanddeutsch.*"', '".*[Hh]eimatvertr.*b.*"')

dis2 <- dispersion(
  "GERMAPARL",
  query = q2,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of '".*[Aa]uss.*ied.*"', '"[Rr]usslanddeutsch.*"', '".*[Hh]eimatvertr.*b.*"'

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
  main = '".*[Aa]uss.*ied.*", "[Rr]usslanddeutsch.*", ".*[Hh]eimatvertr.*b.*"')

head(dis2)


## single query dispersions of .*[Ff]l(u|ü)cht.*

q3 <- c('".*[Ff]l(u|ü)cht.*"')

dis3 <- dispersion(
  "GERMAPARL",
  query = q3,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Ff]l(u|ü)cht.*

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
  main = '.*[Ff]l(u|ü)cht.*')

head(dis3)


## single query dispersions of '".*[Ee]inwander.*"', '".*[Mm]igr.*"'

q1 <- c('".*[Ee]inwander.*"', '".*[Mm]igr.*"')

dis1 <- dispersion(
  "GERMAPARL",
  query = q1,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of '".*[Ee]inwander.*"', '".*[Mm]igr.*"'

p1 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col1 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

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
  main = '".*[Ee]inwander.*"', '".*[Mm]igr.*"')

head(dis1)


## single query dispersions of .*[Aa]syl.*

q4 <- c('".*[Aa]syl.*"')

dis4 <- dispersion(
  "GERMAPARL",
  query = q4,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Aa]syl.*

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
  main = '.*[Aa]syl.*')

head(dis4)


## single query dispersions of .*[Aa]b.*sch(ie|o)b.*

q5 <- c('".*[Aa]b.*sch(ie|o)b.*"')

dis5 <- dispersion(
  "GERMAPARL",
  query = q5,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Aa]b.*sch(ie|o)b.*

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
  main = '.*[Aa]b.*sch(ie|o)b.*')

head(dis5)
  
  
## single query dispersions of .*[Ee]in.*bürger.*
  
q6 <- c('".*[Ee]in.*bürger.*"')

dis6 <- dispersion(
  "GERMAPARL",
  query = q6,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Ee]in.*bürger.*

p6 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col6 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

ts <- xts(x = dis[, ..p6],
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
  main = '.*[Ee]in.*bürger.*')

head(dis6)
  
  
## single query dispersions of .*[Aa]us.*bürger.*
  
q7 <- c('".*[Aa]us.*bürger.*"')

dis7 <- dispersion(
  "GERMAPARL",
  query = q7,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Aa]us.*bürger.*

p7 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col7 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

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
  main = '.*[Aa]us.*bürger.*')

head(dis7)
  
  
## single query dispersions of [Ff]amilienzusammenführ.*
  
q8 <- c('"[Ff]amilienzusammenführ.*"')

dis8 <- dispersion(
  "GERMAPARL",
  query = q8,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of .*[Aa]us.*bürger.*

p8 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col8 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

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
  main = '[Ff]amilienzusammenführ.*')

head(dis8)


## single query dispersions of [Gg]renz(en|schließung|kontrolle).*

q9 <- c('"[Gg]renz(en|schließung|kontrolle).*"')

dis9 <- dispersion(
  "GERMAPARL",
  query = q9,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of [Gg]renz(en|schließung|kontrolle).*

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
  main = '[Gg]renz(en|schließung|kontrolle).*')

head(dis9)


## single query dispersions of [Tt]error.*

q10 <- c('"[Tt]error.*"')

dis10 <- dispersion(
  "GERMAPARL",
  query = q10,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of [Tt]error.*

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
  main = '[Tt]error.*')

head(dis10)


## single query dispersions of [Ii]slam.*

q11 <- c('"[Ii]slam.*"')

dis11 <- dispersion(
  "GERMAPARL",
  query = q11,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of [Ii]slam.*

p11 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col11 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

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
  main = '[Ii]slam.*')

head(dis11)


## single query dispersions of [Cc]hrist(en|li).*

q12 <- c('"[Cc]hrist(en|li).*"')

dis12 <- dispersion(
  "GERMAPARL",
  query = q12,
  cqp = TRUE,
  s_attribute = c("year", "parliamentary_group"),
  freq = F
)


# time series plot of [Cc]hrist(en|li).*

p12 <- c("CDU/CSU", "SPD", "GRUENE", "FDP", "LINKE", "PDS", "V1")

col12 <- c("black", "red", "green", "yellow", "darkred", "darkred", "blue")

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
  main = '[Cc]hrist(en|li).*')

head(dis12)