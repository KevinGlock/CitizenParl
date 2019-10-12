## getting the dictionary samples

# The following workflow creates two partitions from the GermaParl corpus,
# subseted by parties ideological position (left/right or progressive/conservative)
# regarding issuses of national and transnational citizenship.


## load libraries

library("polmineR")
library("magrittr")
library("data.table")

use("GermaParl")


## create partition

coi_fdp16 <- partition("GERMAPARL",
                       parliamentary_group = "FDP",
                       year  = 2012:2016,
                       interjection= F,
                       role = c("mp", "government"))


## as partition bundles

pb2 <- partition_bundle(coi_fdp16, s_attribute = "date")


nested2 <- lapply(pb2@objects,
                  function(x) partition_bundle(x,
                                               s_attribute = "agenda_item",
                                               verbose = F
                  )
)


## flatten nested data frames

debates2 <- flatten(nested2)

names(debates2) <- paste(blapply(debates2,
                                 function(x) s_attributes(x, "date")),
                         blapply(debates2,
                                 function(x) name(x)),
                         sep = "_"
)


## dictionaries

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"',
        '".*[Ss]taatsangeh.*rig.*"', '".*[Ss]taatszugeh.*rig.*"', '"[Ss]taatenlos.*"',
        '"[Aa]us.*bürger.*"', '"[Ee]in.*bürger.*"', '"Doppelpass.*"', '"Doppelpa�Y.*"',
        '"Pass"', '"Pa�Y"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburtsrecht.*"', '"Geburtsprinzip.*"',
        '"[Ii]us soli"', '"[Ii]us sanguinis"', '"[Jj]us soli"', '"[Jj]us sanguinis"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Abstammungsrecht.*"', '"Abstammungsprinzip.*"')

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Doppelpass.*"', '"Doppelpa�Y.*"',
        '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"', '"Optionsmodell.*"')

q3 <- c('".*[Aa]syl.*"', '".*[Ff]lucht.*"', '".*[Ff]lücht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '"[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*schie.*"', '".*[Aa]b.*schob.*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied.*"',
        '"Aufnahme.*"', '"[Vv]isa.*"', '"[Vv]isum.*"', '"Loyalitätskonflikt"', '"Identitätsfeststellung"',
        '"Rückführung.*"', '".*[Aa]usländ.*"','"[Aa]ufenthalt.*"', '"Rückübernahme.*"', '"Ehegattennachzug"', '"Duldung.*"',
        '"Residenzpflicht"', '"Regelanfrage"', '".*Vertreib.*"', '".*Vertrieb.*"', '"AZR"', '"Aufnahme.*"')

q4 <- c(q1, q2, q3)


## erease quotation marks to highlight protocols

q1_regex <- gsub('^\\"(.*?)\\"$', '\\1', q1)

q2_regex <- gsub('^\\"(.*?)\\"$', '\\1', q2)

q3_regex <- gsub('^\\"(.*?)\\"$', '\\1', q3)

q4_regex <- gsub('^\\"(.*?)\\"$', '\\1', q4)


## get samples for Foreigners� Policy

dt2 <- count(debates2,
             query = q4,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt2)

debates_foreign2 <- debates2[[ subset(dt2, TOTAL >= 10)[["partition"]] ]]


## debates on Foreigners� Policy between 2012 and 2016

debates_foreign2[[24]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 24th debate on Foreigners� Policy for FDP between 2012 and 2016

warnings()


## get samples for citizenship

dt4 <- count(debates2,
             query = q1,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt4)


debates_citizen4 <- debates2[[ subset(dt4, TOTAL >= 4)[["partition"]] ]]


## citizenship debates between 2012 and 2016

debates_citizen4[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on citizenship for FDP between 2012 and 2016


## get samples for dual citizenship

dt6 <- count(debates2,
             query = q2,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt6)


## define minimum count for search words to gurantee that the debate is on the interested field

debates_dual2 <- debates2[[ subset(dt6, TOTAL >= 1)[["partition"]] ]]


## debates on dual citizenship between 2012 and 2016

debates_dual2[[1]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 3rd debate on dual citizenship for FDP between 2012 and 2016


## Hartfrid Wolff 2013-06-05 FDP

p <- partition("GERMAPARL", speaker = "Hartfrid Wolff", date = "2013-06-05", encoding = "UTF-8")

read(p)

## Hartfried Wolff (FDP)

## Wieder einmal fordert die SPD die Abschaffung des Optionsmodells. Dieses Modell hat sie selbst - das gilt übrigens auch für die Grünen - vor gut zehn Jahren mit beschlossen. Im Herbst 2012 überraschte die SPD die Nation mit ihrer angeblich neuen Forderung nach Hinnahme der Mehrstaatsangehörigkeit.
## ( Rüdiger Veit [ SPD ]: Unsinn! )
## Kurz danach haben wir über einen Antrag der SPD aus dem Jahr 2010, mit dem das gleiche Ziel verfolgt wurde, diskutiert. Ohne die heutige Bundestagsdebatte abzuwarten, hat die baden-württembergische Integrationsministerin, natürlich von der SPD, angekündigt, eine weitere Bundesratsinitiative zum selben Thema zu starten.

## Dabei ist die FDP durchaus bereit, über die vermehrte Hinnahme der doppelten Staatsangehörigkeit nachzudenken und in diese Richtung zu gehen.

## Aber wir sind uns auch bewusst, liebe Kollegen von den Grünen, dass die Staatsangehörigkeit für den Erfolg von Zuwanderung und Integration nicht primär entscheidend ist, sondern die persönliche und berufliche Perspektive der Menschen, die nach Deutschland kommen. Das ist entscheidend, damit sie hierbleiben wollen.

## Erstmals gibt es für minderjährige und heranwachsende geduldete Ausländer ein vom Aufenthaltsrecht der Eltern unabhängiges Bleiberecht in einem Bundesgesetz. Wir haben zwangsverheirateten Frauen in Not durch ein Rückkehrrecht die Chance gegeben, sich zu befreien und zurückzukommen.
## Was haben SPD und Grüne in ihrer Regierungszeit eigentlich diesbezüglich unternommen? Die rechtlichen Hürden für die Zuwanderung von Fachkräften wurden durch uns deutlich gesenkt und entbürokratisiert, und wir haben zusätzliche Integrationsanreize geschaffen.
## Eine zukunftsgerichtete Zuwanderungspolitik gibt den Menschen Perspektiven. Bereits 2011 haben wir im Bund das Gesetz zur Anerkennung ausländischer Abschlüsse verabschiedet. 2011 wurde dieses Anerkennungsgesetz verabschiedet. Herr Oppermann, ich muss zugeben, dass Hamburg Vorreiter ist. Aus meiner Sicht ist es aber trotzdem peinlich, dass gerade SPD-regierte Länder und das von den Grünen geführte Bundesland Baden-Württemberg in der Folge noch immer kein Anerkennungsgesetz für ausländische Abschlüsse geschaffen haben, zum Beispiel in Bezug auf Pflegeberufe, Ingenieure und Fachausbildungsabschlüsse.

## Es ist richtig, auch darüber nachzudenken, weitere Anreize zu geben, damit sich die Menschen stärker in unsere Gesellschaft einbringen. Die Vereinfachung der Erlangung der deutschen Staatsangehörigkeit kann dazugehören, zum Beispiel durch eine Verkürzung der entsprechenden Frist. Aus meiner Sicht ist es aber entscheidend, eine Willkommenskultur zu schaffen.
## ( Aydan �-zo?uz [ SPD ]: Und wie ist es nun mit der doppelten Staatsbürgerschaft? - Abg. Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ] meldet sich zu einer Zwischenfrage )

## ...

## es war damals einer der vielen wichtigen Erfolge von Max Stadler, einem wahren Liberalen,
## ( Rüdiger Veit [ SPD ]: Das erste wahre Wort! )
## die festgefahrenen Fronten im Staatsangehörigkeitsrecht endlich aufzubrechen. Das Optionsmodell war damals ein von der FDP vorbereiteter Kompromiss, um zwischen Rot-Grün und dem Bundesrat endlich weiterzukommen. Vor vier Jahren haben wir in der Koalition die sinnvolle Vereinbarung getroffen,
## ( Zuruf des Abg. Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ] )
## erst einmal Erfahrungen zu sammeln, wie sich diese Regelungen auswirken, und danach zu schauen, wie wir damit umgehen. Alles andere wäre wohlfeiler Aktionismus gewesen.
## Jetzt erst kommen die ersten Jahrgänge tatsächlich in die Entscheidungsphase. Die bisher gesammelten Daten - der Herr Staatssekretär hat sie vorgetragen - bestätigen unser Vorgehen. Gleichwohl hei�Yt es, nicht wegzusehen und die Augen nicht vor der Realität zu verschlie�Yen.
## ( Memet Kilic [ B�oNDNIS 90/DIE GR�oNEN ]: Genau das tun Sie! )
## Deshalb wollen die Liberalen eine Modernisierung des Staatsangehörigkeitsrechts. Aber wir bestehen darauf - anders als es sich zum Teil bei Vorschlägen der Opposition darstellt -,
## ( Hans-Christian Ströbele [ B�oNDNIS 90/DIE GR�oNEN ]: Jetzt kommt es! )
## dies nicht gedankenlos, nicht ohne Augenma�Y und nicht ideologisch anzugehen.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 3rd debate on dual citizenship for FDP between 2012 and 2016


## Hartfrid Wolff 2012-02-09 FDP

p <- partition("GERMAPARL", speaker = "Hartfried Wolff", date = "2012-02-09", encoding = "UTF-8")

read(p)

## Hartfried Wolff (FDP) speech

## Und täglich grü�Yt die SPD. Wieder einmal fordern die Sozialdemokraten die Abschaffung des Optionsmodells. Klasse! Das hat die SPD erst vor zehn Jahren selbst beschlossen. Im vergangenen Herbst überraschte Rüdiger Veit die Nation mit der angeblich neuen Forderung nach Hinnahme von Mehrfachstaatsangehörigkeiten.

## Wir Liberalen haben seinerzeit das Optionsmodell vorgeschlagen, um den Weg hin zu einer �-ffnung des deutschen Staatsangehörigkeitsrechts in Richtung auf das Jus Soli zu ermöglichen.
## Für in Deutschland aufgewachsene junge Menschen ist es nach Auffassung von Rot-Rot-Grün aber unzumutbar, sich bei Volljährigkeit für die deutsche Staatsangehörigkeit zu entscheiden. Die Partei Die Linke tut sich mit der Wahlfreiheit, der Kompetenz des Individuums, sich entscheiden zu dürfen, ja generell schwer.
## Wer die doppelte Staatsangehörigkeit fordert, stoppt die Modernisierung des Staatsangehörigkeitsrechts. Galt Linken, Grünen und Sozialdemokraten das Abstammungsrecht bei deutschen Aussiedlern jedenfalls noch als reaktionäres Rechtsprinzip, ist es im Hinblick auf die Doppelstaatsangehörigkeit, etwa für Araber, plötzlich wieder erwünscht.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 3rd debate on dual citizenship for FDP between 2012 and 2016


## speech from FDP doesn�t refer to dual citizenship

