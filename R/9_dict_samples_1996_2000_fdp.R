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

coi_fdp00 <- partition("GERMAPARL",
                       parliamentary_group = "FDP",
                       year  = 1996:2000,
                       interjection= F,
                       role = c("mp", "government"))


## as partition bundles

pb1 <- partition_bundle(coi_fdp00, s_attribute = "date")


nested1 <- lapply(pb1@objects,
                  function(x) partition_bundle(x,
                                               s_attribute = "agenda_item",
                                               verbose = F
                  )
)


## flatten nested data frames

debates1 <- flatten(nested1)

names(debates1) <- paste(blapply(debates1,
                                 function(x) s_attributes(x, "date")),
                         blapply(debates1,
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

dt1 <- count(debates1,
             query = q4,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt1)

debates_foreign1 <- debates1[[ subset(dt1, TOTAL >= 10)[["partition"]] ]]


## debates on Foreigners� Policy between 1996 and 2000

debates_foreign1[[39]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 39th debate on Foreigners� Policy for FDP between 1996 and 2000


## get samples for citizenship

dt3 <- count(debates1,
             query = q1,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt3)

debates_citizen3 <- debates1[[ subset(dt3, TOTAL >= 4)[["partition"]] ]]


## citizenship debates between 1996 and 2000

debates_citizen3[[12]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 12th debate on citizenship between 1996 and 2000 for FDP


## get samples for dual citizenship

dt5 <- count(debates1,
             query = q2,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt5)


## define minimum count for search words to gurantee that the debate is on the interested field

debates_dual1 <- debates1[[ subset(dt5, TOTAL >= 4)[["partition"]] ]]


## debates on dual citizenship between 2012 and 2016

debates_dual1[[1]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 5th debate on dual citizenship between 1996 and 2000 for FDP


## Guido Westerwelle 1999-03-19 FDP

p <- partition("GERMAPARL", speaker = "Guido Westerwelle", date = "1999-03-19", encoding = "UTF-8")

read(p)

## Westerwelle (FDP)

## Wir werden in Deutschland mit diesem Gesetz nach einer langjährigen Diskussion jetzt ein modernes Staatsangehörigkeitsrecht bekommen.
## ( Dr. Jürgen Rüttgers [ CDU/CSU ]: Das glauben Sie doch selber nicht! )
## Das neue Staatsangehörigkeitsrecht wird zu einer Verbesserung der Integration der dauerhaft und rechtmä�Yig in Deutschland lebenden Menschen ausländischer Herkunft führen. Die Verbesserung der Integration dieser Menschen ist im Interesse unserer gesamten Gesellschaft dringend notwendig.

## Diese Reform - das ist aus Sicht der Freien Demokraten besonders wichtig - wird von einer breiten Mehrheit in der Bevölkerung akzeptiert. Ich möchte mich deshalb - Sie werden verstehen, da�Y ich das zu Beginn meiner Rede tue - ausdrücklich bei denen herzlich bedanken, die bei diesem wichtigen, ja historischen Schritt mitgewirkt haben. Zunächst möchte ich mich ganz herzlich bei unserer früheren Kollegin Cornelia Schmalz-Jacobsen bedanken, die als seinerzeitige Ausländerbeauftragte der Bundesregierung ma�Ygeblichen Anteil an der hier vorliegenden Reform hat.

## Wichtig war uns - deswegen hatten wir einen eigenen Gesetzentwurf eingebracht -, da�Y die hier geborenen Kinder von dauerhaft und rechtmä�Yig in Deutschland lebenden Ausländern mit Geburt die deutsche Staatsangehörigkeit erwerben. Wer den Eindruck erweckt, eine im achten oder neunten Monat Schwangere könne gewisserma�Yen durch Deutschland reisen und ihr hier geborenes Kind wäre dann automatisch Deutscher, führt eine absolut unzutreffende Polemik ein. Wir reden hier von den Kindern seit langem in Deutschland rechtmä�Yig lebender Ausländer. Es dient unserer Gesellschaft, wenn wir diese Kinder integrieren.

## Deshalb halten wir es für gerechtfertigt, von dem einbürgerungswilligen Ausländer die Aufgabe seiner bisherigen Staatsangehörigkeit zu verlangen.
## Das gilt insbesondere für diejenigen Ausländer, die bereits lange in Deutschland leben. Wer 30 Jahre in Deutschland gelebt hat, der kennt dieses Land gut genug, um sich entscheiden zu können, ob er Deutscher sein will oder nicht. Aber bei den hier geborenen Kindern nehmen wir die Mehrstaatigkeit für eine gewisse Zeit in Kauf, weil sie eben als Minderjährige nicht selbst entscheiden können.
## Deswegen möchten wir, da�Y diese Kinder sich erst als junge Erwachsene, nach Erreichen der Volljährigkeit, zwischen der Staatsangehörigkeit der Eltern und unserer deutschen Staatsangehörigkeit entscheiden müssen.
## Das ist unser Optionsmodell, das die Koalition nun übernommen hat. Damit wir zu nennenswerten Integrationsfortschritten kommen, haben wir als F.D.P. vorgeschlagen, da�Y dieses Modell auch auf bereits geborene Kinder übertragen wird, die noch nicht älter als zehn Jahre sind. Ich bin froh darüber, da�Y sich in unserem Gruppenantrag gerade dieses Angebot an die bereits in Deutschland geborenen Kinder findet.
## Wir halten an dem Grundsatz der Vermeidung von Mehrstaatsangehörigkeit klar und eindeutig fest. Der Katalog der Ausnahmetatbestände wird nicht erweitert, sondern lediglich flexibler gestaltet.
## Da Sie, Herr Kollege Zeitlmann, aus dem Gesetzentwurf zitiert haben - Sie können jetzt nicht zuhören, weil Sie telefonieren müssen -, möchte ich noch einmal auf § 87 des Ausländergesetzes in unserem Gesetzentwurf hinweisen. Sie haben von älteren Bürgern gesprochen. So, wie Sie das wiedergegeben haben, stimmt es einfach nicht. Hier steht:
## Einbürgerung unter Hinnahme von Mehrstaatigkeit Sie wird hingenommen, wenn der Einbürgerung älterer Personen ausschlie�Ylich das Hindernis eintretender Mehrstaatigkeit entgegensteht, die Entlassung auf unverhältnismä�Yige Schwierigkeiten stö�Yt und die Versagung der Einbürgerung eine besondere Härte darstellen würde.

## Es gibt keine verfassungsrechtlich ernstzunehmenden Bedenken gegen das Optionsmodell. Das Optionsmodell ist von mehreren Justizministern geprüft worden. Es ist dem früheren Bundesjustizminister vorgelegt und dort geprüft worden, ebenso der derzeitigen Bundesjustizministerin, dem Innenminister, dem Landesjustizminister von Rheinland-Pfalz, und es ist übrigens natürlich auch vom Wissenschaftlichen Dienst des Deutschen Bundestages eingehend geprüft worden. Dabei wurde klar festgestellt, da�Y das Optionsmodell vollständig verfassungsgemä�Y ist und da�Y es selbstverständlich auch zulässig ist, den jungen Erwachsenen eine Entscheidung abzuverlangen.
## Weil in diesem Zusammenhang Art. 16 des Grundgesetzes immer wieder zitiert wird, der dem angeblich entgegensteht, möchte ich einmal darauf hinweisen, was dort steht. Natürlich hei�Yt es in Art. 16 Abs. 1 Satz 1:
## Die deutsche Staatsangehörigkeit darf nicht entzogen werden.
## Die Juristen wissen aber: Immer einen Satz weiterlesen.
## ( Heiterkeit bei Abgeordneten der SPD )
## Denn Satz 2 lautet:
## Der Verlust der Staatsangehörigkeit darf nur auf Grund eines Gesetzes und gegen den Willen des Betroffenen nur dann eintreten, wenn der Betroffene dadurch nicht staatenlos wird.
## Entziehung ist eben nicht Verlust. Wer sich selbst entscheiden kann, der bekommt die Staatsangehörigkeit nicht gegen seinen oder ohne seinen Willen entzogen. Vielmehr führt er durch seine eigene Willensentscheidung den Verlust oder den Behalt der deutschen Staatsangehörigkeit herbei.

## Ja, ganz im Gegensatz zu einer generellen Vergabe der doppelten Staatsbürgerschaft, die mit der Verfassung nicht zu vereinbaren wäre. Das Optionsmodell ist verfassungskonform, weil keine generelle doppelte Staatsangehörigkeit verliehen wird und weil keine Entziehung der deutschen Staatsangehörigkeit vorgesehen ist.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 5th debate on dual citizenship between 1996 and 2000 for FDP


## Burkhard Hirsch 1996-02-08 FDP

p <- partition("GERMAPARL", speaker = "Burkhard Hirsch", date = "1996-02-08", encoding = "UTF-8")

read(p)


## Burkhard Hirsch (FDP) answering Marschewski

## Herr Kollege Marschewski, wir bemühen uns ja gemeinsam, auf vielen Gebieten zu einer europäischen Rechtsangleichung zu kommen. Nun gibt es eine Europäische Konvention zur Verhinderung von Mehrstaatigkeit. Das zweite Zusatzprotokoll zu dieser Konvention ist 1993 in Kraft getreten, weil es von Italien, Frankreich und den Niederlanden gezeichnet worden ist.
## In dieser Konvention wird den Mitgliedstaaten ausdrücklich nachgelassen, da�Y sie Angehörige der zweiten Generation einbürgern, ohne zu verlangen, da�Y diese ihre bisherige Staatsangehörigkeit aufgeben. Es wird ausdrücklich vorgesehen, da�Y Kinder, die aus gemischtstaatigen Ehen kommen - davon gibt es Hunderttausende in der Bundesrepublik -, selbstverständlich die Staatsangehörigkeit beider Eltern bekommen können. Es ist ein weiteres Abkommen in Vorbereitung - darüber verhandelt auch die Bundesrepublik -, in dem vorgesehen ist, da�Y niemand, der eine zusätzliche Staatsangehörigkeit annimmt, deswegen seine bisherige aufgeben soll.
## Sie müssen sich doch die Frage gefallen lassen, warum wir ausgerechnet in dieser Frage von dem europäischen Gleichklang abweichen wollen, warum wir nicht nur der europäischen Entwicklung nicht folgen, sondern geradezu schnurstracks ins 19. Jahrhundert zurückgehen wollen. Diese Frage müssen Sie beantworten. Wir stellen sie doch nicht nur im Interesse der einzubürgernden Ausländer - um deren Einbürgerung zu erleichtern -, sondern auch im Interesse unseres eigenen Volkes, damit das Anwachsen einer Diaspora von Bürgern zweiter Klasse in unserem Lande verhindert wird.


## ...


## Cornelia Schmalz-Jacobson (FDP, Ausl�nderbeauftagte) pleading

## Es gibt in diesem Hause eine klare Mehrheit für �"nderungen im Staatsbürgerschaftsrecht.
## Es gibt übrigens auch in der �-ffentlichkeit eine breite Mehrheit für �"nderungen, und zwar von '' " Pro Asyl '' über die Kirchen, über viele Vereinigungen und Verbände bis hin zur '' " Frankfurter Allgemeinen Zeitung '' . Es gibt eine klare Mehrheit innerhalb der Bevölkerung; das haben Umfragen deutlich gemacht.
## Ich möchte hier nicht stehen, ohne mich auch zu bedanken bei all denjenigen, die sich, quer durch die Fraktionen, bemüht haben, Brücken zu bauen. ( Beifall bei der F.D.P. )
## Mitunter sind ja die lauten Töne so geartet, da�Y sie das überdecken. Aber es gibt die Brückenbauer, und ich danke ihnen.

## Die SPD hat einen neuen und, wie ich finde, sehr interessanten Antrag vorgelegt, ( Cem �-zdemir [ B�oNDNIS 90/DIE GR�oNEN ]: Den Sie ablehnen werden! )
## in dem zum erstenmal auch der Verlust von Staatsbürgerschaft deutlich gemacht wird. Herr Kollege �-zdemir, Sie wissen, da�Y der Entwurf, den Sie vorgelegt haben und den wir in den Ausschüssen beraten haben - der übrigens nicht mein ehemaliger ist; das sage ich ausdrücklich -, in einigen Punkten von unseren Vorstellungen abweicht. Wir halten ihn für zu weitgehend.

## Wir sind ganz eindeutig für eine Verkürzung der Fristen. Wir sind der Meinung, da�Y diejenigen, die sich seit acht Jahren rechtmä�Yig hier aufhalten, einen Rechtsanspruch auf Einbürgerung haben sollten.
## Ich bin der Meinung, da�Y Kinder, die hier geboren werden, in der zweiten Generation - eine Mehrheit ist der Meinung, in der dritten Generation - automatisch die deutsche Staatsbürgerschaft haben sollten. Das ist neben dem Abstammungsprinzip ein kleines Türchen des Geburtsrechts.

## Heute spricht niemand mehr - jedenfalls mit Sicherheit nicht bei uns - von einer generellen Doppelstaatsbürgerschaft, von flächendeckender Doppelstaatsbürgerschaft.
## Aber es gibt Staatsrechtler wie Herrn Professor Hailbronner, der deutlich gemacht hat, da�Y die rechtspolitischen Argumente gegen die Doppelstaatsbürgerschaft längst an Bedeutung verloren haben.
## Herr Kollege Hirsch hat schon auf das zweite Zusatzprotokoll zum Europaratsabkommen zur Vermeidung von Mehrstaatlichkeit aus dem Jahre 1963 hingewiesen, das unsere drei Nachbarländer, an denen wir uns sonst eher orientieren als zum Beispiel an Ru�Yland und Polen, nämlich Frankreich, Italien und die Niederlande, gezeichnet haben. Im Moment arbeitet eine Expertengruppe des Europarats an einer neuen europäischen Konvention zur Staatsangehörigkeit. Es geht hier im Kern um das Recht, bei Einbürgerung die alte Staatsbürgerschaft beizubehalten.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 5th debate on dual citizenship between 1996 and 2000 for FDP


## Guido Westerwelle 1999-05-07 FDP

p <- partition("GERMAPARL", speaker = "Guido Westerwelle", date = "1999-05-07", encoding = "UTF-8")

read(p)

## Guido Westerwelle (FDP)

## Ich habe hier eine schöne kleine dtv-Ausgabe '' " Ausländerrecht '' aus dem Hause Beck, die jedermann im ersten Semester Jura bekommt.
## ( Dr. Wilfried Penner [ SPD ]: Lang, lang ist 's her! )
## - Das ist aber keine alte Ausgabe, sondern eine ganz neue. So schlecht, da�Y wir uns das nicht leisten könnten, steht es um die Finanzen der F.D.P. noch nicht.
## ( Heiterkeit und Beifall bei der F.D.P. - Ludwig Stiegler [ SPD ]: Nein, Herr Marschewski war gemeint! )
## Ich zitiere aus den Einbürgerungsrichtlinien 5.3.3.:
## Ausnahmen - gemeint sind Ausnahmen von dem Prinzip der Vermeidung der Mehrstaatigkeit - können in Betracht kommen, wenn vorrangige Gesichtspunkte es erfordern, da�Y das rechtspolitische Ordnungsprinzip, Mehrstaatigkeit zu vermeiden, zurücktritt,
## ( Erwin Marschewski [ CDU/CSU ]: Das ist ein Punkt! Ja! )
## und - jetzt kommt es - wenn die Versagung der Einbürgerung eine unzumutbare Härte darstellen würde.
## ( Erwin Marschewski [ CDU/CSU ]: Das ist ja wahr! )
## Im weiteren finden Sie in den Einbürgerungsrichtlinien dann die gesamten einzelnen Ziffern, die Sie jetzt auch bei uns im Gesetzentwurf finden.
## ( Zuruf Erwin Marschewski [ CDU/CSU ]: Das ist falsch! - Meinrad Belle [ CDU/CSU ]: Drei sind hinzugekommen! )
## Zum Beispiel finden Sie dort bereits das, was von Ihnen kritisiert worden ist:
## Danach kommen Ausnahmen vom Einbürgerungshindernis eintretender Mehrstaatigkeit in Betracht, wenn - jetzt kommt Ziffer 5.3.3.4. - der Einbürgerung älterer Personen ausschlie�Ylich das Hindernis eintretender Mehrstaatigkeit entgegensteht, die Entlassung auf unverhältnismä�Yige Schwierigkeiten stö�Yt und die Versagung der Einbürgerung eine besondere Härte darstellen würde.

## Sie erwecken den Eindruck, wir würden mehr Doppelpässe zulassen. Dieser Eindruck ist falsch.
## ( Beifall bei der F.D.P. sowie bei Abgeordneten der SPD )
## Wir bleiben dabei: Das Prinzip der Vermeidung der Mehrstaatigkeit wird nicht in Frage gestellt.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 5th debate on dual citizenship between 1996 and 2000 for FDP


## Cornelia Schmalz-Jacobsen 1997-10-30 FDP

p <- partition("GERMAPARL", speaker = "Cornelia Schmalz-Jacobsen", date = "1997-10-30", encoding = "UTF-8")

read(p)

## Cornelia Schmalz-Jacobsen (FDP)

## Auch Sie alle werden heute den gro�Yen Leitartikel auf der ersten Seite der '' " Frankfurter Allgemeinen Zeitung '' gelesen haben - wahrlich kein Blatt, das linksradikaler Umtriebe als verdächtig gilt.
## ( Eckart von Klaeden [ CDU/CSU ]: Eine gute Zeitung! Die beste, die ich kenne! )
## Dort steht der schöne Satz, bezogen auf die Kinder: '' " Es lassen sich am besten Staatsbürger zu Staatsbürgern erziehen. ''
## ( Beifall bei der F.D.P. und der CDU/CSU )
## Das mu�Y man sich auf der Zunge zergehen lassen; das ist sehr klug.
## Ich möchte nun in aller Kürze darlegen, worum es geht und worum es nicht geht, und zwar nach der Methode: Falsch ist.; richtig ist vielmehr.
## Es ist nicht richtig, wenn behauptet wird, da�Y jetzt alle ausländischen Kinder, die in Deutschland geboren werden, zu Deutschen werden sollen. Das ist nicht das Thema. Wir möchten das an bestimmte Kriterien binden, zum Beispiel an das Kriterium, da�Y ein Elternteil bereits lange hier lebt und einen verfestigten Aufenthaltsstatus hat.
## Es geht nicht in erster Linie um Doppelstaatsbürgerschaften, sondern um eine bessere Integration. Der Grundsatz der Vermeidung der Mehrstaatlichkeit bleibt nach unseren Vorstellungen erhalten.
## Es ist falsch, wenn in die Welt gesetzt wird - übrigens vom Oberbürgermeister von Stuttgart, wenn ich mich richtig erinnere -, da�Y wir einen grö�Yeren Zustrom hätten. Ich will Ihnen eine Zahl des Statistischen Bundesamtes, das ebenfalls unverdächtig ist, nennen. In dem letzten statistisch erfa�Yten Jahr ist bei den Türken - wir reden offenbar nur über Türken - unterm Strich ein Plus von 30 000 Menschen verzeichnet. Dazu gehören der Familiennachzug und auch die Asylbewerber; die meisten sind Kurden und haben die türkische Staatsbürgerschaft.
## Es ist nicht richtig, wenn gesagt wird: Bei uns gibt es keine Doppelstaatsbürgerschaft. Es gibt sie, und zwar massenhaft. Ich wei�Y nicht, bei wieviel Familien unseres Koalitionspartners es diese Fälle gibt. Ich wei�Y von einigen binationalen Ehen. Offenbar ist dies wenig dramatisch.

## Das hat zur Folge, da�Y die Kinder eigentlich gleich sind, da�Y sie gleich aufwachsen: Kinder mit zwei deutschen Elternteilen, Kinder mit einem deutschen Elternteil, die binational sind, und ausländische Kinder, die hier aufwachsen. Nur, wir sind der Meinung, da�Y sie sich nicht nur als gleiche empfinden sollten, da�Y sie vielmehr gleichberechtigt sein sollten und das auch wissen sollten. - Eine Doppelstaatsbürgerschaft auf Zeit, meine Damen und Herren, ist kein Glaubensartikel.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[5]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 5th debate on dual citizenship between 1996 and 2000 for FDP


## debate doesn�t refer to dual citzinship
