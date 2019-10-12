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

coi_cdu16 <- partition("GERMAPARL",
                       parliamentary_group = "CDU/CSU",
                       year  = 2012:2016,
                       interjection= F,
                       role = c("mp", "government"))


## as partition bundles

pb2 <- partition_bundle(coi_cdu16, s_attribute = "date")

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

q1 <- c('"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"', '".*[Ss]taatsangeh.*rig.*"',
        '".*[Ss]taatszugeh.*rig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Pass"', '"Pa�Y"',
        '"Blutsrecht.*"', '"Geburtsrecht.*"', '"Geburtsprinzip.*"',
        '"[Ii]us soli"', '"[Ii]us sanguinis"', '"[Jj]us soli"', '"[Jj]us sanguinis"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Abstammungsrecht.*"', '"Abstammungsprinzip.*"')

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Doppelpass.*"', '"Doppelpa�Y.*"',
        '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"', '"Optionsmodell.*"')

q3 <- c('".*[Aa]syl.*"', '".*[Ff]lucht.*"', '".*[Ff]lücht.*"', '".*[Mm]igra.*"',
        '".*[Ee]in.*wander.*"', '".*[Gg]renz.*"', '"[Ff]amilienzusammen.*"', '".*[Aa]us.*bürger.*"',
        '".*[Aa]b.*schie.*"', '".*[Aa]b.*schob.*"', '".*[Ee]in.*bürger.*"', '".*[Aa]us.*sied.*"',
        '"Aufnahme.*"', '"[Vv]isa.*"', '"[Vv]isum.*"', '"Loyalitätskonflikt"', '"Identitätsfeststellung"',
        '"Rückführung.*"', '".*[Aa]usländ.*"', '".*[Rr]usslanddeutsch.*"',
        '"[Aa]ufenthalt.*"', '"Rückübernahme.*"', '"Ehegattennachzug"', '"Duldung.*"',
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

debates_foreign2 <- debates2[[ subset(dt2, TOTAL >= 25)[["partition"]] ]]


## debates on Foreigners� Policy between 2012 and 2016

debates_foreign2[[78]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 78th debate on Foreigners� Policy for CDU/CSU between 2012 and 2016

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

debates_citizen4 <- debates2[[ subset(dt4, TOTAL >= 25)[["partition"]] ]]


## citizenship debates between 2012 and 2016

debates_citizen4[[5]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 5th debate on citizenship for CDU/CSU between 2012 and 2016


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

debates_dual2 <- debates2[[ subset(dt6, TOTAL >= 4)[["partition"]] ]] 

## debates on dual citizenship between 2012 and 2016

debates_dual2[[1]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Reinhard Grindel 2013-06-05 CDU on integration of people with migration background

p <- partition("GERMAPARL", speaker = "Reinhard Grindel", date = "2013-06-05", encoding = "UTF-8")

read(p)

## Reinhard Grindel (CDU)

## Auch von den Kindern, die hier in Deutschland geboren sind, haben viele nicht die Sprachkompetenz, die man haben muss, um zum Beispiel in der Grundschule erfolgreich zu sein. Zu viele Schüler verlassen die Schule ohne Schulabschluss.
## ( Ekin Deligöz [ B�oNDNIS 90/DIE GR�oNEN ]: Oh! - Weiterer Zuruf vom B�oNDNIS 90/DIE GR�oNEN: Und das wird durch den Optionszwang besser? )
## Jugendliche mit Migrationshintergrund kommen weniger oft in Ausbildung, als das im Durchschnitt der Fall ist. Es gibt Parallelgesellschaften.
## Darauf die Antwort zu geben: " Ihr bekommt aber die deutsche Staatsbürgerschaft ", das ist Steine statt Brot.
## ( Lachen bei Abgeordneten des B�oNDNISSES 90/DIE GR�oNEN )
## Diese Menschen brauchen Arbeit, sie brauchen Ausbildung, sie müssen die deutsche Sprache lernen, sie brauchen Förderung für ihre Kinder; das würde helfen. Die deutsche Staatsbürgerschaft allein hilft da wenig.


## ...


## Grindel (CDU) aswering Delig�z (Gr�ne)

## Es geht doch gar nicht um die Frage, ob Schülerinnen und Schüler deutsche Staatsbürger werden können.
## Es geht darum, dass die Integration, die Herr Oppermann gefordert hat - Stichworte " Willkommenskultur " und " sein Glück machen können in Deutschland " -, nicht allein an der Staatsbürgerschaft hängt.
## Eine erfolgreiche Integration hängt davon ab, dass man die deutsche Sprache spricht, dass man in der Schule Erfolg hat, dass man eine Ausbildung machen kann, dass man arbeiten kann, dass man eine Firma gründen kann. Darum geht es mir: Integration umfasst viel mehr als nur die Staatsbürgerschaft.
## Wenn die Grünen die Staatsbürgerschaft unter erleichterten Bedingungen anbieten wollen, dann ist das Wahlkampftaktik; damit will man Stimmen gewinnen. Dass die Menschen hier in Deutschland ihr Glück machen können, wird mit der Verleihung der Staatsbürgerschaft allein nicht erreicht.
## Ich will Ihnen noch etwas sagen - Sie können sich ruhig hinsetzen, Frau Deligöz; aber es gehört noch zur Antwort auf Ihre Frage -: Es ist in der Tat so gewesen - Sie haben das richtig beschrieben -, dass man bei der Schaffung des Optionsmodells vor allen Dingen die Situation der Kinder in den Schulen im Blick hatte. Man hat - frei nach der Position von Frau Künast - gesagt: Die integrieren sich sowieso, und es kann doch nicht angehen, dass bei einer Klassenfahrt ins Ausland die drei türkischstämmigen Kinder ein Visum benötigen.
## Das Problem ist aber, Frau Künast, dass heute zwar alle mitfahren können, viele aber - gerade türkische Mädchen - nicht mitfahren dürfen, weil ihre Eltern es verbieten.
## Solange wir solche Parallelstrukturen haben, die dafür sorgen, dass Kinder nicht gemeinsam Sport machen dürfen - Herr Steinbrück findet das auch noch gut -, dass Kinder nicht gemeinsam auf Klassenfahrt gehen dürfen, so lange zementieren wir Parallelgesellschaften
## ( Zuruf der Abg. Christine Buchholz [ DIE LINKE ] )
## und erreichen trotz Verleihung der deutschen Staatsbürgerschaft keine echte Integration. Das ist - dies wollte ich deutlich machen - der eigentliche Kern des Problems.

## Ich will Ihnen ganz ehrlich sagen: Die Einbürgerung darf kein Instrument der türkischen Politik sein, Einfluss in Deutschland zu gewinnen. Vielmehr ist die Einbürgerung der Schlussstein eines gelungen Integrationsprozesses. Die Regierungschefin für diese Mitbürger ist Angela Merkel und nicht Herr Erdogan; auch das müssen wir einmal deutlich machen.


## ...


## Grindel (CDU) aswering �zoguz (SPD)

## dass der Ansatz, beim Thema Integration einzig und allein auf die Staatsbürgerschaft zu schauen, zu kurz greift.
## Vor allen Dingen müssen wir immer wieder das Signal senden, dass wir kein Nebeneinander haben wollen. Wir leben in unterschiedlichen Stadtquartieren. Wir kommunizieren in unterschiedlichen Sprachen. Unsere Kinder gehen - Sie sind doch vom Fach; Sie wissen das - zu unterschiedlichen Zeiten in die Kitas: morgens mehr Deutsche, nachmittags mehr Migrantenkinder.
## Wir haben in unserer Gesellschaft zu viel Nebeneinander. Wir brauchen auf allen Ebenen ein Miteinander. Deswegen sage ich: Wenn wir auf Dauer zulassen, dass es auch bei der Staatsbürgerschaft ein Nebeneinander gibt, sogar mit unterschiedlichen Loyalitäten - ich erinnere an ein entsprechendes Zitat von Erdogan -, dann führt das in die Irre.
## Wer Ja zu Deutschland sagt, wer gerne bei uns leben will, von dem kann ich auch die Entscheidung für die deutsche Staatsbürgerschaft unter Ablegung seiner alten Staatsbürgerschaft erwarten.



## Grindel (CDU) aswering

## Das Klatschen zeugt leider nicht von gro�Yer rechtlicher Kenntnis und auch, so glaube ich, von einem falschen Staatsverständnis. Herr Röspel, wenn Sie die Situation im Verhältnis zwischen Deutschland und Schweden, also zwischen zwei EU-Staaten, mit der Situation zwischen Deutschland und der Türkei vergleichen und sagen: " Das verstehe ich nicht ", dann verstehe ich Sie nicht.
## Deutschland und Schweden sind Mitglieder in der Europäischen Union; sie gehören also zu einer europäischen Werteunion.
## Das, was zurzeit in der Türkei passiert, hat mit den Werten der Europäischen Union nichts zu tun.
## Deshalb ist es in Ordnung, eine doppelte Staatsbürgerschaft im Zusammenhang mit Schweden zu haben, aber nicht, schon gar nicht in der aktuellen Situation, mit der Türkei.
## Die Türkei gehört nicht zur EU. Das ist ein gewaltiger staatsrechtlicher und völkerrechtlicher Unterschied. Nehmen Sie das bitte zur Kenntnis!


## ...


## Grindel (CDU) aswering K�nast

## Frau Künast, darin stimme ich Ihnen selbstverständlich hundertprozentig zu.
## Die Realität in der Türkei dieser Tage zeigt, dass sie sehr weit von der EU entfernt ist.
## Deshalb gibt es keine Grundlage, Staatsbürger der Türkei - das war die Frage von Herrn Röspel - so zu behandeln wie Staatsbürger aus EU-Staaten. Das ist der Unterschied. Das habe ich, glaube ich, sehr deutlich gesagt.
## Wir sind in einer Wertegemeinschaft mit Schweden, �-sterreich und allen anderen EU-Ländern. Deswegen sagen wir: Deren Staatsbürger können, wenn sie es wollen, beide Staatsbürgerschaften beibehalten. Aber von der Türkei sind wir meilenwert entfernt.
## Deswegen vergleicht Herr Röspel �"pfel mit Birnen, wenn er mit dem Beispiel Schweden kommt und mich damit wegen meiner Position gegenüber türkischen Staatsangehörigen zu kritisieren versucht. Ich glaube, jeder hier im Saal hat das jetzt verstanden.
## ( Beifall bei Abgeordneten der CDU/CSU )


## ...


## Serkan T�ren (FDP) speech

## Frau Kolbe, ich finde es geradezu unverschämt, dass Sie dem Kollegen Grindel Rassismus vorwerfen.

## Bei der Optionspflicht geht es für die betreffenden jungen Menschen gar nicht so sehr um eine Loyalitätsentscheidung, also für welches Land sie eintreten, sondern oft um die Frage, ob es Brüche im Lebenslauf gibt; das ist manchmal eine sehr schwierige Entscheidung. Eine Studie des BAMF zeigt: 66 Prozent der Betroffenen wünschen sich tatsächlich die Beibehaltung der Herkunftsstaatsangehörigkeit.
## Wir haben auf unserem letzten Parteitag die grundsätzliche Anerkennung der doppelten Staatsangehörigkeit in unser Wahlprogramm aufgenommen.
## Wir hatten im Jahre 2011� 107� 000 Einbürgerungen zu verzeichnen, davon rund 51 Prozent unter Inkaufnahme der doppelten Staatsangehörigkeit. Für mich ist es eine Frage der Gerechtigkeit, wie wir mit den anderen 49 Prozent verfahren.

## Ich glaube, dass die doppelte Staatsbürgerschaft die Teilhabe und die Integration vieler Menschen fördert. Ein Beispiel: Ich bin mit Anfang 20 seinerzeit eingebürgert worden, und zwar unter Inkaufnahme der doppelten Staatsangehörigkeit, weil ich nicht aus der türkischen Staatsangehörigkeit entlassen werden konnte; denn ich hatte meinen Militärdienst in der Türkei nicht abgeleistet. Der nächste Schritt, nachdem ich eingebürgert worden bin - das war für mich ein Signal dafür, dass die Gesellschaft mich will und ich Teil dieser Gesellschaft bin -, war einer der besten Schritte überhaupt, die man machen kann: Ich bin einige Monate später zur FDP gegangen und bin Mitglied dieser Partei geworden, weil ich selbst etwas gestalten und in dieser Gesellschaft mitwirken wollte.
## Gegner der doppelten Staatsangehörigkeit reden von Loyalitätskonflikten. Ich frage dann manchmal, wie diese Loyalitätskonflikte eigentlich aussehen. So richtig konkrete Antworten bekomme ich selten. Ein Problem - darüber kann man durchaus diskutieren - war der Wehrdienst; aber den haben wir - auch dank der FDP - mittlerweile nicht mehr. Insofern ist ein wichtiger Grund für einen Loyalitätskonflikt, wie man ihn sonst kannte, weggefallen.
## Einige Sachverständige haben uns gesagt, dass es eine aktive Staatsangehörigkeit da gibt, wo man lebt, wo man Grundrechte ausüben kann und wo man Pflichten erfüllt, dass es aber auch eine passive Staatsangehörigkeit gibt, und zwar da, wo man nicht lebt.
## Auch an meinem Beispiel zeigt sich, dass ich die Grundrechte in der Türkei nie geltend machen konnte, weil ich dort eben nicht gelebt habe. Das Einzige, was gestört hat, war die Pflicht zum Militärdienst. Deswegen habe ich vor einigen Jahren meine türkische Staatsangehörigkeit aufgegeben.
## Wir sind ein Land, das um Fachkräfte ringt. Wir brauchen Hochqualifizierte. Diese Regierung hat deswegen die Einführung der Bluecard beschlossen, etwas, was Sie von der Opposition jahrelang nicht geschafft haben. Sie haben nur darüber geredet, aber nichts geleistet. Wir haben das hinbekommen. Wir kämpfen jetzt im weltweiten Wettbewerb um die besten Köpfe.
## Dazu gehört auch, dass man Anreize schafft. Die angelsächsischen Staaten erlauben im Grundsatz die doppelte Staatsangehörigkeit. Im Wettbewerb mit diesen Staaten müssen wir Anreize schaffen und ebenfalls über die doppelte Staatsangehörigkeit nachdenken.

## Ich möchte dazu aus der Bild-Zeitung von vor einigen Monaten zitieren: " Mehrstaatlichkeit erleichtert Kriminalität und dient denen, die Unrechtes im Schilde führen. " Sie wissen, wer das gesagt hat: ein SPD-Bürgermeister hier in Berlin. Daran erkennt man die Scheinheiligkeit.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Stephan Mayer 2012-02-09 CSU on integration of people with migration background

p <- partition("GERMAPARL", speaker = "Stephan Mayer", date = "2012-02-09", encoding = "UTF-8")

read(p)

## Stephan Mayer (CSU)

## Meine sehr geehrten Kollegen von der SPD, es ist nicht neu, dass Sie für die Abschaffung des Optionsmodells sind, dass Sie für die generelle Zulassung der Mehrstaatigkeit sind. Mich wundert nur, dass Sie in regelmä�Yigen Abständen mit den gleichen Anträgen oder Gesetzentwürfen kommen.
## Ich kann mir das - mit Verlaub - nur so erklären, dass Sie, lieber Herr Kollege Veit, immer noch traumatisiert sind, weil Sie 1999 dem Kompromiss beim Staatsangehörigkeitsrecht zugestimmt haben.
## Auch wenn gewisse Fragen mittlerweile nicht mehr akut sind - beispielsweise aufgrund der Aussetzung der Wehrpflicht in Deutschland -, bleiben eklatante rechtliche Schwierigkeiten für den Fall bestehen, dass man generell die Mehrstaatigkeit zulässt.
## Es gibt schon heute die Möglichkeit, Mehrstaatigkeit zuzulassen. Es gibt die Härtefallregelung des § 12 des Staatsangehörigkeitsgesetzes. Wenn die Aufgabe der bisherigen Staatsangehörigkeit eine besondere Härte darstellt, wenn das andere Land jemanden nicht aus der Staatsangehörigkeit entlässt, wenn unzumutbare Bedingungen erhoben werden oder erhebliche Nachteile drohen, dann gibt es auch heute schon die Möglichkeit, Mehrstaatigkeit zuzulassen. Es besteht deshalb aus meiner Sicht überhaupt keine Notwendigkeit, die Mehrstaatigkeit auf deutschem Boden generell einzuführen.

## Ich glaube, wir sind bisher gut damit gefahren, die Mehrstaatigkeit nicht generell zuzulassen. In Ausnahmefällen ist dies aber sehr wohl der Fall und möglich.

## Ich glaube, wir sind gut beraten, wenn wir zunächst einmal die Erfahrungen abwarten, die wir mit dem Optionsmodell machen werden. Ich spreche ungern - auch das sage ich ganz offen - von Optionspflicht; denn an sich ist das ja eine zusätzliche Möglichkeit, unter verschiedenen Alternativen zu wählen.

## Sie wissen ganz genau: Wenn man sich bis zum 23. Lebensjahr nicht entscheidet - man muss sich nicht entscheiden -, dann entfällt automatisch die deutsche Staatsangehörigkeit. Auch de jure besteht also keine Optionspflicht, sondern es gibt eine Optionsmöglichkeit.
## Wir haben ein Optionsmodell.


## ...


## Memet Kilic (Gr�ne) speech

## An dieser Stelle möchte ich den Antrag der SPD loben. Darin fordert die SPD grö�Ytenteils die inhaltliche Umsetzung unserer Gesetzentwürfe aus dem Jahr 2010. Unsere Kernforderungen sind: erstens die Abschaffung des Optionszwangs. Es ist integrationspolitischer Unsinn, in Deutschland geborene Jugendliche vor die Zwangswahl zwischen ihren zwei Staatsbürgerschaften zu stellen.
## Zweitens. Einbürgerungsanträge von Rentnern dürfen nicht wegen fehlender Lebensunterhaltssicherung abgelehnt werden.
## Drittens fordern wir die uneingeschränkte Hinnahme der Mehrstaatigkeit. In Deutschland lebt seit Jahrzehnten eine Vielzahl von Menschen ohne Probleme mit zwei Staatsangehörigkeiten. So haben Millionen von Spätaussiedlern die deutsche Staatsangehörigkeit erhalten, ohne dass sie ihre bisherige Staatsangehörigkeit aufgeben mussten.
## Ebenso haben alle EU-Bürgerinnen und EU-Bürger das Recht auf Mehrstaatigkeit. 2010 erfolgten 53,1 Prozent aller Einbürgerungen unter Hinnahme der Mehrstaatigkeit. In vielen europäischen Staaten wird die Mehrstaatigkeit generell hingenommen. Probleme verursacht die Mehrstaatigkeit dort nicht. Lassen Sie uns diese integrationspolitische Katastrophe endlich gemeinsam beenden und die Mehrstaatigkeit uneingeschränkt hinnehmen.


## Ingo Wellenreuther (CDU) speech

## Wieder einmal hat die SPD das Thema Staatsangehörigkeitsrecht auf die Tagesordnung gesetzt. Sie wollen mit Ihrem Antrag die Optionspflicht abschaffen und die mehrfache bzw. doppelte Staatsbürgerschaft, Herr Veit, ermöglichen. Bereits viermal haben wir in den letzten zwei Jahren im Deutschen Bundestag über entsprechende Anträge der Opposition debattiert, zuletzt - es wurde angesprochen - vor genau drei Monaten.
## Jedes Mal haben die Regierungsfraktionen erklärt, am Grundsatz, mehrfache Staatsangehörigkeiten prinzipiell zu vermeiden, festzuhalten. Dieser Grundsatz ist völkerrechtlich anerkannt und prägt das deutsche Staatsangehörigkeitsrecht. Sie kennen unsere überzeugenden Argumente dazu.

## Vollkommen kontraproduktiv ist daher auch das, was die neue grün-rote Landesregierung in meinem Heimatland Baden-Württemberg gerade vollzogen hat. Durch �"nderungen bei der Ausführung des Staatsangehörigkeitsrechts sollen künftig insbesondere mehr Fälle der Mehrstaatigkeit hingenommen und Abstriche beim Erfordernis der Deutschkenntnisse gemacht werden.

## Auch zum zweiten Punkt, der von der Opposition geplanten Abschaffung der Optionspflicht, kennen Sie aus den Debatten unsere klare Haltung. In der Koalitionsvereinbarung ist vorgesehen, die Erfahrungen mit einer nennenswerten Zahl der ersten Optionsfälle auszuwerten und einen möglichen Verbesserungsbedarf zu prüfen.

## Die Evaluierung betrifft zum anderen die Evaluierung der Ma�Ynahme selbst. Hier werden erstens die von den Ländern zum 31. Januar dieses Jahres zur Verfügung gestellten Zahlen über das Entscheidungsverhalten der Optionspflichtigen ausgewertet. Auch wenn diese Auswertung gerade erst begonnen hat, zeichnet sich bisher die Tendenz ab - Herr Veit, Sie wissen das wahrscheinlich -, dass sich 95 Prozent der Optionspflichtigen, die sich bisher gemeldet haben, für die deutsche Staatsbürgerschaft entschieden haben.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Helmut Brandt 2014-06-05 CDU

p <- partition("GERMAPARL", speaker = "Helmut Brandt", date = "2014-06-05", encoding = "UTF-8")

read(p)

## Helmut Brandt (CDU)

## Ausdruck dieser �oberzeugung waren unter anderem internationale Verträge zur Vermeidung doppelter Staatsangehörigkeit. Dafür gab und gibt es gute Gründe. Aber wir leben in einer globalisierten, mobilen Welt, und der Doppelpass wird weltweit zunehmend zur Realität. Deutschland hat sich sukzessive zu einem Einwanderungsland entwickelt mit einem heute bestehenden hohen Bedarf an Fachkräften.

## Abgeordnete von der Linken und vom Bündnis 90/Die Grünen behaupten immer wieder, die Optionspflicht sei integrationsfeindlich. Woher das genommen wird, erschlie�Yt sich mir, offen gesagt, nicht. Der Wert eines Gutes steigt bekanntlich nicht, wenn es leichter zu erwerben ist.

## Hinter unserer bisherigen Skepsis gegenüber der doppelten Staatsangehörigkeit stand - das gebe ich offen zu - selbstverständlich auch die Frage, ob wir im Gegenzug zur Staatsangehörigkeit auf die Loyalität der Doppelstaatler zählen können. Schlie�Ylich reden wir hier über die deutsche Staatsangehörigkeit und nicht über eine Parkerlaubnis, wie der Kollege Strobl in der letzten Debatte über dieses Thema so markant sagte. Staatsangehörigkeit umfasst ein Bündel an Pflichten und Rechten, darunter das Wahlrecht und den Zugang zu öffentlichen �"mtern bis hin zum Beamtentum. Das ist übrigens ein Punkt, den ich für äu�Yerst wichtig halte. Unser Bestreben muss sein, mehr Menschen mit Migrationshintergrund in das Beamtentum zu bekommen.

## Ich will ein weiteres Beispiel dafür anführen - das ist schon angeklungen -, welche Probleme die Doppelstaatigkeit mit sich bringt. Der Zeit-Chefredakteur Giovanni di Lorenzo hat im Fernsehen offen bekundet, dass er bei der Europawahl sowohl in Deutschland als auch in seinem Konsulat gewählt hat.

## Wie ich eingangs bereits sagte, leben wir in einer globalisierten Welt, und der Doppelpass wird zunehmend selbstverständlich. Die Bundesregierung hat nun einen Gesetzentwurf vorgelegt, der junge Menschen nicht mehr in die für sie - jedenfalls teilweise - offensichtlich unangenehme Situation bringt, sich zwischen zwei Staatsbürgerschaften entscheiden zu müssen, wenn sie hier geboren und aufgewachsen sind. Die Entscheidung zwischen der deutschen Staatsangehörigkeit und der des Herkunftslandes der Eltern, ist - zumindest zum Teil - ein Problem für diese jungen Migranten, die hier geboren sind und hier leben wollen. Diese Gruppe wollen wir durch diese Neuregelung entlasten.

## Wenn wir nun die hier lebenden jungen Migranten, die hier geboren sind und hier leben wollen, entlasten können, indem wir die Optionspflicht durch eine praktikable Neuregelung ihrer Lebenswirklichkeit anpassen, dann sollten wir dies tun.

## Aber mit diesem Beispiel wollte ich nur deutlich machen, dass Doppelstaatigkeit auch Probleme mit sich bringt
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Das hat doch mit Doppelstaatigkeit nichts zu tun! Das hat mit dem Woanderswohnen zu tun! )


## Ulla Jelpke (LINKE) speech

## Die Linke hat in eigenen Anträgen schon zu Beginn dieser Legislaturperiode die Anforderungen an ein - modernes und fortschrittliches Staatsangehörigkeitsrecht genannt. Dazu gehört in erster Linie: Mehrstaatlichkeit muss bei Einbürgerung und Geburt in Deutschland generell hingenommen werden. Hier noch einmal ganz deutlich gesagt: Nicht nur in anderen EU-Staaten, sondern auch in den USA, Israel sowie in vielen anderen Ländern dieser Welt ist es eine Selbstverständlichkeit, dass ein Mensch die Staatsbürgerschaft des Landes erhält, in dem er geboren wurde. Er muss sich nicht verbiegen und - irgendwelche Schul - und Ausbildungsabschlüsse nachweisen, wie es bei uns der Fall ist. Das kann doch wohl nicht sein.


## Eva H�gl (SPD) speech

## Mit diesem Gesetz zur Staatsangehörigkeit verändern wir unsere Gesellschaft; das ist uns sehr wichtig. Wir machen also einen gro�Yen Schritt nach vorne.
## Nach vielen Jahren gesellschaftlicher Diskussion - wir haben um das Für und Wider gerungen - legen wir nun den Entwurf eines Gesetzes zur Aufhebung der - Optionspflicht vor. Es ist ein wirklicher Erfolg, dass uns das gelingt.
## Das stellt eine wichtige Verbesserung für viele junge Menschen mit Migrationshintergrund in unserer Gesellschaft dar. Wir tragen dazu bei, dass unser Staatsange-hörigkeitsrecht weiter modernisiert wird. Das ist ein wichtiges Signal und eine wichtige Reform.

## Aber wir haben uns darauf verständigt, und deswegen ist der Gesetzentwurf über die Abschaffung der Optionspflicht, über den wir heute beraten, ein erster wichtiger Schritt. Es ist ein guter Vorschlag, über den wir beraten.
## Ich möchte ganz kurz zurückblicken - ich will das nicht lange ausführen; es ist schon gesagt worden, woher die Optionspflicht kommt - und daran erinnern, dass das, was uns alle geschmerzt hat, ist, dass mit diesem Optionszwang junge Menschen zu Deutschen auf Probe wurden. Das ist etwas, was wir nicht akzeptieren und was nicht sein darf. Deswegen schaffen wir den Optionszwang ab. Niemand, der oder die hier in Deutschland geboren ist, ist bei uns Deutscher oder Deutsche auf Probe. Das ist ein wichtiges Signal.
## ( Beifall bei der SPD sowie des Abg. Stephan Mayer [ Altötting ] [ CDU/CSU ] - Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Dann müssen Sie den Optionszwang abschaffen! )
## Ich erinnere die Grünen daran, dass der Optionszwang nicht einfach so in das Gesetz gekommen ist.
## Wir haben unter Rot-Grün gemeinsam das Staatsangehörigkeitsrecht 1999 reformiert. Das war ein ganz gro�Yer Schritt weg vom Abstammungsprinzip hin zum Prinzip des Geburtsortes. Wir machen jetzt, 15 Jahre danach, den nächsten Schritt mit der Abschaffung der Optionspflicht, die uns schon immer geschmerzt hat.
## ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Sie schaffen nicht ab! )
## Das Optionsmodell war im �obrigen auch ein Integrationshemmnis in unserer Gesellschaft; denn wenn wir jungen Leuten sagen, sie seien Deutsche auf Probe, dann sind sie auch auf Probe in unserer Gesellschaft.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Helmut Brandt 2014-01-16 CDU

p <- partition("GERMAPARL", speaker = "Helmut Brandt", date = "2014-01-16", encoding = "UTF-8")

read(p)

## Es ist noch gar nicht lange her, da haben wir hier im Deutschen Bundestag vor der letzten Wahl auf Antrag von Bündnis 90/Die Grünen und der Linken über dieses Thema, über die Abschaffung des Optionszwangs, gesprochen. Herr Beck, wenn Sie mit Ihren Anträgen auch nur ein paar Wochen gewartet hätten, dann hätten Sie den Gesetzentwurf der Regierung gesehen und ihm hoffentlich mit Freude zugestimmt.
## Weil Sie es nicht richtig geschildert haben und weil Sie das, was seinerzeit gemacht worden ist, als Unsinn bezeichnet haben - was ich zurückweise; es war schon sehr sinnvoll -, will ich die Rechtslage noch einmal verdeutlichen.
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Warum schaffen Sie es denn ab, wenn es sinnvoll war? )

## Voraussetzung für den seit dem Jahr 2000 geltenden Jus-Soli-Erwerb war und ist, dass mindestens ein Elternteil seit acht Jahren rechtmä�Yig seinen gewünschten Aufenthalt im Inland hat und über ein befristetes Aufenthaltsrecht verfügt.
## Diese Kinder müssen sich nach Vollendung des 18. Lebensjahres bis zum 23. Lebensjahr für eine der beiden Staatsbürgerschaften entscheiden, also entweder bei der deutschen verbleiben oder die Staatsbürgerschaft, die sie durch einen der beiden Elternteile erworben haben, beibehalten.
## Seit 2000 waren davon immerhin 450 000 Kinder betroffen und sind auf diesem Wege deutsche Staatsangehörige geworden. Das ist eine beachtliche Zahl. Für die ersten dieser Kinder, die im Jahre 2008 18 Jahre alt wurden, ist die Optionsphase im vergangenen Jahr abgelaufen.
## Jetzt ist es interessant, zu sehen, wie sie sich entschieden haben. Weil sich die meisten, nämlich 98 Prozent, für die deutsche Staatsbürgerschaft entschieden haben, muss ich den Vorwurf des Unsinns zurückweisen.
## Für mich ist das ein Beweis dafür, dass diese Optionspflicht, die damals eingeführt worden ist, durchaus Sinn gemacht hat und nach meiner persönlichen Auffassung auch heute noch macht. Denn die Entscheidung für eine der beiden Staatsbürgerschaften als klares Bekenntnis zu einem Land halte ich nach wie vor für einen Menschen, der schon 18 bis 23 Jahre lang hier gelebt hat, für durchaus zumutbar.
## ( Beifall bei Abgeordneten der CDU/CSU )
## Aber es gibt noch weitere gute Gründe für diese Optionspflicht. ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Sie haben das Optionsmodell nicht verstanden! )


## �zcan Mutlu (Gr�ne) asking Brandt (CDU)

## Sie haben gerade die Punkte Loyalitätskonflikt und Strafverfolgung angesprochen. Ist Ihnen bekannt, dass Deutschland mit 53 verschiedenen Ländern dieser Erde bereits sogenannte Doppelstaatsbürgerschaftsabkommen geschlossen hat? Dabei gibt es keines der Probleme, von denen Sie hier reden. Es gibt niemanden, der sich in einem Loyalitätskonflikt befindet oder der sich der Strafverfolgung entzieht.


## Brandt (CDU) answering

## Das ist durchaus zutreffend, aber es gibt darüber hinaus mehr als 100 weitere Länder, mit denen solch ein Abkommen nicht besteht. Von denen habe ich gerade gesprochen.
## Ich komme zurück zum Loyalitätskonflikt. Ich will einmal, weil die Menschen mit türkischstämmigem Hintergrund hier eine besondere Bedeutung haben, auf die Regierung Erdogan zu sprechen kommen. Sie hat ja bekanntlich eine Behörde ins Leben gerufen, die sich speziell an im Ausland lebende Türken wendet und das Ziel verfolgt, diese im Ausland lebenden Türken für ihre Interessen zu gewinnen.
## Ich meine, dass dies zumindest ein starkes Indiz dafür ist, dass Menschen mit doppelter Staatsbürgerschaft für Ziele vereinnahmt werden, die in unserem Land keine Rolle spielen, sondern nur in der Türkei. Wenn Ministerpräsident Erdogan sagt: " Geschichte und Schicksal mögen uns in unterschiedliche Länder versetzt haben, aber unsere Herzen schlagen immer zusammen ", dann spricht doch diese Aussage für sich.

## Das ist seine Aussage; das ist vollkommen richtig. Aber er übt Einfluss auf die aus, die hier in Deutschland leben.
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Machen Sie sich doch Erdogan nicht zu eigen, auch wenn es Ihre Schwesterpartei ist! )
## Deshalb gibt es gute Gründe, Herr Beck, die Sie bei Ihren Ausführungen natürlich alle verschwiegen haben, das Optionsmodell nicht als Unsinn zu bezeichnen.
## Ich muss im �obrigen auch Ihre Einschätzung zurückweisen, Herr Beck, dass heute über alle politischen Lager hinaus Einigkeit darin besteht, dass sich die Optionspflicht nicht bewährt hat. Das ist nicht richtig. Ich hatte das eben ausgeführt.
## Richtig ist, dass CDU, CSU und SPD im Koalitionsvertrag vereinbart haben, die Optionspflicht abzuschaffen bzw. es dem betroffenen Personenkreis leichter zu ermöglichen, die doppelte Staatsbürgerschaft zu behalten.
## Die Entscheidung zwischen der deutschen Staatsangehörigkeit und der des Herkunftslandes der Eltern oder eines Elternteils ist für junge Migranten, die hier geboren sind und hier leben wollen, natürlich ein Problem. Das sehen wir auch. Aber für uns ist nach wie vor von gro�Yer Bedeutung, dass wir die Integration dieser Gruppe im Blick behalten.

## Es ist einfach eine Tatsache, dass in dieser Gruppe ein hoher Prozentsatz - doppelt so hoch wie der Durchschnitt - keinen Schulabschluss macht und später auch keine Berufsausbildung aufnimmt. All das halten wir für nicht akzeptabel. Herr Beck, Sie können es drehen, wie Sie wollen: Wir halten den Druck, den wir ausüben wollen, damit sich die Menschen in Deutschland wirklich integrieren und sich den Möglichkeiten öffnen, die unser Staat bietet, für wichtig. Unser Modell " Integration geht vor Staatsangehörigkeit " halte ich nach wie vor für richtig.


## ...


## Brandt (CDU) aswering

## Wir halten uns strikt an das, was im Koalitionsvertrag vereinbart worden ist, und werden das auch umsetzen.


## Pau (Linke) speech

## Ich teile auch die Kritik von Kenan Kolat, dem Vorsitzenden der Türkischen Gemeinde in Deutschland; denn herausgekommen ist keine gro�Ye europäische Lösung, sondern eine kleine deutsche Geste, und die spaltet erneut.
## Ja, ich erkenne an: Der Optionszwang soll fallen. Hier geborene junge Menschen sollen nicht mehr entscheiden müssen, ob sie Deutsche oder beispielsweise Türken sind. Aber �"ltere oder neu Eingewanderte stehen weiter vor der Qual der Wahl. Sie dürfen nicht einfach Mensch sein; über sie entscheidet weiter der Pass.

## Ich kenne im �obrigen keine triftigen Gründe gegen eine doppelte Staatsbürgerschaft. In zahlreichen EU-Staaten ist eine doppelte Staatsbürgerschaft längst Usus und obendrein ein Erfolgsmodell; in Deutschland nicht. Es ist wie bei der direkten Demokratie: Auch im Staatsbürgerschaftsrecht ist Deutschland nicht etwa spitze, sondern ein EU-Entwicklungsland. Ich finde, das ist blamabel.
## Nun ist selbst die Abschaffung des unsäglichen Optionszwangs bislang lediglich eine pure Ankündigung der Gro�Yen Koalition. Bündnis 90/Die Grünen fordern mit ihrem Antrag ein schnelleres Handeln, und das unterstützen wir natürlich. Aber es bleibt die kleine Lösung auf Koalitionsniveau. Wir als Linke drängen weiter auf weitergehende �"nderungen.

## Wir wollen, dass das deutsche Staatsbürgerschaftsrecht grundlegend modernisiert wird und Einbürgerungen unbürokratisch erleichtert werden. Wir möchten, dass der Pass der Pass bleibt und dass der Mensch - jetzt sind wir bei Ihrem Widerspruch - auch Bürger sein kann, anerkannt und gleichberechtigt. Dazu gehört, dass Bürgerinnen und Bürger, die seit Jahren hier leben, auch ohne deutschen Pass mitbestimmen und wählen können.
## Sie dürfen es bislang nicht, und so bleiben sie Bürger zweiter Klasse. Das lehne ich ab, und das will die Linke grundlegend ändern.


## ...


## Frieser (CSU) speech

## Das ist der altbekannte Alarmismus. Zahlen werden in den Raum geworfen. Es ist von 5 000 Menschen die Rede, die ihre Staatsangehörigkeit verlieren. Es handelt sich um ein Optionsmodell, das sich über einen Zeitraum von fünf Jahren erstreckt. Bis zum Ablauf dieses Zeitraums ist definitiv eine Regelung von der Regierung zu erwarten.
## Es hat sehr lange gedauert, bis man sich auf dieses Optionsmodell geeinigt hat. Es war im klassischen Sinne des Wortes ein Kompromiss; verschiedene Positionen mussten sich aufeinander zubewegen. Eines ist deshalb klar: Nun bedarf selbstverständlich auch das Abwägen der Folgen Zeit. Auch das Beseitigen ungewollter Folgen bedarf seiner Zeit. Gründlichkeit ist angesagt. Auch hier gilt: Gründlichkeit geht vor Schnelligkeit. Wir sollten definitiv abwarten.

## Menschen in Deutschland, Autochthone wie Menschen mit einer Zuwanderungsgeschichte, halten dieses Land nach wie vor für ein weltoffenes Land, für ein tolerantes Land, für ein Land, das Zuwanderer, Menschen, die hier leben wollen, willkommen hei�Yt. Trotzdem muss man definitiv sagen dürfen: Die doppelte Staatsangehörigkeit hat nun einmal Nachteile.
## Reden wir doch nicht drum herum: Selbstverständlich kann man die doppelte Staatsangehörigkeit nur bezogen auf die Länder akzeptieren, mit denen wir hochdiffizile, hochkomplexe Doppelstaatsangehörigkeitsverträge abgeschlossen haben, in denen alle Fragen des täglichen Lebens abgeklopft wurden. Das gilt eben nicht für alle Länder.
## Ich muss in diesem Zusammenhang Folgendes sagen: Ein politisches Grundsatzprogramm, nach dem jeder alles darf - egal wie lange er hier ist, egal warum er hier ist, er darf an allen Prozessen teilhaben -, klingt zwar angenehm und offen.
## ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Lesen Sie doch unseren Antrag! Das steht da nicht drin! )
## Es bedeutet aber absolute Beliebigkeit, und Beliebigkeit befördert nicht die Zugehörigkeit. Die deutsche Staatsangehörigkeit ist etwas Besonderes, und sie muss etwas Besonderes bleiben, das zu erwerben sich lohnt.
## Deshalb bleibt es dabei, dass wir versuchen, Mehrstaatigkeit zu vermeiden. Dass das nicht immer geht, ist doch klar.
## Wir mussten erkennen, dass es Menschen zerrei�Yt - das ist eine unangenehme Folge des Optionsmodells -, die eine Zuwanderungsgeschichte haben - die haben viele - und andererseits eine Sozialisierung in diesem Land erlebt haben, die es ihnen möglich macht, auch zu diesem Land eine emotionale Verbindung aufzubauen.
## Genau das haben wir im Koalitionsvertrag geregelt, nämlich dass es eine Mehrstaatigkeit für die Menschen gibt, die hier in diesem Land sozialisiert werden, die hier aufwachsen und definitiv hier in der Schule ihre Sozialisierung erleben. Das ist genau das, was wir tatsächlich wollten.
## Jetzt den Vorwurf zu machen, man habe sein Wort gebrochen, ist unangebracht. Darum geht es doch überhaupt nicht.
## Es geht darum, dass man an dieser Stelle deutlich sagt: Die Auswirkungen des Optionsmodells, die wir alle in dieser Härte nicht wollten, können beseitigt werden.

## Verwässern wir jetzt nicht das Signal! Das Signal muss hei�Yen: Menschen, die durch ihre Familie eine Zuwanderungsgeschichte haben, sollen sich zu diesem Land zugehörig fühlen, sich hier willkommen und beheimatet fühlen. Das sind sie, wenn sie hier tatsächlich aufgewachsen sind. Diese Menschen wollen wir nicht vor diese Zwangsentscheidung stellen. Das ist die Grundlage eines modernen Staatsangehörigkeitsrechts.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[5]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Thomas Strobl 2014-03-12 CDU

p <- partition("GERMAPARL", speaker = "Thomas Strobl", date = "2014-03-12", encoding = "UTF-8")

read(p)

## Thomas Strobl (CDU)

## Selin ist in Deutschland, in der Nähe von Stuttgart, geboren. Ihre Mutter ist Türkin. Auch der Vater ist türkischer Staatsangehöriger. Sie ist nicht nur hier geboren. Als sie in die Schule gekommen ist, konnte sie schon ein bisschen rechnen. Sie ist zweisprachig aufgewachsen. Selin ist ein intelligentes, flei�Yiges Mädchen, gut vorankommend in der Schule. Deswegen geht sie auf ein baden-württembergisches Gymnasium. Dort macht sie - Abitur. Sie möchte in Deutschland bleiben und Physik studieren. Wir haben mit den Sozialdemokraten vereinbart, dass wir sie nicht vor die Frage stellen wollen, ob sie sich für die türkische - weil natürlich ihre Eltern aus der Türkei kommen und ihre Gro�Yeltern dort noch leben - oder für die deutsche Staatsbürgerschaft entscheiden will, sondern wir haben gesagt:
## In diesem Fall - akzeptieren wir die deutsche Staatsbürgerschaft als Doppelstaatsbürgerschaft, damit Selin in Deutschland auch wählen kann und möglicherweise eines Tages Bundeskanzlerin wird.
## Das ist unsere Vereinbarung.
## Jetzt gibt es aber leider nicht nur Selin, sondern es gibt auch Abida.
## �ober diesen Fall sind von türkischen Frauen ganze Bücher geschrieben worden. Es ist nämlich so, dass Abida in Deutschland geboren wird und kurz nach ihrer Geburt in die Türkei verbracht wird; denn der Vater möchte nicht, dass sie in dieser dekadenten verweltlichten Republik aufwächst. Sie kommt ganz bewusst zu den Gro�Yeltern nach Anatolien, geht dort auf eine Koranschule. Mit 15 Jahren heiratet sie einen Mann, den sie vorher noch nie gesehen hat. Sie spricht kein Deutsch, sie hat Deutschland nie gesehen, sie hat mit Deutschland null Komma null Identifikation. Das möchte ihre Familie so.
## ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Ist das jetzt eine Märchenstunde oder was? )
## Ich möchte das gar nicht bewerten; aber klar ist - jedenfalls für die Unionsfraktion -: Das ist nicht das, was wir uns unter einer gelungenen Integration vorstellen.
## Jedenfalls wollen wir solche Fälle nicht auch noch mit einer deutschen Staatsbürgerschaft honorieren.
## Was wollen wir, und was haben wir mit den Sozialdemokraten in den Koalitionsverhandlungen vereinbart? Wir haben gesagt: Wenn jemand mit ausländischen Eltern hier geboren und aufgewachsen ist, akzeptieren wir die Doppelstaatsbürgerschaft. - Der Kollege Beck hat hier auf die Länderinteressen verwiesen. Ich habe auch vonseiten der Länder den Vorwurf gehört, dass da eine ungeheure Bürokratie aufgebaut werde.

## Wir reden hier nicht über die Verlängerung einer Park-zonenerlaubnis. Es geht um die deutsche Staatsbürgerschaft. Es geht um die Frage: Wie definieren wir unser Staatsvolk? Es geht um die Frage: Wer ist hier Bürgerin, wer ist hier Bürger? Es geht um die Frage: Wer ist diesem Land lebenslang mit Rechten und Pflichten verbunden? Es geht nicht zuletzt um die Frage: Wer ist hier wahlberechtigt? Wer kann hier Bundeskanzlerin oder Bundeskanzler wählen?


## ...


## Zertik (CDU) speech

## Wir sprechen heute über die Staatsangehörigkeit. Ich selber bin in Kasachstan geboren und mit meiner Familie Ende der 80er-Jahre nach Deutschland gekommen. Wir haben darum gekämpft, hierherzukommen. Es war schwierig und mühselig, die nötigen Papiere für zahlreiche Anträge zusammenzustellen und unsere Ausreise voranzutreiben. Warum haben wir es getan? Weil wir aus voller �oberzeugung in Deutschland leben wollten. Für uns war es keine Frage, ob wir unsere alte Staatsbürgerschaft behalten oder nicht. Uns war klar, dass wir die deutsche Staatsbürgerschaft haben wollten, nicht nur um alle Rechte zu erlangen, sondern auch bewusst Pflichten als deutsche Staatsbürger zu übernehmen.

## Es geht um die Identifikation, um die Identifikation mit Deutschland, mit unserer Kultur und unserer Geschichte. Es geht um die Identifikation mit unseren Grundwerten Demokratie und Freiheit.
## Vielen ausländischen Mitbürgern ist das bewusst. Das belegen auch Zahlen einer Einbürgerungsstudie, die das Bundesamt für Migration und Flüchtlinge zur Optionsregelung im Jahr 2011 erstellt hat.
## Demnach schaffen soziale und berufliche Einbettung starke alltagspraktische Bindungen. Dies gilt auch für die privaten und beruflichen Zukunftsplanungen, die sich bei den befragten - Optionspflichtigen überwiegend auf Deutschland richten.
## Knapp 90 Prozent der Optionspflichtigen, die befragt wurden, haben sich für den deutschen Pass ausgesprochen, weil sie hier ihren Lebensmittelpunkt haben, weil sie die Rechte eines deutschen Staatsbürgers behalten wollen, weil sie auch die Vorteile nutzen möchten, als EU-Bürger zu reisen, zu leben und zu arbeiten.
## Liebe Kolleginnen und Kollegen, Optionszwang hin oder her - das ist eine Formalität. In Deutschland kann jeder, der sich mit diesem Land und seinen Werten identifiziert, der die Sprache spricht und für seinen Lebensunterhalt sorgen kann, hier eingebürgert werden und einen deutschen Pass erhalten.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[6]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Stephan Mayer 2016-09-23 CSU

p <- partition("GERMAPARL", speaker = "Stephan Mayer", date = "2016-09-23", encoding = "UTF-8")

read(p)

## Stephan Mayer (CSU)

## Man könnte es sich leicht machen oder lapidar sagen: " olle Kamellen " oder " alter Wein in neuen Schläuchen ". Die Grünen legen einen Gesetzentwurf zur Liberalisierung des Staatsangehörigkeitsrechts vor und sprechen sich für die generelle Anerkennung der Mehrstaatigkeit aus. 
## Aber, meine sehr verehrten Kolleginnen und Kollegen, so leicht möchte ich es mir nicht machen, weil aus meiner Sicht insbesondere dieser Gesetzentwurf, den Sie heute in erster Lesung vorlegen, auf eine parteipolitische, aber auch eine gesellschaftspolitische Geisterfahrt führt. Es geht nicht nur um Detailregelungen, die Sie im Staatsangehörigkeitsrecht ändern wollen, sondern - dieser festen �oberzeugung bin ich - Ihr Ansatz hat eine gesellschaftspolitische Dimension.

## Die Grünen wollen generell den Grundsatz der Mehrstaatigkeit anerkennen und sich insoweit von der bisherigen, bewährten rechtlichen Grundlage abkehren, dass die Mehrstaatigkeit die Ausnahme ist. Sie offenbaren dies sehr verräterisch in Ihrer Begründung, indem Sie ganz dezidiert von einer Einbürgerungsoffensive sprechen. Sie wollen in Deutschland eine Einbürgerungsoffensive vornehmen.
## Ich bin der felsenfesten �oberzeugung: Der überwiegende Teil, der Gro�Yteil der deutschen Bevölkerung will dies nicht. Wir brauchen keine Einwanderungsoffensive. Gerade in einer Zeit, in der wir ohnehin in schwierigem Fahrwasser sind, in der sich unsere Gesellschaft ohnehin eher auseinanderdividiert, in der wir eine Polarisierung unserer Gesellschaft erleben, ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Die Sie betreiben! - Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Scheuer und AfD, das ist die Polarisierung! ) in der die Zentrifugalkräfte, die Fliehkräfte in unserer Gesellschaft zunehmen, wäre es genau kontraproduktiv, wenn wir jetzt, wie von Ihnen intendiert und gefordert, eine Einwanderungsoffensive vornähmen. 

## Meine sehr verehrten Kolleginnen und Kollegen, wir haben in dieser Legislaturperiode unser Staatsangehörigkeitsrecht bereits grundlegend geändert. Ich mache keinen Hehl daraus: Es war nicht der Wunsch der CDU/CSU, in Teilen auf das Optionsmodell zu verzichten. Wir haben aber aus meiner Sicht einen verträglichen Kompromiss dahin gehend gefunden, dass das Optionsmodell nur in den Fällen obsolet ist und nicht mehr angewandt wird, in denen konkrete Nachweise erbracht sind, dass eine Person in Deutschland Fu�Y gefasst hat, ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Das war doch nur eine absolute Mogelpackung! Da hat die SPD mitgemacht! ) wenn sie sich also mindestens acht Jahre in Deutschland aufgehalten hat, wenn sie sechs Jahre in Deutschland zur Schule gegangen ist oder wenn sie in Deutschland einen erfolgreichen Schulabschluss oder einen erfolgreichen Berufsschulabschluss vorweisen kann.
## Das sind ganz konkrete Indizien dafür, dass jemand in Deutschland angekommen ist und sich in die deutsche Gesellschaft erfolgreich integriert hat, sodass aus meiner Sicht unter diesen Voraussetzungen auf das Optionsmodell verzichtet werden kann.
## Weiter gehende Wünsche im Hinblick auf eine Liberalisierung werden wir auf jeden Fall nicht mittragen. ( Beifall bei der CDU/CSU ) Die angesprochenen Loyalitätskonflikte gibt es natürlich.

## Das war natürlich, meine sehr verehrten Damen und Herren, von langer Hand geplant. Da sieht man doch genau an diesem konkreten Fall, wie es dann bei Anerkennung der doppelten Staatsbürgerschaft zu Loyalitätskonflikten kommen kann. Ich möchte auf einen weiteren konkreten Fall der Praxis zu sprechen kommen, bei dem sich ebenfalls Loyalitätskonflikte zeigen könnten - wohlgemerkt: könnten -, der aber noch nicht endgültig ausermittelt ist. Seit dem Putschversuch in der Türkei im Juli dieses Jahres befinden sich sechs deutsche Staatsangehörige in der Türkei in Haft.

## Es ist noch nicht klar, ob diese neben der deutschen die türkische Staatsangehörigkeit haben. Ich nehme an, dass es dem Erdogan-Regime ziemlich egal ist, ob die Betreffenden, wenn sie die türkische Staatsangehörigkeit haben, auch die deutsche Staatsangehörigkeit besitzen; denn allein das Vorhandensein der entsprechenden Staatsangehörigkeit reicht für viele Staaten auf der Welt aus - so auch die Türkei -, die betreffenden Personen als ihre Staatsangehörigen zu behandeln. Diesen Personen bringt es in einem Konfliktfall überhaupt nichts, auch die deutsche Staatsangehörigkeit zu besitzen.
## Es gibt überhaupt keinen Grund, nun einer weiteren Liberalisierung des Staatsangehörigkeitsrechts näherzutreten. Wir fordern in Teilen sogar eine Verschärfung, insbesondere wenn es darum geht, potenziellen IS-Kämpfern oder Kämpfern, die sich in Kampfhandlungen des Dschihad engagieren, die deutsche Staatsangehörigkeit zu entziehen, sofern sie über eine weitere Staatsangehörigkeit verfügen.


## UNDEF (probably Ulla Jelpke) (Linke) speech

## Herr Mayer, die allermeisten in Deutschland lebenden Migrantinnen und Migranten sind loyaler gegenüber dieser Gesellschaft und dem Grundgesetz ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Als die Bayern! ) als der Nazimob, der Flüchtlinge durch die Stra�Yen jagt und Unterkünfte in Brand steckt, oder CSU-Politiker, die über ministrierende, fu�Yballspielende Senegalesen in unserer Gesellschaft schwadronieren. Das ist nicht loyal gegenüber unserer Gesellschaft, Herr Kollege Mayer.
## ( Beifall bei der LINKEN und dem B�oNDNIS 90/DIE GR�oNEN - �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Die spalten die Gesellschaft! - Stephan Mayer [ Altötting ] [ CDU/CSU ]: Entschuldigen Sie sich dafür! ) Zur Redlichkeit gehört, zum Thema zu sprechen. Warum sprechen Sie, wenn mein Kollege Volker Beck von Einbürgerungsoffensive spricht, von Einwanderungsoffensive? Sie bauen hier einen Pappkameraden auf, um Stimmung gegen das Thema Einbürgerung zu machen.

## Wer auf Dauer in Deutschland lebt, soll auch gleichberechtigt am politischen Leben teilhaben können und darf im Berufsleben nicht benachteiligt werden. Hier lebende Migrantinnen und Migranten dürfen nicht länger Bürgerinnen und Bürger zweiter Klasse sein, egal seit wann sie hier leben und arbeiten. Wer hier lebt und arbeitet, wer hier zur Schule geht oder gegangen ist, eine Ausbildung gemacht oder eine Universität besucht hat, aber keinen deutschen Pass hat, darf beispielsweise nicht verbeamtet werden oder ein Schöffenamt übernehmen.
## Das sind nur zwei Diskriminierungsbeispiele dafür, warum die erleichterte Einbürgerung längst überfällig ist. Die Integrationsbeauftragte Ihrer Bundesregierung, Aydan UNDEF, schätzt, dass fast drei Viertel der 7,6 Millionen Ausländer einen deutschen Pass beantragen können. Allein, das ist nicht gewollt, und die Hürden werden bewusst hoch gelegt, etwa indem unsinnigerweise gefordert wird, je nach Herkunft die Herkunftsstaatsangehörigkeit abzugeben. Die Einbürgerungsquote in Deutschland liegt unter dem Durchschnitt der Europäischen Union. Wenn Sie dieses Land europäisieren wollen, dann müssen Sie auch die Einbürgerung erleichtern.


## R�diger Veit (SPD) speech

## Augenblick, ich komme jetzt zu eurem Antrag; hier grenze ich mich deutlich von Stephan Mayer ab, der für die Union gesprochen hat -, ( Beifall bei der SPD und dem B�oNDNIS 90/DIE GR�oNEN - Stephan Mayer [ Altötting ] [ CDU/CSU ]: Gott sei Dank! ) wenn wir insgesamt im Staatsbürgerschaftsrecht weitere Erleichterungen generell vornehmen würden, so wie es euer Gesetzentwurf, wie ich finde, richtigerweise an sehr vielen Stellen vorschlägt: genereller Verzicht auf das Verbot von Mehrstaatlichkeit, Verkürzung des Voraufenthaltes, Anrechnung von Voraufenthaltszeiten, Erleichterungen für Junge und Alte im Bereich der Notwendigkeit, den Lebensunterhalt zu bestreiten, und dergleichen Dinge mehr.


## Ostermann (CDU) speech

## In dem Gesetzentwurf und in dem Antrag der Grünen werden im Wesentlichen drei Dinge gefordert: erstens eine generelle Ermöglichung der Mehrstaatlichkeit, zweitens ein Aufweichen der Einbürgerungsregeln und drittens eine besonders unkomplizierte Einbürgerung hier ansässiger Briten, weil es die ja derzeit angeblich nicht geben würde. Zu diesen drei Forderungen möchte ich in meinem Debattenbeitrag Stellung nehmen. Zunächst zur Mehrstaatlichkeit: Es wird Sie nicht überraschen - Stephan Mayer hat das auch schon zum Ausdruck gebracht -, dass die CDU/CSU-Bundestagsfraktion nach wie vor ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Im 20. Jahrhundert ist! ) es konsequent ablehnt, die doppelte Staatsbürgerschaft zum Regelfall zu machen.
## Thema Staatsangehörigkeitsrecht geführt haben. In dieser Debatte hat die damalige Kollegin Christina Kampmann - sie ist mittlerweile Landesministerin in NRW - gesagt, zugegebenerma�Yen mit einer anderen Intention: Für die meisten Menschen ist die Staatsangehörigkeit viel mehr als ein Pass. - Genau das ist der Punkt. Die Staatsangehörigkeit drückt die Loyalität zur Gesellschaft und den in ihr vorhandenen Werten und Regeln aus. Damit ist sie Ausdruck einer ganz besonderen Verbundenheit. Und diese Verbundenheit ist für uns als Union nicht teilbar.
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Was machen wir mit den AfDlern? Wollen Sie die ausbürgern? ) Die Folge ist, dass wir Mehrstaatlichkeit zulassen, aber eben nur in eng begrenzten Ausnahmefällen, die auch jetzt schon geregelt sind. Für eine Abkehr von diesem Prinzip stehen wir nicht zur Verfügung. 
## Was die Aufweichung der Einbürgerungsregeln angeht, sind es vor allem zwei Dinge, mit denen Sie die Einbürgerung erleichtern wollen. Zum einen wollen Sie sämtliche Arten an Aufenthaltserlaubnissen gleichstellen. Das soll zum Beispiel auch für Fälle von vollziehbar Ausreisepflichtigen gelten, nachdem es also ein Verwaltungsverfahren gab, das BAMF festgestellt hat, dass es hier keinen Aufenthaltsstatus gibt, und Gerichte das meistens auch bestätigt haben. Selbst in den Fällen, wo es nur aus humanitären Gründen eine Aufenthaltserlaubnis gibt, soll eine Gleichstellung erfolgen.
## So etwas ist mit uns nicht zu machen. Sie sprachen die Einbürgerungstests an. Unter bestimmten Voraussetzungen soll das Erfordernis, einen solchen Test durchzuführen, wegfallen, etwa wenn in Deutschland ein Berufs - oder Schulabschluss gemacht worden ist.

## Die Problemlage, auf die Sie versuchen hinzuweisen, gibt es einfach nicht. Darum sagen wir: Man muss nicht schon jetzt auf hypothetische Folgen eines in der Zukunft liegenden ungewissen Ereignisses reagieren. Blo�Yen Aktionismus halten zumindest wir in der Union selten für ein Erfolgsrezept. Liebe Kolleginnen und Kollegen von den Grünen, Sie wollen mit Ihrem Gesetzentwurf das Prinzip der Mehrstaatlichkeit in unserer Rechtsordnung verankern und die Voraussetzungen für den Erhalt der deutschen Staatsbürgerschaft verbessern. Gleichzeitig greifen Sie mit Ihrem Antrag ein Problem auf, das es überhaupt nicht gibt. Daher wird es Sie nicht überraschen, dass wir als Union Ihren Antrag und auch Ihren Gesetzentwurf ablehnen werden.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[7]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Stephan Mayer 2014-07-03 CSU

p <- partition("GERMAPARL", speaker = "Stephan Mayer", date = "2014-07-03", encoding = "UTF-8")

read(p)

## Stephan Mayer (CSU)

## Es bleibt beim Grundsatz der Optionspflicht. Es bleibt auch beim richtigen Grundsatz der Vermeidung der doppelten Staatsangehörigkeit. Es wird allen Unkenrufen zum Trotz auch in Zukunft in Deutschland keinen generellen Doppelpass geben.

## Ich bin der festen �oberzeugung: Die mit diesem Gesetz vorgelegten �"nderungen, in denen die Bedingungen dafür genannt werden, wie man von der Optionspflicht ausgenommen werden kann, sind aus meiner Sicht mehr als ein Indiz dafür, dass die betreffenden Personen in Deutschland integriert sind.
## Wenn jemand mindestens acht Jahre in Deutschland lebt, wenn jemand mindestens sechs Jahre in Deutschland die Schule besucht hat, wenn jemand in Deutschland erfolgreich die Schule oder eine Berufsausbildung absolviert hat, dann sind das ganz klare Hinweise darauf, dass diese Person in Deutschland angekommen ist, dass sie in Deutschland beheimatet ist und dass sie in Deutschland integriert ist.
## Mit diesem Gesetz machen wir guten Gewissens deutlich, dass wir den Koalitionsvertrag in seinem eigentlichen Sinn umsetzen: Wir werden die Optionspflicht für die Personen, die in Deutschland geboren und aufgewachsen sind, abschaffen.

## Es ist doch ganz einfach: Wenn jemand in Deutschland erfolgreich die Schule abgeschlossen hat, dann braucht er nur das Schulabschlusszeugnis an die Ausländerbehörde zu schicken. Damit wird er von der Optionspflicht befreit und hört nie mehr etwas vom Staat.
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Sie haben das Gesetz nicht gelesen! Erst einmal prüft das Ausländeramt, ob die Meldebescheinigung vorliegt! Ein Teil der Meldedaten ist gar nicht vorhanden, wenn die Leute hierher gezogen sind! )
## Wenn jemand erfolgreich seine Berufsausbildung abgeschlossen hat, muss er nur sein Abschlusszeugnis an die Ausländerbehörde schicken, und er hört von den Ausländerbehörden nie mehr etwas.


## ...


## Brandt (CDU) speech

## Natürlich hätte man heute noch einmal sehr lange über diesen Gesetzentwurf diskutieren können, Herr Kollege Veit. Aber wir haben im letzten Jahr über die Frage der Staatsangehörigkeit und über die Frage der Optionspflicht - ja oder nein? - oft diskutiert. Jetzt debattieren wir schon zum fünften Mal darüber. Deshalb halte ich es für angemessen, dass wir die Debatte heute abschlie�Yen.

## Wir haben mit diesem Kompromiss natürlich nicht alle Erwartungen erfüllen können. Es gab und gibt bis heute vehemente Befürworter einer kompletten Abschaffung der Optionspflicht. Aber die Anhörung der Sachverständigen hat deutlich gezeigt, dass die geplante - Modifizierung der Optionspflicht ausgewogen und praktikabel ist, dass sie den verfassungsrechtlichen Vorgaben genügt und vor allen Dingen auch sachgerecht ist. Sie verstö�Yt gerade nicht, wie von den Linken und vom Bündnis 90/Die Grünen immer wieder behauptet, gegen das Grundgesetz.

## Die jungen Erwachsenen, die nach dieser neuen Regelung von der Optionspflicht betroffen sein werden, haben es selbst in der Hand, ob sie sich für die deutsche Staatsangehörigkeit oder für die ihrer Eltern entscheiden, auch wenn diese Entscheidung in dem einen oder anderen Fall vielleicht eine unbequeme Entscheidung ist - unbequem, aber durchaus zumutbar.

## Einer Hinnahme des Verlustes der deutschen Staatsangehörigkeit steht das legitime Interesse des deutschen Staates an der Vermeidung von Konflikten rechtlicher, politischer, auch persönlicher Art gegenüber, die vielleicht nicht regelmä�Yig, aber eben doch mit einer doppelten Staatsangehörigkeit verbunden sind. Auch wenn einige das nicht gerne hören oder nicht glauben wollen, ist es nun einmal so, dass eine doppelte Staatsangehörigkeit zu Loyalitätsproblemen führen kann, insbesondere wenn im Heimatland der Eltern ganz andere Wertvorstellungen als in Deutschland vorherrschen. Genau deshalb halte ich die Bedingungen, die wir an den Wegfall der Optionspflicht geknüpft haben, für absolut notwendig und integrationsfördernd.

## Der mit dem Verlust der deutschen Staatsangehörigkeit einhergehende Verlust der Unionsbürgerschaft beeinträchtigt zwar das Recht auf Freizügigkeit.

## Der Verhältnismä�Yigkeit des Verlustes, die der Europäische Gerichtshof verlangt, steht jedoch auch hier gegenüber, dass es der Betreffende selbst in der Hand hat, sich die deutsche Staatsangehörigkeit und damit den Status der Unionsbürgerschaft zu erhalten. Der Europäische Gerichtshof hat zudem explizit festgestellt, dass es legitim ist, dass der Mitgliedstaat das zwischen ihm und seinen Staatsbürgern bestehende Verhältnis besonderer Verbundenheit und Loyalität sowie die Gegenseitigkeit der Rechte und Pflichten schützt.

## Der Kollege Veit hat recht, dass die von der SPD-Fraktion präsentierten sachverständigen Praktiker Anregungen gegeben haben. Wir wollten das nicht in der Kürze der Zeit übers Knie brechen, zumal dadurch vielleicht auch die Möglichkeiten der Zustimmung durch den Bundesrat vergeben worden wären.
## Aber wir sind bereit, darüber in den nächsten Monaten mit den Ländern und auch mit Ihnen zu diskutieren


## debates on dual citizenship between 2012 and 2016

debates_dual2[[8]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Michael Frieser 2015-04-23 CSU

p <- partition("GERMAPARL", speaker = "Michael Frieser", date = "2015-04-23", encoding = "UTF-8")

read(p)

## Michael Frieser (CSU)

## Bei dieser emotional geführten Debatte - das ist etwas, was ich begrü�Yen kann, weil es immerhin
## ( Katrin Göring-Eckardt [ B�oNDNIS 90/DIE GR�oNEN ]: Weil es um Wei�Ywürste geht! )
## um die Wurzeln dieses Staates geht - geht es um etwas sehr Grundsätzliches, nämlich um das Staatsangehörigkeitsrecht. Da darf man auch einmal sehr emotional sein. Wir haben bei der �"nderung des Staatsangehörigkeitsrechts nun wirklich mehrere Handvoll Anträge, Gesetzentwürfe, die wir da hinterherwerfen. Die Frage stellt sich schon: Wie viele Reden von grünen Abgeordneten muss man gehört haben, um eine Einbürgerung in dieses Land zu verdienen?
  
## Da muss man auch Menschen, die es mit diesem Staat, mit dieser Verfassung, mit dieser Demokratie ernst meinen,
## ( Katrin Göring-Eckardt [ B�oNDNIS 90/DIE GR�oNEN ]: Wollen Sie jetzt sagen, wir meinten es mit dieser Verfassung nicht ernst? )
## sagen: Ja, auch diese Leidensfähigkeit gehört dazu, ein Deutscher zu sein und eingebürgert zu werden, auch wenn es ein hartes Stück Brot ist. Deshalb ist die Debatte über das Ius soli im Grunde schon eine geschichtliche Debatte, die wir hier in Deutschland führen, und wir führen sie auch nicht zum ersten Mal.

## Letztendlich bleibt es dabei: Die Einbürgerung ist ein Akt, der am Ende eines erfolgreichen Prozesses steht, eines Prozesses, der mit dem Bekenntnis zu diesem Staat, zu seiner Gesellschaft und zu seinen Zielen und Grundwerten beginnt. Dieser Akt kann nicht am Anfang stehen.
## Das bedeutet auch, dass man deutlich sagen muss: Es ist schwierig, sich in diesen Dingen mit anderen zu vergleichen. Ich kann mich nicht erinnern, dass die Opposition vor allem die Vereinigten Staaten oder Australien einmal herzzerrei�Yend gerne als besonders hoch gehängten Ma�Ystab angenommen hat, wenn es um Flüchtlinge und Einbürgerung ging.

## Ich glaube, den Vergleich mit den Vereinigten Staaten brauchen wir nicht. Wir können gerne in Europa bleiben. Hier sind die Vergleiche nun wirklich eindeutig. Es gibt die unterschiedlichsten Modelle des Abstammungs - und Staatsangehörigkeitsrechts in Europa. Es ist hier möglich, Bulgarien, Dänemark, Finnland, Italien, �-sterreich, Polen, Schweden und das gesamte Baltikum, wo es genau so geregelt ist, wie wir uns das in Deutschland vorstellen, in einem Atemzug zu nennen.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[9]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for CDU/CSU between 2012 and 2016


## Michael Frieser 2015-04-23 CSU

p <- partition("GERMAPARL", speaker = "Michael Frieser", date = "2015-04-23", encoding = "UTF-8")

read(p)

## Michael Frieser (CSU)

## Bei dieser emotional geführten Debatte - das ist etwas, was ich begrü�Yen kann, weil es immerhin
## ( Katrin Göring-Eckardt [ B�oNDNIS 90/DIE GR�oNEN ]: Weil es um Wei�Ywürste geht! )
## um die Wurzeln dieses Staates geht - geht es um etwas sehr Grundsätzliches, nämlich um das Staatsangehörigkeitsrecht. Da darf man auch einmal sehr emotional sein. Wir haben bei der �"nderung des Staatsangehörigkeitsrechts nun wirklich mehrere Handvoll Anträge, Gesetzentwürfe, die wir da hinterherwerfen. Die Frage stellt sich schon: Wie viele Reden von grünen Abgeordneten muss man gehört haben, um eine Einbürgerung in dieses Land zu verdienen?
## Das ist ein harter Stresstest.

## Ja, auch diese Leidensfähigkeit gehört dazu, ein Deutscher zu sein und eingebürgert zu werden, auch wenn es ein hartes Stück Brot ist. Deshalb ist die Debatte über das Ius soli im Grunde schon eine geschichtliche Debatte, die wir hier in Deutschland führen, und wir führen sie auch nicht zum ersten Mal. Da bekommt die Opposition den Preis für Hartnäckigkeit. Aber man muss ehrlich sagen: Sie bekommt auch den Preis für die beste Realitätsverdrängung.

## Letztendlich bleibt es dabei: Die Einbürgerung ist ein Akt, der am Ende eines erfolgreichen Prozesses steht, eines Prozesses, der mit dem Bekenntnis zu diesem Staat, zu seiner Gesellschaft und zu seinen Zielen und Grundwerten beginnt. Dieser Akt kann nicht am Anfang stehen.
## Das bedeutet auch, dass man deutlich sagen muss: Es ist schwierig, sich in diesen Dingen mit anderen zu vergleichen. Ich kann mich nicht erinnern, dass die Opposition vor allem die Vereinigten Staaten oder Australien einmal herzzerrei�Yend gerne als besonders hoch gehängten Ma�Ystab angenommen hat, wenn es um Flüchtlinge und Einbürgerung ging.


## Dagdelem (Sevim UNDEF) (Linke) speech

## Deshalb ist klar: Die Linke unterstützt diesen Gesetzentwurf der Grünen - um das unmissverständlich zu sagen. Wir Linke fordern seit Jahren, die bestehende Dominanz des Blutsrechts, des Ius sanguinis, im deutschen Staatsangehörigkeitsrecht abzuschaffen.
## Wir wollen eben nicht - Herr Beck hat das richtig gesagt -, dass nur diejenigen der hier geborenen Kinder die deutsche Staatsangehörigkeit erhalten, deren Eltern die deutsche Staatsangehörigkeit bereits besitzen.
## Auch die hier geborenen Migrantinnen - und Migrantenkinder sind frei und gleich an Rechten geboren, wie es in der französischen Erklärung der Menschen - und Bürgerrechte von 1789 hei�Yt. Ich finde, wir sollten im 21. Jahrhundert nicht hinter die Zeit von 1789 zurückfallen. Diese Bürgerinnen - und Bürgerrechte sollten wir uns zu eigen machen.
## Kinder von Migrantinnen und Migranten sollen hier als gleichberechtigte Staatsbürgerinnen und Staatsbürger aufwachsen können. Das geltende Staatsangehörigkeitsrecht macht aus den hier geborenen Menschen in vielen Fällen Ausländer, obwohl sie eben Inländer sind.

## Auch wenn ich das prinzipiell nicht mache, möchte ich Ihnen ein Beispiel aus meinem Leben geben. Ich bin in Duisburg in Nordrhein-Westfalen als Kind von Eltern aus der Türkei geboren, die als Gastarbeiter hierhergekommen sind. Ich bin hier geboren. Weil meine Eltern die türkische Staatsangehörigkeit hatten, hatte auch ich die türkische Staatsangehörigkeit.
## Ich bin hier geboren, aufgewachsen, habe hier die Schule, die weiterführende Schule und die Universität besucht. Ich habe mich die ganze Zeit geweigert, für etwas einen Antrag stellen zu müssen, was meiner Meinung nach eine Selbstverständlichkeit sein sollte. Warum konnten sich meine deutschen Freundinnen und Freunde deutsche Staatsbürger nennen, während ich sagen musste: " Nein, ich bin keine deutsche Staatsbürgerin "? Dabei bin ich genauso hier geboren und aufgewachsen wie die anderen.
## An der Universität musste ich nochmals eine Diskriminierung erleben. Als ich ein Stipendium gewonnen hatte, um ein Jahr lang in Australien zu studieren, wollte ich einen Antrag auf Auslands-BAföG stellen, so wie das auch meine Kommilitonin tat, die mit mir dorthin fahren wollte. Meiner Kommilitonin wurde das gestattet, mir wurde das nicht gestattet. Warum? Weil ich keine deutsche Staatsbürgerin war. Ich finde das einfach unfair. Ich finde, das ist ungerecht.
## Mit mir zusammen finden es Tausende davon betroffene Menschen ungerecht, für eine Selbstverständlichkeit erst einmal einen Antrag zu stellen, was andere nicht tun müssen, obwohl man wirklich in jeder Hinsicht genauso wie die Freundinnen und Freunde mit einem deutschen Pass ist.
## Die Anforderungen des Geburtsrechts, des Territorialprinzips Ius sanguinis im deutschen Staatsangehörigkeitsrecht sind einfach deutlich zu hoch. Hier müssen wir die Hürden absenken, wie das mein Kollege Beck sagte, gerade wenn wir an einer wirklichen Integrationspolitik interessiert sind, meine Damen und Herren.
## SPD und Grüne haben damals bei der Reform des Staatsbürgerschaftsrechts 1999 einen längst überfälligen Einstieg in das Ius soli gemacht.
## Doch leider war dieser Schritt zögerlich und unzureichend.

## Ein Geburtsfehler unter Rot-Grün, die Optionspflicht, wurde anderthalb Jahrzehnte später mehr schlecht als recht beseitigt.
## Aber die sehr hohen Anforderungen an das Ius soli, an den Aufenthaltsstatus wie den achtjährigen Aufenthalt oder das unbefristete Aufenthaltsrecht der ausländischen Eltern hier geborener Kinder sind nach wie vor in Kraft. Insofern gibt es Handlungsbedarf. Ich bleibe dabei: Diese hohen Hürden müssen endlich abgesenkt werden. Deshalb begrü�Yen wir diesen Gesetzentwurf.
## Ich begrü�Ye den Gesetzentwurf der Grünen auch, weil die Reform der Staatsangehörigkeit bei den Grünen bisher zumeist sehr unkritisch als Erfolg der rot-grünen Regierungszeit gefeiert worden ist und die verbliebenen Hürden und Härten gering geschätzt wurden, was mit diesem Gesetzentwurf ein Stück weit korrigiert wird. Ja, über die Stichworte " deutliche Gebührenerhöhung ", " höhere Sprachanforderungen " und " Beseitigung des sogenannten Inländerprivilegs ", das dazu führte - wir wissen es -, dass sehr viele türkische Staatsangehörige, die bisher die doppelte Staatsbürgerschaft hatten und auch deutsche Staatsangehörige waren, zu Tausenden und Zehntausenden ihre Staatsangehörigkeit verloren hatten, wurde einfach hinweggegangen.

## Deshalb finde ich es gut, dass man sieht, dass diese Hürden immer noch bestehen und es keinen Grund gibt, wie bisher zu feiern, sondern dass die Dinge beim Namen genannt werden und die Abschaffung des Optionszwangs gefordert wird. Das unterstützen wir.
## Wir unterstützen auch, dass der sinnlose Aufwand im Zusammenhang mit dem Optionsmodell, das von dieser Regierung eben nicht abgeschafft worden ist, grundsätzlich vollständig abgeschafft wird. Deshalb appelliere ich an die SPD, sich hier endlich zu bewegen und sich nicht weiterhin der Ausgrenzungspolitik von CDU und CSU anzuschlie�Yen.


## ...


## Ostermann (CDU) speech

## In Deutschland gilt bislang das eingeschränkte Geburtsortsprinzip. Das hei�Yt, wer sich zum Zeitpunkt der Geburt in Deutschland befindet, der wird dann deutscher Staatsangehöriger, wenn mindestens ein Elternteil seit wenigstens acht Jahren hier aufhältig ist und über ein unbefristetes Aufenthaltsrecht verfügt.
## Aus Sicht der Grünen soll diese Einschränkung nun wegfallen. Künftig würden, wenn der Vorschlag der Grünen Gesetz würde, Neugeborene bereits dann die deutsche Staatsangehörigkeit erhalten, wenn ein Elternteil seinen rechtmä�Yigen, gewöhnlichen Aufenthalt in Deutschland hat.
## Die Grünen begründen diesen Schritt damit, dass die globale Mobilität der Menschen zunehme. Daher komme es zu einem Spannungsverhältnis zwischen den zugezogenen in Deutschland lebenden Menschen und dem wahlberechtigten Staatsvolk. Die Grünen legen damit ein Verständnis von Staat und Staatsbürgerschaft an den Tag,
## das die CDU/CSU-Bundestagsfraktion nicht teilen kann. Einerseits degradieren Sie, Herr Beck, unter dem Deckmantel der Globalisierung und der Mobilität die Staatsangehörigkeit zu einem beliebigen Status.
## Nach Ihrer Ansicht ziehen Menschen in andere Länder und nehmen die dortige Staatsangehörigkeit an, wie es ihnen gerade so passt. Mit der Zeit gefällt diesen Menschen ihr Aufenthaltsort vielleicht nicht mehr. Bei einem erneuten Umzug um den Globus lässt man die Staatsangehörigkeit entsprechend hinter sich und nimmt weitere Staatsangehörigkeiten an. Das ist eine Politik, die unserem Verständnis eklatant widerspricht.

## Was verstehen wir unter dem Erwerb der Staatsangehörigkeit? Aus unserer Sicht ist damit nicht lediglich der Erwerb einiger zusätzlicher Rechte gemeint wie etwa des Wahlrechts, des Zugangs zum Beamtenstand, des konsularischen Schutzes im Ausland oder der Möglichkeit, BAföG zu erhalten. Stattdessen ist der Erwerb eine bewusste Entscheidung für einen Staat und für seine Werte. Mit dieser Entscheidung drückt der Erwerber seine Zugehörigkeit zu einer Schicksals - und Wertegemeinschaft aus. In diese Gemeinschaft soll er sich einbringen, und er soll sich ihr angehörig fühlen.
## ( Sven-Christian Kindler [ B�oNDNIS 90/DIE GR�oNEN ]: Das gilt für viele Kinder in Deutschland! Die entscheiden sich nicht, weil sie hier leben! )
## Das macht aus unserer Sicht den Erwerb der Staatsangehörigkeit aus.
## Die zweite Frage lautet: Wen wollen wir als neue Staatsbürger gewinnen?
