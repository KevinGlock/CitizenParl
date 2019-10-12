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

coi_spd16 <- partition("GERMAPARL",
                       parliamentary_group = "SPD",
                       year  = 2012:2016,
                       interjection= F,
                        role = c("mp", "government"))


## as partition bundles

pb2 <- partition_bundle(coi_spd16, s_attribute = "date")


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

debates_foreign2 <- debates2[[ subset(dt2, TOTAL >= 25)[["partition"]] ]]


## debates on Foreigners� Policy between 1996 and 2000

debates_foreign2[[39]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 39th debate on Foreigners� Policy for SPD between 2012 and 2016

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


## citizenship debates between 1996 and 2000

debates_citizen4[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 3th debate on citizenship for SPD between 2012 and 2016


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
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016

## no ref to dual citizen


## debates on dual citizenship between 2012 and 2016

debates_dual2[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## Eva H�gl 2014-03-12 SPD

p <- partition("GERMAPARL", speaker = "", date = "", encoding = "UTF-8")

read(p)


## Eva H�gl (SPD) speech

## Ich möchte es zu Beginn einmal ganz deutlich sagen: Diese Gro�Ye Koalition wird den Optionszwang abschaffen.

## Für in Deutschland geborene und aufgewachsene Kinder ausländischer Eltern entfällt in Zukunft der Optionszwang und die Mehrstaatigkeit wird akzeptiert.
## Das ist so weit klipp und klar; ich komme gleich zu den Schwierigkeiten. Wir bringen damit ganz deutlich zum Ausdruck, dass der Optionszwang abgeschafft wird.
## Ich sage es ganz deutlich: Die Optionspflicht schadet der Integration. Das stellen wir immer wieder fest. Sie belastet die Verwaltung, und sie passt nicht zu einem modernen Land wie Deutschland.
## Natürlich ist das in der Koalition ein umstrittenes Thema; das leugnet hier doch niemand. Das war eine schwere Entscheidungsfindung in der allerletzten Nacht der Koalitionsverhandlungen. Das wissen alle, die in diesem Haus sind, und das wissen alle, die diese Debatte verfolgen.
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Jetzt haben Sie die doppelte Staatsbürgerschaft schon nicht gekriegt! Jetzt können Sie wenigstens den Optionszwang abschaffen! )
## Das ist nicht unumstritten, und es ist auch richtig so, weil es nämlich ein wichtiges Thema ist, weil es um eine ganz grundsätzliche Frage geht, die viele Menschen in unserem Land betrifft.

## Natürlich ist es kein Geheimnis - es ist ein offenes Geheimnis -, dass die beiden Wörter " und aufgewachsen " nicht einfach zu definieren sind. Wir ringen darum, eine vernünftige Formulierung zu finden, was wir mit " und aufgewachsen " meinen.
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Ich dachte, " und aufgewachsen " bedeutet 40 b im Staatsangehörigkeitsgesetz! )
## Unsere generelle Linie ist: Wir schaffen die Optionspflicht ab, und wir erleichtern die Möglichkeit, die doppelte Staatsangehörigkeit zu behalten, für viele Menschen in unserem Land.

## Für uns soll es nicht länger Deutsche auf Probe geben. Wir wollen diejenigen nicht schlechterstellen, die bisher schon ein Recht darauf haben, eine deutsche Staatsangehörigkeit zu bekommen und eine andere zu behalten. Wir wollen nicht zu viel Bürokratie schaffen und selbstverständlich internationale Lebensläufe und die europäische Freizügigkeit berücksichtigen. Trotzdem - das besagt die Formulierung " und aufgewachsen " - wollen wir sicherstellen, dass die betroffenen Personen einen Bezug zu Deutschland haben. Es ist gut, dass wir versuchen, das sicherzustellen. Das ist der Kompromiss, den wir in der Gro�Yen Koalition gefunden haben.

## Für die SPD ist ganz klar - ich sage das noch einmal sehr deutlich -: Wir wollen selbstverständlich nicht, dass alle betroffenen Personen einzeln den Nachweis erbringen müssen, dass sie nicht optionspflichtig sind.
## Vielmehr sagen wir: Das ist ein falsches Signal. Wir wollen das Verfahren erleichtern. Wir wollen den Entscheidungszwang abschaffen. Wir werden - seien Sie dessen versichert, Kolleginnen und Kollegen von der Opposition - eine vernünftige Lösung für genau diese Frage finden, eine gute und praktikable Lösung.

## Das drückt doch etwas aus, Herr Beck; das wissen Sie ganz genau, das wissen alle Beteiligten hier. Die SPD wollte mehr. Die SPD möchte die doppelte Staatsangehörigkeit für einen viel grö�Yeren Personenkreis, auch für Personen, die hier schon länger leben. Wir können den Bundesländern, in denen die SPD mitregiert, eine solche Bundesratsinitiative selbstverständlich nicht verwehren. Aber ich rate auch hier zu ein bisschen weniger Aufregung; denn für die gesamte SPD, im Bund und in den Ländern, gilt der geschlossene Koalitionsvertrag. Das sage ich hier unmissverständlich.

## Wir ignorieren die Störungen von au�Yen. Wir freuen uns über kluge Hinweise von Ihnen, Herr Beck, wie wir die Wörter " und aufgewachsen " gut definieren können. Ich verspreche Ihnen, Herr Beck: Bei der nächsten Debatte zum Thema Optionszwang werden wir eine gute Regelung vorgelegt haben.
## Ich freue mich auf die Beratungen zu dem Gesetzentwurf der Bundesregierung und auf Ihre Unterstützung bei der Abschaffung des Optionszwangs; denn darum geht es.


## ...


## Uli Gr�tsch (SPD)

## Frau Kollegin Buchholz, ich wei�Y nicht, ob es Ihnen zusteht, die SPD-Bundestagsfraktion mit Schimpf und Schande zu überziehen. Ich wei�Y schon gar nicht, ob es Ihnen zusteht, unsere Integrationsministerin als Integrationsverweigerin zu bezeichnen.
## ( Beifall bei der SPD und der CDU/CSU - Christine Buchholz [ DIE LINKE ]: Die habe ich nicht gemeint! )
## Sie wissen ja, wer die Akteure sind, die sich um Integration in unserem Land verdient machen. Bevor Sie solche Worte benutzen, sollten Sie kurz einmal schauen, wer auf der Regierungsbank sitzt und wer nicht.
## Natürlich ist Deutschland ein Einwanderungsland, und das ist auch gut so.

## Natürlich braucht unser Land künftig ein modernes Staatsangehörigkeitsrecht. Daran zweifeln nur die, die man da, woher ich komme, Hinterwäldler nennt. Auch ich meine, dass es gut ist, dass sich alle politischen Akteure in Deutschland auch au�Yerhalb des Deutschen Bundestages und ausdrücklich auf allen Ebenen Gedanken darüber machen, wie dieses Recht in Zukunft aussehen soll. Niemand hier will doch den Ländern das Recht absprechen, ihre in der Verfassung verankerten Rechte zu nutzen und sich am politischen Diskurs aktiv zu beteiligen.
## Die Kollegin Högl hat schon darauf hingewiesen. Es ist natürlich auch kein Geheimnis, dass die Fraktionen der SPD und der CDU/CSU bei der Haltung zur Abschaffung des Optionszwangs und darüber, wie ein künftiges Staatsangehörigkeitsrecht in Deutschland ausgestaltet wird, unterschiedlicher Meinung sind. Wir wissen, dass es schon lange gesellschaftliche Realität ist, dass das bis dahin geltende Staatsangehörigkeitsrecht überaltert ist und es einer Neuregelung bedarf. Es ist peinlich genug, dass wir mehr als 20 Jahre brauchen, um gesellschaftliche Realitäten mit Mehrheiten im Deutschen Bundestag abzubilden.

## Natürlich ist diese Reise kein Kurztrip, sondern eine ziemlich lange Reise, aber es lohnt sich auch, diese lange Reise zu machen, weil es zu dem wirklich erstrebenswerten Ziel führt. Für uns wird es eine gro�Ye Errungenschaft sein, wenn wir das Ziel erreicht haben, weil wir mit der Abschaffung der Optionspflicht eines der ganz gro�Yen gesellschaftspolitischen Ziele erreicht haben.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## Thomas Oppermann 2013-06-05 SPD

p <- partition("GERMAPARL", speaker = "Thomas Oppermann", date = "2013-06-05", encoding = "UTF-8")

read(p)

## Thomas Oppermann (SPD)

## Nach Ihrer Vorstellung gibt es in der Europäischen Union 26 Nebenregierungen, die 2 Millionen EU-Bürger in Deutschland dirigieren.
## ( Stefan Müller [ Erlangen ] [ CDU/CSU ]: Reden Sie hier als Mitglied des Schattenteams? )
## Das scheint ein Eingriff in unsere nationalstaatliche Souveränität zu sein. So haben Sie eben sinngemä�Y argumentiert.
## Ich kann Ihnen nur sagen: Mit Ihrem Festklammern an der doppelten Staatsangehörigkeit befinden Sie sich mental immer noch auf der Höhe des nationalistischen Denkens aus der Kaiserzeit.
## ( Serkan Tören [ FDP ]: Festhalten an der doppelten Staatsangehörigkeit? )
## Ihr Standpunkt ist aus dem letzten bzw. vorletzten Jahrhundert. Kommen Sie endlich aus Ihrer Ecke heraus, und gestalten Sie mit uns zusammen ein modernes Staatsangehörigkeitsrecht für ein modernes Deutschland!
## ( Beifall bei der SPD und dem B�oNDNIS 90/DIE GR�oNEN )
## Rot-Grün und Bundeskanzler Schröder haben vor 14 Jahren die erste gro�Ye Modernisierung unseres Staatsangehörigkeitsrechtes auf den Weg gebracht. Erstmals wurde geregelt, dass die Kinder von längerfristig in Deutschland lebenden Einwohnern automatisch die deutsche Staatsangehörigkeit bekommen. Das war eine fundamentale Abkehr vom Reichs - und Staatsangehörigkeitsrecht der Kaiserzeit, und es stellte eine klare Zäsur in der Einwanderungspolitik dar mit einer klaren Absage an nationalistische Deutschtümelei, meine Damen und Herren.
## ( Beifall bei der SPD sowie bei Abgeordneten des B�oNDNISSES 90/DIE GR�oNEN )
## Endlich haben wir der Tatsache Rechnung getragen, dass wir eine Einwanderungsgesellschaft sind.

## In diesem Land leben 15 Millionen Menschen, Herr Kauder, die entweder Einwanderer sind oder direkt von Einwanderern abstammen. Diese Menschen dürfen nicht länger Bürgerinnen und Bürger zweiter Klasse sein.
## ( Beifall bei der SPD und dem B�oNDNIS 90/DIE GR�oNEN sowie bei Abgeordneten der LINKEN )
## Sie leben ganz überwiegend dauerhaft bei uns. Sie arbeiten hier, zahlen ihre Steuern, zahlen Sozialversicherungsbeiträge. Deshalb brauchen wir faire Regeln beim Zugang zur vollen Staatsbürgerschaft. Wir wollen die Einbürgerung erleichtern, wir wollen die doppelte Staatsangehörigkeit ermöglichen, und wir wollen endlich Schluss machen mit der unwürdigen Praxis des Optionszwanges.

## Fast eine halbe Million junger Menschen muss sich in den nächsten 15 Jahren entscheiden, ob sie Deutsche bleiben wollen oder Ausländer werden müssen. Dabei ist für zwei Drittel von ihnen völlig klar, dass sie beide Staatsangehörigkeiten behalten wollen. Was ist das für ein Signal an junge Menschen, die 23 Jahre lang Deutsche sind und sich jetzt gegen die Staatsangehörigkeit ihrer Eltern und Gro�Yeltern, gegen ihre Herkunft entscheiden müssen, um Deutsche bleiben zu können? Was ist das für ein Staatsangehörigkeitsrecht, das aus Deutschen Ausländer macht? - Das ist ein absurdes Staatsangehörigkeitsrecht, meine Damen und Herren.


## ...


## Aydan Saliha �zoguz (�-zo?uz) (SPD) asks a question to Grindel (CDU)

## Es ist doch so, dass hier junge Menschen mit Migrationshintergrund schon bei ihrer Geburt, wie Sie selber sagten, die deutsche Staatsangehörigkeit bekommen. Daher ist es natürlich etwas eigenartig, wenn Sie sagen, diese seien später nicht integriert und es gebe viele Probleme. Ich frage mich jetzt: Was wollen Sie damit sagen? Diese Menschen sind von Geburt an Deutsche und bleiben erst einmal Deutsche. Wir sagen: Diese Menschen sollen sich nicht gegen ihre Herkunft entscheiden müssen.
## Sie haben von einem gelungenen Integrationsprozess gesprochen. Muss ich Sie so verstehen, dass diejenigen, die Sie für nicht integrierbar halten, mit der Volljährigkeit die deutsche Staatsbürgerschaft wieder verlieren sollen? Diese Möglichkeit besteht doch nicht. Deswegen frage ich mich, was Sie mit Ihrer Aussage bezwecken.


## ...


## Ekin Delig�z (Gr�ne) asks a question to Grindel

## Stimmen Sie mir zu, dass das jetzige Optionsrecht es zulässt, dass Schülerinnen und Schüler mit Migrationshintergrund unabhängig von ihren Schulnoten die deutsche Staatsangehörigkeit behalten können, auch wenn sie nicht so ganz in das Schema der Personen passen, die Sie gerne einbürgern würden?


## ...



## Ren� R�spel (SPD) asks Grindel (CDU)

## Ich habe jetzt verstanden, dass Sie eine doppelte Staatsbürgerschaft für integrationshemmend oder - feindlich halten
## und Sie der Auffassung sind, dass man, wenn man in einem anderen Land lebt, seine alte Staatsbürgerschaft abgeben muss. Jetzt frage ich Sie in der Konsequenz dieser Logik, ob wir dann auch anraten müssten, dass im Ausland lebende Deutsche, die zusätzlich die Staatsbürgerschaft des neues Landes annehmen, die deutsche Staatsbürgerschaft abgeben müssen.
## Muss mein Schwager, der mit seiner Frau und seinen Kindern in Schweden lebt und der die schwedische Staatsbürgerschaft angenommen hat, auf die deutsche Staatsbürgerschaft verzichten, die er behalten will, weil Deutschland sein Heimatland ist?


## ...


## Daniela Kolbe (SPD) speech

## Es fällt selbst Konservativen schwer, plausibel zu erklären, was das Optionsmodell konkret bringen soll. Da helfen auch allerlei Verrenkungen nicht. Es geht hier um die Frage, warum junge Menschen, die qua Geburt die deutsche Staatsangehörigkeit bekommen - das haben wir gemeinsam beschlossen -, im Alter zwischen 18 und 23 Jahren erklären müssen, ob sie Deutsche bleiben wollen oder nicht. Was ist der Sinn dieses erneuten Bekenntnisses?
## Wir haben jetzt allerlei gehört, was nicht zum Thema gehört hat und eher Ausdruck von Ressentiments bis hin zum Anklang von Rassismen war.
## Ich meine auch, es war Ausdruck einer merkwürdigen Grundhaltung gegenüber türkeistämmigen Menschen, die in unserem Land leben.
## Welche Argumente aber gibt es denn nun tatsächlich von Ihrer Seite für das Optionsmodell? Damit solle Mehrstaatigkeit verhindert werden.
## Dabei wird gegenwärtig bei mehr als der Hälfte der Einbürgerungen in Deutschland Mehrstaatigkeit akzeptiert.
## Im Moment leben - geschätzt - 4,5 Millionen Mehrstaatler in unserem Land. Meines Wissens ist der Untergang des Abendlandes trotzdem ausgeblieben.
## Sie sagen, die deutsche Staatsangehörigkeit stehe am Ende eines Integrationsprozesses. Damit erkennen auch viele Konservative an, dass es natürlich auch Teil eines gelingenden Integrationsprozesses sein kann, irgendwann deutscher Staatsangehöriger zu sein. Schauen wir uns doch einmal die Realität an, die das Optionsmodell hervorruft: Für manche junge Menschen steht im Moment am Ende eines gelungenen Integrationsprozesses der Rausschmiss aus der deutschen Staatsangehörigkeit. Das ist doch verkehrte Welt.

## Sehr konservative Menschen - Herr Schröder möchte auch sehr konservativ sein; so ist zumindest heute mein Eindruck - sagen, die doppelte Staatsangehörigkeit entwerte die deutsche Staatsangehörigkeit. Aus meiner Sicht ist das totaler Quatsch.
## An dieser Stelle wünsche ich mir das Selbstbewusstsein der US-Amerikaner. Diese laden Menschen, die dauerhaft in den Vereinigten Staaten leben wollen, ein und fordern sie geradezu auf, sich zu den USA zu bekennen und US-Amerikanerinnen oder US-Amerikaner zu werden. Welche Staatsangehörigkeit die betreffenden Menschen mitbringen, ist dabei vollkommen egal. Das ist ein selbstbewusster Umgang mit dem Staatsangehörigkeitsrecht. Ehrlich gesagt, ein solches Selbstbewusstsein wünsche ich uns im Zusammenhang mit dem deutschen Staatsangehörigkeitsrecht.
## ch möchte zwei Punkte aus der Anhörung, die wir zu diesem Thema durchgeführt haben, aufgreifen: zum einen die Bürokratie und zum anderen den Rausschmiss von gut integrierten Menschen aus der deutschen Staatsangehörigkeit.
## Herr Jungnickel meint, dass die hier in Rede stehenden Fälle in etwa mit Einbürgerungsfällen vergleichbar sind, was den bürokratischen Aufwand angeht. Die Frage an Sie lautet also: Was macht es im Hinblick auf den bürokratischen Aufwand für einen Sinn, 40 000 zusätzliche Einbürgerungsverfahren durchzuführen? Wenn Sie wirklich für Bürokratieabbau sorgen und etwas für Integration in diesem Land tun wollen, dann nutzen Sie doch diese Gelegenheit und schaffen Sie endlich das Optionsmodell ab. Es ist bürokratischer Wahnsinn und unter integrationspolitischen Aspekten Unsinn.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## Lars Castellucci 2015-04-23 SPD

p <- partition("GERMAPARL", speaker = "Lars Castellucci", date = "2015-04-23", encoding = "UTF-8")

read(p)

## Lars Castellucci (SPD)

## 1997 habe ich, glaube ich, zum ersten Mal richtig verstanden, worum es bei dem Thema geht. Damals habe ich nämlich in den Vereinigten Staaten studiert, und ich war bei einer wunderbaren Gastfamilie untergebracht. Meine Gast-eltern waren 1972 für ein knappes Jahr in Deutschland. Dort ist ihr Sohn auf die Welt gekommen, und zwar in Baiertal, in der Schulstra�Ye, auf der Couch, mit Unterstützung einer Hebamme.
## Als es um die Ausweispapiere ging und meine Gast-eltern sich wieder nach Hause aufmachen wollten, gab es plötzlich ein Problem. Die deutschen Behörden haben nämlich gefragt: Was wollen Sie eigentlich von uns? Sie sind doch Amerikaner. Das ist ein amerikanisches Kind. - Die amerikanischen Behörden wiederum haben gefragt: Was wollen Sie denn? Das Kind ist in Deutschland geboren. Es ist ein deutsches Kind.
## Das Beispiel zeigt: Wir haben unterschiedliche Traditionen. Es wurde bereits angesprochen: Ius soli hei�Yt, es gilt, wo man geboren ist. Ius sanguinis hei�Yt, es gilt die Abstammung.
## Sie schlagen nun vor, das Geburtsprinzip im deutschen Staatsangehörigkeitsrecht zu verankern, und zwar für alle Kinder, deren Eltern sich rechtmä�Yig hier aufhalten und ihren gewöhnlichen Aufenthalt hier haben.

## Mit Ihrem Vorschlag jedenfalls würden beispielsweise Kinder von Studierenden, die hier geboren werden und deren Eltern eine ausländische Staatsbürgerschaft haben, Deutsche werden können.
## ( Volker Beck [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Das wäre total schlimm! )
## Ausländische Studierende sind für uns eine wichtige Zielgruppe. Es gibt sogar Bundesprogramme, mit denen wir sicherstellen wollen, dass diese bei uns bleiben können. Andere gehen zurück und sind dann hoffentlich gute Botschafter unseres Landes in der Welt. Aber die Kinder derer, die bleiben, würden von Anfang an als Deutsche aufwachsen. Das hätte eine ganze Menge Vorteile.

## Vielleicht ist der behutsame Weg der Veränderung, den wir schrittweise gehen, deshalb auch angemessen. 1999 haben wir den ersten Anlauf genommen; es ist davon die Rede gewesen. Wir haben einiges erreicht. Das Staatsangehörigkeitsrecht stammte aus dem Kaiserreich und hie�Y auch so. Wir haben es modernisiert. Seitdem gelten Elemente des Geburtsortsprinzips. Die Optionspflicht haben wir mit der neuen Reform fast überwunden. Ich will sagen: Wir sind auf dem Weg. Es geht in die richtige Richtung, und den Rest schaffen wir auch noch.
## Bis wir so weit sind, können wir aber auch über ein paar Fragen nachdenken. Da spreche ich Sie als Oppositionsfraktion, die den Gesetzentwurf eingebracht hat, direkt an. Wie ist das eigentlich mit der Weitervererbung von Mehrstaatigkeit? Diese Frage ist aus meiner Sicht nicht sinnvoll und nicht konzeptionell gelöst. Wie können also Regelungen über Generationen hinweg aussehen, die dafür sorgen, dass es nicht zu einer Multiplikation von Staatsangehörigkeiten kommt? Müssen wir über etwas nachdenken, was beispielsweise eine ruhende Staatsangehörigkeit ist?
## Au�Yerdem begründen Staatsbürgerschaften Rechte und Pflichten. Ich äu�Yere mich jetzt einmal als Sprecher der AG Demokratie: Wo soll man denn eigentlich das Wahlrecht haben - in der ersten Generation, in der zweiten Generation und dann in der dritten Generation, wenn möglicherweise gar keine Bezüge zu den Ursprungsländern mehr da sind? Nach meiner Vorstellung sollte eine Person - aber dann durchaus auch alle Gruppen, die Sie angesprochen haben - dort wählen, wo sie lebt, wo sie ihren Lebensmittelpunkt hat.

## Wir können diese Fragen also weiterbearbeiten; das sollten wir auch tun. Aber wir müssen dabei auch nicht stehen bleiben, sondern wir können heute schon Spielräume nutzen, die unter dem aktuellen Staatsbürgerschaftsrecht möglich sind. Beispielsweise in Baden-Württemberg, woher ich komme, ist die Anzahl der Einbürgerungen im letzten Jahr auf den höchsten Stand seit 2003 gestiegen. Das ist kein Selbstläufer, sondern dahinter steckt eine einbürgerungsfreundliche Politik.

## Dort gibt es zielgruppengerechte Informationen und weitere gute Sachen. Von Einbürgerung profitieren schlie�Ylich alle.
## Das ist wieder so ein Argument. Die Statistiken zeigen wirklich klar: Eingebürgerte erreichen höhere Bildungsabschlüsse; sie sind erfolgreicher auf dem Arbeitsmarkt; sie erzielen höhere Einkommen; sie zahlen mehr Steuern. Einbürgerung ist wirklich ein Gewinn für die gesamte Gesellschaft. Wir müssen jetzt aber nicht auf das Staatsangehörigkeitsrecht starren wie das Kaninchen auf die Schlange, sondern wir können heute schon die Spielräume nutzen und kreativ und engagiert sein.

## wir waren länger in unserer Geschichte ein Auswanderungsland. Erst langsam gewöhnen wir uns daran, ein Einwanderungsland zu sein. Ich finde, es ist in Ordnung, wenn wir uns daran gewöhnen. Es ist ein Prozess.


## Mahmut �zdemir (SPD)

## Ich kann nur mutma�Yen, was der Entwurf eines Gesetzes zur Verwirklichung des Geburtsrechts im Staatsangehörigkeitsrecht zu diesem Zeitpunkt bewirken soll.

## Und sind sie politisch so unterbelichtet, dass sie die folgende Formulierung im schwarz-grünen Koalitionsvertrag nicht einzuschätzen in der Lage sind? Ich zitiere aus dem schwarz-grünen Koalitionsvertrag:
## Auf bundespolitischer Ebene werden wir die Aufhebung der Optionspflicht und die Akzeptanz von Mehrstaatigkeit im Staatsangehörigkeitsrecht für in Deutschland geborene und aufgewachsene Kinder ausländischer Eltern unterstützen.

## Zwei gewichtige Ideale von uns Sozialdemokraten sind seit mehr als zwei Jahrzehnten die Verwirklichung des Geburtsrechtes im Staatsangehörigkeitsrecht ebenso wie das Bekenntnis zur Mehrstaatigkeit. Wir liefern uns diesbezüglich vielleicht viele Wortgefechte im Plenum.


## ...


## Aber drau�Yen bei den Bürgerinnen und Bürgern bringen Sie, werte Kolleginnen und Kollegen der Grünen, nicht nur die SPD in Misskredit, sondern auch den politischen Prozess als solchen, indem Sie das Vorurteil bedienen, dass nach der Wahl Versprechen nichts mehr wert seien. Besonders verwerflich ist es hierbei, einen politischen Weggefährten in diese Situation zu bringen, der 1999 das Geburtsrecht im Zusammenhang mit der Mehrstaatigkeit erstmals gesetzlich billigte und damit dann teilweise das Abstammungsprinzip verdrängte. Das Staatsangehörigkeitsrecht eignet sich deshalb nicht unbedingt für solche Spielchen. Das Spiel mit der Identität hier geborener junger Menschen, deren Eltern ausländische Staatsangehörige sind, aber auch das Spiel mit der Lebensleistung derer, die als sogenannte Gastarbeiter kamen und dem Wirtschaftswunder mit Geistes - und Körperkraft Auftrieb verschafften, eignen sich nicht für die politische Bühne,
## ( Beifall bei Abgeordneten der SPD sowie des Abg. Dr. Philipp Lengsfeld [ CDU/CSU ] )
## erst recht nicht, wenn die letztere Gruppe bei völlig überlasteten Ausländerbehörden im hohen Alter auf die Abwicklung ihrer Anträge warten muss. Das Staatsangehörigkeitsrecht ist die notarielle staatliche Beurkundung eines Bandes, des Bandes, das mich mit meiner Geburt im Krankenhaus Duisburg-Homberg vor 27 Jahren mit diesem Land, meiner Heimat, verbunden hat, eine Verbindung, die stärker ist als jedes Dokument. Gerade deshalb war die Zeit des Wartens auf diese Beurkundung für mich und viele andere bis zur richtigen politischen Mehrheit in diesem Land erträglich.

## Wir erwarten nicht mehr, dass hier geborene junge Menschen mit einer Entscheidung, spätestens mit dem 23. Lebensjahr, den Beweis antreten, ob sie der Beurkundung der Staatsangehörigkeit wert sind. 
## Damit tragen wir ihrer Identität, ihrer Lebenssituation und ihrem inneren Frieden Rechnung. Dies tun wir mit einem Koalitionspartner, der auf Landesebene in Hessen mit einer Kampagne gegen den Doppelpass das Ende einer rot-grünen Bundesratsmehrheit einläutete.
## Im �obrigen ist die reine Debatte um das Staatsangehörigkeitsrecht auch nicht geeignet, die Lebensrealitäten der betroffenen Menschen tatsächlich abzubilden.

## Im Grundgesetz hei�Yt es in Artikel 116:
## Deutscher im Sinne dieses Grundgesetzes ist vorbehaltlich anderweitiger gesetzlicher Regelung, wer die deutsche Staatsangehörigkeit besitzt.
## Dieser Status ist wichtig für die Berechtigung spezifischer deutscher Grundrechte. Dieses Statut ist mittlerweile im Hinblick auf die Grundrechtsberechtigung von Unionsbürgern aufgrund eines Diskriminierungsverbotes europarechtlich überlagert und dem faktischen Wandel unterworfen worden.
## Selbst ein aktives und passives Kommunalwahlrecht für Unionsbürger wird davon getragen. Ich möchte darauf hinaus, dass die Formulierung im Grundgesetz bewusst auf eine einfache gesetzliche Definition durch den Bundestag setzt, damit wir auf - gesellschaftlichen Wandel reagieren können. Der gesellschaftliche Wandel ist im Bundestag insoweit angekommen, als die Optionspflicht durch die aktuelle Bundes-regierung aufgehoben wurde als Beginn - ich betone: als Beginn - exakt des Wandels, den Sie - genauso wie wir - vollumfänglich im Gesetzentwurf zu beschreiben versuchen. Für Ihren Gesetzentwurf in der vorliegenden Form besteht aber derzeit leider keine politische Mehrheit in diesem Hause, 
## ( Sylvia Kotting-Uhl [ B�oNDNIS 90/DIE GR�oNEN ]: Das liegt ja an Ihnen! )
## aber eben nicht aus Gründen mangelnder Ideale, sondern aus Gründen zwingender demokratischer Mechanismen. Wenn wir hier im Deutschen Bundestag über Gesetze oder deren �"nderung reden, dann denken wir an die Staatsgewalt, die auf unserem Staatsgebiet die rechtsstaatliche Ordnung durchsetzt. Den Begriff des Staatsvolkes behandeln wir hierbei jedoch recht stiefmütterlich, obwohl uns das Grundgesetz neben der Möglichkeit, das Staatsangehörigkeitsrecht sukzessive anzupassen, zumindest die Hausaufgabe aufgibt, gesellschaftliche Realitäten abzubilden.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[5]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## R�diger Veit 2012-02-09 SPD

p <- partition("GERMAPARL", speaker = "R�diger Veit", date = "2012-02-09", encoding = "UTF-8")

read(p)

## R�diger Veit (SPD)

## Um gleich die Antwort auf eine mir eben auf dem Weg hierher von der Kollegin Daðdelen gestellte Frage zu geben: Wir, liebe Kolleginnen und Kollegen von der SPD genauso wie vom Bündnis 90/Die Grünen und von der Linkspartei, müssen zum vermehrten Male diese Koalition oder das, was von ihr vielleicht noch wahrnehmbar übrig ist, darauf hinweisen, dass es allerhöchste Zeit ist, endlich das Staatsbürgerschaftsrecht vernünftig zu reformieren.
## ( Beifall bei Abgeordneten der SPD und der LINKEN )
## Wir wollen, dass die Hinnahme von Mehrstaatigkeit, die sogenannte doppelte Staatsbürgerschaft, generell zulässig ist. Wir wollen die Optionspflicht abschaffen und die Voraussetzungen für die Einbürgerung nachhaltig erleichtern. Denn, meine sehr verehrten Damen und Herren, für uns Sozialdemokraten ist die Einbürgerung nicht etwa der ins Schaufenster gestellte, krönende Abschluss der Integration, sondern ein ganz wichtiger Zwischenschritt auf dem Weg zur vollständigen Integration in unsere Gesellschaft, in unser Gemeinwesen. Das wollen wir befördern. 

## Sie haben leider unseren Gesetzentwurf am 10. November 2011 abgelehnt, mit dem wir die gleiche Intention verfolgt haben, und zwar in namentlicher Abstimmung mit sämtlichen Stimmen der Abgeordneten von CDU/CSU und FDP; der Rest des Hauses hat freundlicherweise zugestimmt.

## Wir müssen Sie jetzt auffordern, endlich einen entsprechenden Gesetzentwurf vorzulegen. Ich sage auch deswegen " endlich ", weil spätestens im nächsten Jahr die Frist für diejenigen, die dann 23 werden, abläuft, um sich in der Frage der Staatsbürgerschaft - entweder die deutsche oder die ausländische Staatsbürgerschaft - zu entscheiden. Spätestens dann wird sich erweisen, dass wir mit dem Optionsmodell eine Art Bürokratiemonster geschaffen haben; darauf komme ich noch zurück.
## Liebe Kolleginnen und Kollegen, manchmal hat man bei den Debatten um die Staatsbürgerschaft den Eindruck, dass das, was Rot-Grün dem Haus in den Jahren 1998 und 1999 präsentiert hat, etwas völlig Neues war. Dabei würde ich gerne einmal daran erinnern, dass die doppelte Staatsbürgerschaft bzw. die Hinnahme von Mehrstaatigkeit - so lautet der Fachausdruck - keineswegs so furchtbar neu und revolutionär ist.

## Bevor die Doppelpasskampagne des damaligen hessischen Ministerpräsidenten Roland Koch losgetreten wurde, wurde von der hessischen CDU eine Werbeagentur beauftragt, eine Kampagne zu entwerfen, durch die sich das Blatt zugunsten der CDU wenden könnte. Man hat sich dann dieses emotionalisierende Thema ausgesucht. �obrigens hat die SPD damals nicht deswegen verloren. Wir haben im Februar 1999 sogar noch 1,2 Prozent dazugewonnen. Leider haben die Grünen aus verschiedenen Gründen erheblich verloren, und das konnten wir nicht mehr kompensieren. So ging dann auch die Mehrheit dahin.

## Bis zum Inkrafttreten des von uns initiierten Rechtes war es in Deutschland fast ausnahmslos die Regel, dass jemand türkischer Herkunft, der zum Konsulat gegangen ist, um seine türkische Staatsbürgerschaft abzugeben, und der anschlie�Yend die deutsche erworben hat, auf ausdrückliches Bitten des Konsulatsmitarbeiters hinterher noch einmal erschienen ist, um seine türkische Staatsbürgerschaft wieder zu beantragen.

## Deswegen spreche ich davon, dass es in besonderer Weise doppelbödig, hinterhältig und auch verlogen war, dass mit dieser Kampagne seinerzeit gegen die Hinnahme von Mehrstaatigkeit Stimmung gemacht wurde.
## Wir wollen das generell ermöglichen, auch deswegen, weil wir das integrationspolitische Ziel verfolgen, möglichst viele der bei uns lebenden Bürgerinnen und Bürger im Sinne eines einheitlichen Wahlvolkes zu Staatsbürgern zu machen. Wir wollen, dass sich die Betreffenden stärker, besser und intensiver mit der deutschen Kultur identifizieren. Das würde uns jedenfalls sehr freuen. Dazu kann der Erwerb der Staatsbürgerschaft einen wichtigen Beitrag leisten. Deswegen sollten wir den Menschen keine Hindernisse in den Weg legen.

## Wir sollten nicht nur Sonntagsreden über Integration halten. Wir sollten einen Beitrag dazu leisten. Dazu gehört in erster Linie die Beseitigung von Hindernissen für die Einbürgerung und die Abschaffung der Optionspflicht, damit nicht jene jungen Menschen, die demnächst, also 2013 - ich habe das Datum bereits genannt -, 23 Jahre alt werden, in einen Loyalitätskonflikt zwischen ihrer Abstammung und dem Herkunftsland der Eltern und der deutschen Kultur, in der sie aufgewachsen sind, geraten. Vielmehr sollten wir uns freuen, dass sie einen Beitrag zur Integration leisten, indem sie weiterhin deutsche Staatsbürger bleiben. Das ist unser Anliegen. Wir werden nicht lockerlassen. Darauf können Sie sich verlassen.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[6]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## Uli Gr�tsch 2014-01-16 SPD

p <- partition("GERMAPARL", speaker = "Uli Gr�tsch", date = "2014-01-16", encoding = "UTF-8")

read(p)

## Gr�tsch (SPD) speech

## Herr Kollege Beck, damit Willkommenskultur nicht nur eine Worthülse ist, bedarf es einer ständigen Weiterentwicklung, eines ständigen gesellschaftlichen Diskurses und auch der politischen Diskussion darüber. Um dieses gesellschaftliche Klima in Deutschland zu fördern, brauchen wir ein modernes Staatsangehörigkeitsrecht. Das ist in diesem Haus, so denke ich, weitestgehend unstrittig.
## Wir haben gesagt - das ist richtig -, wir unterschreiben keinen Koalitionsvertrag ohne die doppelte Staatsbürgerschaft. Aus diesem Grund war es meiner Fraktion und der SPD in ihrer Gesamtheit ein elementares Anliegen, im Koalitionsvertrag festzuschreiben, dass für in Deutschland geborene Menschen der Optionszwang abgeschafft und Mehrstaatigkeit damit akzeptiert wird. Damit ist ein weiterer wichtiger Schritt in die richtige Richtung hin zu einem modernen Staatsangehörigkeitsrecht getan.

## elbstverständlich sind auch wir der Meinung, dass eine Neuregelung des Staatsangehörigkeitsgesetzes so gestaltet sein muss, dass möglichst viele Menschen, die dauerhaft in Deutschland leben, die deutsche Staatsbürgerschaft erwerben können.
## Mehrere Punkte in Ihrem Gesetzentwurf bzw. den vorliegenden Anträgen sind unterstützenswert, etwa die vereinfachte Erteilung von Beibehaltungsgenehmigungen, damit die ausländische Staatsbürgerschaft im Optionsverfahren nicht gewisserma�Yen automatisch verloren geht.
## Der Umgang damit liegt aber in der Kompetenz der Länder. Deren Prüfungen bezüglich Erleichterungen sollte der Bundestag nicht vorgreifen; ich wei�Y zumindest von SPD-regierten Ländern, dass daran bereits intensiv gearbeitet wird.
## Bis zur Neufassung des § 29 des Staatsangehörigkeitsgesetzes sind die zuständigen Landesbehörden aufgefordert, auf die bis dahin von der Optionspflicht betroffenen jungen Menschen dahin gehend hinzuwirken, dass diese rechtzeitig einen Antrag auf eine Beibehaltungsgenehmigung stellen.

## Die SPD wird weiter die Triebfeder sein, wenn es darum geht, mit der Neuregelung des Staatsangehörigkeitsrechts Deutschland als ein modernes und weltoffenes Land zu präsentieren.
## Jeder hier im Saal wei�Y, dass wir von der SPD nicht neu in diesem Thema sind. Wir sind bereits seit 1998 ständig bestrebt, möglichst vielen Menschen, die dauerhaft in Deutschland leben, die Möglichkeit zu geben, im Rahmen einer doppelten Staatsangehörigkeit endgültig alle Rechte und Pflichten wahrzunehmen,

## Auch die Vertreter der türkischen Gemeinden wissen, dass wir bei unserem Koalitionspartner im Wort stehen. Aber wir werden unsere Kraft im Deutschen Bundestag gemeinsam dafür einsetzen - auch ich werde dies tun -, dass wir diejenigen gewisserma�Yen nachholen, die aufgrund bisheriger Regelungen ihre ursprüngliche Staatsangehörigkeit abgeben mussten. Das verstehe ich unter einer Willkommenskultur.


## ...

## R�diger Veit (SPD)

## Darf ich das wiederholen, was Frau Kollegin Jelpke eben schon gesagt hat? Sie benutzte in Erinnerung an einen anderen Debatteninhalt - Stichwort " Spracherwerb vor Ehegattennachzug " - den Begriff " Zwangsehe ".

## Eines ist doch völlig klar - das brauchen Sie uns nicht immer wieder zu sagen; das wissen wir selber -: Wir wollten - das war schon 1998 so - die generelle Hinnahme von Mehrstaatigkeit. Das ist ja nun wirklich kein Geheimnis. �obrigens bestand hier in unserer gesamten Partei ein Konsens in einer Breite, wie es bei anderen Themen durchaus nicht immer selbstverständlich ist.

## Denn aufgrund der veränderten Mehrheitsverhältnisse im Bundesrat hie�Y es damals: Wenn wir keine Zustimmung im Bundesrat bekommen, ist die gesamte Reform des Staatsbürgerschaftsrechts, die wir uns vorgenommen hatten, im Eimer.

## Wir wissen doch ganz genau, dass der Wegfall des Optionszwanges bestenfalls nur 50 Prozent von dem darstellt, was wir uns eigentlich wünschen. Aber mehr war in den Koalitionsverhandlungen eben nicht durchsetzbar. Ich bedaure das au�Yerordentlich, aber ich kann es nicht ändern. Ich kann ja niemanden prügeln und sagen, dass er seine �oberzeugung gänzlich aufgeben und uns in der Weise entgegenkommen muss, in der wir es für richtig halten. Wir werden weiter �oberzeugungsarbeit leisten.
## Ich persönlich bin übrigens der Auffassung: Wenn klar ist, dass nach jetzt geltendem Recht sowieso über 50 Prozent aller Einbürgerungen unter Hinnahme von Mehrstaatigkeit erfolgen
## ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Legal! )
## und dass alle, die hier geboren werden und dadurch die deutsche Staatsbürgerschaft erwerben, zwei Staatsbürgerschaften behalten können, dann ist es hoffentlich nur eine Frage der Zeit, bis diejenigen, die das bisher verneinen, ein Einsehen haben und die generelle Hinnahme von Mehrstaatigkeit akzeptieren.
## ( Beifall bei der SPD sowie bei Abgeordneten des B�oNDNISSES 90/DIE GR�oNEN )
## Denn, liebe Kolleginnen und Kollegen, nach einem Bericht des Bundesamtes für Migration und Flüchtlinge - im letzten Jahr herausgekommen - wissen wir, dass zwei Drittel aller potenziellen Einbürgerungsbewerber bzw. des Potenzials derer, die Bürger werden könnten, sagen: Nein, ich stelle keinen Antrag auf Einbürgerung, weil ich meine Staatsbürgerschaft nicht aufgeben möchte. - Zwei Drittel! Bei einem Drittel all derer, die das gemacht haben, ist das Bedauern, dass sie ihre ausländische Staatsbürgerschaft aufgeben mussten, überdeutlich.
## ( Michael Frieser [ CDU/CSU ]: Das sind aber nicht diejenigen, die unter diese Regelung fallen! )
## Das hei�Yt also, es handelt sich dabei um ein Einbürgerungshindernis. Einbürgerungshindernisse können wir alle nicht wollen.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[7]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## R�diger Veit 2014-07-03 SPD

p <- partition("GERMAPARL", speaker = "R�diger Veit", date = "2014-07-03", encoding = "UTF-8")

read(p)

## R�diger Veit (SPD)

## Herr Minister, ich stimme Ihnen ausdrücklich zu - das sieht auch die SPD so -: Es ist ein besonderer Tag, und es ist ein bedeutendes Gesetz zu einer ausgesprochen wichtigen Frage.

## Das sage ich mit der Bitte, dies als Trost aufzufassen, sowohl an die Kolleginnen und Kollegen der CDU/CSU-Fraktion als auch ein Stück weit an die sozialdemokratische Seite gerichtet. Wir brauchen von Ihnen, von euch nicht daran erinnert zu werden: Wir treten seit 1998, 1999 konsequent dafür ein, dass in Deutschland die Mehrstaatlichkeit generell hingenommen werden darf.

## Deswegen - liebe Ulla Jelpke, ich fahre mit meinem Satz fort - haben wir bei der Staatsbürgerschaftsreform dieses alte Gesetz aus der Kaiserzeit zwar nicht ganz ersetzen können - durch die hessische Landtagswahl ging die Mehrheit im Bundesrat verloren -, sondern wir mussten diesen Kompromiss mit der Optionspflicht eingehen.
## Alle Sozialdemokraten haben nie etwas davon gehalten. Wir haben uns ein bisschen damit getröstet, dass die Optionspflicht spätestens im Verwaltungsvollzug bei den ersten Fällen noch einmal von fachlicher Seite durchleuchtet wird. Das haben wir auch als wichtiges Ziel im Wahlprogramm formuliert. Es stand auch im Hundert-Tage-Programm von Peer Steinbrück. 

## Wir hätten selbstverständlich gerne im Koalitionsvertrag mit der Union eine Regelung gehabt, dass wir die Mehrstaatlichkeit generell hinnehmen. Das ist nicht gelungen. So ist es zu einem Kompromiss gekommen.

## Das Entscheidende ist - darauf haben uns auch die Praktiker in der Anhörung hingewiesen -, dass mit den Regelungen, die jetzt gefunden worden sind - da bin ich Ihnen, Herr Minister de Maizière, genauso dankbar wie Heiko Maas, der an dieser Einigung mitgewirkt hat -, höchstwahrscheinlich allenfalls eine Zahl im einstelligen Prozentbereich dieser jungen Menschen - wie gesagt 4 000 bzw. fast 40 000 -, unter die Optionspflicht fällt. Für alle anderen ist mit den jetzt zu schaffenden gesetzlichen Voraussetzungen das Problem, sich irgendwann einmal zwischen zwei Staatsbürgerschaften entscheiden zu müssen, vom Tisch.


## ...


## Ministerin �zoguz (SPD) speech

## Das hei�Yt, genau zehn Jahre nach Inkrafttreten des Zuwanderungsgesetzes bekennt sich Deutschland zu den Kindern seiner Einwanderer mit ihren Herkünften. Das ist ein sehr schöner Befund.
## ( Beifall bei der SPD sowie bei Abgeordneten der CDU/CSU - �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Dann können Sie es ja vorbehaltlos abschaffen! )
## Ich kann Kritik sehr gut verstehen. Ich höre auch sehr genau zu. Man zählt die Nachteile, die durch die Optionspflicht entstehen, die einmal gemeinsam beschlossen wurde, auf - das sind eventuell die Ausnahmen, die noch bestehen bleiben - und übersieht vollkommen, dass Hunderttausende Kinder und Jugendliche - die Zahlen sind gerade genannt worden -, die schon geboren sind, von diesem neuen Gesetz profitieren werden,

## Das ist doch ein gutes Signal, welches wir an die deutsche Jugend mit zwei Pässen und alle anderen senden.
## Die Härtefallregelung gibt es auch noch. Sie ist ja gerade für solche Fälle gedacht, die wir uns heute nicht alle überlegen können. Diese Jugendlichen können dann zeigen: Ich habe einen Bezug zu Deutschland, ich bin hier genauso verwurzelt. Ich finde, das Staatsangehörigkeitsrecht wird ein Stück gerechter. Es hat mit Identität zu tun, mit Verwurzelung, nicht mit dem Herkunftsland, aus dem jemand kommt. Danach haben wir ja bisher unterschieden. Wir werden allen jungen Menschen, die jetzt so gebannt darauf warten, dass wir das endlich umsetzen, dass sie endlich eben nicht mehr diese Angst haben müssen, diese Angst nehmen, das Gesetz jetzt umsetzen und nicht sagen: Lieber gar nichts, wenn man nicht 100 Prozent und alles bekommt.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[8]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## Katarina Barley 2016-12-16 SPD

p <- partition("GERMAPARL", speaker = "Katarina Barley", date = "2016-12-16", encoding = "UTF-8")

read(p)

## Katarina Barley (SPD)

## Sagen Sie mir mal einen vernünftigen Grund, warum Sie diesen Kompromiss wieder aufschnüren wollen! Sagen Sie mir doch mal einen vernünftigen Grund, der dafür spricht,
## ( �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Den gibt es nicht! )
## dass Menschen wie ich, Menschen wie Sie, Menschen wie Herr Mutlu sich zwischen ihren beiden Staaten entscheiden müssen! Was bringt es denn irgendeinem von Ihnen, wenn wir unseren zweiten Pass abgeben? Gar nichts! Es verändert Ihr Leben nicht. Es verändert dieses Land nicht.
## ( Beifall bei der SPD, der LINKEN und dem B�oNDNIS 90/DIE GR�oNEN )
## Die Wahrheit ist: Doppelstaatler sind keine besseren Menschen, sie sind auch keine besseren Deutschen, aber sie sind auch keine schlechteren. Seien Sie froh, dass es Menschen gibt, die Brücken zwischen Staaten bauen.
## ( Dr. Philipp Lengsfeld [ CDU/CSU ]: Sie haben nicht eine Minute darüber nachgedacht, was Sie da erzählen! )
## Die allermeisten Doppelstaatler bauen Brücken zwischen Staaten. Davon brauchen wir eher mehr als weniger.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[9]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on dual citizenship for SPD between 2012 and 2016


## doesn�t refer to dual citizenship

