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

coi_lefts16 <- partition("GERMAPARL",
                 year = 2012:2016,
                 parliamentary_group = c("PDS", "LINKE", "LINKE/PDS"),
                 interjection= F,
                 role = c("mp", "government"))


## as partition bundles

pb2 <- partition_bundle(coi_lefts16, s_attribute = "date")


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


## debates on Foreigners� Policy between 2012 and 2016

debates_foreign2[[22]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 22nd debate on Foreigners� Policy for GRUENE between 2012 and 2016

warnings()


## get samples for citizenship

dt3 <- count(debates2,
             query = q1,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt3)


debates_citizen2 <- debates2[[ subset(dt3, TOTAL >= 10)[["partition"]] ]]


## citizenship debates between 2012 and 2016

debates_citizen2[[7]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on citizenship for LINKE between 2012 and 2016


## get samples for dual citizenship

dt4 <- count(debates2,
             query = q2,
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt4)


## define minimum count for search words to gurantee that the debate is on the interested field

debates_dual2 <- debates2[[ subset(dt4, TOTAL >= 4)[["partition"]] ]]


## debates on dual citizenship between 2012 and 2016

debates_dual2[[1]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for LINKE between 2012 and 2016


## Sevim Dagdelen (Daðdelen) 2012-02-09 LINKE

p <- partition("GERMAPARL", speaker = "Sevim Dagdelen", date = "2012-02-09", encoding = "UTF-8")

read(p)

## Sevim Dagdelen (Daðdelen) (LINKE) speech

## Denn anders kann ich mir diese ideologische Borniertheit der CDU/CSU und der FDP nicht erklären, vor allen Dingen auch deshalb nicht, weil ich von der FDP erkennbar auch andere Stimmen zur Kenntnis nehme.
## So hat zum Beispiel die FDP im Niedersächsischen Landtag eine komplett andere Position und sagt in ihrem Papier zur Ausländer - und Flüchtlingspolitik, dass die Situation in Niedersachsen unerträglich sei.
## Sie sagt, der Umgang mit türkischen Staatsangehörigen sei nicht hinnehmbar. Ihnen werde in Niedersachsen die Mehrstaatigkeit verweigert, und auch die Optionspflicht sei unerträglich. Deshalb müsse eine bundeseinheitliche �"nderung vorgenommen werden.
## Ich wünsche mir, dass man diesen Kolleginnen und Kollegen und auch dem Doppelstaatler, dem niedersächsischen Ministerpräsidenten McAllister von der CDU, entgegenkommt und sagt: Wir schaffen diese blöde Optionsregelung ab.
## Herr Mayer, Sie haben gesagt, in Ausnahmefällen gibt es in Deutschland die Mehrstaatigkeit. Demgegenüber muss ich Sie daran erinnern, dass die Mehrstaatigkeit in Deutschland längst Realität und allgemeine Praxis ist. �ober 57 Prozent aller Eingebürgerten in Deutschland sind Doppelstaatler. Das sind über 4,5 Millionen Menschen.
## Was ist eigentlich Ihr Problem mit der Optionspflicht und der generellen Hinnahme der Mehrstaatigkeit? Dazu muss ich sagen: Offensichtlich geht es Ihnen um etwas anderes. In Ihrer Rede haben Sie die Scharia erwähnt; ich wüsste nicht, welche Bundestagsfraktion die Einführung der Scharia gefordert hat. Sie versuchen hier, einen Popanz aufzubauen.

## Die Quote der akzeptierten Mehrstaatigkeit bei Einbürgerungen beträgt bundesweit über 53 Prozent. Bei türkischen Staatsangehörigen liegt sie bei nur 28 Prozent. Das hei�Yt, Mehrstaatigkeit wird bei nichttürkischen Staatsangehörigen in Deutschland mehr als doppelt so häufig akzeptiert wie bei türkischen.

## Die Doppelstaatlerquote nichttürkischer Staatsangehöriger beträgt in Bayern 64,5 Prozent. Die gezielte Einbürgerung zum Beispiel türkischer Staatsangehöriger wird extrem erschwert. Diese ausgrenzende Praxis, die gezielte Verweigerung der Einbürgerung vor allem türkischer Staatsangehöriger - dies geschieht besonders in Bayern, aber auch zum Beispiel in Baden-Württemberg -,

## Ich meine, es ist auch nicht zeitgemä�Y, dass das Staatsangehörigkeitsgesetz so rigide ist. Es geht nicht nur um die Optionspflicht. Das ist unsere Kritik an dem Antrag der SPD: Sie glauben, durch die Abschaffung der Optionspflicht wäre das Thema gegessen.
## So werden wir das Demokratiedefizit - dies hat auch das Bundesverfassungsgericht konstatiert - bei der Problematik nicht beseitigen, dass Menschen, die dauerhaft in Deutschland leben, ausgegrenzt werden, indem sie nicht an Wahlen teilnehmen können. Dieses Problem werden wir nicht allein dadurch beheben, dass wir die Optionspflicht abschaffen. Dazu müssen wir zum Beispiel die Voraussetzungen für Einbürgerungen ändern.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for LINKE between 2012 and 2016


## Petra Pau 2014-03-12 LINKE

p <- partition("GERMAPARL", speaker = "Petra Pau", date = "2014-03-12", encoding = "UTF-8")

read(p)

## Pau (Linke) speech

## Bei der Optionspflicht geht es um eine Bestimmung des Staatsbürgerschaftsrechts, die seinerzeit unter der rot-grünen Bundesregierung eingeführt wurde und nun wieder abgeschafft werden soll. Ich darf hier daran erinnern, dass die Linke schon damals gegen die Optionspflicht und für eine generelle Hinnahme von doppelten Staatsbürgerschaften gestimmt hat.
## Im Jahr 2001 wurde in Hamburg Süleyman Tasköprü hingerichtet. Aysen Tasköprü ist seine Schwester. 2013 schrieb sie an Bundespräsident Joachim Gauck diese Zeilen:
## Noch im März 2011 konnte ich darüber lachen, als eine Sachbearbeiterin im Rathaus zu meinem Sohn sagte, er sei kein Deutscher. Der Kleine war ganz erstaunt und erklärte ihr sehr ernsthaft, dass er sehr wohl Deutscher sei, er habe schlie�Ylich einen deutschen Pass.. Heute kann ich darüber gar nicht mehr lachen. Ich hatte mal ein Leben und eine Heimat. Ich habe kein Leben mehr.. Ich habe auch keine Heimat mehr, denn Heimat bedeutet Sicherheit. Seitdem wir wissen, dass mein Bruder ermordet wurde, nur weil er Türke war, haben wir Angst. Was ist das für eine Heimat, in der du erschossen wirst, weil deine Wurzeln woanders waren?
## Nun reden wir heute nicht über das NSU-Desaster und natürlich auch nicht über Mord, wohl aber über Heimat, in der man sich wohl und auch sicher fühlen soll, auch mit fremden Wurzeln. Ein Doppelpass wäre hier hilfreich.


## ...


## Christine Buchholz (LINKE)

## Wir finden es gut, dass drei rot-grüne Bundesländer die Bundesratsinitiative gestartet haben, das Recht auf doppelte Staatsbürgerschaft für Kinder, die hier geboren sind, zu gewähren; denn damit würde der diskriminierende Op-tionszwang, nach dem sich diese Kinder zwischen zwei Staatsbürgerschaften entscheiden müssen, endlich bedingungslos abgeschafft.
## ( Beifall bei der LINKEN und dem B�oNDNIS 90/DIE GR�oNEN - �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ]: Da klatsche ich doch glatt mit! )
## Denn was bedeutet Optionszwang praktisch? Im Regierungsbezirk Darmstadt, in dem mein Wahlkreis liegt, haben bereits im ersten Halbjahr 2013 28 Jugendliche die deutsche Staatsangehörigkeit automatisch verloren, fast alle Kinder türkischer Eltern. Diese jungen Menschen besitzen jetzt nur noch die Staatsangehörigkeit ihrer Eltern. In Hanau verlor eine 23-Jährige ihren deutschen Pass, weil sie nicht rechtzeitig zwischen deutscher und türkischer Staatsangehörigkeit gewählt hat. Dabei hätte sie lieber den deutschen Pass behalten. Sie hat keine Chance, das Versäumnis zu heilen; die Behörde sieht keinen Spielraum.
## 248 jungen Menschen wurde 2013 durch den Op-tionszwang bundesweit die deutsche Staatsbürgerschaft entzogen, der überwiegenden Mehrheit, weil sie Fristen versäumt hat. Was bedeutet das für diese jungen Menschen, die bereits 23 Jahre lang Deutsche waren? Wie fühlt sich das für sie an?

## Union und SPD haben im Koalitionsvertrag vereinbart, Kindern von Zuwanderern die doppelte Staatsangehörigkeit zu gewähren, sofern sie in Deutschland geboren und aufgewachsen sind. Man könnte es so verstehen, als ob der Optionszwang damit abgeschafft würde. Aber ich sage Ihnen: Ihr angeblicher Doppelpasskompromiss ist faul. Die Optionspflicht bleibt, und sie wird noch bürokratischer. Sogenannte Optionskinder müssen unter Beweis stellen, dass sie " richtige Deutsche " sind. Als Nachweis sollten dafür die Betroffenen die Geburtsurkunde, eine deutsche Meldebescheinigung und ein deutsches Schulabschlusszeugnis vorlegen. Wenn Sie, Herr Kollege Strobl, sagen: " Das sollen sie doch machen ", dann ignorieren Sie bewusst und wissentlich, dass es eine Diskriminierung von Migranten im deutschen Bildungssystem gibt. Herr Strobl, damit erschweren Sie gerade diesen Jugendlichen die Erlangung der deutschen Staatsbürgerschaft und damit des Doppelpasses.
## ( Beifall bei der LINKEN sowie bei Abgeordneten des B�oNDNISSES 90/DIE GR�oNEN - Helmut Brandt [ CDU/CSU ]: Wo nehmen Sie das jetzt her? )
## Warum wollen CDU und CSU diese Optionspflicht unbedingt beibehalten, dieses bürokratische Monster, wie es der Kollege Veit in der vergangenen Legislatur richtigerweise bezeichnet hat? Eine �oberprüfung von Hunderttausenden Lebensläufen wird damit verewigt. Selbst nach Angabe von Innenminister de Maizière werden 90 Prozent aller sogenannten Optionskinder beide Staatsangehörigkeiten behalten können. Warum dann diese Schikane? Ich sage es Ihnen: Die Optionspflicht gilt nicht für Kinder von EU-Bürgern oder Schweizern. Im Wesentlichen ist die Optionspflicht eine Diskriminierung von Kindern türkischer Eltern in Deutschland.
## ( Beifall bei Abgeordneten der LINKEN und des B�oNDNISSES 90/DIE GR�oNEN )
## Sie ist in Gesetz gegossener Rassismus. Auch deshalb muss der Optionszwang dringend weg.

## Im Koalitionsvertrag von Schwarz-Grün steht:
## Auf bundespolitischer Ebene werden wir die Aufhebung der Optionspflicht und die Akzeptanz von Mehrstaatigkeit im Staatsangehörigkeitsrecht für in Deutschland geborene und aufgewachsene Kinder ausländischer Eltern unterstützen.
## Selbstverständlich haben die Wählerinnen und Wähler und auch viele Betroffene gehofft, dass damit auf Bundesebene klare Kante gezeigt wird. Jetzt wollen sie sich enthalten. Gerade das macht die Entscheidung für die Betroffenen so bitter.
## ( Beifall bei der LINKEN )
## Au�Yerdem zeigt es, dass die Geister, die Roland Koch 1999 im Hessen-Wahlkampf mit seiner Unterschriftenkampagne gegen die doppelte Staatsbürgerschaft rief, immer noch spuken und wirksam sind. Leider ist der faule Kompromiss nicht der einzige, den die Gro�Ye Koalition fabriziert hat. Die Gro�Ye Koalition hat ausdrücklich vereinbart, dass es zu keiner Erleichterung der Einbürgerung kommt und dass es für Migranten auch weiterhin keine doppelte Staatsbürgerschaft und auch nicht die notwendige Reform des auf dem Blutsprinzip beruhenden Staatsbürgerschaftsrechtes geben wird.
## Die Linke fordert, Einbürgerungen endlich zu erleichtern, das Wahlrecht für alle, die mehr als fünf Jahre hier leben, einzuführen und die doppelte Staatsbürgerschaft für alle Migranten zu ermöglichen. Ich sage Ihnen: Die Integrationsverweigerer sitzen hier auf der Regierungsbank. Zeigen Sie den jungen Menschen aus Migrationsfamilien endlich, dass sie hier willkommen sind - ohne Wenn und Aber.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for LINKE between 2012 and 2016


## Sevim Dagdelen (UNDEF) 2014-06-05 LINKE

p <- partition("GERMAPARL", speaker = "Sevim Dagdelen", date = "2014-06-05", encoding = "UTF-8")

read(p)

## Sevim Dagdelen (UNDEF) (LINKE)

## Ich werde der SPD keinen Koalitionsvertrag vorlegen, in dem die doppelte Staatsbürgerschaft nicht drin ist.
## Dies erklärte der Vorsitzende der SPD und jetzige Vizekanzler Sigmar Gabriel auf dem SPD-Parteitag - nach den Bundestagswahlen, vor dem Koalitionsvertrag - am 2. November 2013.
## Im Vorfeld, im Bundestagswahlkampf, ging es vor allen Dingen auch darum, Wählerinnen - und Wählerstimmen unter Migrantinnen und Migranten zu bekommen. So suchte man die Nähe zu Migrantenselbstorganisationen und warb um die Unterstützung bei der Wahl. Das konkrete Versprechen lautete: Man wird sich für die Rechte der Migrantinnen und Migranten, besonders die der Türkinnen und Türken, einsetzen. Was steht jetzt im Koalitionsvertrag? Darin steht nichts von doppelter Staatsangehörigkeit und nichts von der Abschaffung der Optionspflicht. Darin steht:
## Wer in Deutschland geboren und aufgewachsen ist, soll seinen deutschen Pass nicht verlieren und keiner Optionspflicht unterliegen.
## Wie befürchtet - von unserer Seite, aber auch von vielen Migrantinnen und Migranten -, entpuppte sich der Kompromiss im Koalitionsvertrag von CDU, CSU und SPD als faul; denn was die Formulierung " in Deutschland geboren und aufgewachsen " bedeutet, machte im Februar dieses Jahres Bundesinnenminister Thomas de Maizière deutlich: Entfallen solle die Optionspflicht bei denjenigen, die bis zu ihrem 23. Lebensjahr zwölf Jahre hier gelebt haben, davon mindestens vier Jahre zwischen ihrem 10. und 16. Lebensjahr. Nachgewiesen werden könne dies anhand von Meldebescheinigungen, alternativ reiche auch ein deutscher Schulabschluss.
## Bereits seit Jahren wird der bürokratische Aufwand - man nennt es auch Bürokratiemonster - bei den Op-tionspflichtigen in den Einbürgerungsbehörden kritisiert. Gerade dieser enorme Bürokratieaufwand hat drei von der SPD mitregierte Länder eine Initiative in den Bundesrat einbringen lassen, mit der die generelle Abschaffung der Optionspflicht gefordert wird.

## Noch im April, also vor zwei Monaten, hatten viele Organisationen und Verbände den SPD-Vorsitzenden Sigmar Gabriel in einem offenen Brief aufgefordert, gegenüber den Unionsparteien an der vollständigen Abschaffung der Optionspflicht im Staatsangehörigkeitsrecht festzuhalten und Wort zu halten. Doch auch dieser Appell blieb leider ohne Erfolg. So ist der vorliegende Gesetzentwurf kümmerlich geblieben; denn herausgekommen ist ein kleingeistiger, engstirniger, ja ein fauler Kompromiss zwischen den Koalitionsfraktionen.
## ( Swen Schulz [ Spandau ] [ SPD ]: Aber ein Fortschritt! )
## In Deutschland aufgewachsen und von der Optionspflicht befreit ist nach dem vorliegenden Gesetzentwurf, wer bei Vollendung seines 21. Lebensjahres mindestens acht Jahre in Deutschland lebt, sechs Jahre lang eine Schule in Deutschland besucht hat, einen deutschen Schulabschluss oder eine abgeschlossene Berufsausbildung hat. Falls kein Antrag der betroffenen Person vorliegt, prüft die Behörde nach dem 21. Geburtstag die Voraussetzungen von Amts wegen.

## Es ist wirklich absurd und nur mit ideologischer Borniertheit zu erklären, dass an diesen Zehntausenden Optionsverfahren pro Jahr festgehalten werden soll - ab 2018 etwa 40 000 im Jahr -, nur damit am Ende einigen wenigen Menschen der Doppelpass vorenthalten werden kann.
## ( Beifall bei der LINKEN )
## So bleibt es bei diesem Wahnsinn der Optionspflicht in Deutschland, einer weltweit wirklich einmaligen Regelung. Die völlig gleichberechtigte Zugehörigkeit, also die deutsche Staatsbürgerschaft, hier geborener Kinder wird in einer oft ohnehin schwierigen Lebensphase - das müsste hier eigentlich jeder wissen - infrage gestellt. Künftig wird es - so das Gesetz - Deutsche nach Absatz 1 des § 29 Staatsangehörigkeitsgesetz geben, das bedeutet nichts anderes als Deutsche zweiter Klasse.
## Meine Damen und Herren, insbesondere türkische Migrantinnen und Migranten fühlen sich erneut vor den Kopf gesto�Yen; denn Kinder mit einer deutsch-EU - oder deutsch-schweizerischen Doppelstaatsangehörigkeit sollen künftig generell nicht mehr optieren müssen. Man sieht: Was für sehr viele gilt, gilt nicht für türkische - Migrantinnen und Migranten. Sie müssen nachweisen, dass sie wirkliche, tatsächliche Deutsche sind, wenn sie ihren Doppelpass behalten wollen. Dieser diskriminierende Effekt ist etwas, was wir abschaffen wollen.
## ( Beifall bei der LINKEN )
## Diese Diskriminierungen müssen aus Sicht der Linken ein Ende haben. Deshalb fordern wir Sie auf: �-ffnen Sie die Fenster, schaffen Sie endlich die Optionspflicht bedingungslos ab, und akzeptieren Sie auch endlich etwas, was mittlerweile zum Normalzustand in der Europäischen Union gehört, nämlich die doppelte Staatsbürgerschaft!

## Es gibt eine Mehrheit im Deutschen Bundestag und auch im Bundesrat für die bedingungslose Abschaffung dieser wirklich unsäglichen Optionspflicht. Lassen Sie uns gemeinsam diesen Schritt gehen, und lassen Sie uns sagen: Diese wahnsinnige, weltweit einmalige Regelung gibt es in Deutschland nicht mehr, wir sind für ein fortschrittliches Staatsbürgerschaftsrecht, wir sind für die Abschaffung der Optionspflicht. Lassen Sie uns gemeinsam dieses Zeichen setzen für Integration,


## debates on dual citizenship between 2012 and 2016

debates_dual2[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for LINKE between 2012 and 2016


## Sevim Dagdelen (UNDEF) 2014-07-03 LINKE

p <- partition("GERMAPARL", speaker = "Sevim Dagdelen", date = "2014-07-03", encoding = "UTF-8")

read(p)

## Sevim Dagdelen (LINKE)

## Herr Innenminister de Maizière, das Gesetz, das Sie hier vorgelegt haben, ist weder ein gro�Yer Schritt, noch ist es ein gutes Gesetz; es ist eine wirklich kleingeistige �"nderung des bestehenden Staatsangehörigkeitsgesetzes. Es ist nichts weiter als Murks. Es ist eigentlich ein Armutszeugnis, dass auch diese Koalition es nicht geschafft hat, die unsägliche Optionsregelung tatsächlich ersatzlos abzuschaffen -
## ( Beifall bei der LINKEN sowie des Abg. �-zcan Mutlu [ B�oNDNIS 90/DIE GR�oNEN ] )
## und das nur, weil Sie aus der Union ideologisch borniert an dem längst überholten Dogma der Vermeidung von Mehrstaatigkeit in diesem Land festhalten. Allein deshalb werden ab dem Jahr 2018 etwa 40 000 Optionsverfahren pro Jahr durchgeführt werden müssen. 40 000 Optionsverfahren jährlich! Was, wenn nicht ein Bürokratiemonster, ist das bitte schön, meine Damen und Herren?

## Ich finde es wirklich unsäglich, wenn man, wie bei der ersten Beratung des Gesetzentwurfs im Parlament, auch noch wahrheitswidrig behauptet, dass mit dem Gesetz die Optionspflicht abgeschafft werden würde. Das ist schlicht falsch, und das wissen Sie auch.
## Die Optionspflicht bleibt im Grundsatz in diesem Gesetz enthalten. Natürlich kann die Optionsregelung auch künftig dazu führen, dass hier geborene Kinder ihre deutsche Staatsangehörigkeit im Erwachsenenalter wieder verlieren. Ich bitte Sie deshalb, redlich zu sein und bei den Fakten zu bleiben. Sagen Sie den Leuten klar, was Sie hier machen! Sie verhindern nämlich dauerhaft die doppelte Staatsbürgerschaft als Regel.
## ( Beifall bei der LINKEN - Johannes Kahrs [ SPD ]: Das ist doch Unsinn! )
## Wenn Sie die Abschaffung der Optionspflicht tatsächlich wollen, müssten Sie den § 29 des Staatsangehörigkeitsgesetzes komplett abschaffen.

## Das Gute ist: Sie würden damit auch das erreichen, was Sie schon in der ersten Beratung versprochen haben: Sie würden sozusagen eine rechtlich verbindliche Regelung für all die Menschen schaffen, die die deutsche Staatsangehörigkeit infolge des Optionsmodells bereits verloren haben. Die Zahl dieser Menschen steigt von Tag zu Tag. Diese Menschen darf man nicht vage auf irgendwelche Ermessensspielräume im geltenden Recht verweisen, wie Sie es machen.

## Neben vielen Betroffenen wären auch die Mitarbeiterinnen und Mitarbeiter der Einbürgerungsbehörden dankbar für eine konsequente Abschaffung der Optionspflicht. Herr Bundesinnenminister - ich muss Sie enttäuschen -, die Sachverständigenanhörung in der letzten Woche, bei der ich anwesend war, hat ergeben, dass die Arbeitszeit, die für die jährlich etwa 40 000 Optionsverfahren aufgewendet werden muss, weitaus besser für eine Verkürzung der viel zu langen Einbürgerungsverfahren genutzt werden könnte.

## Die Erleichterungen bei der Optionspflicht wollen Sie nur dann beschlie�Yen,
## ( Zuruf von der SPD: Nur dummes Geschwätz! )
## wenn im Gegenzug Verschlechterungen im Asylrecht im Bundesrat eine Mehrheit finden. Geben Sie es doch zu! Wir haben darüber doch debattiert. Wir finden das Verfahren zum Thema Staatsangehörigkeitsrecht einfach unwürdig. Deshalb haben wir Ihnen zwei Anträge vorgelegt: einen Gesetzentwurf, unterstützt von drei SPD-regierten Ländern
## ( Zuruf des Abg. Johannes Kahrs [ SPD ] )
## - Sie können dem zustimmen und damit ein gemeinsames Zeichen setzen für gleiche Rechte, gegen Ausgrenzung und tatsächlich für die Abschaffung der Optionspflicht -, und einen Antrag, in dem wir umfangreiche Vorschläge für ein fortschrittliches Staatsangehörigkeitsrecht gemacht haben. Ich denke, es ist wichtig, die Optionspflicht abzuschaffen. Aber es ist auch wichtig und richtig, Einbürgerungen zu erleichtern. Auch das ist eine Erkenntnis aus der Sachverständigenanhörung.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[5]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for LINKE between 2012 and 2016


## Sevim Dagdelen (UNDEF) 2016-12-16 LINKE

p <- partition("GERMAPARL", speaker = "Sevim Dagdelen", date = "2016-12-16", encoding = "UTF-8")

read(p)

## Sevim Dagdelen (LINKE)

## Herr Tauber, wer hier ideologische Scheuklappen hinsichtlich des Themas doppelte Staatsbürgerschaft trägt, das sind Sie. Das haben Sie gerade mit Ihrer Parteitagsrede statt einer Bundestagsrede bewiesen.

## Vor 18 Jahren fand im Bundesland Hessen, wo ich zu dem Zeitpunkt studiert habe, der bisher geschmackloseste und auch gefährlichste Wahlkampf in der Geschichte des Bundeslandes Hessen statt. Ihr Spitzenkandidat, der später aufgrund eines Wahlkampfes gegen den Doppelpass Ministerpräsident wurde, hat auf dem Rücken von Migrantinnen und Migranten Stimmung gemacht. Die Menschen sind zu den CDU-Infoständen in den Fu�Ygängerzonen gegangen und haben gefragt: Wo kann ich gegen Ausländer unterschreiben?
## ( Tankred Schipanski [ CDU/CSU ]: Nein! " Gegen den Doppelpass unterschreiben! " )
## Sie wollten nicht etwa gegen die doppelte Staatsbürgerschaft oder etwas �"hnliches unterschreiben. Die CDU hat das bewusst kalkulierend, also aus wahltaktischen Gründen, gemacht. Der Beschluss des Essener Bundesparteitages, die doppelte Staatsbürgerschaft abzulehnen, die sogenannte Optionspflicht wieder einzuführen, ist nichts anderes als " Roland Koch reloaded ". Das ist ein Weg zurück in die Vergangenheit, und wir wollen dies nicht mitmachen.

## Deshalb sage ich Ihnen auch noch einmal - ich kann es nicht oft genug betonen -: Dieser Beschluss zur Optionspflicht gibt ein abweisendes Signal an die Hunderttausenden jungen Deutschen, deren Eltern aus der Türkei nach Deutschland eingewandert sind; denn diese Gruppe wird vor allem betroffen sein, und das wissen Sie auch. Das wurde in den Reden auf dem Parteitag ja auch gesagt. Sie sagen nämlich: Ihr seid Deutsche nur auf Probe und Bürger zweiter Klasse.
## ( Zuruf von der CDU/CSU: Dummes Zeug! ) Das hat eine ausgrenzende statt eine integrierende Wirkung.


## ...


## Ulla Jelpke (LINKE)

## Lassen Sie mich grundsätzlich noch einmal festhalten: Die Abschaffung der Optionspflicht war ein wichtiger Schritt zur Integration von jungen Migranten in unserer Gesellschaft. Ich denke, das dürfen wir uns von der CDU/CSU nicht kaputtmachen lassen.
## Wenn es nach der Union geht, soll ein Mensch nur einen Pass besitzen dürfen, gleichsam als Test seiner Loyalität zu Deutschland. Das ist ein Denkansatz, der in der Tat aus dem vorigen Jahrhundert stammt. Loyalität zu einer Gesellschaft kann man eben nicht an der Frage der Staatsbürgerschaft messen. Die Wiedereinführung der Optionspflicht würde einen Generalverdacht gegenüber den hier geborenen Kindern bedeuten, denen Sie damit das Signal geben würden: Ihr gehört nur zu uns, wenn ihr euch für einen deutschen Pass entscheidet, andernfalls bleibt ihr dauerhaft Fremde. - Damit wird Integration erschwert, und das ist genau der falsche Ansatz.

## Meine Damen und Herren, wir können nach dem CDU-Parteitag feststellen, dass führende Vertreter der Union vom Parteitagsbeschluss abrücken. Das ist eine erfreuliche Nachricht. Andererseits muss man sagen: Den Leitantrag muss man genau lesen; denn der Beschluss zur Optionspflicht ist noch lange nicht das Schlimmste gewesen, was auf dem CDU-Parteitag beschlossen wurde. Man muss hier sehr deutlich sagen: Diese rassistische Rhetorik, die Sie für den Wahlkampf 2017 in diesem Antrag ankündigen, ist wirklich unerträglich.

## Der Antrag enthält Forderungen, die darauf hinauslaufen, das Asylrecht bis zum Gehtnichtmehr zu verschärfen: Ausweitung der Abschiebehaft, rücksichtslose Abschiebung; wir haben hier eben eine Debatte über Abschiebungen nach Afghanistan geführt. Im Leitantrag der Union wird sogar gesagt, dass es eine nationale Kraftanstrengung geben soll, um diese Dinge umzusetzen. Ich denke, genau mit dieser Rhetorik gie�Yen Sie Wasser auf die Mühlen von Rechtsextremisten, und Sie untergraben zugleich die Aufnahme - und Hilfsbereitschaft in Deutschland, die viel grö�Yer ist, als Sie hier unterstellen.



## debates on dual citizenship between 2012 and 2016

debates_dual2[[6]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for LINKE between 2012 and 2016


## Sevim Dagdelen (UNDEF) 2013-06-05 LINKE

p <- partition("GERMAPARL", speaker = "Sevim Dagdelen", date = "2013-06-05", encoding = "UTF-8")

read(p)

## Sevim Dagdelen (UNDEF) (LINKE)

## Herr Wolff, wer an Ideologie leidet, sieht man an den Aussagen Ihres Herrn Staatssekretärs Schröder
## ( Hartfrid Wolff [ Rems-Murr ] [ FDP ]: Sie kennen doch Ideologie! )
## und an Ihrer unentwegten Abneigung gegenüber Menschen mit Migrationshintergrund, die sich eine erleichterte Einbürgerung in Deutschland wünschen. Das ist Ideologie!

## Wir sprechen heute über zwei Themen, die meines Erachtens zusammenpassen und sehr viele Gemeinsamkeiten haben: zum einen über das Staatsangehörigkeitsrecht - Stichwort " Optionszwang " -, zum anderen über das deutsch-türkische Assoziationsrecht. Auf den ersten Blick sind dies verschiedene Themen, aber beide verbindet meines Erachtens der Aspekt der gezielten Ungleichbehandlung von Migrantinnen und Migranten und ganz besonders von türkischen Staatsangehörigen in Deutschland. Sie, liebe Kolleginnen und Kollegen, sind Weltmeister im Einfordern von Integration, aber Sie schaffen nicht die Rahmenbedingungen, die es Menschen in Deutschland ermöglichen, sich zu integrieren.

## - Ja, Sie haben es richtig gehört: Es geht um die türkenfeindlichen Aspekte in den entsprechenden Debatten. Denn es werden insbesondere die Rechte von türkischen Staatsangehörigen verletzt; bei der Optionspflicht sind es sogar 70 Prozent.

## Die Quote für die Akzeptanz der Mehrstaatigkeit bei Einbürgerungen beträgt bei nicht türkischen Staatsangehörigen etwa 59 Prozent, bei türkischen Staatsangehörigen liegt sie bei nur 27 Prozent. Das hei�Yt, die Mehrstaatigkeit bei nicht türkischen Staatsangehörigen in Deutschland wird doppelt so häufig akzeptiert wie bei türkischen Staatsangehörigen.

## Die Linke fordert die Abschaffung des Optionszwangs. Einbürgerungen müssen massiv erleichtert werden, und die Mehrfachstaatsangehörigkeit muss generell akzeptiert werden. Im Hinblick auf das Assoziationsrecht fordert die Linke nichts anderes, als dass die Bundesregierung die Rechtsstaatlichkeit nicht mehr mit Fü�Yen tritt und die Rechte vor allen Dingen türkischer Arbeitsmigranten und ihrer nachfolgenden Generationen endlich anerkennt. Die Linke jedenfalls steht an der Seite der Migrantinnen und Migranten, besonders an der Seite der türkischen Staatsangehörigen, die von dieser Bundesregierung immer wieder diskriminiert werden.


