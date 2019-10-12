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

coi_greens16 <- partition("GERMAPARL",
                          parliamentary_group = "GRUENE",
                          year  = 2012:2016,
                          interjection= F,
                          role = c("mp", "government"))


## as partition bundles

pb2 <- partition_bundle(coi_greens16, s_attribute = "date")


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

debates_foreign2[[112]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 112th debate on Foreigners� Policy for GRUENE between 2012 and 2016

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


debates_citizen4 <- debates2[[ subset(dt4, TOTAL >= 10)[["partition"]] ]]


## citizenship debates between 2012 and 2016

debates_citizen4[[11]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 9th debate on citizenship for GRUENE between 2012 and 2016


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

debates_dual2 <- debates2[[ subset(dt6, TOTAL >= 10)[["partition"]] ]]


## debates on dual citizenship between 2012 and 2016

debates_dual2[[1]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for GRUENE between 2012 and 2016


## Renate K�nast 2013-06-05 GRUENE

p <- partition("GERMAPARL", speaker = "Renate K�nast", date = "2013-06-05", encoding = "UTF-8")

read(p)

## Renate K�nast (Gr�ne) asks Grindel

## Was wollten Sie uns eigentlich damit sagen, als Sie im Hinblick auf die Türkei feststellten, sie gehöre nicht zu Europa, und einen Zusammenhang zur Staatsbürgerschaft herstellten?


## ...


## Renate K�nast (GRUENE)

## Alle reden immer über Integrationspolitik. Schauen wir einmal, was darunter verstanden wird. Ich denke, nachhaltige Integrationspolitik sollte für eines sorgen, nämlich dass Zuwanderer am Ende schnellstmöglich Deutsche werden können und werden wollen.
## ( Reinhard Grindel [ CDU/CSU ]: Nein, sie sollen integriert sein! Das ist es! )
## - Beides gehört dazu. Es wäre ja ein wei�Yer Schimmel, wenn Integrationspolitik hei�Yen würde, dass man integriert ist. Ich meine, das Ziel - man muss ja eine Vorstellung davon haben - von Integrationspolitik muss sein, diesen Weg zu eröffnen, dass man möglichst schnell die Staatsangehörigkeit haben kann und auch den Wunsch entwickelt, sie zu haben.

## Das grüne Motto ist jedenfalls, eine Perspektive auf die Staatsangehörigkeit zu haben. Ich will klarstellen, dass das natürlich Verantwortung auf beiden Seiten hervorruft: zum einen die Verantwortung von Staat und Gesellschaft, von denen, die schon hier sind und hier leben, jedem Menschen Teilhabe zu ermöglichen und ihn aufzunehmen, jedem Menschen die Chance zu geben, Teil zu sein bzw. zu werden und die Chance auf sozialen Aufstieg zu geben. Zum anderen haben die Zuwanderer eine spiegelbildliche Aufgabe, nämlich einen eigenen Beitrag zu leisten, um tatsächlich teilzuhaben.

## Was uns immer noch fehlt, ist, anzuerkennen, dass zu dieser Vielfalt auch Mehrstaatigkeit gehört. Wir haben ein komisches System: Auf der einen Seite akzeptieren wir Mehrstaatigkeit bei US-Bürgern, wir akzeptieren Mehrstaatigkeit bei ungefähr 2 Millionen EU-Bürgern, wir akzeptieren Mehrstaatigkeit bei circa 3 Millionen Spätaussiedlern und Spätaussiedlerinnen. Gleichzeitig haben wir ein Staatsangehörigkeitsgesetz, das Mehrstaatigkeit bei vielen jungen Leuten, die hier geboren und aufgewachsen sind, nicht zulässt. Dabei sind sie Deutsche und fühlen sich auch so, oder sie fühlen sich zumindest wie die Mitglieder des Vereins DeuKische Generation, weil sie eben auch andere Wurzeln und Bezüge haben. Das " D " steht aber vorne: DeuKische Generation. Das ist doch eine Identifikation. Unsere Frage ist daher: Warum zwingen wir diese jungen Deutschen eigentlich, sich zu entscheiden, ob sie diesen oder jenen Pass haben wollen? Dieser Optionszwang ist ein politischer Fehler; darum geht es heute.
## ( Beifall beim B�oNDNIS 90/DIE GR�oNEN und bei der SPD sowie bei Abgeordneten der LINKEN )
## Wir machen Menschen, die zum Gro�Yteil sogar hier geboren sind, also geborene Deutsche sind, zu Ausländern in ihrem eigenen Land. Das ist doch Irrsinn. Derzeit sind 300 000 junge Deutsche dem Optionszwang, sich zwischen der einen und der anderen Staatsangehörigkeit zu entscheiden, unterworfen. 70 Prozent von ihnen haben türkische Wurzeln. Das zeigt uns, dass dieser Optionszwang zielgerichtet in eine Richtung ausgeübt wird. Meine Damen und Herren, was für ein Bild vermitteln wir eigentlich diesen jungen Leuten?
## Bis 2017 gibt es jährlich 3 000 bis 7 000 optionspflichtige Menschen. Ab 2018 werden es sogar noch deutlich mehr sein. Zwei Drittel aller Optionspflichtigen sagen, sie würden gerne den Doppelpass haben, und warten auf eine neue Mehrheit im Deutschen Bundestag. Ich glaube, heute und hier ist der Tag gekommen -
## ( Serkan Tören [ FDP ]: Das stand nicht in der Studie drin! )
## - Dass Sie jetzt als weltläufige FDP hier einen Zwischenruf machen, ist putzig. Aber gut.

## Lassen Sie uns mit einem neuen Staatsangehörigkeitsrecht den Leuten die Chance geben, ein Teil von uns zu sein und sich in dieser Frage nicht mehr zwangsweise entscheiden zu müssen. Ich glaube, zu einem modernen Land gehört der Respekt davor, dass Menschen, die hier geboren und aufgewachsen sind, ihren Teil zur Struktur des Landes beigetragen haben. Damit alle die gleichen Chancen bekommen,
## ( Beifall beim B�oNDNIS 90/DIE GR�oNEN sowie bei Abgeordneten der SPD )
## sollten wir am heutigen Tag - dazu fordere ich Sie auf - den Optionszwang abschaffen. Davon profitieren nicht nur die, die jetzt unter Druck stehen. Davon werden am Ende ganz Deutschland und Europa profitieren. Darum geht es heute.


## ...


## Volker Beck (Gr�ne)

## Herr Kollege Schröder, Sie haben eben gesagt, die Optionspflicht richte sich vor allen Dingen gegen die Menschen, die aus der Türkei stammen. Das ist folgerichtig. Wir dagegen sind wie die USA und andere Mitgliedstaaten der Europäischen Union
## ( Claudia Roth [ Augsburg ] [ B�oNDNIS 90/DIE GR�oNEN ]: Polen! )
## der Auffassung, dass das Bekenntnis zu Deutschland nicht dadurch infrage gestellt wird, dass die Menschen ihren alten Pass, den ihrer Eltern und Gro�Yeltern, einfach behalten.

## Warum muten wir den Menschen, die aus der Türkei zu uns eingewandert sind, und ihren Kindern, die hier geboren sind, diese Entscheidung zwischen der Tradition ihrer Familie und dem Land, in dem sie leben und leben wollen und das sie als einziges richtig kennen, zu? Warum spalten Sie Familien und treiben die Kinder zu dieser Entscheidung?


## debates on dual citizenship between 2012 and 2016

debates_dual2[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for GRUENE between 2012 and 2016


## Volker Beck 2014-01-16 GRUENE

p <- partition("GERMAPARL", speaker = "Volker Beck", date = "2014-01-16", encoding = "UTF-8")

read(p)

## Volker Beck (GRUENE)

## Dieses Gesetz hatte damals allerdings einen gro�Yen Makel. Den hat uns der Bundesrat, genauer das Land Rheinland-Pfalz und die FDP, eingebracht. Ich bin froh, dass wir jetzt, nachdem die FDP nicht mehr im Haus ist, diese liberale Hinterlassenschaft einmütig dadurch beseitigen wollen, dass wir, wie es die Koalition beschlossen hat, die Optionspflicht abschaffen.

## Der Parlamentarische Staatssekretär Schröder sagte damals: Wir wollen die deutsche Staatsbürgerschaft nicht verramschen. - Die Optionspflicht sei ein Erfolgsmodell, und er sei gegen eine generelle Hinnahme von Mehrstaatlichkeit. Der Kollege Grindel sagte, wer Ja zu Deutschland sage und gerne hier leben wolle, von dem könne er auch die Entscheidung für die deutsche Staatsbürgerschaft unter Ablegung seiner alten Staatsbürgerschaft erwarten. Gut, dass wir dies nun zumindest bei der Optionspflicht zu den Akten legen.

## Auf den Schreibtischen der deutschen Ausländerbehörden liegen gegenwärtig 5 000 Fälle, in denen wegen der bestehenden Optionspflicht weiterhin der Entzug der deutschen Staatsangehörigkeit droht. Sie wollen diesen Unsinn doch beenden. Aber machen Sie jetzt auch Schluss damit? Sie haben es doch selber in der Hand. Deshalb sagen wir: Wir als Bundestag wollen die Länder auffordern - der Bundesinnenminister könnte das in einer entsprechenden Auslegungsentscheidung mitteilen -, dass jeder, der gegenwärtig eine Beibehaltungsgenehmigung beantragt, sie entweder sofort erhält oder dass man das Verfahren ruhen lässt, bis der Gesetzgeber die Optionspflicht abgeschafft hat.
## ( Beifall beim B�oNDNIS 90/DIE GR�oNEN )
## Der Bundesinnenminister hat mir gestern in der Befragung der Bundesregierung gesagt: Wir werden zum Thema Optionspflicht sehr schnell, ohne schuldhaftes Zögern, einen Gesetzentwurf vorlegen, mit dem die Koalitionsvereinbarung exakt umgesetzt wird. 

## Der Optionszwang war - wir alle sind heute dieser Auffassung - rechtspolitischer, integrationspolitischer Unsinn. Daher darf man diesen Unsinn auch nicht weiter praktizieren, und dann darf man Menschen unter diesem Unsinn nicht weiter leiden lassen. Deshalb fordern wir: Wer jetzt aufgrund der noch fortbestehenden Optionspflicht die deutsche Staatsbürgerschaft verliert oder bereits verloren hat, der muss sie unbürokratisch und gebührenfrei auf Antrag zurückbekommen.

## Das sieht unser Gesetzentwurf bei der Neufassung des § 29 des Staatsangehörigkeitsgesetzes vor.
## Wer infolge des Optionszwangs in der Vergangenheit seine ausländische Staatsangehörigkeit aufgegeben oder verloren hat, der muss die Genehmigung erhalten, sie wieder zu beantragen. Das ist konsequent.

## Wir wollen nicht, dass noch irgendjemand Opfer dieses politischen Nonsens wird. �oberlegen Sie sich einmal: Sie geben mit der Abschaffung des Optionszwangs die Ideologie des Verbots der doppelten Staatsangehörigkeit auf. Bei der Einbürgerung halten Sie allerdings daran fest. Das macht überhaupt keinen Sinn. Wenn man sich den Migrationsbericht, den die Bundesregierung gestern vorgelegt hat, anschaut, dann sieht man: Schon heute ist jede zweite Einbürgerung mit Hinnahme der doppelten Staatsangehörigkeit verbunden. Lassen Sie uns beim Thema Staatsangehörigkeit auch den anderen 50 Prozent sagen: Ja, auch ihr dürft euren alten Pass behalten, wenn ihr Deutsche werden wollt; denn ihr seid uns willkommen. - Beim Thema Willkommenskultur hat dieses Land noch einiges nachzuholen.


## ...


## Mutlu (Gr�ne) speech

## Eine ganz konkrete Frage, Herr Kollege. Sie haben gerade die Punkte Loyalitätskonflikt und Strafverfolgung angesprochen. Ist Ihnen bekannt, dass Deutschland mit 53 verschiedenen Ländern dieser Erde bereits sogenannte Doppelstaatsbürgerschaftsabkommen geschlossen hat? Dabei gibt es keines der Probleme, von denen Sie hier reden. Es gibt niemanden, der sich in einem Loyalitätskonflikt befindet oder der sich der Strafverfolgung entzieht.


## ...

## Ihnen, lieber Herr Kollege Brandt, kann ich nur sagen: Ich hoffe, dass viele Menschen, die aus der Türkei stammen, schon jahrzehntelang in unserem Land leben und längst integriert sind, Ihre Rede nicht gehört haben. Denn mit dieser Rede würden Sie diese Menschen in die Hände von diesem Herrn Erdogan treiben, den Sie hier immer wieder zitieren, wenn Ihnen hinsichtlich der Türkei etwas nicht passt. Mit dieser Rede haben Sie keinen Beitrag dazu geleistet, dass sich diese jungen Menschen endlich zu diesem Land bekennen. Insofern kann ich Ihnen sagen: Sie können viel von Frau �-zo?uz lernen, der ich im �obrigen eine glückliche Hand wünsche, weil sie viel mit Ihnen zu tun haben wird.
## Ich wei�Y zudem nicht, woher Sie die Zahl von 98 Prozent nehmen, die Sie hier genannt haben. Ich würde gerne wissen, ob die 98 Prozent, die angeblich freiwillig die - in Anführungszeichen - " Heimatstaatsbürgerschaft " aufgegeben haben, dies gerne getan haben oder durch den Optionszwang dazu gezwungen waren.

## Deshalb, liebe SPD, haben wir keine Zeit, auf eine Regierungsvorlage zu warten. Sie haben im Wahlkampf auf den Marktplätzen und Stra�Yen versprochen, keinen Koalitionsvertrag zu unterschreiben, in dem die doppelte Staatsbürgerschaft nicht steht. Diesen Anspruch haben Sie aufgegeben. Bleiben Sie doch wenigstens Ihrer eigenen Forderung, das Optionsmodell abzuschaffen, treu. Sorgen Sie dafür, dass die jungen Menschen, die tagtäglich zwangsweise ausgebürgert werden - nach einer Information der Bundesregierung sind es bereits über 200 junge Menschen -, ihre beiden Staatsbürgerschaften zumindest so lange behalten können, bis Ihr neues Gesetz gilt.
## In diesem Sinne appelliere ich an Ihre Vernunft: Springen Sie über Ihren Schatten. Lassen Sie an einer so wichtigen Stelle das Spiel zwischen Opposition und Regierung sein, und stimmen Sie unserem Gesetzentwurf und unserem Antrag zu.


## ...


## Beck (Gr�ne) asking Brandt (CDU)

## Sie haben begründet, warum der Optionszwang eine wunderschöne Sache war. Hei�Yt das, der Satz, wie er im Koalitionsvertrag steht, wird von Ihnen teilweise in Zweifel gezogen? Wollen Sie noch Bedingungen an die Aufgabe des Optionszwanges stellen?


## debates on dual citizenship between 2012 and 2016

debates_dual2[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for GRUENE between 2012 and 2016


## Volker Beck 2014-03-12 GRUENE

p <- partition("GERMAPARL", speaker = "Volker Beck", date = "2014-03-12", encoding = "UTF-8")

read(p)

## Volker Beck (GRUENE)

## Deutschland tut sich mit dem Staatsangehörigkeitsrecht seit jeher schwer. Bis 1999 hat es gedauert, dass wir neben das Blutsrecht, das Ausdruck einer spät gekommenen Nation im Staatsangehörigkeitsrecht war, endlich auch das Geburtsrecht gestellt haben. Dies geschah wegen des Bundesrates damals allerdings zu dem Preis, dass wir die doppelte Staatsangehörigkeit weitgehend vermieden und die Optionspflicht für hier geborene junge Deutsche, die ausländische Eltern haben, eingeführt haben.
## Noch in der letzten Wahlperiode hat die Bundesregierung das Dogma betont, die Vermeidung von Mehrstaatigkeit sei " eines der prägenden Elemente des deutschen Staatsangehörigkeitsrechtes " - so in einer Antwort auf eine Kleine Anfrage meiner Fraktion. Dagegen haben der andere Teil des Hauses und die nicht mehr existente FDP seit Jahren argumentiert und gesagt: Wir müssen bei der Einbürgerung liberalisieren. Wir müssen die Optionspflicht überwinden. - Die SPD hat in ihrem Regierungsprogramm geschrieben:
## Deshalb wollen wir die doppelte Staatsbürgerschaft von Bürgerinnen und Bürgern akzeptieren.

## Dann hie�Y es: Die Optionspflicht wird fallen. - Herr Gabriel sah sich wenigstens in diesem Punkt bestätigt. Ehrlich gesagt, auch ich habe, wie die Sozialdemokraten, den Text Ihres Vertrages so verstanden, dass die - Optionspflicht nun ein für alle Mal Geschichte ist. Da hei�Yt es:
## Für in Deutschland geborene und aufgewachsene Kinder ausländischer Eltern entfällt in Zukunft der Optionszwang und die Mehrstaatigkeit wird akzeptiert.
## Das ist eigentlich eine klare Ansage. Klar war sie bis zu dem Tag, als der Referentenentwurf aus dem Bundesinnenministerium kam, der nicht ein Optionspflichtabschaffungsgesetz ist, sondern ein Optionspflichtverlängerungs - und - komplizierungsgesetz.

## Sie müssen Schluss damit machen - ich glaube, die Menschen drau�Yen im Lande sind es satt, sich das anzuhören -, dass darunter, dass Herr Friedrich geplappert hat und sich wie ein Minister in einer Bananenrepublik benommen hat und Herr Oppermann ausgeplaudert hat, dass er sich wie in einer Bananenrepublik benommen hat,
## ( Dr. Karamba Diaby [ SPD ]: Das ist ein anderes Thema! )
## das Ausländerrecht, die Migranten und die Qualität der Politik für unser Land leiden müssen. Machen Sie Politik für unser Land! Machen Sie es länderfreundlich! Machen Sie es integrationsfreundlich und bürokratiearm! Dann können Sie unseren Gesetzentwurf oder den des Bundesrates zur Grundlage für die Abschaffung der - Optionspflicht nehmen. Das wäre angemessen.


## ...


## Mutlu (Gr�ne) speech

## Eines ist aber erneut klar geworden: Sie von der Gro�Yen Koalition haben weder eine gemeinsame Haltung in dieser wichtigen Frage, noch wissen Sie überhaupt, wohin die Reise geht.
## Das ist ein Problem. Die Wahrheit ist doch: Die SPD konnte sich und kann sich nach wie vor nicht durchsetzen, und die CDU hat ihre weltoffene Maske schnell abgelegt.
## Das, was Sie als Entwurf vorlegen oder demnächst zur Diskussion stellen wollen, ist nicht die Abschaffung des Optionszwangs.
## Im Gegenteil: Sie perfektionieren ihn, indem Sie ihn zum einen mit Attributen versehen, die mehr Bürokratie bedeuten, und zum anderen den betroffenen Jugendlichen sagen: Wir wollen euch schon haben, aber wir wollen auch Hürden. - Genau das ist das Problem, und das machen wir nicht mit.

## Es ist ein Skandal, wie Sie seit Monaten mit diesem gesellschaftlich wichtigen Thema umgehen. Ich finde Ihre Spielchen in dieser Auseinandersetzung einfach beschämend, weil Sie verkennen, dass diese jungen Menschen sich sehr wohl zu diesem Land bekennen können, auch wenn sie die Staatsbürgerschaft der Eltern oder Gro�Yeltern beibehalten.
## Wir reden inzwischen von hybriden Identitäten, und Sie bestehen darauf und verlangen, dass diese jungen Menschen ein einseitiges und alleiniges Bekenntnis zu Deutschland abgeben, im Wissen, wie schwierig das in vielen Fällen ist. Genau das ist das Problem in dieser Debatte.
## Es ist auch beschämend, weil Sie diese Auseinandersetzung auf dem Rücken dieser jungen Menschen austragen, die tagtäglich zwangsweise ausgebürgert werden.
## ( Michael Grosse-Brömer [ CDU/CSU ]: Haben die keine Entscheidungsmöglichkeit? )
## Inzwischen sind schon 400 Menschen per Gesetz ausgebürgert worden. Es geht um 8 500 - das sind im �obrigen Zahlen aus den Statistiken des Bundesinnenministeriums -, die sich in den nächsten zwei Jahren entscheiden müssen. Wir Grünen sagen: Damit muss Schluss sein. Schluss mit diesem Optionszwang, ohne Wenn und Aber!
## Sie reden von Integration - das hat auch Kollege Strobl gemacht -, wollen aber dieses integrationsfeindliche Instrument fortführen und ausbauen. Wir schaffen damit, wenn es - gegen unsere Stimmen - durchkommt, ein Bürokratiemonster, das Geld und Zeit kostet und unnötigen �"rger verursacht.
## Oliver Welke von der heute-show - er ist Ihnen allen bekannt - brauchte nur den Vorschlag von Bundesinnenminister de Maizière vorzulesen und hatte schon die Lacher auf seiner Seite. Aber das Schlimme an dieser Debatte ist, dass es keine Satire ist.
## Der Innenminister meint es ernst. Er will die Optionspflicht abschaffen, hei�Yt es. Ich meine, er will sie nur neu interpretieren. Hier geboren und aufgewachsen muss man dann sein. Aber die Frage, was " aufgewachsen sein " bedeutet, hat uns auch heute niemand beantwortet.
## Wie viele Jahre muss man Luft in Deutschland geatmet haben, damit man tatsächlich deutsch genug ist? Kann man nicht im Ausland aufwachsen und trotzdem wertvoller Teil dieser Gesellschaft sein, vor allem in einem immer stärker zusammenwachsenden Europa?
## Wie lässt sich der Entwurf des Innenministers mit der Freizügigkeit in Europa vereinbaren? Kollege Beck hat es bereits gesagt. Was ist denn, wenn meine Tochter tatsächlich nach Paris geht, dort ihren AbiBac macht und zurückkommt? Dann hat sie keinen deutschen Schulabschluss, und sie darf nicht die doppelte Staatsbürgerschaft behalten. Das ist ein Problem.
## Ein Problem ist es, dass Sie optionspflichtigen Kindern oder Jugendlichen, die einen ausländischen Abschluss machen, dies zum Verhängnis machen. Das passt weder hinten noch vorne zusammen, liebe Kolleginnen und Kollegen. Deshalb werden wir Ihren Entwurf ablehnen.

## Aus diesem Grunde sagen wir, dass die Vorschläge aus den Ländern bzw. die Bundesratsinitiative der Länder Baden-Württemberg, Rheinland-Pfalz und Schleswig-Holstein richtig sind. Begreifen Sie das als eine Unterstützung, liebe Kollegen von der SPD! Nehmen Sie das an, und setzen Sie sich endlich durch! Lassen Sie nicht zu, dass sich ein Herr Strobl und Gleichgesinnte in dieser für unsere Gesellschaft wichtigen Frage durchsetzen.
## Zuletzt möchte ich Herrn Gabriel und Frau �-zo?uz an ihre Versprechen wenige Tage vor dem Mitgliederentscheid der SPD erinnern. Da stand es nämlich klar und deutlich: Der Optionszwang wird abgeschafft. - Da stand nicht: Wir interpretieren das neu.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for GRUENE between 2012 and 2016


## Volker Beck 2014-06-05 GRUENE

p <- partition("GERMAPARL", speaker = "Volker Beck", date = "2014-06-05", encoding = "UTF-8")

read(p)


## Volker Beck (GRUENE)

## Der grö�Yte Schritt war wahrscheinlich 1999 die Reform des Staatsbürgerschaftsrechts.
## ( Christine Lambrecht [ SPD ]: Genau! )
## Neben das ius sanguinis trat das ius soli. Seitdem kann Deutscher werden, wer in Deutschland geboren wurde, auch wenn seine Eltern es beide nicht sind. Inzwischen wächst auch die Gelassenheit, doppelte Staatsbürgerschaften als selbstverständlich hinzunehmen.

## Sie setzen eine Diskriminierungspolitik fort; die schwarze Pädagogik der Integrationspolitik der Union führt die Feder. Für ein kleines Häuflein von Menschen, wie der Deutsche Anwaltverein schreibt, bauen Sie ein bürokratisches Monstrum auf, um den jungen Deutschen, die hier geboren sind, deren Eltern aber aus dem Ausland stammen, weiter zu sagen: Ihr seid Deutsche auf Bewährung. Ihr seid Deutsche mit Verfallsdatum. Ihr seid Deutsche auf Probe. - Das ist das Gegenteil von Willkommenskultur. Deshalb muss die Optionspflicht ganz fallen. Erst das wäre ein richtiger Schritt nach vorne.

## Sie reden sich ja bei allen Problemen auf die Härtefallklausel heraus. Ich habe vorhin schon Herrn Krings gefragt: Was machen wir eigentlich mit Menschen, die mit ihrem deutschen Pass die EU-Freizügigkeit wahrnehmen und, wenn sie im Ausland womöglich noch nicht einmal erfahren haben, dass sie optionspflichtig sind, plötzlich die deutsche Staatsangehörigkeit verlieren und dann Drittstaatausländer in einem anderen europäischen Land sind und sich damit die aufenthaltsrechtlichen Fragen für diese jungen Menschen auf einmal neu stellen? Das zeigt: Ihr Gesetzentwurf ist national gedacht. Sie sind nicht in Europa angekommen.

## Aber welche Gesetzgebung ist das, wo der Bürger nicht wei�Y, unter welche Regelung er fällt, und alle konkreten Einzelfälle unter eine Härtefallklausel fallen, bei der keiner von Ihnen hier sagen kann, was das Ausländeramt damit konkret macht,
## ( Beifall beim B�oNDNIS 90/DIE GR�oNEN sowie bei Abgeordneten der LINKEN )
## und Sie hoffen können, dass das Bundesverwaltungsgericht das irgendwann in zehn Jahren klarstellt? Das ist keine Integrationspolitik. Das ist schlechte Gesetzgebung!
## Sie müssen auch einmal sagen, warum wir bei Kindern zwei Klassen von deutschen Doppelstaatlern haben. Wir haben einerseits die Kinder, von denen beide Elternteile Ausländer sind. Sie werden durch Geburtsrecht Deutsche. Dann haben wir die Kinder von binationalen, also deutsch-ausländischen Ehepaaren, die, weil eine Deutsche oder einer Deutscher ist, sie also eine deutsche Abstammung haben, auch beide Pässe haben. Die kommen für die Optionspflicht freilich nicht infrage. Ich muss Ihnen sagen: Das ist eine ethnische Diskriminierung derjenigen, die keine deutsche Abstammung haben, weil ihnen eine Pflicht auferlegt wird, die für alle anderen Bürgerinnen und Bürger richtigerweise nicht gilt.


## debates on dual citizenship between 2012 and 2016

debates_dual2[[5]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for GRUENE between 2012 and 2016


## Volker Beck 2016-09-23 GRUENE

p <- partition("GERMAPARL", speaker = "Volker Beck", date = "2016-09-23", encoding = "UTF-8")

read(p)

## Volker Beck (GRUENE)

## Wir wollen heute darüber reden, wie wir beim Thema " Integration und Einbürgerung " besser vorankommen. Dazu haben wir zwei Initiativen vorgelegt. Zum einen geht es um eine umfassende Liberalisierung des Staatsangehörigkeitsrechts unter der �oberschrift " Wir wollen mehr Mehrstaatigkeit wagen ", zum anderen wollen wir eine Antwort auf die Auswirkungen der anstehenden Brexit-Verhandlungen auf die britischen Bürger geben.

## Das haben wir in einem Antrag aufgeschrieben. Schon das gegenwärtige Recht erlaubt es, europäische Staatsbürger, die sich kürzer als sechs Jahre hier in Deutschland aufhalten, unter Hinnahme der Doppelstaatigkeit einzubürgern.

## Deshalb schlagen wir Ihnen heute vor, im jetzigen Staatsangehörigkeitsrecht ganz wesentliche Veränderungen vorzunehmen: Wir wollen von dem Prinzip der Vermeidung der Mehrstaatigkeit grundsätzlich abrücken. Wir halten das in einer globalisierten Welt nicht für zeitgemä�Y. Springen Sie über Ihren Schatten! Ein Pass bzw. eine Staatsangehörigkeit ist kein Religionsbekenntnis, sondern die Ermöglichung der gleichberechtigten Teilhabe für die Menschen, die hier arbeiten, leben und Steuern zahlen. 

## Meine Damen und Herren von der Union, wir hatten in diesem Jahr angesichts der Demonstrationen für Erdogan in meiner Heimatstadt Köln eine Diskussion darüber, ob die deutsch-türkischen Doppelstaatler ein Loyalitätsproblem mit unserem Land haben. ( Marian Wendt [ CDU/CSU ]: Gute Frage! ) Ich muss Ihnen sagen: Bei der Anzahl der Doppelstaatler liegen die Menschen aus der Russischen Föderation vorne. Die Türken liegen an dritter oder vierter Stelle bei der Anzahl der Personen, die von der Doppelstaatigkeit Gebrauch machen durften. Wir wissen nicht, wer da demonstriert hat, ob das welche mit türkischem Pass, mit deutschem Pass oder mit einem deutschen und einem türkischen Pass waren, und selbstverständlich muss es doch durchaus auch möglich sein, sich zu den Verhältnissen im Herkunftsland der Eltern politisch zu artikulieren. Wer sich da artikuliert hat und wie sie sich artikuliert haben: Damit habe ich auch einen Dissens. Das gehört aber zu einer Auseinandersetzung in einer demokratischen Einwanderungsgesellschaft dazu. Nicht alle Migranten sind gleich.

## Man muss sich schon einmal die Frage stellen, was geschähe, wenn der Satz richtig wäre, dass jemand, der von woanders herkommt, sich in politische Debatten seiner Herkunftsregion nicht mehr einmischen sollte. Wollen wir allen Ernstes, dass ein deutsch-britischer Doppelstaatler hier in Deutschland nicht dafür wirbt, dass die Entscheidung für den Brexit falsch ist und dass Gro�Ybritannien besser in der Europäischen Union aufgehoben wäre? Wollen wir allen Ernstes einem deutsch-französischen Doppelstaatler sagen, es wäre falsch, dass er seine Regierung unterstützt, wenn sie sich gegen rechtsradikale und antisemitische Politiker in ihrem Lande wendet? Wollen wir einem deutsch-costa-ricanischen Doppelstaatler untersagen, dass er seine Regierung dabei unterstützt, wenn sie sich für Biodiversität und erneuerbare Energien einsetzt? Was wollen wir sagen, wenn ein deutsch-kolumbianischer Doppelstaatler die kolumbianische Regierung unterstützt, wenn diese sich für den Ausgleich und für Friedensverhandlungen mit der FARC einsetzt?


## debates on dual citizenship between 2012 and 2016

debates_dual2[[6]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # # vary 1st to 6th debate on dual citizenship for GRUENE between 2012 and 2016


## �zcan Mutlu 2014-07-03 GRUENE

p <- partition("GERMAPARL", speaker = "�zcan Mutlu", date = "2014-07-03", encoding = "UTF-8")

read(p)

## �zcan Mutlu (GRUENE)

## Herr Minister, letzte Woche war eine junge Frau bei mir, die im August 23 Jahre alt wird und optionspflichtig ist. Sie wird, bevor dieses Gesetz, dieser sogenannte gro�Ye Schritt, von dem Sie reden, in Kraft getreten ist, vermutlich ihre deutsche Staatsbürgerschaft zwangsweise verlieren, weil sie eben ihre beiden Staatsbürgerschaften gerne behalten würde.


## ...


## Volker Beck (GRUENE)

## Frau Präsidentin! Meine Damen und Herren! Wir - beraten heute über das Optionspflichtverlängerungs - und - abschmelzungsgesetz. Es beinhaltet eben nicht die Abschaffung der Optionspflicht,
## ( Rüdiger Veit [ SPD ]: Fast! - Sevim Da?delen [ DIE LINKE ]: Richtig! Genau! )
## obwohl Ihr Parteivorsitzender Ihnen im November letzten Jahres sogar versprochen hat, er unterschreibe nur einen Koalitionsvertrag, der die doppelte Staatsangehörigkeit beinhalte.

## Das Dramatische daran, Rüdiger Veit, sind nicht diese 400 Leute, die übrig bleiben und sich dann optionspflichtig zwischen einem deutschem Pass und dem Pass des Herkunftslandes ihrer Eltern entscheiden müssen. Das Dramatische ist: Wir sagen jungen Deutschen, dass sie nur Deutsche auf Probe sind. Das sagen wir all diesen 40 000 jungen Menschen. Das ist verfehlt. Es gibt keine Deutschen unterschiedlichen Rechts.

## Ich will Ihnen einmal plastisch machen, wie absurd das im Ergebnis ist: José ist in Bolivien geboren. Sein Vater, der Deutscher ist, verlässt die Mutter noch während der Schwangerschaft, erkennt aber die Vaterschaft an. José hat seinen deutschen Vater nie kennengelernt. Er war nie in Deutschland. Er spricht kein Wort Deutsch. Er ist Deutscher und nicht optionspflichtig.
## Veli ist in Köln-Ehrenfeld geboren. Seine Eltern sind 30 Jahre zuvor nach Deutschland eingewandert, aber noch nicht eingebürgert. Nach seinem sechsten Lebensjahr geht seine Familie - der Vater Ingenieur, die Mutter Deutschlehrerin - nach Frankreich, um dort zu arbeiten. Er unterliegt nach Ihrem Gesetz nicht nur der Optionspflicht, sondern er wird wahrscheinlich auch seinen deutschen Pass verlieren, obwohl er - das ist das Absurdeste an Ihrem ganzen Vorhaben - das Recht auf Freizügigkeit innerhalb der Europäischen Union, die ihm als deutschem Staatsbürger zusteht, wahrnimmt und sich in einem anderen Mitgliedstaat der Europäischen Union aufhält.

## Wir sind für die Abschaffung der Optionspflicht ohne Wenn und Aber. Ihre damalige Einführung war ein hoher Preis, um das Geburtsortsprinzip überhaupt ins deutsche Recht übernehmen zu können.

## Wir waren uns einig, dass es der grö�Yte Unfug ist, was wir da im Staatsangehörigkeitsrecht anrichten, und haben immer gehofft, die Optionspflicht zu überwinden.
## ( Rüdiger Veit [ SPD ]: Den Rest beseitigen wir, wenn wir wieder eine rot-grüne Mehrheit haben! )
## Ich will Sie an jene Länder erinnern, die eine ganz andere Rechtskultur haben. Daran hat der Bundespräsident am 22. Mai in seiner gro�Yen Rede zur Einbürgerungsfeier im Schloss Bellevue erinnert. Er hat nämlich gesagt, die Deutschen würden sich gar nicht mehr daran stören, dass man durch Geburt Deutscher wird, auch wenn man ausländische Eltern hat. - Leider ist es noch nicht so weit. Es ist nicht nur die Optionspflicht, die noch besteht; die Eltern müssen hier zudem acht Jahre lang eine Aufenthaltserlaubnis gehabt haben, bevor ihre Kinder überhaupt als Deutsche in diesem Land zur Welt kommen können, unabhängig davon, wie lange sie sich hinterher tatsächlich in diesem Land aufhalten.
