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

coi_spd00 <- partition("GERMAPARL",
                       party = "SPD",
                       year  = 1996:2000,
                       interjection= F,
                       role = c("mp", "government"))


## as partition bundles

pb1 <- partition_bundle(coi_spd00, s_attribute = "date")


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

debates_foreign1 <- debates1[[ subset(dt1, TOTAL >= 25)[["partition"]] ]]


## debates on Foreigners� Policy between 1996 and 2000

debates_foreign1[[27]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 27th debate on Foreigners� Policy for SPD between 1996 and 2000 ## 28 does not belong to Foreigners� Policy

warnings()


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

debates_citizen3[[19]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 19th debate on citizenship between 1996 and 2000 for SPD


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
  ) # vary 1st to 7th debate on dual citizenship between 1996 and 2000 for SPD


## Michael B�rsch 1999-03-19 SPD

p <- partition("GERMAPARL", speaker = "Michael B�rsch", date = "1999-03-19", encoding = "UTF-8")

read(p)

## Michael B�rsch (SPD)

## Die Reform des Staatsangehörigkeitsrechts ist nicht nur für das Gelingen der Ausländerintegration von zentraler Bedeutung. Sie steht auch für die Reform - und Zukunftsfähigkeit der Politik insgesamt. - Diese richtige Feststellung stammt von den Kollegen Altmaier und Röttgen aus der Unionsfraktion. Ich stimme dem nachdrücklich und uneingeschränkt zu. Wir befassen uns heute in der Tat mit einem Modellprojekt für die Reformfähigkeit unserer Gesellschaft.

## ich plädiere für eine Form des politischen Streits, die dem Ernst des Themas und seiner gesellschaftlichen Bedeutung Rechnung trägt. Lassen Sie uns vom heutigen Tage an engagiert, aber sachlich, Herr Rüttgers, leidenschaftlich, aber tolerant über das Staatsangehörigkeitsrecht streiten!
## ( Beifall bei der SPD und dem B�oNDNIS 90/DIE GR�oNEN )
## So mü�Yten im Grunde auch Sie von der CDU/CSU denken; denn Sie haben öffentlich erklärt und so Ihren Antrag begründet, bei der Reform des Staatsangehörigkeitsrechts handle es sich um ein höchst sensibles Thema. Wie wahr!

## Nach dem Motto: Wo bleibt das Positive?, möchte ich zunächst herausstellen, worüber wir uns beim Thema Staatsangehörigkeitsrecht im Prinzip einig sind. Fraktionsübergreifende �obereinstimmung besteht darüber, da�Y das veraltete Reichs - und Staatsangehörigkeitsrecht von 1913 dringend reformbedürftig ist. Unbestritten ist auch die Notwendigkeit, den dauerhaft in Deutschland lebenden Menschen umfassende politische Teilhabe zu ermöglichen.
## Schon 1984 war die damalige Bundesregierung der Auffassung: Kein Staat kann es auf Dauer hinnehmen, da�Y ein zahlenmä�Yig bedeutender Teil der Bevölkerung über Generationen hinweg au�Yerhalb der staatlichen Gemeinschaft und au�Yerhalb der Loyalitätspflichten ihm gegenübersteht.
## Schlie�Ylich kann niemand in diesem Hause und in öffentlichen Diskussionen ernsthaft bestreiten, da�Y bei einer Zahl von über 7 Millionen Ausländern nur verstärkte Integrationsbemühungen den sozialen Frieden in Deutschland sichern können.
## Was sind nun die entscheidenden Fortschritte im vorgelegten Staatsangehörigkeitsrecht? Innenminister Schily wird am Ende der Debatte in seinem Beitrag noch ausführlich erläutern, was Neues in dem Entwurf steht. Vor allem mit der Einführung des Territorialprinzips und der deutlichen Verkürzung der Einbürgerungsfristen erreichen wir wichtige Verbesserungen. Auch für Vertriebene und Aussiedler gibt es Vereinfachungen. Auf die Einführung des Territorialprinzips oder Jus soli haben wir Sozialdemokraten sehr lange gewartet, um genau zu sein: 86 Jahre lang. Schon 1913 kämpften wir - damals erfolglos - für dessen Einführung. Der sozialdemokratische Abgeordnete Landsberg prophezeite damals:

## Auf jeden Fall können wir am Ende dieses Jahrhunderts nun endlich damit beginnen, unsere Vorstellungen von einem modernen Staatsangehörigkeitsrecht zu verwirklichen. An zwei Punkten entzündet sich die augenblickliche Debatte besonders: am sogenannten Optionsmodell und an der Hinnahme doppelter Staatsangehörigkeit.
## Zum Optionsmodell. Wir sind der Meinung, da�Y der vorgelegte Entwurf mit der Verfassung, insbesondere mit Art. 16, vereinbar ist. Zu dieser rechtspolitischen Frage wird meine Kollegin Christine Lambrecht noch im einzelnen Stellung nehmen
## Auch viele Mitglieder der Unionsfraktion sind offenbar - wie wir - von der Verfassungsmä�Yigkeit der Optionslösung überzeugt; denn nur so ist es zu erklären, da�Y über ein Drittel der Unionsabgeordneten auf einer Fraktionssitzung im Januar für das Optionsmodell votiert haben. Es gibt bei der CDU sogar einen sehr prominenten Kronzeugen, der sich schon 1993 bei einem Besuch der Türkei für das Optionsmodell ausgesprochen hat. Herr Rüttgers, Herr Marschweski, es handelt sich um Helmut Kohl, den Altkanzler. Er sagte damals bei einem Türkeibesuch: Dieses Optionsmodell ist in Ordnung. Für fünf Jahre sollen die jungen Leute die Möglichkeit für die doppelte Staatsangehörigkeit haben.

## In Ihrer Fraktion gibt es ja nachhaltige Stimmen auch für das Optionsmodell. Geben Sie die Abstimmung frei; dann hätten wir am Ende nämlich tatsächlich die breite parlamentarische Mehrheit, die auch Sie immer gefordert haben.
## ( Beifall bei Abgeordneten der SPD und des B�oNDNISSES 90/DIE GR�oNEN )
## Zum zweiten Streitpunkt, der Frage der Doppelstaatlichkeit. Die Notwendigkeit, Doppelstaatlichkeit zumindest in bestimmten Fällen hinzunehmen, wird im Grundsatz von allen Fraktionen anerkannt. Der vorliegende Gesetzentwurf ergänzt nun in einigen Punkten - sehr zurückhaltend - die bereits existierenden Möglichkeiten zur Hinnahme von Mehrstaatlichkeit, unter anderem für junge Menschen während einer Optionszeit von fünf Jahren.
## Besonders dringlich ist es, der ersten Ausländergeneration, die wir als Arbeitskräfte ins Land geholt haben und die hier seit vielen Jahren integriert ist, volle Bürgerrechte zu gewähren und die Einbürgerung zu erleichtern. Darauf haben zum Beispiel die evangelische und die katholische Kirche zu Recht hingewiesen.
## Emotionale Barrieren beim Verzicht auf die alte Staatsangehörigkeit sollten wir dabei nicht als Mi�Ytrauensbeweis und Zeichen von Illoyalität werten. Für viele lange hier lebende Ausländer wird die Aufgabe der alten Staatsangehörigkeit als Bruch mit der eigenen Kultur, als Lösung von früheren menschlichen und familiären Bindungen empfunden. Solchen emotionalen und psychologischen Aspekten müssen wir bei der Gesetzgebung Rechnung tragen.

## Namentlich bei der Frage der Hinnahme von Doppelstaatlichkeit hätten wir uns bekanntlich eine etwas weniger engherzige Lösung gewünscht. Aus meiner Sicht gibt es auch nach wie vor keinen durchschlagenden sachlichen Grund, die Hinnahme von Mehrstaatlichkeit zu diskreditieren:
## Bereits heute wird in der Bundesrepublik Deutschland die doppelte Staatsangehörigkeit akzeptiert,. ohne da�Y dies zu gravierenden praktischen, juristischen oder politischen Problemen geführt hätte.

## Diese wunderbar klarsichtige Formulierung stammt nicht aus der SPD, sondern ist Originalton F.D.P. Bereits im April 1993 hat die F.D.P.-Fraktion einen Gesetzentwurf befürwortet, '' " der die Aufgabe der bisherigen Staatsangehörigkeit nicht mehr verlangt '' .
## Im übrigen waren es Union und F.D.P. selbst, die die doppelte Staatsbürgerschaft seit 1990 in einer Weise gesetzlich ermöglicht haben, da�Y bei rund einem Drittel der Eingebürgerten die Beibehaltung ihrer alten Staatsbürgerschaft zugelassen wird. Allzugern verschwiegen wird auch, da�Y die über 2 Millionen Doppelstaatler in Deutschland, unter ihnen honorige Lehrerinnen und Lehrer, Verwaltungsbeamte und Polizisten,
## ( Marieluise Beck [ Bremen ] [ B�oNDNIS 90/DIE GR�oNEN ]: Abgeordnete! - Erwin Marschewski [ CDU/CSU ]: 2 Millionen ist falsch! 500 000, nicht 2 Millionen! Das ist Quatsch! )
## tagtäglich die unproblematische Handhabung von Mehrstaatlichkeit vorleben.

## Nehmen Sie als Beispiel die überaus beliebte niederländische Königin Beatrix. Sie besitzt nicht eine, nicht zwei, nicht drei, sie besitzt vier Staatsbürgerschaften, neben der niederländischen auch die deutsche, die englische und die kanadische; man höre und staune.

## Auch käme niemand auf die Idee, Herr Zeitlmann, einem Bayern vorzuhalten, er könne nicht gleichzeitig auch ein guter Deutscher und ein guter Europäer sein.


## ...


## Hans-Peter Kemper (SPD)

## Herr Kollege Zeitlmann, Sie haben in Ihrer Rede erstens behauptet, wir würden Kriminelle einbürgern. Ich weise Sie darauf hin, da�Y Sie wider besseres Wissen mehrere Dinge unterstellt haben, die so von uns in keiner Weise angedacht worden sind. Das wissen Sie ganz genau. Denn wir schlie�Yen die Einbürgerung von Kriminellen bzw. von Extremisten aus.
## Zweitens haben Sie in Ihrer Darstellung eine infame Unterstellung begangen. Denn Sie haben den Eindruck erweckt, als ob ausländische Mitbürger deutlich krimineller wären als vergleichbare deutsche Gruppen. Sie wissen ganz genau, da�Y die ausländische Bevölkerung, die sich seit langem in der Bundesrepublik aufhält und arbeitet, nicht krimineller ist als vergleichbare deutsche Gruppen.
## Sie wissen auch ganz genau, da�Y die Kriminalitätsbelastung im wesentlichen auf die einreisenden organisierten Kriminellen und auf die ausländerspezifischen Straftaten, die die Deutschen gar nicht begehen können, zurückzuführen ist. Ich halte es für sehr bedauerlich, da�Y Sie in einer solchen Rede, vor einem solchen Publikum diese unwahren Behauptungen wiederholen.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on dual citizenship between 1996 and 2000 for SPD


## Otto Schily 1999-05-07 SPD

p <- partition("GERMAPARL", speaker = "Otto Schily", date = "1999-05-07", encoding = "UTF-8")

read(p)

## Otto Schily (SPD)

## Die frühere Regierungskoalition hat 16 Jahre darüber debattiert, allerdings ohne Ergebnis.
## Das Thema ist bis ins kleinste Detail ausdiskutiert worden. Mittlerweile dauert diese Debatte so lange, da�Y einige in der Opposition ihre früheren Argumente bereits vergessen haben.

## Gewi�Y war das kein einfacher Gesetzgebungsproze�Y. Das kann bei einem so schwierigen Thema auch gar nicht anders sein. Das Ergebnis, das wir heute vermutlich auf breiter Grundlage beschlie�Yen werden, ist ein Kompromi�Y. Ein Kompromi�Y - das ist das Kennzeichen eines Kompromisses - lä�Yt natürlich auf der einen oder anderen Seite Wünsche offen. Ich sage in allem Freimut, da�Y ich den Gesetzentwurf, den ich im Januar vorgelegt habe, für den konsequenteren Entwurf halte.

## Das ist ein ganz wichtiger Reformschritt, den wir heute vollziehen, der durchaus historische Dimensionen hat.

## Als Beleg dafür kann ich eine sachverständige Persönlichkeit zitieren, die Ihrer Partei, der CDU, angehört
## ( Erwin Marschewski [ CDU/CSU ]: Wer ist das? )
## und die den ersten Entwurf aus ihrer Sicht kritisiert hat, aber den Kompromi�Y, den wir heute vorlegen, mit folgenden Worten kommentiert:
## Das ist keine kleine Reform, sondern eine gro�Ye Reform. Wir haben das Staatsangehörigkeitsrecht um das Territorialelement ergänzt, das es vorher nicht gab. Das ist ein gro�Yer Modernisierungsschritt. Er ist richtig, weil er eine elegante, unbürokratische und integrative Form der Zugehörigkeit bietet.

## Die einen sagen, es war der sozialdemokratische Innenminister Zuber, die anderen sagen, es war der freidemokratische Justizmi - nister Caesar, und wieder andere sagen, es war der F.D.P.-Abgeordnete Westerwelle, der das Optionsmodell in die Debatte gebracht hat.

## Der Kollege Altmaier hat im Oktober 1995 Grundsätze zu einem neuen Staatsangehörigkeitsrecht veröffentlicht, in denen das Optionsmodell benannt wird. Dort hei�Yt es sehr zutreffend:
## Ohne die soziale und rechtliche Integration der auf Dauer in Deutschland lebenden Ausländer droht eine nachhaltige Gefährdung des gesellschaftlichen Friedens. Wir sehen CDU und CSU in einer besonderen Verantwortung, diese Herausforderung anzunehmen. Dabei kommt der Reform des Staatsangehörigkeitsrechts eine wichtige Bedeutung zu.

## Altmaier im Oktober 1995. - Wie wahr, wie wahr!
## Dies hat er - wie übrigens auch in dem gerade angesprochenen Dokument - in einem Interview in der '' " Frankfurter Rundschau '' vom 11. Dezember 1995 noch einmal präzisiert. Da sagte er:
## Ich verspreche mir immer noch am meisten von dem sogenannten Optionsmodell, das ich gemeinsam mit den Kollegen Eckart von Klaeden und Norbert Röttgen vorgelegt habe. Danach erwirbt das Kind, sofern die Eltern nicht widersprechen, mit der Geburt zusätzlich zur Staatsbürgerschaft der Eltern auch die deutsche Staatsangehörigkeit.
## Er beschreibt dort also, da�Y das Optionsmodell seine Zielsetzung ist. Jetzt fände ich es angemessen, da�Y die betreffenden Kolleginnen und Kollegen auch dazu stehen.
## Auch Frau Süssmuth hatte im Juli 1998 folgendes zu sagen: Gerade für Kinder und Jugendliche könnte die doppelte Staatsangehörigkeit eine unterstützende Hilfe zur Integration sein. Frau Süssmuth wörtlich:
## Wir brauchen ein Staatsangehörigkeitsrecht, bei dem das Abstammungsprinzip und das Territorialprinzip in eine ausgewogene Balance gestellt werden.
## Um die bestehenden Urheberrechte klar zur Geltung zu bringen, möchte ich sehr ausführlich und in vollem Bewu�Ytsein den Reformaufruf zitieren, den einige der genannten Kollegen veröffentlicht haben. Da hie�Y es:
## Die soziale und rechtliche Integration der in Deutschland lebenden ausländischen Mitbürger ist eine moralische Verpflichtung gegenüber den Betroffenen und unverzichtbar für die dauerhafte Bewahrung des gesellschaftlichen Friedens.
## Der Schaffung eines zeitgemä�Yen Staatsangehörigkeitsrechtes kommt dabei eine zentrale Bedeutung zu. Als Volkspartei, die dem christlichen Menschenbild und den Grundwerten von Freiheit, Solidarität und Gerechtigkeit verpflichtet ist, steht die CDU in einer besonderen Verantwortung.

## Das Thema Integration, das hier zu verhandeln ist, ist wahrlich ernst. Sie haben damals formuliert: Es geht um den gesellschaftlichen Frieden. Es geht um einen gro�Yen Teil der Wohnbevölkerung, der auf Dauer bei uns lebt und leben wird. Das können Sie nicht rückgängig machen und wollen es hoffentlich auch nicht. Wenn Sie es rückgängig machen wollten, müssen Sie das hier vorne sagen. Da Sie es aber nicht rückgängig machen können, müssen wir das tun, was uns auch das Bundesverfassungsgericht als Gebot auferlegt hat: Wir müssen dafür sorgen, da�Y Staatsvolk und Wohnbevölkerung zusammenkommen. Das ist für die Festigkeit unserer Gesellschaft notwendig.

## Das müssen wir uns als Zukunftsprognose vor Augen führen. Sie müssen versuchen, Ihr Vorstellungsvermögen so weit zu entwickeln, da�Y Sie beide Entwicklungen vergleichen, die in Gang gesetzt werden, wenn wir auf die Reform des Staatsangehörigkeitsrechts verzichten oder wenn wir sie vollziehen. Ich glaube, wenn wir es bei dem Status quo belassen, dann werden wir eine zunehmende Entfremdung der Zuwanderer haben, dann werden wir eine Abkehr der Jugendlichen, die in solchen Familien aufwachsen, von der Gesellschaft haben, wir werden eine zunehmende Gettoisierung haben, wir werden zunehmende Parallelgesellschaften haben - wir brauchen also diese Brücke in eine neue Entwicklung, die es uns ermöglicht, diese Menschen in die Gesellschaft hineinzunehmen und sie nicht davor stehen zu lassen.

## Eine Politik, die die Einheit einer Nation mit rassischen Argumenten betreibt, gründet also auf einer Chimäre; sie würde die europäische Zivilisation zugrunde richten.
## Wie wahr hat Renan damals gesprochen.
##'' " Eine Nation ist auch nicht identisch mit der Sprache '' - selbst das sagt er mit Recht. Er sagt: Sonst wären die Vereinigten Staaten und Gro�Ybritannien heute noch zusammen, sonst wären Spanien und Südamerika noch zusammen. Selbst die Sprache ist nicht unbedingt ein Einheitsband. Die Schweiz ist eine Nation mit verschiedenen Sprachen.
## Auch die Religion - sagt er - ist es nicht, was eine Nation ausmacht. Es sind auch nicht die Interessen. Er sagt: '' " Ein Zollverein ist kein Vaterland. '' Zur Geographie sagt er:
## Es gibt keine willkürlichere, gefährlichere Theorie, als die Nation zwischen '' " natürlichen Grenzen '' errichten zu wollen; die Vergangenheit zeigt, da�Y die Lebensräume der Nationen immer fluktuiert haben.

## Meine Damen und Herren, eine homogene Gesellschaft ist, entgegen allen verbreiteten Vorurteilen, nicht tragfähig, weil sie ein Konstrukt ist, das sich nicht mit der Wirklichkeit in Einklang bringen lä�Yt.


## ...


## Otto Schily (SPD) aswering Scholz (CDU)

## Die Frage ist mir sehr willkommen, Herr Kollege Scholz, weil sie genau im Duktus meiner weiteren Ausführungen liegt. Ich bin der Meinung: Wir müssen uns in der Tat darauf einlassen, zu fragen, wie wir unsere künftige Gesellschaft gestalten wollen und wie das Verhältnis von Staat und Gesellschaft aussehen soll.
## Wir müssen im zusammenwachsenden Europa begreifen, da�Y sich Nationen, Kulturen, Ethnien und Sprachfamilien anders begegnen können als unter dem homogenen Nationalstaat, der ein Irrtum des vorigen Jahrhunderts war, der übrigens auch am Ende des ersten Weltkriegs ein Irrtum war, wie in den 14 Punkten Wilsons deutlich wird.
## Auf Grund der guten Erfahrungen mit der Integration Otto von Habsburgs hält es die Staatsregierung für vertretbar, bei Persönlichkeiten, die einen vergleichbaren Bezug zur deutschen und europäischen Geschichte aufweisen, Doppelstaatsangehörigkeit hinzunehmen.

## Meine Damen und Herren, ich bin ein überzeugter Demokrat. Ich mu�Y Ihnen sagen: Die Doppelstaatsangehörigkeit nur als Adelsprivileg zuzulassen widerspricht meinen Grundüberzeugungen.

## Meine Damen und Herren, heute ist die Stunde der modernen Demokratie. Deshalb ist es vielleicht nicht so ganz angebracht, da�Y ich mich dauernd mit Bezügen zum Adel aufhalte. 
## Friedrich der Gro�Ye hat, als er gefragt wurde, ob ein Katholik - im damals protestantischen Preu�Yen - das Bürgerrecht erwerben dürfe, geantwortet:
## Alle Religionen seindt gleich und guht, wann nur die Leute, so sie profesieren, erliegte Leute seindt, und wenn Türken und Heiden kämen und wollten das Land pöplieren, so wollten wir sie Mosqueen und Kirchen bauen.
## Das ist eine gute Devise auch für unser Staatsangehörigkeitsrecht.


## ...


## Cornelia Sonntg-Wolgast (SPD) asking R�ttgers

## Herr Kollege Rüttgers, da Sie sich soeben dagegen verwahrten, irgendwelche Beziehungen, Vergleiche oder Bezüge zwischen unserer heutigen Diskussion um die Reform des Staatsangehörigkeitsrechts und dem Kosovo-Konflikt herzustellen: Wie finden Sie es, da�Y Kollegen aus Ihren Reihen, nämlich der Unionsfraktionen, in den letzten Wochen noch einmal mit Verve gefordert haben, man solle wegen der aktuellen Diskussion um das Kosovo und wegen der Diskussion um Flüchtlingsaufnahme die Reform des Staatsangehörigkeitsrechts, die meiner Meinung nach auf einem völlig anderen Blatt steht, jetzt zurückziehen und zunächst einmal ruhen lassen? Finden Sie nicht, da�Y �"ngste in der Bevölkerung vor Zuwanderungsströmen wieder dadurch geschürt worden sind, da�Y man zwei grundverschiedene Themen durcheinandergeworfen hat?


## ...


## Otto Schily (SPD) answering Altmaier (CDU)

## Herr Altmaier, Sie wissen doch ganz genau, da�Y wir uns in der vorangegangenen Legislaturperiode auf der Basis des Optionsmodells hätten einigen können, wenn Sie nicht der Gefangene von Herrn Kanther gewesen wären. Sie konnten sich doch untereinander nicht einigen und haben sich nicht getraut, einen entsprechenden Entwurf vorzulegen. Sie sollten die Dinge hier nicht verdrehen.

## ( Wolfgang Zeitlmann [ CDU/CSU ]: Lassen Sie uns doch über das Optionsmodell reden! )
## - Nein, ich bin für eine weitergehende Lösung. Das ist völlig richtig, Herr Zeitlmann. Wenn wir die Mehrheit dafür weiterhin gehabt hätten, wäre ich auch dabei geblieben - das sage ich ganz offen und ehrlich -, nicht zuletzt deshalb, weil wir uns jetzt einigen Verwaltungsaufwand einhandeln. Ich mu�Y mich aber jetzt auf das zubewegen, was mehrheitsfähig ist. Das ist auch in Ordnung; in der Politik ist das manchmal so.

## Weil Sie das Thema Doppelpa�Y angesprochen haben: Ich darf Sie bitten - das meine ich sehr ernst -, zur Kenntnis zu nehmen, da�Y es mir wahrlich nicht um die Herbeiführung möglichst vieler doppelter Staatsbürgerschaften geht. Das ist nicht unser Ziel. Ich bin sogar der Meinung, da�Y doppelte Staatsbürgerschaften vermieden werden sollten. Nur will ich daran erinnern, da�Y für uns Integration wichtiger als die Vermeidung der Mehrstaatigkeit ist.
## ( Beifall bei der SPD und dem B�oNDNIS 90/DIE GR�oNEN ) Was Sie Otto von Habsburg zubilligen,
## nämlich eine Mehrstaatigkeit, das sollten Sie auch dem einfachen türkischen Mitbürger zubilligen. Sie tun es ja auch, wenn auch beschränkt auf eine gewisse Dauer, im Rahmen des Optionsmodells.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on dual citizenship between 1996 and 2000 for SPD


## Lilo Friedrich 1999-11-04 SPD

p <- partition("GERMAPARL", speaker = "Lilo Friedrich", date = "1999-11-04", encoding = "UTF-8")

read(p)

## Lilo Friedrich (SPD)

## In acht Wochen tritt das neue Staatsbürgerschaftsrecht in Kraft. Mit dieser Reform wird endlich ein deutliches Zeichen für die Weltoffenheit und Modernität unseres Landes gesetzt, ein Land, das der Integration unserer ausländischen Mitbürgerinnen und Mitbürger einen hohen Stellenwert einräumt.
## Die Kernpunkte sind hinlänglich bekannt: Die Einbürgerungsfristen werden verkürzt, und für Härtefälle wird eine verbesserte Ausnahmeregelung bei der Hinnahme von Mehrstaatigkeit geschaffen. Dies hat zum Ziel, da�Y sich zwischen der in Deutschland lebenden ausländischen Bevölkerung und dem deutschen Staatsvolk nicht eine immer grö�Yer werdende Lücke bildet.

## Nach vielen politischen Auseinandersetzungen hat der Bundestag mit Zustimmung des Bundesrates das Gesetz zur Reform des Staatsangehörigkeitsrechtes beschlossen. Nun gilt es, diese gesetzlichen Vorgaben in eine praxiserleichternde Einbürgerung umzusetzen. Aufgabe hierbei ist es, die Richtlinien für den Verwaltungsvollzug so zu gestalten, da�Y das neue Gesetz seiner Aufgabe und Zielsetzung, insbesondere der Integration, gerecht werden kann.

## Zur Begründung: Mit dem neuen Staatsbürgerschaftsrecht wird die Einbürgerung vieler Antragsteller erleichtert, die Probleme mit den ausländischen Behörden bei ihren Entlassungsbemühungen erfahren. Für die deutschen Einbürgerungsbehörden ist jedoch die Beurteilung der Einbürgerungsvoraussetzungen, die eine Hinnahme von Mehrstaatigkeit ermöglichen, in manchen Fällen besonders schwierig. So ist die Verwaltungspraxis einiger ausländischer Staaten, zum Beispiel Iran oder Bundesrepublik Jugoslawien, nicht immer nachvollziehbar.

## Mit der von uns eingeleiteten Reform des deutschen Staatsbürgerschaftsrechts wird die Bedeutung des Schlu�Yprotokolls für die Einbürgerung von Iranern weiter abnehmen. Denn die Zeit des Inlandsaufenthaltes, die für einen Einbürgerungsantrag erforderlich ist, unterliegt laut mehreren Entscheidungen des Bundesverwaltungsgerichtes nicht dem Schlu�Yprotokoll. Sie wird durch das neue Staatsbürgerschaftsrecht nahezu halbiert werden, das hei�Yt auf künftig acht Jahre. - Das ist die eine bedeutende Verbesserung.
## Die zweite besteht darin, da�Y das neue Staatsbürgerschaftsrecht hinsichtlich des Grundsatzes der Vermeidung von Mehrstaatigkeit eine erhebliche Erweiterung des Ausnahmekatalogs vorsieht.
## Das Festhalten am Grundsatz der Vermeidung von Mehrstaatigkeit bewirkt zwar weiterhin, da�Y der Ablauf der Einbürgerungsverfahren ausländischer Staatsangehöriger in hohem Ma�Ye vom Recht und der Behördenpraxis des jeweiligen Herkunftsstaates abhängt. Hier können jedoch zahlreiche Schwierigkeiten auftreten: zum Beispiel die faktische Unmöglichkeit, das Ausscheiden aus der ausländischen Staatsangehörigkeit herbeizuführen, willkürhafte oder unangemessene Anforderungen des Herkunftsstaates im Entlassungsverfahren, eine vorangegangene diskriminierende oder entwürdigende Behandlung des Einbürgerungsbewerbers, eine überlange Verfahrensdauer, überhöhte Entlassungsgebühren, erhebliche Nachteile als Folge des Ausscheidens aus der ausländischen Staatsbürgerschaft oder eine vorangegangene politische Verfolgung.
## Die Entscheidung über eine Einbürgerung mu�Y sich meines Erachtens vorrangig an den Gesichtspunkten orientieren, die zwischen dem Einbürgerungsbewerber und der Bundesrepublik Deutschland als dem aufnehmenden Staat von Bedeutung sind. Daher ist es bei auftretenden Schwierigkeiten ausländsicher Staatsangehöriger insbesondere aus dem Iran und der Bundesrepublik Jugoslawien im Entlassungsverfahren geboten, den Grundsatz der Vermeidung von Mehrstaatigkeit zurückzustellen, wenn diese Schwierigkeiten das im Einzelfall zumutbare Ma�Y überschreiten. Somit kommt dem § 87 des neuen Ausländergesetzes, der die Ausnahmefälle regelt, in denen Mehrstaatigkeit hingenommen wird, entscheidende Bedeutung zu.

## Au�Yerdem wird die Einbürgerungsbehörde künftig erstmals in die Lage versetzt, besondere Schwierigkeiten bei älteren Einbürgerungsbewerbern zu berücksichtigen. Des weiteren kann ein Einbürgerungsanspruch festgestellt werden, wenn dem Ausländer bei der Aufgabe der ausländischen Staatsangehörigkeit erhebliche Nachteile - zum Beispiel vermögens - oder erbrechtlicher Art - entstehen. Ferner wird Mehrstaatigkeit hingenommen, wenn '' " der ausländische Staat die Entlassung aus der Staatsangehörigkeit aus Gründen versagt hat, die der Ausländer nicht zu vertreten hat '' . Dies ist häufig bei �"rzten oder sonstigen Fachkräften der Fall.
## Bei jugoslawischen Einbürgerungsbewerbern treten besondere Schwierigkeiten bei Staatsangehörigen der Bundesrepublik Jugoslawien, das hei�Yt bei solchen aus Serbien und Montenegro, auf. Vielfach erfolgt die Einbürgerung unter Hinnahme von Mehrstaatigkeit, weil die Entlassungsgebühren unzumutbar hoch sind.
## Daneben gibt es auch hier Fallgruppen, in denen weitere Entlassungsbemühungen als unzumutbar anzusehen sind und Mehrstaatigkeit hingenommen werden sollte. Dies gilt unter anderem bei Einbürgerungsbewerbern, die bereits vor den Kriegsereignissen einen vollständigen und formgerechten Antrag auf Entlassung aus der jugoslawischen Staatsangehörigkeit gestellt haben und deren Entlassungsantrag aus von ihnen nicht zu vertretenden Gründen nach zweijährigen Entlassungsbemühungen nicht weiter bearbeitet wird, sowie in solchen Fällen, in denen bereits die Entgegennahme des vollständigen und formgerechten Entlassungsantrags durch den ausländischen Staat trotz mehrfacher ernsthafter und nachhaltiger Bemühungen des Einbürgerungsbewerbers über einen Zeitraum von sechs Monaten hinweg nicht erfolgt ist.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on dual citizenship between 1996 and 2000 for SPD


## debate 4 doesn�t refer to citizenship


## debates on dual citizenship between 2012 and 2016

debates_dual1[[5]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on dual citizenship between 1996 and 2000 for SPD


## Fritz Rudolf K�rper 1996-02-08 SPD

p <- partition("GERMAPARL", speaker = "", date = "1996-02-08", encoding = "UTF-8")

read(p)

## Fritz Rudolf K�rper (SPD)

## Die Diskussion über die Neuregelung des Staatsangehörigkeitsrechtes tritt schon ziemlich lange auf der Stelle, eigentlich viel zu lange. Spätestens seit der Wiedervereinigung Deutschlands am 3. Oktober 1990 ist die Notwendigkeit einer zeitgemä�Yen Reform des deutschen Staatsangehörigkeitsrechtes unbestritten.

## Mit der gesetzlichen �oberleitung der Statuseigenschaft in die deutsche Staatsangehörigkeit werden Hunderte, Tausende von Einbürgerungsverfahren entbehrlich, so da�Y die Einbürgerungsbehörden entlastet werden und die verbleibenden Einbürgerungsverfahren wesentlich zügiger als bisher abgewickelt werden können. Ich begrü�Ye ausdrücklich den unter der Federführung des Landes Schleswig-Holstein entstandenen Gesetzesentwurf dazu.

## Gegen die vermehrte Hinnahme von Mehrstaatigkeit werden immer wieder zwei Argumente vorgebracht. Zum einen wird eingewandt, Doppelstaatler hätten auch Rechte und Pflichten. Das trifft in der Praxis auch zu. Pflichten und Rechte sind nicht nur naturgegeben, sondern können durch internationale �obereinkommen und Gesetze geregelt werden. So gibt es zum Beispiel Absprachen innerhalb der NATO, die die Fragen der Wehrpflicht regeln.
## Als zweites Gegenargument gegen unsere Vorschläge ist der Loyalitätsgesichtspunkt beliebt. Ich teile nicht die Bedenken gegen Doppelstaatler aus Gründen der Loyalität. Der oft zitierte Satz, man könne nicht gleichzeitig zwei Herren dienen,
##  Erwin Marschewski [ CDU/CSU ]: Sehr wahr! )
## lä�Yt sich nicht auf heutige Verhältnisse anwenden. ( Erwin Marschewski [ CDU/CSU ]: Weder zwei Herren noch zwei Frauen! )
## Die Auffassung, Staatsloyalität gebiete zwingend nur eine Staatsangehörigkeit, stammt aus dem 19. Jahrhundert, in dem der Bürger als Untertan des Staates gesehen wurde. In unserem heutigen modernen Staatswesen hat sie keine Berechtigung mehr.
## ( Beifall bei der SPD ) Im übrigen, lieber Kollege Marschewski, die Hunderte, Tausende von Doppelstaatlern in der Bundesrepublik Deutschland zeigen uns täglich, wie unsin - nig die Behauptung ist, sie seien keine loyalen Staatsbürger.


## debates on dual citizenship between 2012 and 2016

debates_dual1[[6]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on dual citizenship between 1996 and 2000 for SPD

## no ref to dual citizenship



## debates on dual citizenship between 2012 and 2016

debates_dual1[[7]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on dual citizenship between 1996 and 2000 for SPD


## Willfried Penner 1997-10-30 SPD

p <- partition("GERMAPARL", speaker = "Willfried Penner", date = "1997-10-30", encoding = "UTF-8")

read(p)

## Willfried Penner (SPD) asking Belle (CDU)

## Herr Kollege Belle, manches lä�Yt sich ja nachvollziehen, wenn man Ihren Standpunkt zugrunde legt. Aber könnten Sie mir vielleicht dabei behilflich sein zu verstehen, was Sie denn mit den 1,8 Millionen in der Bundesrepublik schon lebenden Doppelstaatlern anstellen wollen?


## Meinrad Belle (CDU) aswering

## Lieber Kollege Penner, Sie sollten und wir müssen alle zur Kenntnis nehmen, da�Y es natürlich ein ganz wesentlicher Unterschied ist, ob jemand die Doppelstaatsbürgerschaft durch Geburt erreicht, weil eines der Elternteile die deutsche Staatsangehörigkeit hat. Hier ist eine direkte, unmittelbare Verbindung gegeben.
## ( Kerstin Müller [ Köln ] [ B�oNDNIS 90/DIE GR�oNEN ]: Das ist ja wohl ein Witz! )
## Hier ist die Integration von vornherein gegeben. In einem solchen Fall sind da keine Probleme zu erwarten.
## Uns geht es eben darum, mit der Staatsbürgerschaft den Schlu�Ypunkt für eine gelungene Integration zu setzen. Darin liegt der wesentliche Unterschied.


## Schily (SPD) asking Belle (CDU)

## Herr Kollege, ich gehe davon aus, da�Y Sie sehr für einen Austausch zwischen den Vereinigten Staaten von Amerika und Deutschland sind. Das führt mitunter dazu, da�Y eine Familie zeitweise Aufenthalt in den Vereinigten Staaten von Amerika nimmt, und mitunter fügt es sich, da�Y ein Kind einer deutschen Familie dann in Amerika geboren und auf diese Weise Doppelstaatler wird. Hat dieses Kind dann Probleme in der Integration, wenn es in seine Heimat zurückkehrt?


## Belle (CDU) aswering Schily (SPD)

## Natürlich nicht.
## ( Otto Schily [ SPD ]: Danke! - Zuruf vom B�oNDNIS 90/DIE GR�oNEN: Wieso nicht? )
## Das ist doch gar keine Frage.
## ( Erwin Marschewski [ CDU/CSU ]: Das ist doch nicht jemand aus der Türkei, der sich abschottet, der nichts mit dieser Gesellschaft zu tun haben will! )
## Wir müssen das doch bitte, meine Damen und Herren, von dem Fall unterscheiden, da�Y zum Beispiel ein türkischer Staatsbürger, der bei uns in Deutschland lebt und dessen Kinder hier geboren werden, gar keine Voraussetzungen, gar keine Bereitschaft zu einer Integration zeigt. Das mu�Y doch klar und deutlich gesagt werden.
## Es ist doch ein ganz wesentlicher Unterschied, ob die doppelte Staatsbürgerschaft aus einer binationalen Ehe entsteht oder automatisch zugestanden wird, einfach weil das Kind eines türkischen Ehepaares bei uns in Deutschland wohnt. Hier haben wir doch wirklich ganz praktische, tatsächliche Unterschiede, die man einfach zur Kenntnis nehmen mu�Y, auch wenn Sie das nicht wollen.


## ...


## Sonntag-Wolgast (SPD) speech

## Jetzt appelliere ich an die Reformgegner in der CDU/CSU: Holen Sie bitte - Herr Belle, das gilt auch für Sie - das Thema '' " doppelte Staatsangehörigkeit '' einmal von dem hohen Sockel herunter, auf den Sie es selber gestellt haben!
## ( Meinrad Belle [ CDU/CSU ]: Sie machen das doch! Sie setzen das doch auf den hohen Sockel! )
## Gehen Sie doch einmal nach Lebenswirklichkeit! Folgen Sie doch einfach der Erkenntnis, da�Y wir fast 2 Millionen hier eingebürgerte Doppelstaatler haben!
## ( Erwin Marschewski [ CDU/CSU ]: Das ist zuviel! Das mu�Y reduziert werden! )
## Und die Welt ist in der Bundesrepublik noch immer nicht aus den Fugen geraten. Ich höre aus Ihren Kreisen nicht Zeter und Mordio, da�Y die meisten davon deutschstämmige Aussiedler sind. Es klappt sogar mit der Loyalität.

