## getting the dictionary samples

# The following workflow creates two partitions from the GermaParl corpus,
# subseted by parties ideological position (left/right or progressive/conservative)
# regarding issuses of national and transnational citizenship.


## load libraries

library("polmineR")
library("magrittr")
library("data.table")

use("GermaParl")


## create partitions

coi_cdu00 <- partition("GERMAPARL",
                     parliamentary_group = "CDU/CSU",
                     year  = 1996:2000,
                     interjection= F,
                     role = c("mp", "government"))


## as partition bundles

pb1 <- partition_bundle(coi_cdu00, s_attribute = "date")


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

q1 <- c('"[Mm]ehrstaat.*"', '".*[Ss]taatsbürger.*"', '".*[Ss]taatsangeh.*rig.*"',
        '".*[Ss]taatszugeh.*rig.*"', '"[Ss]taatenlos.*"', '"[Aa]us.*bürger.*"',
        '"[Ee]in.*bürger.*"', '"Pass"', '"Pa�Y"',
        '"Blutsrecht.*"', '"Geburtsrecht.*"', '"Geburtsprinzip.*"',
        '"[Ii]us"', '"soli"', '"sanguinis"', '"[Jj]us"',
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

debates_foreign1[[57]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 57th debate on Foreigners� Policy for CDU/CSU between 1996 and 2000


## get samples for citizenship

dt3 <- count(debates1,
             query = c(q1, q2),
             regex = T,
             fill = T,
             cqp = T
) %>% setorderv(cols = "TOTAL",
                order = -1L
)

show(dt3)

debates_citizen3 <- debates1[[ subset(dt3, TOTAL >= 25)[["partition"]] ]]


## citizenship debates between 1996 and 2000

debates_citizen3[[1]] %>%
  read() %>%
  highlight(orange = q3_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
            ) # vary 1st to 6th debate on citizenship between 1996 and 2000 for CDU/CSU


## Wolfgang Zeitlmann 1999-03-19 CSU fundamental debate on dual citizenship and the cumpolsory option model and its consequences fro integration

p <- partition("GERMAPARL", speaker = "Wolfgang Zeitlmann", date = "1999-03-19", encoding = "UTF-8")

read(p)

## Wolfgang Zeitlmann (CSU)

## Ich habe mir einmal schriftlich geben lassen, was in den letzten Wochen von namhaften Vertretern der Bundesregierung zum Thema Optionsmodell gesagt worden ist.

## In diesem Fernsehinterview sagte Herr Schröder, eine doppelte Staatsbürgerschaft nur bis zur Volljährigkeit, wie es die F.D.P. vorgeschlagen habe, mache eine Verfassungsänderung nötig.
## Er verwies auf die Bestimmung des Grundgesetzes, nach der eine deutsche Staatsbürgerschaft nicht entzogen werden kann. Diese Bestimmung wolle er nicht ändern; sie sei ein Bollwerk unserer Verfassung.
## Dann gibt es noch die wunderschöne Meldung vom 11. Februar, da�Y Bundesinnenminister Schily dem Info-Radio Berlin gesagt habe, er habe verfassungsrechtliche Bedenken gegen den Vorschlag, Ausländern mit einem Doppelpa�Y, die sich mit 23 Jahren nicht für eine Staatsangehörigkeit entscheiden, den deutschen Pa�Y zu entziehen.

## Sie sagen ganz klar - mit vielen Ausnahmeregelungen -, die Mehrstaatlichkeit solle künftig weit ausgedehnt werden. In einem Absatz hei�Yt es, älteren Bürgern solle die Entscheidung in der Frage der Doppelstaatlichkeit erleichtert werden. Definieren Sie mir einmal, was ein älterer Mensch ist.
## Sie schreiben weiter, vermögensrechtliche und wirtschaftliche Nachteile ( Ludwig Stiegler [ SPD ]: Das steht schon heute im Gesetz! ) sollen zur Möglichkeit der Doppelstaatlichkeit führen. Damit ist für mich klar: Sie legen ein Gesetz vor, in das Sie zwar formal hineinschreiben, die Mehrstaatlichkeit solle vermieden werden, aber Sie schaffen so viele Ausnahmetatbestände, ( Dr. Michael Bürsch [ SPD ]: Die gibt es schon! ) da�Y Sie viele Möglichkeiten eröffnen.

## Es gibt schon derzeit manche Ausnahmeregelungen; das wei�Y auch ich. Wenn Sie hier darauf hinweisen, da�Y es Doppelstaatler gibt, dann ist das unbestritten; das wird durch Wiederholung nicht besser.

## Sie wissen, da�Y es nach den statistischen Zahlen ungefähr 580 000 sind. ( Dr. Michael Bürsch [ SPD ]: Nein, es sind 2 Millionen! ) Aber das ist auch egal. Ich behaupte doch nicht, da�Y jeder, der krank ist, auch schwerkrank sein mu�Y, und ebenso behaupte ich nicht, da�Y jeder Doppelstaatler an sich schon negativ ist. Das hat nie jemand behauptet.
## Aber Sie sagen: Weil es positive Beispiele gibt, machen wir alle zu Doppelstaatlern. Das ist aus Ihrer Diktion hervorgegangen.
## aber irgendwann kämen sie auf ihre Urvorstellungen von genereller doppelter Staatsangehörigkeit zurück. Das können Sie doch nicht bestreiten.

## Sie wollen künftig jedem ausländischen Jugendlichen die doppelte Staatsangehörigkeit geben. ... Ich frage Sie, wie viele Ausländer nach der Kriminalitätsstatistik 
## ... Ich will wissen, ob Sie sich Gedanken darüber gemacht haben, da�Y Sie künftig alle kleinen Mehmets hierbehalten müssen. ( Widerspruch bei der SPD ) Mit der von Ihnen vorgesehenen Regelung müssen Sie diejenigen Menschen, die in diese Gesellschaft absolut nicht passen und alles getan haben, um sich an den Rand dieser Gesellschaft zu begeben, auf Dauer behalten.

## Wenn Sie im Hinblick auf das Thema '' " doppelte Staatsangehörigkeit '' eine Befriedung ernstlich gewollt hätten, dann hätten Sie in Ruhe auch mit uns, mit den Kräften der Opposition, eine gemeinsame, vernünftige Handlungsweise zu finden versucht.

## Herr Kollege Kemper, Sie wissen ganz genau, da�Y Sie hier ein Modell vorlegen - ich habe das eingehend ausgeführt -, mit dem Sie die Konsequenz, nämlich die endgültige doppelte Staatsangehörigkeit, verfassungsrechtlich nicht im Griff haben. Diese Regelung ist nach den Worten Ihres Bundeskanzlers, um es vorsichtig auszudrücken, verfassungsrechtlich bedenklich.
## Wenn Sie die Konsequenz der doppelten Staatsbürgerschaft nicht im Griff haben, dann nehmen Sie in Kauf, da�Y Sie künftig '' " Mehmets '' nicht mehr abschieben können. Das ist Faktum.


## ...

## Wolfgang Bosbach (CDU)

## Im Klartext bedeutet Ihre Argumentation, daß sich die Zahl derjenigen, die deutsche Staatsbürger werden möchten, wesentlich vergrößerte, wenn man die doppelte Staatsbürgerschaft bei Einwanderern hinnehmen würde.
## Ich darf in diesem Zusammenhang aus einer Umfrage der früheren Ausländerbeauftragten, Cornelia Schmalz-Jacobsen, zitieren. In der entsprechenden Drucksache steht wörtlich:
## Diejenigen Befragten, die bislang keine konkrete Absicht haben, sich einbürgern zu lassen�, - das ist der überwiegende Teil; über 90 Prozent derjenigen, die einen Anspruch auf Einbürgerung haben, machen davon keinen Gebrauch -, nennen als Hauptgrund den Wunsch, Türke / Grieche/Italiener/Kroate / Serbe/Bosnier zu blei - ben�
## - Das sind 71 Prozent. - Die Aufgabe der bisherigen Staatsangehörigkeit stellt hingegen für eine weitaus kleinere Gruppe ein Hindernis dar�
## Das sind 18 Prozent.
## Ich akzeptiere und respektiere, wenn drei Viertel der Betroffenen sagen: Mein größter Wunsch ist es, Türke, Serbe, Kroate oder Bosnier bleiben zu wollen. Aber stellt eine solche Haltung für Sie ein ernsthaftes Kriterium dar, diesen Menschen die deutsche Staatsangehörigkeit zu verleihen?
## Zweiter Punkt. Sie haben vorhin gesagt - hier stimme ich Ihnen zu -, das Optionsmodell trage Streit in die ausländischen Familien. Neben Ihnen sitzt der Kollege Beck, der mit mir zusammen an einer Podiumsdiskussion in der Volkshochschule Köln teilgenommen und dort gesagt hat: Das Optionsmodell ist schlecht, weil es in dem Moment Streit innerhalb der ausländischen Familien geben wird, in dem sich der junge Erwachsene zwischen zwei Staatsangehörigkeiten entscheiden muß und die Eltern darauf drängen, daß die angestammte Staatsangehörigkeit beibehalten wird. Das ist doch ein kardinaler Mangel des Optionsmodells. Deswegen können Sie Ihre Auffassung nicht ernsthaft mit den Argumenten, die Sie hier vorgetragen haben, vertreten und gleichzeitig für das Optionsmodell stimmen. Hier bietet unser Modell der Einbürgerungszusicherung einen großen Vorteil.
## Dritter Punkt. Wir können in der Tat nicht verhindern, daß jemand fragt, wo er gegen Ausländer unterschreiben könne. Auf eine solche Frage gibt es für die Mitglieder meiner Fraktion nur eine Antwort: Bei der Union nicht!


## citizenship debates between 1996 and 2000

debates_citizen3[[2]] %>%
  read() %>%
  highlight(orange = q3_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 6th debate on citizenship between 1996 and 2000 for CDU/CSU


## Erwin Marschewski 1996-02-08 CDU fundamental debate on implementing dual citizenship and its consequences

p <- partition("GERMAPARL", speaker = "Erwin Marschewski", date = "1996-02-08", encoding = "UTF-8")

read(p)

## Sie wissen: In der Diskussion sind verschiedene Modelle: das Ruhens-Modell und die Schaffung eines echten oder unechten Jus soli. Gegen ein echtes Jus soli - auch das ist Ihnen sicherlich bekannt - spricht unsere Rechtstradition. In einem Punkt sind wir völlig einer Meinung: Die Forderung nach einer generellen Zulassung der doppelten Staatsbürgerschaft ist für uns nicht akzeptabel.

## Treten Sie doch mit dieser Frage vor das deutsche Volk! Es wird Ihnen eben nicht zustimmen.
## Wir meinen, die doppelte Staatsangehörigkeit mu�Y eine Ausnahme bleiben. Meine Damen und Herren, wir dürfen und müssen vom Einbürgerungswilligen erwarten, da�Y er sich ohne Wenn und Aber zu unserem Staat bekennt.
## Jedes Volk, auch das deutsche, stellt eine Gemeinschaft dar, in die man eben nicht nach Belieben eintreten und aus der man nicht nach Belieben austreten kann. Das hat überhaupt nichts mit völkisch-nationalem Denken zu tun und auch nicht mit den Vorstellungen - das habe ich neulich in der Presse gelesen - von einem deutschen Nationalstaat, was das auch immer - wir sind nicht in der Bismarckzeit - 1995 bedeuten soll.

## Zweitens. Die generelle Zulassung der doppelten Staatsbürgerschaft birgt die Gefahr in sich, da�Y sich die betreffenden Ausländer der Integration verweigern könnten, weil sie ja ohnehin die deutsche Staatsangehörigkeit verliehen bekommen.
## Ich meine auch, meine Damen und Herren, es ist nicht von der Hand zu weisen, da�Y politische Konflikte der Heimatländer in unser Land gelangen und das Zusammenleben beeinträchtigen können.
## Sie kennen die weiteren Probleme im Rechtsbereich, im Eherecht, im Erbrecht. Ich meine, für eine Anerkennung der doppelten Staatsangehörigkeit in genereller Hinsicht besteht überhaupt kein praktisches Bedürfnis.
## Ich will mich den Fällen widmen, die Sie dauernd aufführen. Wenn es irgendwo willkürlich verweigert wird, da�Y jemand auf seine Staatsbürgerschaft verzichtet, dann gibt es schon im derzeitigen Recht die Möglichkeit, im Wege der doppelten Staatsbürgerschaft Deutscher zu werden, falls etwas anderes unzumutbar ist.

##Wir wollen die Integration der hier lebenden Ausländer fördern. Es darf keine Menschen geben, meine Damen und Herren, die zwar die deutsche Staatsangehörigkeit besitzen, letztlich aber Fremde hier in Deutschland bleiben.
## Die generelle Zulassung der doppelten Staatsangehörigkeit löst überhaupt keine Probleme.


## ...


## Marschewski (CDU) answering Hirsch 

## Eine zweite Bewegung ist aber vielleicht noch wesentlicher: Sie wissen, da�Y in vielen Staaten die doppelte Staatsangehörigkeit verweigert wird. Ich denke an Schweden, ich denke an Polen, ich denke an Ru�Yland, ich denke an die Tschechei, ich denke zum Beispiel daran, da�Y restriktivste Einwanderungsbestimmungen in England Platz greifen, in Frankreich Platz greifen.
## Wir wollen die Menschen hier integrieren. Das ist die erste Aufgabe. Nicht das formale Verschaffen einer doppelten Staatsangehörigkeit löst Probleme, sondern die Integration der Bürger, der ausländischen Mitbürger, löst die Probleme in diesem Lande.


## Cem �zdemir (Gr�ne) pleading

## Das Staatsangehörigkeitsrecht der Bundesrepublik Deutschland mu�Y grundlegend novelliert werden. Der Grundsatz der Vermeidung doppelter Staatsangehörigkeit mu�Y in den Fällen, in denen das Gesetz Rechtsansprüche auf Einbürgerung einräumt, aufgegeben werden. Au�Yerdem mu�Y das Recht auf Erwerb der Staatsangehörigkeit für hier geborene Ausländer der zweiten und folgenden Generation verankert werden.
## Dies ist nicht etwa aus dem Programm von Bündnis 90/Die Grünen, SPD oder PDS abgeschrieben, ( Dr. Guido Westerwelle [ F.D.P. ]: Sondern von der F.D.P.! )
## nein, es ist aus dem Programm Ihrer Partei, Herr Hirsch, Ihrer Partei, Herr Westerwelle, es ist das Programm der F.D.P.
## Für diejenigen, die mit diesen drei Buchstaben nichts mehr anfangen können: Es ist die Partei, die einstmals für Liberalismus in diesem unserem Lande stand.
## Die Einbürgerung ist weiter zu erleichtern. Kinder von Ausländern mit verfestigtem Aufenthaltstitel sollten die deutsche Staatsbürgerschaft auch durch Geburt im Bundesgebiet ( ius soli ) erwerben können. Doppelstaatsbürgerschaften sollten verstärkt zugelassen werden.
## Die Ergänzung des ius sanguinis durch das ius soli sowie die Hinnahme von Mehrstaatigkeit sind international bewährte Lösungswege.


## Peter Altmaier (CDU) answering  �zdemir

## Wir müssen aufpassen, da�Y wir das, was wir durch eine verbesserte rechtliche Stellung an Integration erreichen wollen, nicht dadurch entwerten, da�Y wir �"ngste und Emotionen schüren. Durch Ihr jahrelanges Festhalten am Konzept der generellen doppelten Staatsangehörigkeit zum Nulltarif haben Sie die Lösung dieser Fragen erschwert und nicht erleichtert.
## Ich habe keinen Zweifel an der Integrität und der Ehrenhaftigkeit all derer, die quer durch alle Parteien, bei den Grünen, der SPD, der F.D.P. und auch bei uns, für ein besseres Staatsangehörigkeitsrecht streiten. Ich habe allerdings erhebliche Zweifel an der Seriosität Ihres Antrages, weil er genau zu diesem Zeitpunkt kommt - wenige Wochen vor den Landtagswahlen -, weil er aus wahl - und parteitaktischen Motiven darauf angelegt ist, Unterschiede zwischen den Koalitionsfraktionen herbeizureden. Deshalb sage ich Ihnen: Ihr Antrag bekommt von uns die Antwort, die er verdient hat, nämlich ein klares und überzeugtes Nein.

## Cem �zdemir (Gr�ne) answering Altmaier

## Wir haben gehört - Sie haben das noch einmal eindrücklich gesagt -, da�Y es Reformbedarf gibt;
## das wurde insgesamt eingeräumt. Es wurde auch gesagt, wo dieser Reformbedarf anzusiedeln ist, nämlich bei der Verkürzung der Fristen und bei hier geborenen Kindern.
## Unterschiede gab es bei der Akzentuierung der doppelten Staatsbürgerschaft. Manche wollen sie etwas gro�Yzügiger gehandhabt wissen, manche wollen sie gar nicht.

## Ich möchte noch einen letzten Punkt ansprechen, damit bezüglich der doppelten Staatsangehörigkeit klarer wird, worüber man eigentlich spricht. Sie alle kennen das Beispiel der Frau Gen aus Solingen, die fünf ihrer Angehörigen durch einen Brandanschlag verloren hat. Nach diesem Brandanschlag hat sie Deutschland nicht den Rücken gekehrt; sie ist in dieser Republik geblieben. Sie hat nach diesem Ereignis ihr neues Haus wieder in Deutschland gebaut und die Staatsbürgerschaft unseres Landes angenommen.
## Ich frage Sie: Kann man sich ein grö�Yeres Bekenntnis, eine grö�Yere Form der Loyalitätsbekundung zu dieser Republik vorstellen, als da�Y sich eine Frau nach einer solchen Erfahrung zu dieser Heimat bekennt? Ich denke, es wäre ein Zeichen der Gro�Yzügigkeit, eine Geste der Humanität, den Menschen der ersten Generation gro�Yzügig die doppelte Staatsbürgerschaft zu geben.


## ...


## citizenship debates between 1996 and 2000

debates_citizen3[[3]] %>%
  read() %>%
  highlight(orange = q3_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 6th debate on citizenship between 1996 and 2000 for CDU/CSU


## Rupert Scholz 1999-05-07 CDU on nationality

p <- partition("GERMAPARL", speaker = "Rupert Scholz", date = "1999-05-07", encoding = "UTF-8")

read(p)

## Rupert Scholz (CDU)

## Herr Schily, ich finde es gut, da�Y Sie Renan zitiert haben. Renan ist für den modernen Nationenbegriff ganz eindeutig der Richtige. Aber die Renansche These kann man auch zu dem Prinzip zusammenfassen: Nation gründet sich auf die Erlebnis - - das ist die Vergangenheit - und die Willensgemeinschaft. Das ist die kurze Formel für das, was Sie eben vorgelesen haben. Erlebnis - und Willensgemeinschaft setzt allerdings voraus - das können Sie bei Renan sehr deutlich nachlesen -, da�Y eine entsprechende Identifikationsbereitschaft da ist. Erlebnis - und Willensgemeinschaft bedeutet auch, da�Y beide Seiten - wenn es unterschiedliche ethnische Teile gibt - dieses wollen.
## Wie ist das in Deutschland? Haben Sie die Bereitschaft wirklich auf allen Seiten? Nehmen Sie wirklich genug Rücksicht darauf, diese Willens - und Erlebnisgemeinschaft in konfliktfreier Form, in sich wechselseitig akzeptierender und identifizierender Form - eben im Sinne von Renan - zu verwirklichen?


# ...


## J�rgen R�ttgers (CDU) answering Sonntag-Wolgast (SPD) regarding the Citizenship Law reform

## Werte Kolleginnen und Kollegen, wir lehnen Ihren Gesetzentwurf ab. Er ist verfassungsrechtlich bedenklich, er ist integrationspolitisch unausgegoren, und er ist mit einem unvertretbaren Verwaltungsaufwand verbunden. Er wirft mehr Fragen auf, als er beantwortet. Dies war auch das Ergebnis der Sachverständigenanhörung im Innenausschu�Y.
## Wenn man das Ergebnis der Anhörung zusammenfassen will, mu�Y man feststellen, da�Y die Sachverständigen, egal, wie sie zum Optionsmodell stehen, gesagt haben, dieser Gesetzentwurf sei Stückwerk, unausgereift und nachbesserungsbedürftig, auch sei er in sich widersprüchlich, aber vor allen Dingen fehle die Abstimmung mit anderen Gesetzen.
## Ich habe nie verstanden, warum Sie auf seiten der rotgrünen Regierung nicht den Mut gehabt haben, ein Gesamtkonzept für ein neues Staatsangehörigkeitsrecht vorzulegen, sondern hier den Versuch machen, einen einzelnen Punkt herauszugreifen. Ich habe übrigens auch nie verstanden, warum es Ihnen nicht gelungen ist, hier ein übergreifendes Integrationskonzept, das über die Frage der Staatsangehörigkeit hinausgeht, vorzulegen.

## Das, was uns heute zur abschlie�Yenden Abstimmung vorgelegt wird - ich habe es bereits gesagt -, ist Flickwerk. Die Rechtszersplitterung wird vorangetrieben. Sie ist heute schon schlimm genug. Das Gesetz steht im Widerspruch zum Ausländerrecht und enthält Ungereimtheiten, die dazu führen werden, da�Y nicht nur diejenigen, die unsere Auffassung bei dieser Reform teilen, an diesem Gesetz verzweifeln werden, sondern auch diejenigen, die angeblich davon profitieren sollen.
## Wer als ausländischer Mitbürger in der konkreten Verwaltungspraxis mit diesem Gesetz konfrontiert wird, wird seinen Glauben an die Integrationsbereitschaft unseres Staates sehr schnell verlieren, weil die Widersprüche ihn schlichtweg in die Verzweiflung treiben werden:
## Was soll zum Beispiel in dem Fall einer jungen Mutter geschehen, die Doppelstaaterin ist, in Deutschland ein Kind bekommt und - wenn sie nicht optiert - ihre deutsche Staatsbürgerschaft verliert, während ihr Kind diese erhält? Wie soll das innerhalb einer Familie gelöst werden? Oder was soll in dem Fall geschehen, in dem jemand seiner deutschen Wehrpflicht nachgekommen ist, aber der deutsche Staat ihm - wenn er nicht optiert - sagt, du bist nicht mehr erwünscht? Was hat das alles mit Integration zu tun?

## Wir haben uns von seiten der CDU/CSU-Bundestagsfraktion darum bemüht, da�Y die Debatte nicht nur auf die Frage der Staatsbürgerschaft verengt wird. Wir haben uns vielmehr darum bemüht, ein Gesamtkonzept vorzulegen, in dem die drei Bereiche Staatsbürgerschaftsrecht, Zugangsbegrenzung und Integrationskonzept berücksichtigt werden. Wir haben für unser Integrationskonzept viel Zustimmung von Ausländerbeiräten und Ausländerorganisationen erfahren.

## Die IG BCE - die Industriegewerkschaft Bergbau, Chemie, Energie - schreibt:
## Integration bedeutet dabei für uns als Gewerkschaften nicht die Aufgabe der ethnischen, kulturellen und religiösen Identität. Wir begrü�Yen es, da�Y die CDU/CSU dies genauso sieht.

## Wie rechtfertigen Sie eigentlich Ihre Ablehnung dieser konkreten Vorschläge zur Integration? Hiermit würde mehr für die Integration in Deutschland getan als durch die Einführung des Doppelpasses. ( Beifall bei der CDU/CSU )
## Welche anderen Gründe als rein parteipolitisches Kalkül könnte es eigentlich dafür geben? Ist das der Bedeutung der Sache angemessen?

## Die Integration der dauerhaft und rechtmä�Yig in Deutschland lebenden ausländischen Mitbürger ist für den inneren Frieden und die Zukunft unseres Landes unzweifelhaft ein gro�Yes Thema. Wir haben dem Deutschen Bundestag ein Angebot zur Debatte über diese zentrale Frage unseres Landes unterbreitet. Wenn dieses Konzept heute niedergestimmt wird, dann bestätigt sich unser Anfangsverdacht: Ihnen geht es nicht um Integration, sondern um die Erledigung dieses Themas vor den nächsten Wahlen. Genau das wollen Sie.


## ...


## Pter Altmaier (CDU) answering Schily (SPD)

## Es ist wahr, da�Y es in der CDU/CSU eine ganze Reihe von Kolleginnen und Kollegen gibt, die sich seit vielen Jahren für ein vernünftiges Optionsmodell eingesetzt haben und weiterhin einsetzen. Aus diesem Grund werden heute etwa 20 Kolleginnen und Kollegen der Union nicht gegen Ihren Gesetzentwurf stimmen.
## Ich nehme für uns in Anspruch, da�Y wir uns dabei nicht von parteitaktischem Kalkül leiten lassen. Vielmehr sind wir davon überzeugt, da�Y es als Antwort auf die Ver - änderungen, die sich in Deutschland in den letzten 20, 30 Jahren vollzogen haben - in Deutschland werden jedes Jahr 100 000 Kinder geboren, die nicht die deutsche Staatsangehörigkeit haben -, notwendig ist, ein Signal zu geben, das diesen jungen Menschen deutlich macht: Ihr gehört dazu, ihr seid Teil dieser Gesellschaft; wir nehmen euch an. Ich denke an ein Signal, das ohne generelle doppelte Staatsangehörigkeit und ohne all die gefährlichen Assoziationen und Folgen auskommt, die mit diesem Begriff verbunden sind.
## Es gab im Jahre 1998 einen breiten gesellschaftlichen Konsens für das Optionsmodell. Die Kirchen, der Städte - und Gemeindetag und auch viele gesellschaftliche Gruppierungen waren dafür. Sie haben nach der Bundestagswahl diesen Konsens ohne Not aufgekündigt und sind auf Ihren ursprünglichen Vorschlag einer generellen doppelten Staatsangehörigkeit zum Nulltarif - aus, wie ich meine, rein koalitions - und parteitaktischen Gründen - zurückgekommen. Damit haben Sie dem Anliegen der Ausländerintegration geschadet.


## citizenship debates between 1996 and 2000

debates_citizen3[[4]] %>%
  read() %>%
  highlight(orange = q3_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 6th debate on citizenship between 1996 and 2000 for CDU/CSU


## Erwin Marschewski 1997-06-05 CDU speech on naturalisation and Citizenship Law reform

p <- partition("GERMAPARL", speaker = "Erwin Marschewski", date = "1997-06-05", encoding = "UTF-8")

read(p)

## Erwin Marschewski (CDU)

## Natürlich ist es bekannt, da�Y das Staatsangehörigkeitsrecht, das aus dem Jahre 1913 stammt, veraltet und reformbedürftig ist. Das ist klar. Aber es handelt sich in diesem Bereich um eine äu�Yerst sensible Materie; denn dieses Recht regelt das Grundverhältnis zwischen Staat und Bürger und berührt damit das Selbstverständnis der Menschen in diesem Lande. Daher mu�Y diese Reform äu�Yerst behutsam angegangen werden. Das haben wir getan, und das werden wir weiterhin tun.

## Wir haben - ich glaube, als einziger Staat in der Welt - gesetzliche Einbürgerungsansprüche, die sogar einklagbar sind. Wir haben die Aufenthaltsfristen verkürzt, und wir haben die Einbürgerungsgebühren beträchtlich gesenkt. Dies hat zu dem Ergebnis geführt, da�Y sich die Zahl der Einbürgerungen auf 70 000 mehr als verdoppelt hat. Dies gilt auch für die Einbürgerung türkischer Mitbürger.
## Für uns ist dabei eines klar: Die generelle Zulassung der doppelten Staatsbürgerschaft ist für uns nicht diskutabel. Sie birgt die Gefahr in sich, da�Y sich der betreffende Ausländer der Integration verweigert, weil ihm, Herr Kollege Schily, ohnehin die deutsche Staatsangehörigkeit verliehen wird. Sie kennen die weiteren Probleme.
## Es ist doch nicht anachronistisch, wenn ich sage: Deutsche Staatsbürgerschaft bedeutet die Zugehörigkeit zu einer Gemeinschaft. Ich sage auch: Schicksalsgemeinschaft. In diese darf man nicht nach Geschmack eintreten und austreten. Doppelte Staatsbürgerschaft, wie sie die Grünen wollen, führt doch zu einer Rückversicherungsmentalität, zu Rechtsproblemen im Bereich des Familienrechts und im Bereich des Erbrechts.

## Nun zu Ihrem Antrag. Sie wollen die doppelte Staatsangehörigkeit für hier geborene Ausländer. Haben Sie denn gefragt, ob die Ausländer, zum Beispiel die türkischen Mitbürger, dies überhaupt wollen? Wollen Sie ihnen die doppelte Staatsbürgerschaft gegen ihren Willen aufdrängen? Ich frage weiter: Ist dies wirklich integrationsfördernd? Sie haben die Verpflichtung, dafür den Beweis zu erbringen. Ich wäre für diese Lösung zu gewinnen, wenn Sie den Beweis für die Wirksamkeit erbringen könnten. Ich bitte Sie daher, diesen Beweis zu erbringen. Ansonsten hätte Montesquieu recht: Wenn es nicht notwendig ist, ein Gesetz zu erlassen, dann ist es besser, keines zu erlassen. - Bis zu dem Beweis werden wir uns danach richten.
## Wir stellen uns dieser Aussetzung. Wir treten mit dieser Frage vor unser Volk. Sie werden sehen, was das deutsche Volk zu Ihren Vorschlägen sagt.

## Ihre Pläne sind unausgegoren und unschlüssig; sie sind eine Mogelpackung. Sie wollen 850 000 pro Jahr vorweg akzeptieren. Nein, meine Damen und Herren, wir brauchen keine weitere Einwanderung. ( Beifall bei Abgeordneten der CDU/CSU )
## Wir sind kein Einwanderungsland. Wir brauchen vielmehr wirkliche Zuwanderungsbeschränkungen.
## An die '' " vollständig versammelte '' Fraktion der Grünen ein paar Worte zu ihrem Vorschlag. Zu dem, was die generelle doppelte Staatsbürgerschaft anbetrifft, habe ich bereits einiges ausgeführt. Weiterhin wollen Sie, wenn ich Ihren Gesetzentwurf richtig verstehe, eine Erhöhung der Einwanderung in Deutschland. Sie wollen, da�Y der Familiennachzug auf Lebenspartnerinnen und Lebenspartner ausgedehnt werden soll.

## Wir wollen eine konsequente Begrenzung der Zuwanderung. Dies geht durch konsequente Anwendung des Ausländerrechts, durch nicht immer neue Altfallregelungen und meines Erachtens nur durch eine Grundgesetzänderung, insbesondere einer �"nderung der Artikel 6, 16 a und - vielleicht in begrenztem Ma�Ye; das Denken soll nicht verboten sein - 116. Dies sollte man vielleicht einmal überdenken. ( Dr. Willfried Penner [ SPD ]: Das haben Sie ausgeschlossen! )
## - Ja, Herr Kollege Penner. Nur das führt zu einer wirksamen Begrenzung der Zuwanderung. Ich biete Ihnen an - ich habe da Erfahrungen -,
## ( Cem �-zdemir [ B�oNDNIS 90/DIE GR�oNEN ]: Allerdings haben Sie da Erfahrung! ) wie beim Asylkompromi�Y gemeinsam mit uns eine Regelung anzustreben. Nur dies wäre ehrlich und wirksam, Herr Kollege Dr. Penner, nicht Ihr Vorschlag. Der führte zu mehr Zuwanderung, die wir nicht wollen.

## Zum Schlu�Y: Die Integration der hier lebenden Ausländer - an diesem Ziel arbeiten wir - mu�Y gefördert werden. Aber nur derjenige, der integriert ist, darf erwarten, da�Y wir ihm die deutsche Staatsangehörigkeit verleihen werden; denn es hat doch keinen Sinn, dies anders zu regeln.


## ...


## Cornelia Schmalz-Jacobsen (FDP, Ausl�nderbeauftragte) pleading

## Ich habe vor mehreren Jahren hier gesprochen und gesagt, man solle zwei Themen aus dem Giftschrank nehmen, nämlich die Themen Einwanderungsland und doppelte Staatsbürgerschaft. Aus dem Schrank sind sie nun heraus. Das ist schon einmal einen Schritt weiter; sie liegen nämlich auf dem Tisch. Es gibt sehr viel mehr Diskussionen, auch wenn man manchen Diskussionen eine gewisse Giftigkeit nicht absprechen kann.
## Ich möchte mich bemühen, hier sehr sachlich und ohne Schärfen zu reden. Im übrigen sprechen wir ja hier nicht nur miteinander, sondern auch für die �-ffentlichkeit in Deutschland; das ist übrigens etwas anderes als die deutsche �-ffentlichkeit.


## citizenship debates between 1996 and 2000

debates_citizen3[[5]] %>%
  read() %>%
  highlight(orange = q3_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 6th debate on citizenship between 1996 and 2000 for CDU/CSU


## J�rgen R�ttgers 1998-11-12 CDU on dual citizenship and integration

p <- partition("GERMAPARL", speaker = "J�rgen R�ttgers", date = "1998-11-12", encoding = "UTF-8")

read(p)

## J�rgen R�ttgers (CDU)

## Da hält Herr Schily laut '' " Spiegel '' die doppelte Staatsangehörigkeit für gerade einmal hinnehmbar. Wenige Sätze weiter preist er das neu konzipierte Staatsangehörigkeitsrecht als '' " Reformwerk von historischen Dimensionen '' . Was denn nun, Herr Schily? Hinnehmbar oder historisch? Ja oder nein? ( Beifall bei der CDU/CSU sowie bei Abgeordneten der F.D.P. )

## All das sagt etwas über Ihr Staatsverständnis aus.
## Das gilt auch - und vielleicht sogar besonders - für die angekündigte Reform des Staatsangehörigkeitsrechts. Nun will ich, meine Damen und Herren, zuerst einmal sagen: Die Ausländer - und die Asylpolitik ist ein ganz schwieriges Feld. Ich gebe auch gerne zu, da�Y die alte Koalition in diesem Bereich unterschiedliche Auffassungen hatte.
## ( Dieter Wiefelspütz [ SPD ]: Keine Gestaltungskraft hatte! )
## Aber bei allen unterschiedlichen Auffassungen ist eines wichtig: Deutschland ist ein ausländerfreundliches Land, und das soll so bleiben.
## ( Beifall bei der CDU/CSU und der F.D.P. ) 7,3 Millionen EU-Bürger und Ausländer leben auf Dauer in Deutschland, und sie sind Teil unserer Gesellschaft. Herr Schily, ich stimme Ihnen ausdrücklich zu, wenn Sie sagen, wir brauchen uns nicht ständig selbst anzuklagen, da�Y hier die Menschenrechte mit Fü�Yen getreten werden. Da haben Sie recht.

## Aber die Einführung der doppelten Staatsangehörigkeit ist nicht ein Thema wie jedes andere.
## Anders als im Steuerrecht, anders als im Strafrecht sind die Einführung der doppelten Staatsbürgerschaft und ein Automatismus bei der Einbürgerung von in Deutschland geborenen Kindern eben nicht mehr revidierbar. Selbst in problematischen Fällen, wenn ein Bürger ausländischer Herkunft wiederholt straffällig geworden ist, kann die Verleihung der Staatsbürgerschaft nicht wieder rückgängig gemacht werden. Das verbietet Art. 16 des Grundgesetzes ausdrücklich. Insofern kommt es schon darauf an, genau zu überlegen, was man da macht.

## Die CDU/CSU-Bundestagsfraktion lä�Yt sich bei ihrer Politik von drei Zielen leiten. Das ist einmal die Identität, zweitens die Toleranz und drittens die Integration.
## Das hei�Yt konkret: Erstens. Wir wollen, da�Y die Zugangsbegrenzung für Ausländer, die nach Deutschland kommen wollen, weiter so eng wie möglich gestaltet bleibt.
## Zweitens. Wir wollen das Mögliche tun, um die in Deutschland rechtmä�Yig lebenden Ausländer in unsere Gesellschaft zu integrieren.
## Drittens. Wir halten die regelmä�Yige doppelte Staatsangehörigkeit für falsch.

## Kurt Biedenkopf, meine Damen und Herren, hat Anfang September in der Debatte hier im Bundestag gesagt, da�Y eine Politik scheitern mu�Y, die von einer falschen Sicht der Wirklichkeit ausgeht.
## ( Ludwig Stiegler [ SPD ]: Deswegen sind Sie gescheitert! Darum sind Sie abgewählt worden! )
## Wie ist denn die Wirklichkeit in diesem Bereich?
## Da wird zum Beispiel behauptet, die doppelte Staatsangehörigkeit sei international üblich.
## Wahr aber ist: Mit der Einführung der regelmä�Yigen doppelten Staatsangehörigkeit geht Deutschland einen Sonderweg.
## Zwei Drittel der europäischen Staaten verlangen als Voraussetzung für die Einbürgerung die Aufgabe der bisherigen Staatsangehörigkeit.
## ( Beifall bei der CDU/CSU ) Die Vermeidung von Mehrstaatigkeit ist Weltrechtsstandard.

## Damit, meine Damen und Herren, stellt sich für mich die zentrale Frage: Dient die Einführung der regelmä�Yigen doppelten Staatsbürgerschaft der Integration der hier lebenden Ausländer?
## Ich meine, nein. ( Beifall bei der CDU/CSU )
## Durch die doppelte Staatsangehörigkeit wird die Integration ausländischer Mitbürger nicht gefördert, sondern erschwert.

## Integration hei�Yt, sich mit diesem Land, mit seiner Geschichte, mit seiner Zukunft zu identifizieren.
## Integration hei�Yt, Teil der Gesellschaft zu sein.
## Integration hei�Yt, Rechte und Pflichten anzunehmen.
## Integration hei�Yt, die deutsche Sprache zu sprechen.
## Integration hei�Yt, sich mit unserer Gesellschaft und Verfassungsordnung zu identifizieren.
## Deshalb, meine Damen und Herren, kann Integration nicht alleine durch einen Hoheitsakt, nicht alleine durch die �obergabe des deutschen Passes erreicht werden.
## Die Staatsbürgerschaft steht am Ende und nicht am Anfang der Integration.

## Das Bild einer Zweiklassengesellschaft droht - mit verhängnisvollen Folgen für Toleranz und Integration.
## Denn das ist offenkundig: Auch nur der Verdacht, da�Y hier eine bestimmte Bevölkerungsgruppe privilegiert wird, fördert nicht Toleranz und Aufnahmebereitschaft, sondern beschädigt sie.
## Schon deshalb ist eine doppelte Staatsangehörigkeit der falsche Weg.

## Dieses Thema geht über die reine Frage des Wahlrechts hinaus.
## Das kommunale Wahlrecht für Angehörige von Staaten au�Yerhalb der Europäischen Union erschwert auch die weitere europäische Einigung.
## Gleiches, so befürchte ich, gilt auch für die doppelte Staatsbürgerschaft als Regelform.
## Was werden denn unsere europäischen Partner sagen, wenn die Gewährung der doppelten Staatsangehörigkeit dazu führt, da�Y mit ihr auch ein Aufenthaltsrecht in ihren Ländern eingeräumt wird? Wie wollen Sie eine gemeinsame europäische Flüchtlings - und Migrationspolitik durchsetzen, wenn Sie vorher nationale Alleingänge veranstalten?
## Wie wollen Sie von anderen Ländern einen wirksamen Schutz der Au�Yengrenzen der Europäischen Union vor illegalen Einwanderern und Schleuserbanden verlangen, wenn Sie vorher nicht mit ihnen reden? Was Sie vorhaben, erschwert die europäische Einigung.


## citizenship debates between 1996 and 2000

debates_citizen3[[6]] %>%
  read() %>%
  highlight(orange = q3_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 6th debate on citizenship between 1996 and 2000 for CDU/CSU


## Erwin Marschewski 1998-03-27 CDU on naturalisation and integration

p <- partition("GERMAPARL", speaker = "Erwin Marschewski", date = "1998-03-27", encoding = "UTF-8")

read(p)

## Erwin Marschewski (CDU)

## Mit den vorliegenden Anträgen wollen SPD und Grüne, in welcher Form auch immer, die generelle doppelte Staatsbürgerschaft in Deutschland einführen.
## Sie tun dies mit der Behauptung, die Integration aller hier geborenen Ausländerkinder sei bereits vollzogen;
## dies gelte auch für Jugendliche, die nur fünf Jahre in ausländischen Familien in Deutschland lebten.
## Ihr Vorschlag, meine Damen und Herren der SPD, ist staatspolitisch falsch und integrationshindernd.
## Sein Beurteilungsma�Ystab widerspricht der Wirklichkeit.

## Die Koalitionspartner werden das dem jeweils anderen gegebene Wort halten,
## auch weil wir wie die Mehrheit der Bürger unseres Landes die generelle doppelte Staatsbürgerschaft ablehnen.
## Was wir vielmehr wollen, ist eine wirkliche Integration. Deswegen wollen wir die Einbürgerungsfristen verkürzen. Wir wollen mehr Anspruchseinbürgerungen schaffen.
## Voraussetzung der Einbürgerung sind aber insbesondere ausreichende Sprachkenntnisse, damit wir uns verstehen. Wir wollen, da�Y die Ausländer Art. 3 des Grundgesetzes akzeptieren: Männer und Frauen sind gleichberechtigt.
## Wir wollen den arglistigen Erwerb der deutschen Staatsbürgerschaft ausschlie�Yen.
## Wer zum Erwerb der deutschen Staatsbürgerschaft zunächst zum Schein auf die ausländische Staatsbürgerschaft verzichtet und dann später die ausländische Staatsbürgerschaft wieder annimmt,
## der soll kraft Gesetzes die deutsche Staatsbürgerschaft wieder verlieren. Auch das wollen wir.

## Dies bedeutet: Mehr als 60 Prozent der ausländischen Mitbürger könnten sich ohne Nachteile einbürgern lassen, wenn sie dies nur wollten,
## wenn sie auf Doppelsicherheit, sprich: auf die doppelte Staatsbürgerschaft verzichteten.
## Hierin liegt der Unterschied zwischen SPD und Grünen auf der einen Seite und der CDU/CSU auf der anderen Seite.
## Wir setzen ein gewisses Ma�Y an Integration voraus. Mehrstaatlichkeit ist für uns auf Dauer nicht erstrebenswert,
## weil es dann schwieriger ist, uneingeschränkt loyal zu sein.

## Der Kollege Hirsch hat gestern in der '' " Welt '' gesagt, da�Y 2 Millionen Deutsche die doppelte Staatsbürgerschaft besä�Yen.
## ( Dr. Cornelie Sonntag-Wolgast [ SPD ]: Sehr richtig! )
## Herr Kollege Hirsch, diese Zahl ist falsch. Da Sie ein Ziel verfolgen, ist das ein fahrlässiger Umgang mit der Wahrheit, Herr Kollege Hirsch.
## In Deutschland leben 538 000 ausländische Mitbürger mit doppelter Staatsbürgerschaft. Der grö�Yte Teil davon sind Aussiedler.
## Diese Menschen würden gern auf die usbekische, auf die kasachische Staatsbürgerschaft verzichten, wenn es nicht die hohen Gebühren gäbe, die diese Menschen drangsalierten. Ihre Zahl ist falsch, Herr Kollege Hirsch.


## Burkhard Hirsch (FDP) answering Marschewski (CDU)

## Herr Kollege Marschewski, würden Sie dem Haus bitte sagen, woher Sie diese Zahlen haben, nachdem bisher die Auskünfte des Innenministeriums dahin gehen, da�Y es keine Statistik über die doppelte Staatsangehörigkeit gibt?
## Dies ist ein merkwürdiger Vorgang; denn wenn es ein Problem wäre, mü�Yte man sie ja zählen.
## Vielleicht können Sie dem Haus offenbaren, ob es doch eine Statistik gibt, damit sie allen zugänglich wird.


## Erwin Marschewski (CDU) aswering Hirsch (FDP)

## Herr Kollege Hirsch, ich bin ein wenig verwundert, da�Y Sie nicht einmal diese Grundregeln -
## Sie sind ja lange Innenpolitiker gewesen - beherrschen:
## Wir haben einen Mikrozensus, der besagt, da�Y in Deutschland 538 000 Menschen die doppelte Staatsbürgerschaft besitzen.
## Ich will Ihnen diese Zahlen sehr gern zur Verfügung stellen.
## Wichtigster Punkt unserer Politik ist natürlich, da�Y sich die Menschen, die Deutsche werden wollen,
## auf Dauer zu Deutschland bekennen müssen, da�Y sie sich zu Deutschland hinwenden,
## da�Y sie diese Gemeinschaft akzeptieren, da�Y sie umfassend mitwirken und mitgestalten.
## Deswegen bleibt unser Ziel - wir haben dies durch mehrfache �"nderungen des Ausländerrechts verfolgt -
## die Integration der hier lebenden Ausländer.

## Beides gehört zusammen: Zuzugsbegrenzung und Integration. Deswegen sagen wir zu Ihren Anträgen nein,
## weil sie einfach nicht der Integration dienen und auch nicht dienen sollen.
## Wir sagen deswegen nein zur generellen doppelten Staatsbürgerschaft,
## wie auch die ganz gro�Ye Mehrheit des deutschen Volkes.
## Aus diesem Grunde werden und dürfen Ihre Anträge keine Mehrheit finden.


## Cornelie Sonntag-Wolgast (SPD) referring to Marschewski (CDU)

## Erstens. Ich habe selten erlebt, da�Y jemand so bewu�Yt wider besseres Wissen Anträge mi�Ygedeutet hat, wie Sie es mit unseren eben getan haben.

## ng ist es her, seit die SPD-Bundestagsfraktion in dieser Legislaturperiode ihren ersten Antrag zu diesem Thema
## mit dem Titel '' " Erleichterung der Einbürgerung unter Hinnahme der doppelten Staatsangehörigkeit '' präsentierte. Das war im Januar 1995.
## Schon damals war die Reform überfällig. Schon damals führten wir Gespräche mit reformwilligen Kräften aus der Koalition, um Möglichkeiten von einvernehmlichen Lösungen auszuloten.

## Ich möchte noch einen weiteren Irrtum ausräumen. Wir von der SPD wollen nicht möglichst viele Doppelstaatler heranzüchten;
## aber die Mehrstaatlichkeit soll hingenommen werden. Das betrifft mehr als 2 Millionen Bürger -
## oder auch nicht. Die Zahlen sind interessant.
## Ich stütze mich immer auf die Aussagen der Ausländerbeauftragten und meine, da�Y sie,
## wenn sie von 2 Millionen spricht, dafür gesicherte Daten hat.
## Ob es nun 1 oder 2 Millionen sind: Es sind erkleckliche Zahlen.
## Diese Menschen haben mehr als einen Pa�Y. Unser Staatsgebilde wankt immer noch nicht.

## Der Kollege Eylmann von der CDU nannte kürzlich die deutsche Auffassung,
## da�Y die Einbürgerung im Regelfalle den Verzicht auf die Staatsangehörigkeit des Herkunftslandes
## beinhaltet, eine '' " antiquierte Doktrin '' . Recht hat der Mann.


## Wilfried Penner (SPD) intervention

## ...betont, da�Y die Zahl derer, die bei uns die doppelte Staatsbürgerschaft besitzen,
## 1,8 Millionen beträgt. Ich habe keinen Zweifel daran, da�Y die diesbezüglichen Ausführungen
## der Kollegin Frau Schmalz-Jacobsen auf sorgfältiger Prüfung beruhen.
## Ich bitte allerdings, den Widerspruch innerhalb der Bundesregierung aufzuklären, soweit er besteht.
