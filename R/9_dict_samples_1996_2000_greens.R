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

coi_greens00 <- partition("GERMAPARL",
                          parliamentary_group = "GRUENE",
                          year  = 1996:2000,
                          interjection= F,
                          role = c("mp", "government"))


## as partition bundles

pb1 <- partition_bundle(coi_greens00, s_attribute = "date")


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

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsbÃ¼rger.*"',
        '".*[Ss]taatsangeh.*rig.*"', '".*[Ss]taatszugeh.*rig.*"', '"[Ss]taatenlos.*"',
        '"[Aa]us.*bÃ¼rger.*"', '"[Ee]in.*bÃ¼rger.*"', '"Doppelpass.*"', '"DoppelpaÃY.*"',
        '"Pass"', '"PaÃY"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburtsrecht.*"', '"Geburtsprinzip.*"',
        '"[Ii]us soli"', '"[Ii]us sanguinis"', '"[Jj]us soli"', '"[Jj]us sanguinis"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Abstammungsrecht.*"', '"Abstammungsprinzip.*"')

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Doppelpass.*"', '"DoppelpaÃY.*"',
        '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"', '"Optionsmodell.*"')

q3 <- c('".*[Aa]syl.*"', '".*[Ff]lucht.*"', '".*[Ff]lÃ¼cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '"[Ff]amilienzusammen.*"', '".*[Aa]us.*bÃ¼rger.*"',
        '".*[Aa]b.*schie.*"', '".*[Aa]b.*schob.*"', '".*[Ee]in.*bÃ¼rger.*"', '".*[Aa]us.*sied.*"',
        '"Aufnahme.*"', '"[Vv]isa.*"', '"[Vv]isum.*"', '"LoyalitÃ¤tskonflikt"', '"IdentitÃ¤tsfeststellung"',
        '"RÃ¼ckfÃ¼hrung.*"', '".*[Aa]uslÃ¤nd.*"','"[Aa]ufenthalt.*"', '"RÃ¼ckÃ¼bernahme.*"', '"Ehegattennachzug"', '"Duldung.*"',
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

debates_foreign1[[70]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 70th debate on Foreigners� Policy for GRUENE between 1996 and 2000

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

debates_citizen3[[18]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 18th debate on citizenship between 1996 and 2000 for GRUENE


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


## debates on dual citizenship between 1996 and 2000

debates_dual1[[1]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 4th debate on dual citizenship between 1996 and 2000 for GRUENE


## Kerstin Müller 1999-03-19 GRUENE

p <- partition("GERMAPARL", speaker = "Kerstin Müller", date = "1999-03-19", encoding = "UTF-8")

read(p)

## Kerstin Müller (Grüne) answering Zeitlmann

## Wir haben - auch das will ich nicht verhehlen - nach wie vor groÃYe Bedenken gegen das Optionsmodell. Warum sollen sich Jugendliche mit Erreichen der VolljÃ¤hrigkeit plÃ¶tzlich fÃ¼r eine StaatsbÃ¼rgerschaft entscheiden?

## Ich mÃ¶chte besonders Ihnen, meine Damen und Herren von der F.D.P., einmal zu bedenken geben: Haben Sie sich einmal Ã¼berlegt, daÃY Sie mit dem Optionsmodell, mit dieser erzwungenen Entscheidung in vielen Familien schwere Konflikte auslÃ¶sen kÃ¶nnen?
## Denn viele Eltern werden eine Entscheidung ihrer Kinder fÃ¼r die deutsche StaatsbÃ¼rgerschaft als Abkehr von ihren eigenen Bindungen an ihr Heimatland empfinden, das heiÃYt als Entscheidung gegen die Eltern verstehen. Ich frage Sie: Wollen Sie das wirklich? Das ist unsere Hauptkritik an dem Optionsmodell und nicht, Herr RÃ¼ttgers und meine Damen und Herren von der CDU, die verfassungsrechtlichen Fragezeichen. In vielen FÃ¤llen wird dies die Integration verhindern.

## Bedauerlich ist auch, daÃY die generelle Hinnahme der doppelten StaatsangehÃ¶rigkeit jetzt nicht durchsetzbar war. Nicht, weil sie unser eigentliches Ziel war, wie Sie von der Opposition wider besseres Wissen immer wieder behauptet haben; nein, die doppelte StaatsbÃ¼rgerschaft ist und war immer nur als Instrument gedacht, um die schnelle und unbÃ¼rokratische EinbÃ¼rgerung zu erreichen. Wir haben jetzt zwar die Ausnahmen bei der Hinnahme der Mehrstaatigkeit erweitert, was fehlt, ist aber der BrÃ¼ckenschlag zur ersten Generation. Dies, liebe Kolleginnen und Kollegen von der F.D.P., ist leider an Ihnen gescheitert. AusschlieÃYlich Sie tragen die Verantwortung dafÃ¼r, daÃY der Gesetzentwurf hier nicht weiter geht. Sie haben das blockiert.

## Gerade bei der ersten Generation sind die Bindungen an das Heimatland aber noch sehr stark. Gerade deshalb werden sich die meisten Einwanderer der ersten Generation ohne die MÃ¶glichkeit des Doppel-Passes nicht einbÃ¼rgern lassen. Ich sage einmal folgendes: Gerade diesen Menschen, die wir, meine Eltern, Sie, als Gastarbeiter hierhergeholt haben und die hierbleiben werden, sollte man doch den BrÃ¼ckenschlag erÃ¶ffnen und den Doppel-PaÃY gewÃ¤hren.

## Viele Menschen, die ihre Listen unterzeichnet haben, haben nicht fÃ¼r Integration, sondern, um es mit den Worten von Ignatz Bubis zu sagen, gegen AuslÃ¤nder unterschrieben. Sie haben daher auf dem RÃ¼cken der hier lebenden AuslÃ¤nderinnen und AuslÃ¤nder Stimmungsmache betrieben. Das finde ich unverantwortlich, ( Beifall beim BÃoNDNIS 90/DIE GRÃoNEN und bei der SPD )
## vor allen Dingen auch deshalb, weil Ihre scheinheilige Kampagne gegen den Doppel-PaÃY auf Behauptungen beruht, die schlichter Unfug sind.
## Die doppelte StaatsbÃ¼rgerschaft ist kein Privileg, wie Sie unterstellen, ( Wolfgang Zeitlmann [ CDU/CSU ]: NatÃ¼rlich! ) und sie hat nichts, aber auch gar nichts mit Rosinenpickerei zu tun. Das ist dummes Zeug, und noch dazu gefÃ¤hrlich.
## ( Wolfgang Zeitlmann [ CDU/CSU ]: NatÃ¼rlich ist es ein Privileg! ) Die Rechte und Pflichten von DoppelstaatsbÃ¼rgern richten sich ganz einfach nach dem festen Wohnsitz. Die zweite StaatsangehÃ¶rigkeit zu bekommen bedeutet im Kern nur einen einzigen Vorteil - das muÃY man, glaube ich, einmal deutlich darstellen -,
## ( Wolfgang Zeitlmann [ CDU/CSU ]: Ein Doppelstaatler hat mehr Rechte als ein Deutscher! ) nÃ¤mlich den: Es gibt auÃYer Deutschland ein weiteres Land, in dem man das Recht hat, sich jederzeit niederzulassen. Dieses Recht, meine Damen und Herren, Herr Zeitlmann, hat jeder Deutsche - das heiÃYt, auch Sie in diesem Hause -, und zwar nicht nur in einem anderen Land, sondern in allen 14 LÃ¤ndern der EuropÃ¤ischen Union. Da sollten wir doch nicht von Privilegien reden. Der DoppelpaÃY bedeutet eben keine doppelten Rechte.
## Deshalb fordere ich Sie auf, mit dieser gezielten Desinformation aufzuhÃ¶ren. Sie schÃ¼ren damit in unverantwortlicher Weise NeidgefÃ¼hle.


## ...


## Zum Optionsmodell. Es ist richtig, daÃŸ meine Fraktion hierzu Bedenken hat. Ich habe diese Bedenken genannt. Vor allen Dingen befÃ¼rchten wir, daÃŸ es Konflikte in die Familien hineintragen kÃ¶nnte. Die Umsetzung des Modells bedeutet wahrscheinlich auch einen ziemlich hohen bÃ¼rokratischen Aufwand. Aber ich und viele Mitglieder meiner Fraktion haben diesen Sachverhalt abgewogen. Ich mÃ¶chte Sie, meine Damen und Herren von der CDU/CSU, bitten, dies auch zu tun.
## FÃ¼r mich ist entscheidend, daÃŸ wir mit dieser Reform den Einstieg in das Geburtsrecht leisten. Es handelt sich um einen ersten, rechtspolitisch historischen Schritt. Ich bin der Meinung, daÃŸ man vor diesem Hintergrund die Hinnahme des Optionsmodells akzeptieren kann. Wir werden - das habe ich hier angekÃ¼ndigt, und das meine ich sehr ernst; wir kÃ¶nnen das gerne gemeinsam tun - fÃ¼r gesellschaftliche Mehrheiten kÃ¤mpfen, um das ius soli pur, ohne das Optionsmodell, zu bekommen. Ich finde, ein Einstieg ist besser, als daÃŸ es bei der alten, schlechten Rechtslage bleibt.


## debates on dual citizenship between 1996 and 2000

debates_dual1[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 4th debate on dual citizenship between 1996 and 2000 for GRUENE


## Cem Özdemir 1997-06-05 GRUENE

p <- partition("GERMAPARL", speaker = "Cem Özdemir", date = "1997-06-05", encoding = "UTF-8")

read(p)

## Cem Özdemir (Grüne) speech on Marschewski´s (CDU) arguments

## Ich mÃ¶chte noch etwas zur Frage der doppelten StaatsbÃ¼rgerschaft sagen, weil ich den Eindruck habe, daÃY dies der entscheidende Punkt ist und hierzu Meinungsunterschiede offensichtlich nicht nur in diesem Hause, sondern auch in der Gesellschaft vorhanden sind. Mir scheint eine grundsÃ¤tzliche VerstÃ¤ndigung in dieser Frage - guten Willen natÃ¼rlich vorausgesetzt - durchaus mÃ¶glich.
## Wir haben kÃ¼rzlich den Vorschlag gemacht, zwischen einer ruhenden und einer aktiven StaatsbÃ¼rgerschaft zu unterscheiden. Die Experten, die sich mit diesem Thema beschÃ¤ftigen - Frau Schmalz-Jacobsen und andere -, kennen diesen Vorschlag. Ich denke, daÃY eine ruhende und aktive StaatsbÃ¼rgerschaft eine problemadÃ¤quate LÃ¶sung beinhaltet.
## In einem solchen Konzept werden die Rechte und Pflichten von Doppelstaatlern wie Wahlrecht, wie beispielsweise auch Wehrpflicht zwischen dem Herkunftsland und der Aufnahmegesellschaft in bi - oder multilateralen Abkommen geregelt.

## In der Bundesrepublik Deutschland leben mittlerweile mehr als 2 Millionen BÃ¼rger mit doppelter StaatsbÃ¼rgerschaft. Jede sechste Ehe ist mittlerweile binational. Die Kinder, die aus diesen Ehen hervorgehen, haben ebenfalls vÃ¶llig legal die Mehrstaatlichkeit. Der Grundsatz der Vermeidung der Mehrstaatlichkeit ist also lÃ¤ngst national wie international Ã¼berholt. Mir ist Ã¼brigens auch nicht bekannt, daÃY diese 2 Millionen Menschen mit doppelter StaatsbÃ¼rgerschaft, wie es die AusfÃ¼hrungen von Herrn Marschewski nahelegen, stÃ¤ndig zum Therapeuten rennen und dort auf die Couch mÃ¼ssen, weil sie LoyalitÃ¤tskonflikte haben und nicht wissen, zu welchem Volk sie gehÃ¶ren.
## Es handelt sich um die Iraner, die ganz besonders betroffen sind.
## Gerade nach dem Mykonos-Urteil kÃ¶nnen wir uns das skandalÃ¶se Vorgehen nicht mehr leisten, Iraner zu zwingen, auf die iranische Botschaft zu gehen, um dort die AusbÃ¼rgerung zu beantragen. Es handelt sich um keine Menschen, die die doppelte StaatsbÃ¼rgerschaft wollen, sondern sie sind darauf angewiesen, weil es gar nicht anders geht. Diesen sollten wir doch bei allem Streit um die doppelte StaatsbÃ¼rgerschaft eine unbÃ¼rokratische LÃ¶sung ermÃ¶glichen.


## debates on dual citizenship between 1996 and 2000

debates_dual1[[3]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 4th debate on dual citizenship between 1996 and 2000 for GRUENE


## Cem Özdemir 1999-05-07 GRUENE

p <- partition("GERMAPARL", speaker = "Cem Özdemir", date = "1999-05-07", encoding = "UTF-8")

read(p)


## Cem Özdemir (GRUENE)

## Der Â§ 87 regelt bereits heute AusnahmetatbestÃ¤nde, bei deren Vorliegen die Mehrstaatlichkeit hingenommen werden kann. Wir werden dies um den Punkt der wirtschaftlichen Hindernisse, den Sie genannt haben, erweitern.
## Ich will Ihnen als ganz konkretes Beispiel einen Arbeitgeber nennen, der in einem anderen Land eine Fabrik hat, der aber, wenn er den PaÃŸ des Landes verliert, beispielsweise ausgebÃ¼rgert werden wÃ¼rde, oder aus anderen GrÃ¼nden nicht mehr die MÃ¶glichkeit hÃ¤tte, frei zwischen den LÃ¤ndern zu verkehren. In solchen FÃ¤llen macht es sicherlich Sinn, daÃŸ man die Mehrstaatlichkeit hinnimmt.

## Von daher rate ich Ihnen: Lesen Sie einmal die AntrÃ¤ge Ihrer eigenen Fraktion. Auch Ihre Fraktion ist der Meinung, daÃŸ Â§ 87 ausgebaut werden muÃŸ, weil bisher Menschen, die die doppelte StaatsbÃ¼rgerschaft gar nicht wollen, aber darauf angewiesen sind, teilweise nicht erfaÃŸt werden.


## ...


## Ich halte nichts davon, irgendwelche Zahlen in die Landschaft zu setzen, was die Frage der DoppelstaatsbÃ¼rger angeht. Ich will Ihnen folgendes sagen, vielleicht auch als Argument dafÃ¼r, warum wir Skepsis haben. Ich habe vorhin erlÃ¤utert, warum ich mit den Regelungen fÃ¼r die erste Generation unzufrieden bin. Ich glaube, das Gesetz wird dazu fÃ¼hren, daÃŸ viele von der ersten Generation - leider, ich bedaure das sehr - von dem Instrument der EinbÃ¼rgerung zunÃ¤chst keinen Gebrauch machen werden, weil die Beibehaltung der StaatsbÃ¼rgerschaft fÃ¼r sie aus psychologischen GrÃ¼nden sehr wichtig ist. Die Punkte, die wir beim Â§ 87 des AuslÃ¤ndergesetzes genannt haben, die richtig und nachvollziehbar sind, werden den Kreis erweitern; aber es wird eine bestimmte Gruppe von Menschen geben, vor allem alte Menschen, die davon keinen Gebrauch machen werden, weil sie der Meinung sind, daÃŸ sie, wenn sie ihren PaÃŸ aufgeben mÃ¼ssen, emotionale Nachteile zu befÃ¼rchten haben.
## Von daher glaube ich nicht, daÃŸ es sich um Millionen handeln wird, die eingebÃ¼rgert werden wollen. Im wesentlichen wird das Geburtsrecht dazu fÃ¼hren, daÃŸ Kinder von AuslÃ¤ndern, die hier auf die Welt kommen und deren Eltern bereits hier gelebt haben, deutsche StaatsbÃ¼rger werden. Der Anteil derer, die sich auf Grund des AuslÃ¤ndergesetzes einbÃ¼rgern lassen, wird wachsen, aber er wird nicht in die Millionen gehen. Da kann ich Sie beruhigen.


## ...


## Nein, es ist deshalb kein Etikettenschwindel, Herr Kollege - ich erklÃ¤re es Ihnen noch einmal, vielleicht kommt es dann ja doch noch an -: Die doppelte StaatsbÃ¼rgerschaft ist keine Erfindung dieser Regierung. Wir haben nach den Gesetzen, die Sie mit verabschiedet haben, bereits bis zu 2 Millionen DoppelstaatsbÃ¼rger.
## ( Beifall beim BÃœNDNIS 90/DIE GRÃœNEN und bei der SPD )
## Dazu gehÃ¶ren die Nachfahren der Aussiedler, die zu uns kommen und von denen wir zu Recht - ich glaube, da sind wir uns alle einig - nicht verlangen, daÃŸ sie ihren PaÃŸ aufgeben, weil sie Nachteile hÃ¤tten, weil sie sich freikaufen mÃ¼ÃŸten. Auch binationale Ehen gehÃ¶ren dazu.


## debates on dual citizenship between 1996 and 2000

debates_dual1[[4]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 4th debate on dual citizenship between 1996 and 2000 for GRUENE


## Marieluise Beck 1998-11-12 GRUENE

p <- partition("GERMAPARL", speaker = "Marieluise Beck", date = "1998-11-12", encoding = "UTF-8")

read(p)


## Marieluise Beck (GRUENE)

## Integration ist ein Anspruch und eine An - strengung, zu der es keine Alternative gibt. Dies ist die Quintessenz des Memorandums meiner VorgÃ¤ngerin im Amt der AuslÃ¤nderbeauftragten, Frau Schmalz-Jacobsen. Ich mÃ¶chte an das politische VermÃ¤chtnis, in dem sich Ã¼brigens alle meine AmtsvorgÃ¤ngerinnen und - vorgÃ¤nger einig waren, anschlieÃŸen: erleichterte EinbÃ¼rgerung, rechtliche Gleichstellung und soziale Integration.Integration ist ein Anspruch und eine An - strengung, zu der es keine Alternative gibt. Dies ist die Quintessenz des Memorandums meiner VorgÃ¤ngerin im Amt der AuslÃ¤nderbeauftragten, Frau Schmalz-Jacobsen. Ich mÃ¶chte an das politische VermÃ¤chtnis, in dem sich Ã¼brigens alle meine AmtsvorgÃ¤ngerinnen und - vorgÃ¤nger einig waren, anschlieÃŸen: erleichterte EinbÃ¼rgerung, rechtliche Gleichstellung und soziale Integration.

## Die erleichterte EinbÃ¼rgerung bedeutet in der Tat auch die Hinnahme von Mehrstaatlichkeit. Wir alle wissen, wie schwer es ist, den PaÃŸ zurÃ¼ckzugeben, nicht nur weil dieser Vorgang den emotionalen Abschied von der Heimat bedeutet, sondern weil er auch bedeutet, daÃŸ die RÃ¼ckkehrmÃ¶glichkeit verschlossen ist. Es gibt keinen rationalen Grund, diese HÃ¼rde aufzubauen.
## Es ist infam - Herr SchÃ¤uble hat dies leider vor zwei Tagen in diesem Hause noch einmal getan -, im Zusammenhang mit der doppelten StaatsbÃ¼rgerschaft von '' ” Rosinenpickerei '' zu sprechen. Damit, Herr SchÃ¤uble - ich sage das auch an die Adresse der CDU/CSU-Fraktion -, wird ein sehr gefÃ¤hrlicher Weg der Diffamierung beschritten.
## Die CDU kann nicht das Wort von der Globalisierung immer im Munde fÃ¼hren, wenn sie sich auf der anderen Seite den RealitÃ¤ten eines modernen StaatsbÃ¼rgerschaftsrechts verschlieÃŸt. Sie fordern einerseits flexi - blere ArbeitsmÃ¤rkte und auch eine grÃ¶ÃŸere grenzÃ¼berschreitende MobilitÃ¤t, andererseits beharren Sie aber auf dem Blutrecht als Grundlage fÃ¼r die StaatsangehÃ¶rigkeit. Globalisierung relativiert die Nationalstaatlichkeit, was allerdings neues Denken im StaatsbÃ¼rgerschaftsrecht erfordert.
## Ich empfehle den Blick Ã¼ber die Grenzen. England und Frankreich haben das moderne StaatsbÃ¼rgerschaftsrecht. Als Grundlage dient die Hinnahme der doppelten StaatsbÃ¼rgerschaft. Schauen Sie bitte in diesem Zusammenhang auch nach Holland. Holland hat im Jahre 1996 18 Prozent der tÃ¼rkischen BevÃ¶lkerung eingebÃ¼rgert, wÃ¤hrend wir in diesem Zeitraum nur 1,6 Prozent einbÃ¼rgern konnten.
