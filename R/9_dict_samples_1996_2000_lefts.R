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

coi_lefts00 <- partition("GERMAPARL",
                 year = 1996:2000,
                 parliamentary_group = c("PDS", "LINKE", "LINKE/PDS"),
                 interjection= F,
                 role = c("mp", "government"))


## as partition bundles

pb1 <- partition_bundle(coi_lefts00, s_attribute = "date")


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

q1 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"', '".*[Ss]taatsb¼rger.*"',
        '".*[Ss]taatsangeh.*rig.*"', '".*[Ss]taatszugeh.*rig.*"', '"[Ss]taatenlos.*"',
        '"[Aa]us.*b¼rger.*"', '"[Ee]in.*b¼rger.*"', '"Doppelpass.*"', '"Doppelpa�Y.*"',
        '"Pass"', '"Pa�Y"', '"[Oo]ptionspflicht.*"',
        '"[Oo]ptionszwang.*"', '"Blutsrecht.*"', '"Geburtsrecht.*"', '"Geburtsprinzip.*"',
        '"[Ii]us soli"', '"[Ii]us sanguinis"', '"[Jj]us soli"', '"[Jj]us sanguinis"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Abstammungsrecht.*"', '"Abstammungsprinzip.*"')

q2 <- c('"[Dd]oppelstaat.*"', '"[Mm]ehrstaat.*"',
        '"[Dd]oppel.* [Ss]taat.*"', '"Doppelpass.*"', '"Doppelpa�Y.*"',
        '"[Oo]ptionspflicht.*"', '"[Oo]ptionszwang.*"', '"Optionsmodell.*"')

q3 <- c('".*[Aa]syl.*"', '".*[Ff]lucht.*"', '".*[Ff]l¼cht.*"', '".*[Mm]igra.*"', '".*[Ee]in.*wander.*"', 
        '".*[Gg]renz.*"', '"[Ff]amilienzusammen.*"', '".*[Aa]us.*b¼rger.*"',
        '".*[Aa]b.*schie.*"', '".*[Aa]b.*schob.*"', '".*[Ee]in.*b¼rger.*"', '".*[Aa]us.*sied.*"',
        '"Aufnahme.*"', '"[Vv]isa.*"', '"[Vv]isum.*"', '"LoyalitÃ¤tskonflikt"', '"Identit¤tsfeststellung"',
        '"R¼ckf¼hrung.*"', '".*[Aa]usl¤nd.*"','"[Aa]ufenthalt.*"', '"R¼ck¼bernahme.*"', '"Ehegattennachzug"', '"Duldung.*"',
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

debates_foreign1[[43]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T) # vary 1st to 43th debate on Foreigners� Policy for LINKE between 1996 and 2000

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

debates_citizen3[[8]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 7th debate on citizenship between 1996 and 2000 for LINKE


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
  ) # vary 1st to 2th debate on dual citizenship between 1996 and 2000 for LINKE


## Ulla Jelpke 1999-05-07 PDS

p <- partition("GERMAPARL", speaker = "Ulla Jelpke", date = "1999-05-07", encoding = "UTF-8")

read(p)

## Ulla Jelpke (PDS)

## Die PDS ist der Meinung, da�Y der heute vorliegende Gesetzentwurf trotz einzelner löblicher Ansätze, die in die richtige Richtung gehen, nicht geeignet für ein modernes und demokratisches Staatsbürgerschaftsrecht ist. Er ist allenfalls ein halbherziges Reförmchen, in das neue Rückwärtsgänge in manchen Paragraphen eingebaut sind.
## Es ist richtig, wie hier schon gesagt wurde: Seit Jahren diskutiert dieses Parlament über eine Staatsbürgerschaftsreform. In diesem Zusammenhang möchte ich daran erinnern, da�Y etwa 7 Millionen Migrantinnen und Migranten in Deutschland, die keinen deutschen Pa�Y haben, gehofft haben, da�Y ihre Situation auch mit diesem Gesetz - ich sage bewu�Yt: auch mit diesem Gesetz - endlich eine Veränderung erfährt, und der Zustand beendet wird, häufig als Menschen zweiter oder dritter Klasse in diesem Land behandelt zu werden. 

## Es ist heute viel Kritik geübt worden. Ich möchte auf einen Punkt eingehen, den Herr �-zdemir - auf eine Frage an ihn hin - angesprochen hat. Vielleicht kann ich Ihnen da helfen.
## Da dieses Gesetz nicht die Möglichkeit der Mehrstaatigkeit eröffnen soll, was Sie, SPD und Grüne, allerdings vor den Wahlen versprochen haben, mu�Y einmal auf die Kritik der Migrantenverbände und - organisationen hingewiesen werden. Diese haben durchweg kritisiert, da�Y dieser Gesetzentwurf nicht weit genug greift und da�Y durch die Verengung der Möglichkeit der Mehrstaatigkeit vor allen Dingen Türkinnen und Türken ausgegrenzt werden.

## Natürlich kann derjenige, der türkischer Herkunft ist und Deutscher werden will, die deutsche Staatsangehörigkeit bekommen. Aber der Härtefall wird niemals eintreten, da beispielsweise die Erbschaftsregelungen längst zwischen Deutschland und der Türkei getroffen worden sind; sie behalten das Erbschaftsrecht. Von daher ist der Kreis derer, die die Mehrstaatigkeit haben werden, sehr eingegrenzt. Sie wissen aber doch besser als ich, da�Y viele Menschen türkischer Herkunft, die hierhergekommen sind, ihre türkische Staatsbürgerschaft überhaupt nicht abgeben wollen. Das ist heute auch schon einmal gesagt worden.

## Wir haben au�Yerdem darauf hingewiesen, da�Y die Forderung, da�Y Menschen die deutsche Sprache ausreichend beherrschen müssen, undefiniert ist und da�Y das wahrscheinlich eine sehr hohe Hürde für die Einbürgerung von Menschen darstellt. Ich meine insbesondere diejenigen, die aus der älteren Generation kommen.
## Wir finden besonders bei einer rotgrünen Regierung skandalös, da�Y sie die Gebühren für die Einbürgerung von 100 DM auf 500 DM pro Person erhöhen möchte. Wenn wir uns überlegen, was das für eine Familie kosten würde, dann mu�Y ich schon sagen: Das ist völlig unverständlich.


## debates on dual citizenship between 1996 and 2000

debates_dual1[[2]] %>%
  read() %>%
  highlight(orange = q4_regex,
            lightgreen = q1_regex,
            red = q2_regex,
            regex = T
  ) # vary 1st to 2th debate on dual citizenship between 1996 and 2000 for LINKE


## Ulla Jelpke 1999-03-19 PDS

p <- partition("GERMAPARL", speaker = "Ulla Jelpke", date = "1999-03-19", encoding = "UTF-8")

read(p)

## Ulla Jelpke (PDS)

## Herr Zeitlmann, wer im Niveau so weit heruntergeht, wie Sie, aber auch Ihre Partei es heute wieder mit der Parole getan haben, da�Y der Doppelpa�Y dazu führe, da�Y Kriminelle leichter eingebürgert werden können, wer sich dazu hinrei�Yen lä�Yt, diese Kampagne mit der Parole zu führen, da�Y die Gefahren durch ein modernes Staatsangehörigkeitsrecht bzw. den Doppelpa�Y grö�Yer seien als in den siebziger Jahren die Gefahr durch die RAF, wer Angstmache betreibt mit der Parole, da�Y Menschen ausländischer Herkunft dann auch das Wahlrecht haben und das Ausland die Interessen der Deutschen beeinflussen könnte, wer den Familiennachzug prophezeit und damit Angst erzeugen will, der arbeitet Rechtsextremisten ganz offensichtlich in die Arme, der fördert ein Bewu�Ytsein, das Ausländerfeindlichkeit und Rassismus schürt und den rassistischen Mob, wie wir gesehen haben, auf die Stra�Ye bringt.

## Das liegt meines Erachtens nicht nur an der neuen Mehrheit; denn wer sich die Umfragen anschaut, der wei�Y, da�Y es eben keine gro�Ye Mehrheit in der Bevölkerung gegen den Doppelpa�Y bzw. gegen ein modernes Staatsbürgerschaftsrecht gibt, der wei�Y, da�Y viele Menschen der doppelten Staatsbürgerschaft nur deshalb ablehnend gegenüberstehen, weil sie zuwenig über diesen Pa�Y wissen. Wenn von der Gegenseite, einschlie�Ylich der linken Opposition, eine Kampagne geführt worden wäre, dann wären wir heute mit Sicherheit einen Schritt weiter.
## Wir haben heute einige Redner gehört, die den jetzigen Entwurf, das Optionsmodell, schöngeredet haben. Ich möchte hier eine Aussage des Verbandes binationaler Familien und Partnerschaften zitieren: Der Berg krei�Yte und gebar eine Maus.

## Wer die Geschichte der Debatten über das Staatsangehörigkeitsrecht in diesem Hause verfolgt hat, der wei�Y: Herr Westwelle, Sie haben unrecht. Die F.D.P. hat 1993 die Mehrstaatigkeit, den Doppelpa�Y, gefordert und hat schon 1994 in den Koalititionsvereinbarungen formuliert, da�Y eine Schnupperstaatsbürgerschaft möglicherweise denkbar wäre. Das ist natürlich nicht umgesetzt worden. Sie machen jetzt im Grunde genommen ein Kompromi�Yangebot an die CDU, obwohl Sie wissen, sie wird diesem Entwurf, diesem Optionsmodell nicht zustimmen.

## Meine Damen und Herren, ich fand es nicht wenig demütigend, da�Y die SPD und die F.D.P. als erste bekanntgeben, da�Y man sich über das Optionsmodell geeinigt habe. Auch die Grünen haben meines Wissens aus den Medien erfahren müssen, was jetzt der neue Entwurf sein soll.

## Für viele Menschen - das habe ich schon angedeutet - wird es in diesem Land keinen Doppelpa�Y geben. Für viele Menschen ist das eine gro�Ye Enttäuschung. Für viele Menschen ist es längst so, da�Y Deutschland ein Einwanderungsland ist. Die Erleichterungen der Einbürgerung wären in der Tat wichtiger denn je gewesen, um endlich dem Klima in diesem Land, was Rassismus und Ausländerfeindlichkeit angeht, etwas entgegenzusetzen.
## Zweifellos, in diesem Entwurf gibt es einige positive Ansätze. Ich nenne hier die Fristen, ich nenne die stückweise Abkehr vom Abstammungsrecht, also hin zum Jus soli, was meiner Meinung nach aber auch nur halbherzig passiert. Ich nenne die Tatsache, da�Y es in Zukunft möglich sein wird, schneller und leichter Frauen und Kinder oder Männer und Kinder einzubürgern, die einen deutschen Partner bzw. eine deutsche Partnerin geheiratet haben.

## Verfassungsrechtlich bedenklich ist beispielsweise, da�Y binationale Kinder, die hier geboren sind, den Doppelpa�Y behalten dürfen, während Kinder mit Eltern ausländischer Herkunft sich mit 18 Jahren für eine Staatsbürgerschaft entscheiden müssen. Wie wollen Sie das mit dem Gleichheitsgrundsatz in �obereinstimmung bringen?
## Und was soll nach Auffassung der Bundesregierung geschehen, wenn jemand seinen Pa�Y nicht freiwillig abgibt? Ich denke zum Beispiel daran, da�Y eine junge Frau mit 21 Jahren für einen Landtag kandidiert und sich noch nicht entschieden hat, welche Staatsbürgerschaft sie annehmen will. Was passiert dann? Mu�Y sie ihr Mandat abgeben? Wird sie zwangseingebürgert oder zwangsausgebürgert?

## Ich finde es falsch, da�Y wir uns einzig und allein auf die Frage des Staatsbürgerschaftsrechts beziehen, wenn wir die Debatte führen, wie Menschen integriert werden können. Es besteht die Gefahr - Sie haben selber seit 1993 heftig daran mitgewirkt, vor allen Dingen die rechte Seite in diesem Haus -, da�Y Rechte für Menschen ausländischer Herkunft abgebaut werden. Wenn wirklich gleiche Rechte für Menschen existieren würden, die hier ihren Lebensmittelpunkt haben, dann - Herr Bosbach, da würde ich Ihnen recht geben - würden wahrscheinlich viele gar nicht die deutsche Staatsangehörigkeit anstreben und bei ihrer Staatsbürgerschaft bleiben.
## Diese Debatte mu�Y geführt werden und darf nicht vernachlässigt werden. Denn nur so ist meiner Meinung nach ein wirklich gleichberechtigtes Leben in diesem Land möglich, ist der Kampf gegen Rassismus und Ausländerfeindlichkeit zu führen und tatsächlich ein Stück Frieden in dieses Land einzubringen.
