


library(polmineR)

library(magrittr)

library(tm)

use("GermaParl")


------------------------------------------
## ".*[Aa]us.bürger.*"
------------------------------------------

kwic(
  "GERMAPARL",
     query= '".*[Aa]us.*bürger.*"',
     left= 10, right= 10, cqp= TRUE,
     s_attribute= c(
       "parliamentary_group", "speaker", "date", "url"),
     interjection = FALSE, regex = TRUE) %>%
highlight(
     lightgreen= c(
        "geflohen", "geflohene", "Wehrdienstes", "Paß", "Menschen", "DDR",
        "Optionskinder", "Bestrebungen", "Rentenleistungen", "Vermögenswerte",
        "Auskunft", "Vertrauensschutz", "reformieren", "doppelte", "Staatsbürgerschaft",
        "türkische", "EU-Bürgerinnen", "ausgereist", "schändliche", "Entscheidungsmöglichkeit", "staatenlose",
        "Baath-Regimes", "tagtäglich", "zwangsweise", "Enteignung", "Tschechoslowakei", "Dekrete", "Unrecht",
        "Benes-Dekrete", "Vertreibung", "Sudetendeutschen", "Exzesse", "unschuldig", "Leid", "zugefügt", "ehemaligen",
        "Erklärung", "Bundesregierungen", "Domaschk", "Türkei", "Einschlägige", "Vertei", "unschuldiges", "Staatsangehöriger",
        "Wehrdienstes", "Einbürgerungen", "türkischer", "Abschiebungen", "abgestellt", "vornimmt", "beeeinflussen", "erteilen",
        "angesprochenen", "unvermindert", "anhält", "freigekaufte", "zermürbendes", "Häftlinge", "Westen", "zugeleitet",
        "Abschaffung", "Gebühren", "Staatsangehörigkeit", "entlassen", "Annahme", "verlangt", "Verlieren",
        "Kriminellen", "Sachverhalte", "Kreißsaal", "Mehrstaatlichkeit", "Paß", "Willkommenskultur", "zwangsweise",
        "Schwarzen", "Homosexualität", "Fehlentwicklung", "Letztes", "überwiegende", "erworben", "Rassismus", "abschieben",
        "abgeschoben", "Abschiebung", "facto", "doppelte", "begangen", "schweren"),
      regex = FALSE, interjection = FALSE
      )



------------------------------------------
##  ".*[Ee]in.*bürger.*"
------------------------------------------

  
words1 <- c("Staatsangehörigkeit", "erleichterte", "Staatsbürgerschaft", "Integration", "Hinnahme",
           "erleichtern", "doppelten", "Erleichterung", "Integrationsprozesses", "Migranten",
           "doppelte", "Mehrstaatigkeit", "erleichterten", "Regelanfrage", "Voraussetzungen",
           "Hürden", "Rechtsanspruch", "Erleichterungen", "Ausländer", "lassen",
           "Einbürgerung", "einbürgern", "laßt", "Einwanderer",
           "Staatsangehörige", "diejenigen", "jemanden", "Migranten", "Pass", "Personen",
           "eingebürgert", "erfüllen", "Einbürgerungshürden", "Vereinspolitik", "integrationsbereiten",
           "Menschen", "Zahl", "Einbürgerungen", "gestiegen", "Jahr", "Ausgrenzungen", "Wahlrecht",
           "Ausgrenzung", "umfassend", "erleichtert", "Rückgang", "Anschauung", "Gesinnung", "innere",
           "überprüft", "nachvollziehen", "Einbürgerungstests", "testen", "vermeindliche",
           "Geburt", "per", "Fremdenfreundlichkeit", "Duldungs", "islamistisch-fundamentalisitschen",
           "Muslimtest", "Subkultur", "Rechtsänderung", "Staatsbürgerschaftsrecht", "Jahresbeginn", "einhergehende",
           "großzügige", "Staatsangehörige", "eingeführte", "verwehrt", "schlimme", "verletzen",
           "religiöse", "jemanden", "irgend", "Kriterium", "Hürden", "Integrationsbemühungen", "befördert",
           "entgegen", "meisten", "passende", "geborenen", "Ausländerinnen", "Dokument", "Kanzleramt",
           "Migranten", "Angelegenheit", "erstmals", "irrelevanter", "überreichen", "Akt",
           "Migrantinnen", "rechtlich", "stattgefunden", "auzunehmen", "Bundeskanzlerin", "Einbürgerungszahlen",
           "naturgemäß", "aufwachsen", "sinkende", "gesellschaftliche", "überprüfen", "rechtlichen", "verlieren", "Teilhabe",
           "Einbürgerungsquoten", "Prozentual", "schlechtesten", "stieg", "dreimal", "stützen", "Schweden", "Deutschland",
           "loszutreten", "begrüßenswert", "befürwortet", "werben", "einbürgern", "grundlegend", "scheitern",
           "hinnehmen", "Identitätsstiftung", "gutheißen", "könne", "verstanden", "Schulabschluss",
           "bildungsfernere", "verschiedene", "Verfassungstreue", "abgefragt", "Inwiefern", "Schulsystem",
           "durchlaufen","Tests", "Frage", "Inwieweit", "nachweisen", "deutschen", "verschiedene",
           "Einbürgerungsbewerberinnen", "bewerber", "vermeiden", "Bundesländer", "Einbürgerung", "novellierte",
           "Ermutigung", "Beauftragte", "Gesetzteslage", "aufklären", "Kürze", "bemühen", "Integrationsgipfeln", "durchaus",
           "Mehrstaatigkeit", "lebende", "Festhalten", "verkürzt", "ideologische", "Fristen",
           "Selbstverpflichtung", "Gebühren", "Verschärfung", "Vermeidung", "Themas", "Belehrungsbedarf",
           "willkommen", "stehe", "derjenige", "SPD-Bundestagsfraktion", "verankert",
           "Einbürgerungskurse", "angeforderte", "vorgeschlagenen", "geplante", "absurde", "Züge",
           "verweise", "aktuellen", "Staatsbürgerkursen", "Leibe", "rücken", "überprüfen",
           "Gesinnungstests", "einbürgerungswilligen", "gehend", "Irre", "nützen",
           "bilaterales", "Austausch", "verhandeln", "Abkommen", "abzulegen", "Grundgesetz", "eingeführt",
           "Fluss", "abgearbeitet", "abzubauen", "debattieren", "beklagt", "Einbürgerungsverfahren",
           "human", "beseitigt", "Einbürgerungshindernis", "gestaltet", "beseitigten", "gestalten", "Gesetzeswerk",
           "wirkungsvoll", "Innenausschuß", "Placebo", "vermeidet", "Optionsmodell", "bekommt", "Kinderausweis",
           "Unionsentwurfes", "Verlustgründe", "Kinder", "ausländischen", "Ausländergeneration", "beantragt",
           "eigentliche", "Gutschein", "Alternative", "vorzuenthalten", "Modell", "aufführt", "Optionsmodells",
           "wertlos", "Irrglauben", "Leistungen", "derjenigen", "Ausnhamen", "gemeint", "Ziffern", "Seminar",
           "Gesetzestext", "überführt", "geltendes", "veranstalten", "abhängen", "Interpretation", "vorlesen",
           "Verkürzung", "Territorialprinzips", "Anspruchseinbürgerungen", "Vertriebene", "Härtefälle",
           "Kernpunkte", "hinlänglich", "einräumt", "Aussiedler", "Ausnahmeregelung", "verkürzen", "Erwachsene", "verkürtzt",
           "verbesserte", "deutlichen", "wirkliche", "Stellenwert", "denkt", "ausgehen", "letztlich", "aufgetauchte",
           "Kinderstaatszugehörigkeit", "verunglückte", "Koalitionsvertrages", "Variante", "jüngst", "Aufstockung",
           "Lebensjahr", "sinnvolle", "Rechtslage", "frühere", "Selbststudium", "Andersgläubigen",
           "Einbürgerungsvoraussetzungen", "angehobenen", "Fußballnationalmannschaft", "verfassungsfeindlichen",
           "Rechtstreue", "erfragen", "aufnehmenden", "Einbürgerunstest", "berücksichtigt", "Verfassungsordnung",
           "Potenzials", "sachgerechter", "Hinwendung", "skandalösen", "Gesinnungstest", "Sprachniveau", "miserable",
           "representative", "ausdehnen", "Vorschrift", "Befragung", "abwarten", "absurd", "erfüllen", "Sprachkenntnisse",
           "erschwert", "Staats-angehörigkeit", "Einbürgerungsverhalten", "Hildesheimer", "anforderungen",
           "Einbürgerungsantrag", "ausreichende", "annehmen", "Absenkung", "Einbürgerungsbewerber",
           "Einbürgerungsbehörden", "Ermessensentscheidung", "Loyalität", "Untersuchungen",
           "Einbürgerungsvoraussetzungen", "Forschungsgruppe", "Optionsregelung", "übertriebenen",
           "integrationspolitischen", "Betrachtet", "unionsgeführten", "vernachlässigt",
           "auswirken", "wider", "wissenschaftliche", "umfasst", "allgemein", "vergleichbar", "bürokratischen",
           "stehnden", "Aufwand", "Fälle", "angeht", "Regelanfrage", "Einbürgerungshindernisse",
           "verfassungsschutz", "Strafvorschrift", "Rechtsansprüche", "Ermessensregelungen",
           "Behördenpraxis", "Ayslstatus", "Täuschungsversuche", "bundesweit", "Gebrauchsanweisung",
           "Bundeseinheitliche", "Rechtssystematik", "Optionspflichtige", "Flüchtlinge", "Optionsregelung",
           "Bundeamts", "repräsentativen", "Mitbürgern", "befragt", "Bundesamt", "belegen",
           "baden-württembergischen", "erhebliche", "Wissensfragen", "handhabenden",
           "Entlassungsbemühungen", "Gesprächsleitfaden", "Landesregierung", "Einbürgerungsvoraussetzungen",
           "lahmlegen", "ansässiger", "unkomplizierte", "Aufweichen", "Mehrstaatlichkeit", "entwertet",
           "Ermöglichung", "Aufweichung", "Briten", "generelle", "weiten", "drittens", "akzeptiert",
           "weiten", "Teilen", "akzeptiert", "Bevölkerung", "Explosionsstoffgesetz", "Staatsgesellschaft",
           "zurücknähme", "dazuzugehören", "integrationspolitisch", "Iraner", "erleichterte", "vertiefte", "Spätaussiedlern",
           "verschweigt", "Lebenspartner", "kleinreden", "Zukunftschancen", "Herkunftsstaatsangehörigkeit",
           "Mittelmaß", "extremen", "bescheinigt", "folgendermaßen", "Integrationspolitik", "abzugeben", "europäisches",
           "europäischen", "schlechtesten", "Vergleich", "Einbürgerungsrate", "Prozentual", "Neunte",
           "Eu-Ausländer", "woraus", "Staatsangehöriger", "türkischer", "resultieren", "Statistischen",
           "niedriegsten", "Deutschland", "verantwortungslose", "dreimal", "Migrationshintergrund",
           "dopplete", "differenziert", "niedrige", "normal", "Niveau", "Unterhaltungsfähigkeit",
           "Einbürgerungsbewerbern", "Ausländerin", "eingebürgerten", "unabhängig", "Straflosigkeit",
           "entlarvend", "rechtmäßig", "verkürzt", "eigenständigen", "generelle", "Mitbürger",
           "erworben", "lebenden", "Quizshow", "Hauptschulabschluss", "Test", "bestanden",
           "Staatsangehörigkeitsgesetz", "Einbürgerungstest", "Teilnahme", "Staatsaufbau",
           "bestehen", "abgelegt", "ablegen", "Tests", "Orientierungskurses", "partizipierenden",
           "Hürde", "Schulabschluss", "testweise", "abschreckend", "Ausgestaltung", "Nichtvorliegens",
           "beteiligen", "Zahlen", "Prüfungsfragen", "beantworte", "Orientierungskursen", "Erfolgsschlager",
           "Absolvierung", "Schulferien", "albernen", "Pools", "Dämonisierung", "wirken", "Teilnahme",
           "Voraussetzung", "IMK", "vorbereitenden", "Test", "IMK-Beschlusses", "Lehrstoff", "Selbststudium",
           "Teilnahme", "IMK-Beschluss", "Orientierungskursen", "betraute", "angeforderte", "geplante",
           "Planung", "Gesinnungstest", "organisiert", "Vorbereitung", "Einbürgerungsbewerber",
           "Schulferien", "Curriculum", "Einbürgerungsoffensive", "Einwanderungsoffensive", "sprechen",
           "vornehmen", "felsenfesten", "Pappkameraden", "dezidiert", "überwiegende", "unmißverständlichen",
           "Neubürger", "nachdrücklichen", "verlangen", "zugleich", "Selbstläufer", "dahinter", "steckt",
           "Augsburg", "Einbürgerungszahlen", "zurückgegangen", "Einbürgerung", "dramatisch", "gesunken",
           "Fünftel", "sinkende", "Durchschnittswerte", "Inkrafttreten", "Tiefstand", "Einbürgerungen",
           "Rückgang", "Jahrzehnts", "Frühjahrszeiträumen", "reformbedingte", "diametraler",
           "Einbürgerungsrechts", "modernes", "überprüfen", "Verschärfungen", "Hemmnisse",
           "Völkisch", "PKK-Kämpfer", "getreten", "Belange", "behördliches", "Teilhaberechte",
           "Einbürgerungsverfahren", "Erschwernisse", "ebnet", "aufenthaltsrechtliche",
           "sträuben", "reformierte", "großzügigsten", "liberalsten", "Integrationsleistung",
           "Drittstaatler", "nachweisbare", "Unverzichtbar", "Projektarbeit", "kommunales",
           "Hürden", "geringe", "Unzumutbarkeiten", "Eingewanderte", "Ausländerfragen",
           "Einbürgerungszahlen", "vorzuenthalten", "Arbeitskreises", "eingebürgert",
           "unseres", "Test", "Herzstück", "Widerstände", "liberalen", "erlangen",
           "insbesondere", "Votum", "demokratischer", "Bundesamt", "zusammengestellten",
           "Mehrstaatigkeit", "weitergeleitet", "Statistische", "Statistischen",
           "Erstellung", "deutschem", "eindrucksvoll", "Migrationshintergrund",
           "generell", "türkischstämmigen", "Plenum", "einbringen", "größte",
           "Staatsangehörigkeitsrecht", "Staatsangehörigkeitsrechts", "enthaltenen",
           "umfassender", "Integrationspolitik", "Bundesregierung", "einladende",
           "Einwanderungspolitik", "eingebürgert", "scheut", "plädieren", "modernes", "Sachverhalt",
           "sanguinis", "Rennpferd", "ius", "Anwerbeabkommens", "Verstößen", "ablehne", "fällige",
           "Lebensmittelpunkt", "Einbürgerungen", "gleichberechtigten", "ungeachtet", "enthaltenden",
           "umfassender", "Intregationspolitik", "Bundesregierung", "korregiert", "wesentlichen",
           "entwickelt", "Zuspitzung", "Komplexität", "Problematik", "dient", "symbolischen", "senken",
           "Aufenthaltsfristen", "verzichten", "symbolisches", "Einbürgerungstest", "Krönung", "beträchtlich",
           "vereinfachen", "verkürtzt", "EU-Staaten", "insoweit", "Bellevue", "Schloss", "Bundespräsident",
           "Rede", "eingebürgert", "US-amerikanische", "bemerkenswerte", "Gauck", "Roth", "Winkler", "Wulff", "Maas", "anlässlich",
           "Merkel", "Gewicht", "letztes", "eingeladen", "Domenico", "Inlandsaufenthaltes",
           "anzufragen", "Einbürgerungsvoraussetzungen", "Ausländers", "Costa", "Mutmaßungen",
           "Bundesverwaltungsgerichtes", "beschieden", "Motiven", "angegeben", "Besten", "Ausländern",
           "unterliegt", "stellen", "Einwanderungsgesellschaft", "menschennahen", "einladenden", "modernen",
           "gleichrangige", "leichtfertige", "skurril", "Zauberwort", "Eingleiderungsmaßnahmen", "intakten",
           "abschreckende", "gerecht", "Gesetzesbegründung", "puncto", "FDP-Antrag", "Politikerinnen",
           "Integrationspolitik", "Sprachkenntnisse", "materielle", "Zwang", "Vorteile", "verfügen",
           "rechtliche", "überzeugend", "alt", "Grindel", "praktisch", "Rentner", "angeblich", "Migranten",
           "Staatsbürgerschaft", "Hinnahme", "Ausländer", "Einbürgerung",
           "Mehrstaatigkeit", "Voraussetzungen", "Lebensjahr", "Staatsangehörigkeit", "Begriff",
           "Flüchtlingsstatus", "Iraner", "el-Masri", "leben", "einbürgern",
           "unkompliziert", "zehnte", "Ausländerinnen", "Staatsangehörige", "Einbürgerungsgesetz",
           "Ausländerzentralregister", "sozioökonomische", "Bildungsabschlüsse", "Lebensunterhalts", "rassistisch",
           "günstigere", "Verschärfungen", "Einfach", "festzuschreiben", "Merkmale", "Befund", "ausweisen", "diskriminiert",
           "Nachhinein", "erfolgreicher", "Statistiken", "Türken", "Pass", "Türkin", "eingebürgerten", "lebenden",
           "Deutschen", "Ausländern", "Türkischstämmigen", "Nichtseßhaften", "Güterständen", "deutschen",
           "Einbürgerungsanspruch", "Abstammungsprinzip", "nachziehende", "zugewanderten", "türkischstämmigen",
           "Türkinnen", "trifft", "Studie", "Doppelstaatler", "Prozent", "Beibehaltung", "anforderungen",
           "Geburtsjahrgang", "separat", "Praxis", "Nachzug", "alten", "Nunmehr", "durchgeführte", "ausblenden", "Deutschland",
           "formale", "Menschen", "Einbürgerung", "Kinder", "Jahre", "Ausländer", "Begriff", "Hinnahme", "leben", "Personen",
           "Zahl", "Mehrstaatlichkeit", "Voraussetzungen", "Prozent")

kwic(
  "GERMAPARL",
  query= '".*[Ee]in.*bürger.*"',
  left= 10, right= 10, cqp= TRUE,
  s_attribute= c(
    "parliamentary_group", "speaker", "date", "url"),
  interjection = FALSE, regex = TRUE
  ) %>%
highlight(
  lightgreen = words1,
  regex = FALSE, interjection = FALSE
  )


------------------------------------------
##  "[Ss]taatenlos.*"
------------------------------------------
  
kwic(
  "GERMAPARL",
    query= '"[Ss]taatenlos.*"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
highlight(
    lightgreen= c(
      "staatenlose", "Kurden", "Syrien", "Staatangehörige", "Palistinenser", "doppelt", "Baath-Regimes",
      "radikalisierten", "Puntland", "ausgebürgert", "konsularische", "Rechtsnorm", "zerfallenden", "Personen",
      "stammende", "hinfällig", "leben", "gescheiterte", "abgeschobenen", "Rückführung",
      "Syrerinnen", "Syrern", "gewöhnlichen", "Drittstaatenangehörigen", "demgegenüber", "Übereinkommens",
      "Abschiebung", "Aufenthalt", "vermeintlich", "Roma", "Text", "Minestnormen", "anderweitig", "Status",
      "Schutz", "Anerkennung", "Flüchtlinge", "internationalen", "Ausreisepflichtiger", "Rückübernahme",
      "Damaskus", "allerletzte", "Flüchtlings", "Ausländerbehörden", "Staatangehörigen",
      "Flüchtlingskonvention", "Durchbeförderung", "Drittstaatenangehöriger", "Komponenten",
      "Somit", "Übernahme", "eigener"),
    regex = FALSE, interjection = FALSE
    )

------------------------------------------
##  "[Ss]taatsbürger.*"
------------------------------------------
  
words2 <- c(
    "Kosovo", "Wahlen", "kosovarisch-serbischen", "beteiligen", "binationalen", "Stimmabgabe", "EU-Bürger", "serbischen", "lebenden", "aufgerufen",
    "geboren", "Mutter", "bekommt", "Kind", "geschehen", "Terrororganisation", "Staatsbürgerschaft", "Verlust", "kämpfen", "nenne",
    "Ausland", "Lorenzo", "Giovanni", "einzubürgern", "bringt", "di", "Hinnahme", "Probleme", "angeklungen", "durften", "aufhalten",
    "Staatsangehörigkeit", "eingebürgerte", "Eingebürgerten", "Listen", "cleveren", "illegalen", "Staatsangehörigkeiten", "Loyalität", "Pässe",
    "Bekleidungssitten", "illegaler", "Spannungen", "hingenommen", "Haydar", "Machokultur", "Mehrstaatler", "heranzüchten", "Ernstes", "terroristische",
    "Problemlos", "deutsches", "Penner", "Millionen", "Fehdehandschuh", "Staatsangehörigkeit", "zwei", "Personalausweisgesetztes",
    "Anspruchseinbürgerung", "hingeworfen", "großangelegte", "Loyalitätskonflikte", "geraume", "mußt", "Staatsangehörigkeitsgesetz", "Konfliktfall",
    "Diktion", "Gegenargument", "Mikrozensus", "denkbare", "Staatsangehöriger", "gezielte", "beträgt", "19jährigen", "Wehrdienst", "plötzlich",
    "leistet", "dritten", "Doppelstaatlichkeit", "Streitpunkt", "hinzunehmen", "Sendboten", "engherzige", "Definieren", "Namentlich", "übelste",
    "vermögensrechtliche", "näherzukommen", "Notwenigkeit", "Mehrstaatlichkeit", "bestimmten", "Paulus", "reformschritt", "Damaskus", "Fällen",
    "unbegrenzten", "millionenfach", "zerrissen", "millionenfache", "hierdurch", "irgendwelche", "Begründung", "entstehen",
    "Staatsangehörigkeitsrecht", "erwerben", "großem", "bietet", "Maße", "verfallen", "Irrtum", "Migranten", "privilegiert", "Betreffenden",
    "Integration", "könne", "Staatsbürger", "erschwert", "integriert", "Jus", "Adelsprivileg", "politisch", "reaktionäres", "gleichzeitig",
    "sollen", "Staatsangehörigkeit", "Rechtsprinzip", "sanguinis", "frohlocken", "hochkomplexe", "denen", "täglichen", "Lebens", "akzeptieren",
    "abgeschlossen", "Nurdeutsche", "Berti", "Vogts", "Unverhältnismäßig", "bestimmten", "Nationalmannschaft", "bereichern", "Ämtern",
    "Schmalz-Jacobson", "Staatsbürger", "Landschaft", "Leitung", "Gerichten", "klarmachen", "Wohnsitz", "Pflichten", "festen", "richten",
    "Rechte", "einfach", "Privileg", "ganz", "generellen", "Doppelstaatsbürgerschaft", "erleichterte", "flächendeckender", "Einbürgerung",
    "betrachtend", "Füße", "bayrische", "Staatsbürgerschaft", "Selin", "Iran", "Schily", "Kopf", "deutsche", "akzeptieren", "Ausländerkriminalität",
    "Länderinteressen", "entlässt", "Freundlichkeit", "Kampfansage", "ius", "soli", "Aufenthaltsstatus", "Bundesgebiet", "erwerben", "Geburt",
    "zugelassen", "Papier", "verstärkt", "Erde", "Bundesrepublik", "sogenannte", "Ländern", "Deutschland", "abgeschlossen", "vorhandener",
    "Rücksicht", "Gestatten", "Einzelfall", "Konflikt", "gelöst", "Bundeswehr", "Uniform", "deutsch", "Leitbild", "Führung", "inner",
    "Soldat", "Staatsbürger", "Streitkräfte", "Prinzip", "türkisch", "Vereinigung", "CDU", "Unterricht", "Bildung", "Recht",
    "Pflicht", "politische", "deutsche", "türkische", "syrische", "ausländische", "Visumpflicht", "britische", "Türkei", "iranische",
    "Ausland", "entführt", "Visumfreiheit", "Kurzaufenthalt", "Aufenthalt", "Deutschland", "Opfer", "Ausländer", "Fürsorgeabkommens",
    "Visafreiheit", "konsularische", "türkischen", "deutschen", "Staatsangehörigen", "eigenen", "Staaten", "Staatsangehörigkeit",
    "lebenden", "ausreisepflichtigen", "türkischer", "mazedonischen", "Hoheitsgebiet", "serbischen", "Ehegattennachzug", "Mehrstaatigkeit",
    "Ehegatten", "syrischen", "Einreise", "Mitgliedstaats", "deutscher", "türkischer", "Kurzaufenthalte", "Einreise", "Auslieferung", "ausländischer",
    "Evakuierung", "konsularischer", "assoziationsrechtlichen", "befindlicher", "Rückführung", "Rechtsstatus", "Bundesdienststellen",
    "visumfreie", "Gefangentransporte", "Obhut", "eigener")

kwic(
  "GERMAPARL",
    query= '"[Ss]taatsbürger.*"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
highlight(
    lightgreen= words2,
    interjection = FALSE, regex = FALSE
    )

------------------------------------------
##  "[Dd]oppel(.*|[])staat.*"
------------------------------------------
  
  
kwic(
  "GERMAPARL",
    query= c('"[Dd]oppelstaat.*"|"[Dd]oppelt.*" [] "[Ss]taat.*"| "[Dd]oppel.*" "[Ss]taat.*"'),
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
  highlight(
    lightgreen= c(
      "Kosovo", "Wahlen", "kosovarisch-serbischen", "beteiligen", "binationalen", "Stimmabgabe",
      "EU-Bürger", "serbischen", "lebenden", "aufgerufen", "geboren", "Mutter", "bekommt", "Kind", "geschehen",
      "Terrororganisation", "Staatsbürgerschaft", "Verlust", "kämpfen", "nenne", "Ausland", "Lorenzo",
      "Giovanni", "einzubürgern", "bringt", "di", "Hinnahme", "Probleme", "angeklungen", "durften", "aufhalten",
      "Staatsangehörigkeit", "eingebürgerte", "Eingebürgerten", "Listen", "cleveren", "illegalen",
      "Staatsangehörigkeiten", "Loyalität", "Pässe", "Bekleidungssitten", "illegaler", "Spannungen",
      "hingenommen", "Haydar", "Machokultur", "Mehrstaatler", "heranzüchten", "Ernstes", "terroristische",
      "Problemlos", "deutsches", "Penner", "Millionen", "Fehdehandschuh", "Staatsangehörigkeit", "zwei",
      "Personalausweisgesetztes", "Anspruchseinbürgerung", "hingeworfen", "großangelegte", "Loyalitätskonflikte",
      "geraume", "mußt", "Staatsangehörigkeitsgesetz", "Konfliktfall", "Diktion", "Gegenargument",
      "Mikrozensus", "denkbare", "Staatsangehöriger", "gezielte", "beträgt", "19jährigen", "Wehrdienst",
      "plötzlich", "leistet", "dritten", "Doppelstaatlichkeit", "Streitpunkt", "hinzunehmen", "Sendboten",
      "engherzige", "Definieren", "Namentlich", "übelste", "vermögensrechtliche", "näherzukommen",
      "Notwenigkeit", "Mehrstaatlichkeit", "bestimmten", "Paulus", "reformschritt", "Damaskus", "Fällen",
      "unbegrenzten", "millionenfach", "zerrissen", "millionenfache", "hierdurch", "irgendwelche",
      "Begründung", "entstehen", "Staatsangehörigkeitsrecht", "erwerben", "großem", "bietet", "Maße",
      "verfallen", "Irrtum", "Migranten", "privilegiert", "Betreffenden", "Integration", "könne", "Staatsbürger",
      "erschwert", "integriert", "Jus", "Adelsprivileg", "politisch", "reaktionäres", "gleichzeitig", "sollen",
      "Staatsangehörigkeit", "Rechtsprinzip", "sanguinis", "frohlocken", "hochkomplexe", "denen", "täglichen",
      "Lebens", "akzeptieren", "abgeschlossen", "Nurdeutsche", "Berti", "Vogts", "Unverhältnismäßig", "bestimmten",
      "Nationalmannschaft", "bereichern", "Ämtern", "Schmalz-Jacobson", "Staatsbürger", "Landschaft", "Leitung",
      "Gerichten", "klarmachen", "Wohnsitz", "Pflichten", "festen", "richten", "Rechte", "einfach", "Privileg",
      "ganz", "generellen", "Doppelstaatsbürgerschaft", "erleichterte", "flächendeckender", "Einbürgerung",
      "betrachtend", "Füße", "bayrische", "Staatsbürgerschaft", "Selin", "Iran", "Schily", "Kopf", "deutsche",
      "akzeptieren", "Ausländerkriminalität", "Länderinteressen", "entlässt", "Freundlichkeit", "Kampfansage",
      "ius", "soli", "Aufenthaltsstatus", "Bundesgebiet", "erwerben", "Geburt", "zugelassen", "Papier", "verstärkt",
      "Erde", "Bundesrepublik", "sogenannte", "Ländern", "Deutschland", "abgeschlossen", "vorhandener", "Rücksicht",
      "Gestatten", "Einzelfall", "Konflikt", "gelöst"),
    regex = FALSE, interjection = FALSE
    )


------------------------------------------
##  ".*[Ss]taats(ange|zuge).*"
------------------------------------------
  
kwic(
  "GERMAPARL",
    query= '".*[Ss]taats(ange|zuge).*"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
highlight(
    lightgreen= c(
      "deutsche", "türkische", "syrische", "ausländische", "Visumpflicht", "britische", "Türkei", "iranische",
      "Ausland", "entführt", "Visumfreiheit", "Kurzaufenthalt", "Aufenthalt", "Deutschland", "Opfer", "Ausländer",
      "Fürsorgeabkommens", "Visafreiheit", "konsularische", "türkischen", "deutschen", "Staatsangehörigen",
      "eigenen", "Staaten", "Staatsangehörigkeit", "lebenden", "ausreisepflichtigen", "türkischer", "mazedonischen",
      "Hoheitsgebiet", "serbischen", "Ehegattennachzug", "Mehrstaatigkeit", "Ehegatten", "syrischen", "Einreise",
      "Mitgliedstaats", "deutscher", "türkischer", "Kurzaufenthalte", "Einreise", "Auslieferung", "ausländischer",
      "Evakuierung", "konsularischer", "assoziationsrechtlichen", "befindlicher", "Rückführung", "Rechtsstatus",
      "Bundesdienststellen", "visumfreie", "Gefangentransporte", "Obhut", "eigener", "Haltern", "Österreicher",
      "vorläufige", "Petitionsrecht", "vorgibt", "Einbürgerung", "Geldbeutel", "Erwerb", "unterschreiben",
      "Ausländer", "doppelten", "Letzte", "Konvention", "Entscheidende", "zugehörigkeit", "angekündigten", "sogenannte",
      "Einwanderern", "wollten", "Kinder", "Geburt", "aufgetauchte", "Einbürgerungsgarantie", "Minimallösungen",
      "Schrittchen", "verünglückte", "Totgeburten", "Fehlgeburten", "Meint", "einstmals", "bezweckt", "schwanger",
      "Einführung", "merkwürdigen", "geborene", "schiebt", "Variante", "jüngst", "nötige", "vorhandener", "Rücksicht",
      "Gestatten", "Einzelfall", "Konflikt", "gelöst"),
    regex = FALSE, interjection = FALSE
    )


------------------------------------------
##  "[Ii]us"
------------------------------------------
  
  
kwic(
  "GERMAPARL",
    query= '"[Ii]us"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
highlight(
    lightgreen= c(
      "soli", "sanguinis", "Staatsangehörigkeitsrecht", "Abkömmlinge", "moderne", "Geburtsortprinzips",
      "Einwanderungskonzept", "Territorialprinzips", "bello", "geboren", "wilhelminische", "bellum", "fortwirken",
      "Geburtsrechts", "Zufälligkeit", "achtjährigen", "ius", "Territorialprinzip", "Geburtsrecht", "Untersuchung", "trat",
      "ad", "Seitdem", "Neben", "Deutscher", "Staatsbürgerschaftsrechts", "Hinnahme", "Mehrstaatigkeit", "hieraus",
      "Staatsangehörigkeitsrechts", "Doppelstaatsbürgerschaften", "Staatsbürger", "Blutsrecht", "überfällig",
      "Einbürgerungserleichterungen", "Beide", "Soli", "Jus", "tizministerin", "tizministerium", "Fortschrittlich",
      "unechten", "diametralen", "Öffnung", "Doppelstaatsangehörigkeit", "echtes", "Abkehr", "Richtung", "stoppt", "überzugehen",
      "Justizhaushalt", "Optionsmodell", "liberales", "Arbeitsanreize", "Arbeitnehmerüberlassungsgesetz", "blauen", "geringere",
      "abgelaufen", "Lohnnebenkosten"),
    regex = FALSE, interjection = FALSE
    )

--------------------------------------------
## "[Oo]ptions(modell|pflicht).*"
--------------------------------------------

kwic(
  "GERMAPARL",
    query= '"[Oo]ptions(modell|pflicht).*"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
highlight(
    lightgreen= c(
      "Optionsmodell", "Staatsangehörigkeitsrecht", "Staatsangehörigkeit", "Personengesellschaften",
      "abschaffen", "Betriebssteuer", "Bedenken", "Modell", "Personenunternehmen", "Einbürgerungszusicherung",
      "bürokratischer", "Justizministern", "evaluieren",  "entfristen", "ernstzunehmenden", "entschieden", "Jus",
      "optieren", "rechnet", "Hinnahme", "Abschaffung", "Staatsbürgerschaft", "abgeschafft", "Mehrstaatigkeit",
      "befreit", "abschaffen", "aufgewachsene", "Optionspflicht", "abzuschaffen", "aufgewachsen", "doppelte",
      "Staatsbürgerschaftsrecht", "bundespolitischer", "doppelten", "Staatsangehörigkeitsgesetz", "Mehrstaatlichkeit",
      "fordern", "Einführung", "Verfassungsmäßigkeit", "Aydan", "Standordsicherungsgesetztes", "Modell", "einheitliches",
      "Ergeb-nisse", "Per-sonen", "generelle", "Evaluation", "Oberfinanzpräsident", "kardinaler", "anwendbares",
      "läutet", "fordert", "Jelpke", "Geburtsjahrgang", "Optionspflichtigen", "Prozent", "Knapp", "deutsche",
      "Optionspflichtige", "entschieden", "Entscheidungsverhalten", "melderechtlichen", "Zukunftsplanungen",
      "optionspflichtig", "überwiegend", "Fallgruppe", "beinhaltet", "Pass", "Baccalauréat", "Köln-Ehrenfeld",
      "Veli", "geboren", "Matura", "Optionspflichtigen", "französisches", "Herkunftslandes", "jemand", "österreichische",
      "Hauptschulabschluss", "vollendet", "Uli", "einzeln", "Abitur", "falsches", "Übergangsregelung", "Rot",
      "Grün", "Bundeskanzleramt", "eingeladen", "Ab", "kurzem", "Verwaltungskos", "Eigenmittelbericht",
      "Abfindungsregelungen", "Anrechnungsverfahren", "EU-Haushalts", "Steuerflüchtlinge", "Erweiterungsfähigkeit",
      "Kommissionsbericht", "Einkommenssteuersatz", "Tisch", "Anstiegs", "körperschaftssteuersatz", "durchkommen",
      "Spreizung", "Miarbeiterbeteiligung", "Kommission", "Arbeitsgemeinschaft", "starren", "Kliniken", "zielen",
      "Optionspflichtverlängerungs", "Bundesinnenministerium", "Referentenentwurf", "kam", "Jugendlichen", "jungen",
      "Problem", "empfinden", "untersucht", "Bundesinnenminister", "Neuregelung", "sagte"),
    regex = FALSE, interjection = FALSE
    )


--------------------------------------------
##  "Geburts(recht|prinzip)"
--------------------------------------------
  
  
kwic(
  "GERMAPARL",
    query= '"Geburts(recht|prinzip)"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"), 
    interjection = FALSE, regex = TRUE) %>%
highlight(
    lightgreen= c(
      "Staatsangehörigkeitsrecht", "soli", "geborenen", "Kinder", "Inländerinnen", "Geburtsrechts", "Kindergartenalter",
      "billigte", "Ius", "Schuleintritt", "Weggefährten", "binationalen", "uneingeschränktes",
      "Inländern", "Durchgang", "Jus", "gekommenen", "rechtpolitisch", "eingebürgert", "eingeführt",
      "Abstammung", "verankert", "schlagen"),
    regex = FALSE, interjection = FALSE
    )


--------------------------------------------
## "[Dd]oppelpass.*"
--------------------------------------------

kwic(
  "GERMAPARL",
    query= '"[Dd]oppelpa(ss|ß).*"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
highlight(
    lightgreen= c(
      "Frauenquote", "Angehöriger", "geschilderten", "generellen", "globalisierten", "leben", "Burkas",
      "Dagegen-Politik", "Mesut", "Özil", "Mietpreisbremse", "Schlammschlachten", "kosmopolitisch", "irrwitzig",
      "Zwanziger", "Optionspflichtigen", "Fußballstadion", "Philippika", "eleganter", "gespielt", "Fehlerquellen",
      "verfehlten", "Anstatt", "Stattdessen", "konsequent", "gegangen", "Caffier", "Eleganz", "Burkaverbot",
      "integrationspolitische", "Videoüberwachung", "Gegenseitigkeit", "Zugewinn", "EU-Bürger", "Henkel", "Erlangung",
      "plausibel", "worin", "Staatsbürgerschaft", "Ausländerhetze", "Straßenwahlkampf", "schwarzem",
      "Volksbefragungen", "einhergegangen", "wurde", "losgetreten", "miterlebt", "hessischen", "starten",
      "gemacht", "massiven", "Koch", "kamen", "funktionierten", "punkten", "Methoden", "doppelte", "Staatsangehörigkeit",
      "Hessen", "Zeiten", "ebenso", "faul", "angeblicher", "Optionspflicht",
      "abgeschafft", "Union", "gestoppt", "rot-grüne"),
    regex = TRUE, interjection = FALSE
    )


--------------------------------------------
## "[Mm]ehrstaat.*"
--------------------------------------------
  
kwic(
  "GERMAPARL",
    query= '"[Mm]ehrstaat.*"',
    left= 10, right= 10, cqp= TRUE,
    s_attribute= c(
      "parliamentary_group", "speaker", "date", "url"),
    interjection = FALSE, regex = TRUE) %>%
highlight(
  lightgreen= c(
    "binationalen", "ablesbar", "dass", "Wieso", "Haushalten", "irgendwo", "Fakt", "akzeptiert",
    "Grindel", "Mehrstaatigkeit", "Hinnahme", "Vermeidung", "Einbürgerungen", "Staatsangehörigkeit",
    "Einbürgerung", "hingenommen", "generell", "Staatsangehörigen", "Staatsangehörigkeitsrecht",
    "Optionszwang", "Grundsatz", "EU-Bürger", "aufgewachsene", "Optionspflicht", "türkischen",
    "eingebürgert", "auseinandergehen", "Doppelstaatler", "zugerufen", "Abendlandes", "ausgeblieben",
    "Einbürgerungen", "Mehrstaatigkeit", "beträchtliche", "Untergang", "Vorhin", "Aydan", "geschätzt",
    "seriöse", "Wissens", "Vereinigung", "erworben", "Deutschland", "Kamelle", "Herkunftsstaat",
    "einbürgern", "diplomatische", "problematisch", "derjenige", "erledigt","Hinnahme",
    "Staatsangehörigkeit", "Mehrstaatlichkeit", "Einbürgerung", "hingenommen", "generelle",
    "Staatsbürgerschaft", "Vermeidung", "Optionspflicht", "Doppelstaatlichkeit", "Grundsatz",
    "Staatsangehörigkeitsrecht", "Integrationsprozesses", "binational", "EU-Staaten", "Ausländergesetzes",
    "Habsburg", "Ehen", "Entlassung"),
  regex = FALSE, interjection = FALSE
  )


  