# TRACE (deutsche Version)
Kontaktnetzwerke zur interaktiven Analyse von Verlegungsketten und Kontakten in Krankenhäusern (TRAnsfer and Contact nEtworks)

(es handelt sich um eine Erweiterung der Version von TRACE, die für das Universitätsklinikum Münster UKM, entwickelt wurde; eine englische Version ist unter sandmanns/trace verfügbar)

<p align="center">
    <img height="600" src="https://uni-muenster.sciebo.de/s/cY4CwzoZajP0t0J/download">
</p>



## Voraussetzungen
Um TRACE zu nutzen, benötigen Sie R (Version 4.1.0 oder höher) und R Shiny.

##  Installation
Um TRACE zu installieren, laden Sie einfach das Repository herunter. Alle erforderlichen Pakete werden installiert, alle Funktionen werden automatisch mit Hilfe des global.R-Skripts geladen.

## TRACE ausführen
TRACE ist als R Shiny GUI verfügbar. Um die Software auszuführen: 1) navigieren Sie zu dem Ordner, in dem die folgenden Dateien gespeichert sind: `global.R`, `ui.R`, `server.R`, `www/UKM.png` und `www/white.png`. 2) Führen Sie Run App aus.

## Beispielhafte Nutzung von TRACE

## Input
m Analysen mit TRACE durchzuführen, muss der Input 1) eingelesen und 2) konfiguriert werden.

### Datei einlesen

* Fachabteilungen:
    * `Datei hochladen` Wählen Sie eine tabellarische Datei zum Hochladen aus, die Informationen über die Aufnahme und Entlassung von Fällen auf Fachabteilungen enthält.
    * `Trennzeichen` Wählen Sie das Feldtrennzeichen aus, das in der Eingabedatei verwendet wird (eine von: Komma, Semikolon, Tabulator).
* Stationen:
    * `Datei hochladen` Wählen Sie eine tabellarische Datei zum Hochladen aus, die Informationen über die Aufnahme und Entlassung von Fällen auf Stationen enthält.
    * `Trennzeichen` Wählen Sie das Feldtrennzeichen aus, das in der Eingabedatei verwendet wird (eine von: Komma, Semikolon, Tabulator).

Hinweis: es muss mindestens eine Datei hochgeladen werden. Eine zweite Datei, die z.B. eine feingliedrigere Verlegung von Fällen zwischen Stationen, also innerhalb einer Fachabteilungen, beschreibt, kann hochgeladen werden.

### Input konfigurieren

* `Wählen Sie die Spalte, die Informationen enthält zu...`
  * `...Fallnummer` Name der Spalte, die Informationen über die betrachteten Fälle enthält (Fall-IDs).
  * `...Fachabteilung/Station` Name der Spalte, die Informationen über die Fachabteilung, bzw. die Station enthält.
  * `...Aufnahme` Name der Spalte, die Informationen über das Aufnahmedatum des Falls auf der Fachabtielung, bzw. der Station enthält (Formatvorgabe: JJJJ-MM-TT Std:Min:Sek).
  * `...Enthlassung` Name der Spalte, die Informationen über das Aufnahmedatum des Falls auf der Fachabtielung, bzw. der Station enthält (Formatvorgabe: JJJJ-MM-TT Std:Min:Sek).
  * `...Hauptdiagnose` Name der Spalte, die Informationen zur Hauptdiagnose enthält (ICD-Kode).

      

### Beispiel

Wählen Sie `Demo Daten laden` um die simulierten Beispieldateien `Fachabteilung_V2.txt`und `Station_V2.txt` einzulesen. Beide Dateien sind im www-Ordner verfügbar und können alternativ auch per Hand eingelesen werden.

Wenn Sie die Demodaten automatisch laden, sind die Spalten zur Konfiguration des Inputs bereits korrekt gesetzt ('Fallnummer' für `Fallnummer`, 'Fachabteilung' für `Fachabteilung`, 'Station' für `Station`, 'Aufnahme' für `Aufnahme`, 'Entlassung' für `Entlassung` und 'Hauptdiagnose' für `Hauptdiagnose`. Führen Sie `Input konfigurieren` aus.


## Kontakte & Verlegungen

Es kann zwischen Analysen pro Fachabteilung und pro Station gewechselt werden. 

### Analyse der Fachabteilungen/Stationen
Eine Analyse kann für eine oder mehrere Fachabteilungen (bzw. Stationen) durchgeführt werden. Optional können nur Fälle mit ausgewählten ICD-Kodes oder ICD-Kapitelen betrachtet werden.

### Auswahl Beobachtungszeitraum
Für die Analyse wird ein Beobachtungszeitraum gewählt. 

Als frühestes Datum für den Beginn des Beobachtungszeitraums kann der erste Tag gewählt werden, für den in der Input Datei für die gewählten Abteilungen Daten vorliegen. Als spätestes Datum für den Beginn des Beobachtungszeitraums kann der letzte Tag gewählt werden, für den in der Input Datei für die gewählten Abteilungen Daten vorliegen.

Als frühestes Datum für das Ende des Beobachtungszeitraums kann der für den Beginn des Beobachtungszeitraums gewählte Tag gewählt werden. Als spätestes Datum für das Ende des Beobachtungszeitraums kann der letzte Tag gewählt werden, für den in der Input Datei für die gewählten Abteilungen Daten vorliegen. 

Beachten Sie, dass bei einem großen Beobachtungszeitraum die Berechnung je nach Rechenleistung bis zu 1 Minute dauern kann.

### Kontaktanalyse
Für die Kontaktanalyse muss 1 notwendige Parameter gesetzt werden:

* Minimale Kontakt: Die Zeit, die zwei Fälle in Kontakt miteinander sein müssen, damit für sie ein Kontakt eingezeichnet wird (default: 1 Stunde).


## Ergebnisse
Das primäre Ergebnis der Analyse mit TRACE stellt ein interaktives Kontaktnetzwerk dar (R-Paket 'networkD3').

### Analyse von 1 Abteilung

<b> Darstellung: </b>

Wird nur eine einzige Fachabteilung/Station für die Analyse gewählt, erfolgt eine Analyse von Kontakten von allen Fällen im Beobachtungszeitraum auf dieser Abteilung, entsprechend der gewählten Konfiguration. 

Jeder Kontakt zwischen 2 Fällen (Knoten) wird durch eine graue Kante dargestellt. Die Dicke der Kante korreliert mit der Länge der Kontaktzeit. Die Größe der Knoten korreliert mit der Länge der Aufenthaltsdauer auf der gewählten Fachabteilung, bzw. Station. 

Ist ein Knoten schwarz umrandet, zeigt dies an, dass der Fall mindestens eine weitere Fachabteilung, bzw. Station während des Beobachtungszeitraums besucht hat. 


<b> Pop-up Fenster: </b>

Über jeden Knoten können Detailinformationen abgefragt werden. Klickt man auf einen Knoten, öffnet sich ein Pop-up Fenster. Dort sind Informationen gegeben zu:

* Fall
* Hauptdiagnose
* Aufenthaltsdauer insgesamt
* Kontakte auf Abteilung
* Aufenthaltsdauer auf Abteilung
* Weitere besuchte Abteilungen 


<b> Allgemeine Informationen: </b>

Oberhalb der Visualisierung sind Basis-Informationen zusammengefasst:

* Konfiguration der Analyse
    * Abteilung ausgewählt
    * ICD-Filter
    * Beobachtungszeitraum
    * Minimale Kontaktzeit
* Weiterführende Informationen
    * Anzahl Fälle
    * Kontakte innerhalb der Abteilung
    * Anzahl Fälle mit Verlegung von der gewählten Abteilung
    * Top-5 Ziel-Abteilungen
    * Anzahl Fälle mit Verlegungen hin zur gewählten Abteilung
    * Top-5 Ursprungs-Abteilungen

<b> Beispiel: </b>

Wählen Sie:

* Fachabteilungen
* Analyse der Fachabteilungen: Abteilung 1
* Hauptdiagnose nach ICD-Code-Filtern: nein
* Auswahl Beobachtungszeitraum
    * Beginn des Beobachtungszeitraums: 2023-01-02
    * Ende des Beobachtungszeitraums: 2023-02-01
* Kontaktanalyse
    * Minimale Kontaktzeit: 6 Stunden


### ### Analyse von >1 Abteilung

<b> Darstellung: </b>

Werden 2 oder mehr Fachabteilungen/Stationen für die Analyse gewählt, erfolgt eine Analyse der Verlegungen von allen Fällen im Beobachtungszeitraum zwischen diesen Abteilung, sowie eine zusätzliche Kontaktanalyse innerhalb der Abteilungen, entsprechend der gewählten Konfiguration.

Jede Abteilung wird durch einen großen Abteilungs-Knoten dargestellt. Pro Station wird eine individuelle Farbe gewählt. Die Größe des Knotens korreliert mit der Anzahl der betrachteten Fälle auf der Station. Alle Fälle auf der Abteilung werden als kleinere Fall-Knoten in derselben Farbe wie die Abteilung dargestellt, und sind mit dem Abteilungs-knoten durch eine graue Kante verbunden. Die Größe der Fall-Knoten korreliert mit der Länge der Aufenthaltsdauer auf der entsprechenden gewählten Fachabteilung, bzw. Station. 

Ist ein Fall-Knoten schwarz umrandet, zeigt dies an, dass der Fall mindestens eine weitere Fachabteilung, bzw. Station während des Beobachtungszeitraums besucht hat (berücksichtigt alle Abteilungen, nicht nur die für die Analyse ausgewählten). 

Für jeden Fall wird auf jeder besuchten Abteilung ein Fall-Knoten erstellt. Dies führt dazu, dass ein Fall, der während des Beobachtungszeitraums 2 Abteilungen besucht hat, 2x als Knoten in dem Netzwerk auftritt. Diese Knoten sind durch eine schwarze Kante verbunden und zeigen so die Verlegung des Falls zwischen zwei Abteilungen an. 


<b> Pop-up Fenster pro Fachabteilung/Station: </b>

* Abteilung
* Fälle insgesamt
* Anzahl Fälle mit Verlegung von der gewählten Abteilung
* Top-5 Ziel-Abteilungen
* Anzahl Fälle mit Verlegungen hin zur gewählten Abteilung
* Top-5 Ursprungs-Abteilungen
* Mittlere Aufenthaltsdauer gesamt
* Mittlere Aufenthaltsdauer auf der gewählten Abteilung

<b> Pop-up Fenster pro Fall: </b>

* Fall
* Hauptdiagnose
* Kontakte insgesamt
* Aufenthaltsdauer insgesamt
* Kontakte auf Abteilung
* Aufenthaltsdauer auf Abteilung
* Weitere besuchte Abteilungen 


<b> Allgemeine Informationen: </b>

* Konfiguration der Analyse
    * Abteilungen ausgewählt
    * Abteilungen in Analyse Enthalten (u.U. nicht alle ausgewählten, wenn nach Konfiguration der Analyse alle Fälle herausgefiltert werden)
    * ICD-Filter
    * Beobachtungszeitraum
    * Minimale Kontaktzeit
* Weiterführende Informationen
    * Anzahl Fälle
    * Kontakte innerhalb der Abteilung
    * Verlegungen zwischen den Abteilungen


<b> Beispiel: </b>

Wählen Sie:

* Stationen
* Analyse der Stationen: Station 10A, Station 10B, Station 2A, Station 2B, Station 8A
* Hauptdiagnose nach ICD-Code-Filtern: nein
* Auswahl Beobachtungszeitraum
    * Beginn des Beobachtungszeitraums: 2023-03-12
    * Ende des Beobachtungszeitraums: 2023-04-11
* Kontaktanalyse
    * Minimale Kontaktzeit: 1 Stunde

 


## Zusammenfassung Verlegungen

Es kann zwischen Analysen pro Fachabteilung und pro Station gewechselt werden. 

### Verlegungen
Die Analyse wird für eine Fachabteilung, bzw. Station im Zentrum durchgeführt werden. Es kann zwischen zwei Perspektiven gewählt werden: Verlegungen weg von der Abteilung (`von`), oder Verlegungen hin zu der Abteilung (`nach`). Optional können nur Fälle mit ausgewählten ICD-Kodes oder ICD-Kapitelen betrachtet werden.

### Auswahl Beobachtungszeitraum
Für die Analyse wird ein Beobachtungszeitraum gewählt. 

Als frühestes Datum für den Beginn des Beobachtungszeitraums kann der erste Tag gewählt werden, für den in der Input Datei für die gewählte Abteilung Daten vorliegen. Als spätestes Datum für den Beginn des Beobachtungszeitraums kann der letzte Tag gewählt werden, für den in der Input Datei für die gewählte Abteilung Daten vorliegen.

Als frühestes Datum für das Ende des Beobachtungszeitraums kann der für den Beginn des Beobachtungszeitraums gewählte Tag gewählt werden. Als spätestes Datum für das Ende des Beobachtungszeitraums kann der letzte Tag gewählt werden, für den in der Input Datei für die gewählten Abteilung Daten vorliegen. 

Für diese zusammenfassende Analyse werden große Zeiträume empfohlen (default: 1 Jahr).


## Ergebnisse
Das primäre Ergebnis der Analyse mit TRACE stellt ein interaktives Verlegungsnetzwerk dar (R-Paket 'networkD3').

<b> Darstellung: </b>

Die gewählte Fachabteilung/Station wird im Zentrum des Netwerks dargestellt. Abteilungen zu/von denen es Verlegungen gegeben hat, sind mit den zentralen Abteilung durch eine graue, gerichtete Kante verbunden. Ein Pfeil zeigt die Richtung der Verlegung an. Die größe der Knoten korreliert mit der Anzahl der Fälle gesamt auf der Abteilung im Beobachtungszeitraum. Dicke der Kanten korreliert negativ mit der effektiven Distanz der zwei Abteilungen (geringe Distanz, dicke Kante). Zusätzlich korreliert die Entfernung zweier Knoten positiv mit ihrer effektiven Distanz.

Die effektive Distanz wird berechnet nach Brockmann und Helbing 2013 als 1-log(X/Y), wobei X die Anzahl an Fällen ist, die von A nach B verlegt werden, und Y die Anzahl an Fällen, die insgesamt von A verlegt wurden. Analog für `nach`.


<b> Pop-up Fenster: </b>

Über jeden Knoten können Detailinformationen abgefragt werden. Klickt man auf einen Knoten, öffnet sich ein Pop-up Fenster. Dort sind Informationen gegeben zu:

* Abteilung
* Fälle insgesamt
* Fälle mit Verlegung (wird ein Fall von A-B-A-C verlegt, gibt es zwei Verlegungen von A, sodass der Fall 2x gezählt wird; daher kann Fälle mit Verlegungen u.U. größer sein als Fälle insgesamt)
* Anteil Fälle von Abteilung \<zentral\> nach Abteilung
* Effektive Distanz basierend auf verlegten Fällen auf der zentralen Abteilung
* Effektive Distanz basierend auf allen Fällen auf der zentralen Abteilung


<b> Allgemeine Informationen: </b>

Oberhalb der Visualisierung sind Basis-Informationen zusammengefasst:

* Konfiguration der Analyse
    * Von
    * Nach
    * ICD-Filter
    * Beobachtungszeitraum

<b> xlsx-Export: </b>

TRACE bietet die Möglichkeit, die Ergebnisse als xlsx-File zu exportieren. Die folgenden Informationen sind enthalten:

* Von
* ICD-Filter
* Zeitraum
* Fälle insgesamt
* Fälle mit Verlegung
* Details
    * Verlegt von Abteilung nach
    * Anzahl Patienten absolut
    * Anzahl Patienten relativ
    * Effektive Distanz basierend auf verlegten Fällen
    * Effektive Distanz basierend auf allen Fällen 


<b> Beispiel: </b>

Wählen Sie:

* Stationen
* Verlegungen: von
* Station: Station 12A
* Hauptdiagnose nach ICD-Code-Filtern: nein
* Auswahl Beobachtungszeitraum
    * Beginn des Beobachtungszeitraums: 2023-01-01
    * Ende des Beobachtungszeitraums: 2024-01-01




