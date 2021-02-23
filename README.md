# mobilitaets_index_zh
Verkehrsmonitoring innerhalb der Stadt Zürich  
[https://christophbaur.shinyapps.io/mobilitaets_index_zh/](https://christophbaur.shinyapps.io/mobilitaets_index_zh/)


## Inhalt
- [Moblitätsindex ZH](#Mobilitätsindex-ZH)
- [Datenquellen](#Datenquellen)
- [Methodik](#Methodik)
- [Contact](#Contact)


## Moblitätsindex ZH
### Was ist der Moblitätsindex ZH?

Covid-19 hat und wird auch weiterhin die globale Gesellschaft stark beeinflussen. Neben vielen anderen Aspekten des öffentlichen sowie privaten Lebens 
hat sich unser Mobilitätsverhalten verändert.  
Dies dürfte unbestritten sein. Aber wie sieht es denn wirklich aus in der Mobilität bzw. im Verkehr? Und vor allem, wie sieht es innerhalb der Stadt Zürich damit aus?  
Der Mobilitätsindex ZH zeigt die Veränderung  

* des motorisierten Individualverkehrs (MIV)  
* des öffentlichen Verkehres (öV)  
* sowie des Fuss- und Veloverkehrs  

innerhalb der Stadt Zürich auf. Damit kann dieser Index als Hilfe/Stütze verstanden werden zu verstehen, was tatsächlich im Verkehr passiert.

![Beispielplot - Verkehrsaufkommen und Veränderung des Bewegungsmuster gemäss Google](data/example_plot.png)

### Wo finde ich den Mobilitätsindex ZH?

Die interaktive Shiny-App ist unter folgendem Link erreichbar:  
[https://christophbaur.shinyapps.io/mobilitaets_index_zh/](https://christophbaur.shinyapps.io/mobilitaets_index_zh/)




## Datenquellen

Es wurden folgende Datenquellen vom [Open Data Portal](https://data.stadt-zuerich.ch/) der Stadt Zürich bzw. [Google]() verwendet.

Art | Quelle
------------ | -------------
Daten zum MIV | [Link](https://data.stadt-zuerich.ch/dataset/sid_dav_verkehrszaehlung_miv_od2031)
Daten zu Fuss/Velo| [Link](https://data.stadt-zuerich.ch/dataset/ted_taz_verkehrszaehlungen_werte_fussgaenger_velo)
Daten zum öV | [Link](https://data.stadt-zuerich.ch/dataset/vbz_frequenzen_hardbruecke)
Daten zum Wetter | [Link](https://data.stadt-zuerich.ch/dataset/sid_wapo_wetterstationen)
Daten zum Bewegungsmuster | [Link](https://www.google.com/covid19/mobility/index.html?hl=de)


## Methodik
### MIV / Fuss / Velo / öV 

Die Daten werden im Skript data_preprocessing.R von der [Open Data Portal](https://data.stadt-zuerich.ch/) der Stadt Zürich bezogen und wie folgt weiterverarbeitet

* Bereinigung/Ausschluss von Zählstellen welche keine und/oder unplausible Daten liefern
* Berechnung eines Mittelwertes je Wochentag/Zählstelle im Referenzzeitraum (KW 1-9 2020) um das 100% Nivau jeder Zählstelle zu definieren
* Aggregation auf Tageswerte je Zählstelle
* Indexbildung je Tag und je Zählstelle
* Je Verkehrsart über alle Indexwerte das nach Verkehrsaufkommen gewichtete Mittel bilden
* Gleitenden 7-Tage Mittelwert über die Indexwerte je Verkehrsart bilden

### Bewegungsmuster

Die Daten werden im Skript data_preprocessing.R von [Google](https://www.google.com/covid19/mobility/index.html?hl=de) bezogen und wie folgt weiterverarbeitet  

* Filterung auf "Kanton Zürich"
* Umrechnung auf Indexwerte


## Contact

[LinkedIn](https://www.linkedin.com/in/christoph-baur-89759a202/)