#texte

text_einleitung <- HTML(
  "<big><p align='left'>

Covid-19 hat und wird auch weiterhin die globale Gesellschaft stark beeinflussen. Neben vielen anderen Aspekten des öffentlichen sowie privaten Lebens 
hat sich unser Mobilitätsverhalten verändert.
Dies dürfte unbestritten sein. Aber wie sieht es denn wirklich aus in der Mobilität bzw. im Verkehr? Und vor allem, wie sieht es innerhalb der Stadt Zürich damit aus?
</p>
<p align='left'>
Der Mobilitätsindex ZH zeigt die Veränderung 
<li>des motorisierten Individualverkehrs (MIV) </li>
<li>des öffentlichen Verkehres (öV) </li>
<li>sowie des Fuss- und Veloverkehrs </li>

innerhalb der Stadt Zürich auf. Damit kann dieser Index als Hilfe/Stütze verstanden werden zu verstehen, was tatsächlich im Verkehr passiert.
  
  </p></big>"
)



text_verk_allg <- HTML(
  "<big><p align='left'> 
  
  Anhand der Indexwerte kann die Entwicklung des Verkehrsaukommens je Verkehrsträger
nachvollzogen werden. Als Basis (100%) dienen die Mittelwerte aus den Kalenderwochen 1 bis 9 aus dem Jahr 2020.
Bis zu diesem Zeitpunkt gab es noch kaum Auswirkungen in der Schweiz aufgrund der COVID-19 Pandemie.

</p>
<p align='left'>

Zur besseren Erkennung von Trends wurde ein gleitender 7-Tage-Mittelwert verwendet. Damit
können starke tägliche Schwankungen, insb. beim Fuss- und Veloverkehr geglättet werden. (Im 
Prinzip ähnlich zu den ein oder anderen bekannten Darstellungen bzgl. Neuinfizierte etc.).

</p>
<p align='left'>

Die Daten zum Verkehr und Wetter stammen vom <a href='https://data.stadt-zuerich.ch/'>Open Data Portal der Stadt Zürich</a> und basieren auf ausgewählten
Zähl- und Messstellen innerhalb der Stadt Zürich (siehe auch 'Wo wurde gemessen?').
Die Daten zu den Aufenthaltsorten stammen aus dem COVID-19 Monitoring von Google.
  </p></big>"
)

text_verk_wetter <- HTML(
  "<big><p align='left'>

Zur besseren Einordnung der starken Schwankungen vom Fuss- und Veloverkehr werden
die Wetterdaten mitangegeben. 

</p>
<p align='left'>

Die Grafik ist interaktiv, d.h. es kann gezoomt werden, einzelne Legendeneinträge ab-
bzw. angewählt werden. Ein Mausover liefert die jeweiligen Werte am entsprechenden Datum.


</p></big>
  "
)

text_google_ind <- HTML(
  "<big><p align='left'> 
  
  <a href='https://www.google.com/covid19/mobility/index.html?hl=de'>Google</a> liefert anhand der Aufenthaltsorte von Android-Smartphones ebenso einen Indexwert,
welcher genutzt werden kann, um die Fragen zu beantworten 'Warum entsteht weniger Verkehr?' und
'Welcher Wegzweck ist stärker/weniger stark betroffen?' bzw. auch einfach 'Bleiben die Leute wirklich mehr zuhause?'.

</p>
<p align='left'>

Für den obigen Plot wurden Daten von Nutzern innerhalb des Kanton Zürichs verwendet, die den Standortverlauf auf ihren 
Android-Smartphones aktiviert haben. Diese stellen somit möglicherweise keine 
  repräsentative Stichprobe dar.

</p>
<p align='left'>

Über die Auswahlbox kann die Anzeige des Indikators verändert werden. 
</p>

<p align='left'> Jeder Indikator bezieht sich auf verschiedene Aufenthaltsorte:
<ul align='left'>
<li> <b> Bahnhöfe und Haltestellen</b>: Knotenpunkte des öffentlichen Verkehrs, Taxistand, Autobahnraststäte, Autovermietung </li>
<li> <b>Einkauf und Freizeit</b>: Restaurants, Cafés, Einkaufszentren, Freizeitparks, Museen, Bibliotheken und Kinos </li>
<li> <b>Läden des täglichen Bedarfs</b>: Supermärkte, Lebensmittelgroßmärkte, Bauernmärkte, Feinkostgeschäfte, Drogerien und Apotheken. </li>
<li> <b>Wohnorte</b>: Orte, an denen Menschen wohnen. </li>
<li> <b>Arbeitsstätten</b>: Orte, an denen Menschen arbeiten. </li>
</ul></p>

  </p></big>"
)

text_karte <- HTML(
  "<big><p align='left'> 
  
Es gibt je Verkehrsträger eine unterschiedliche Anzahl an Messstellen. 
Ebenso unterscheiden sich die Örtlichkeiten. Die untenstehende Karte zeigt welche Messstellen
für den Mobilitätsindex verwendet wurden.

</p>
<p align='left'>

Teilweise mussten Zählstellen aufgrund von
fehlenden oder unplausiblen Daten ausgeschlossen werden.

  </p></big>"
)

text_fusszeile <- HTML(
  "<p align='left'>

Der Mobilitätsindex ZH ist als Hilfestellung gedacht das verkehrliche Geschehen einzuordnen und bildet Trends ab.
Es handelt sich bei den Werten um keine offizielle Zahl, zudem können aufgrund von Defekten/Fehlern von Zählstellen Änderungen entstehen.
Dieses Projekt ist rein privat und wird gemäss 'best effort' aktualisiert.

</p>
<p align='left'>

<p align='left'> Vielen Dank an <a href='https://data.stadt-zuerich.ch/'>Open Data Stadt Zürich</a> und <a href='https://www.zh.ch/de/news-uebersicht/mitteilungen/2020/politik-staat/statistik/durch-die-krise-begleiten---gesellschaftsmonitoring-covid19-.html#main_linklist'>Gesellschaftsmonitoring Kanton Zürich</a> 
.

</p>"
)