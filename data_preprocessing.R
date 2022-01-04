rm(list=ls())
gc()

##### Beschreibung: Daten zu MIV/Langsamverkehr/Velo/öV und Mobilitätsverhalten gemäss Google für den 
# Kanton Zürich als Mobiliätsindex aufbereiten und zur Verfügung stellen
# OpenData
# Kanton Zürich


##### Autor: C.Baur

##### Erstellungsdatum: 31.01.2021


#________________________________________________________________
#####                   Setup                     #####
#________________________________________________________________

# Libraries
library(tidyverse)
library(lubridate)
library(DBI)
library(dplyr)
library(plotly)
library(readr)
library(zoo)
library(reshape2)
library(data.table)


# Daten von der OpenData-Plattform der Stadt Zürich laden
#MIV 2020
URL_MIV_2020 <- "https://data.stadt-zuerich.ch/dataset/6212fd20-e816-4828-a67f-90f057f25ddb/resource/44607195-a2ad-4f9b-b6f1-d26c003d85a2/download/sid_dav_verkehrszaehlung_miv_od2031_2020.csv"
#MIV 2021
URL_MIV_2021 <- "https://data.stadt-zuerich.ch/dataset/6212fd20-e816-4828-a67f-90f057f25ddb/resource/b2b5730d-b816-4c20-a3a3-ab2567f81574/download/sid_dav_verkehrszaehlung_miv_od2031_2021.csv"
#MIV 2022
URL_MIV_2022 <- "https://data.stadt-zuerich.ch/dataset/6212fd20-e816-4828-a67f-90f057f25ddb/resource/bc2d7c35-de13-45e9-be21-538d9eab3653/download/sid_dav_verkehrszaehlung_miv_od2031_2022.csv"
#Velo 2020
URL_Velo_2020 <- "https://data.stadt-zuerich.ch/dataset/83ca481f-275c-417b-9598-3902c481e400/resource/b9308f85-9066-4f5b-8eab-344c790a6982/download/2020_verkehrszaehlungen_werte_fussgaenger_velo.csv"
#Velo 2021
URL_Velo_2021 <- "https://data.stadt-zuerich.ch/dataset/83ca481f-275c-417b-9598-3902c481e400/resource/ebe5e78c-a99f-4607-bedc-051f33d75318/download/2021_verkehrszaehlungen_werte_fussgaenger_velo.csv"
#Velo 2022
URL_Velo_2022 <- "https://data.stadt-zuerich.ch/dataset/83ca481f-275c-417b-9598-3902c481e400/resource/9a9f9221-d6a0-4289-9962-410557adef93/download/2022_verkehrszaehlungen_werte_fussgaenger_velo.csv"
#öV 2020
URL_oev_2020 <- "https://data.stadt-zuerich.ch/dataset/vbz_frequenzen_hardbruecke/download/frequenzen_hardbruecke_2020.csv"
#öV 2021
URL_oev_2021 <- "https://data.stadt-zuerich.ch/dataset/vbz_frequenzen_hardbruecke/download/frequenzen_hardbruecke_2021.csv"
#öV 2022
URL_oev_2022 <- "https://data.stadt-zuerich.ch/dataset/vbz_frequenzen_hardbruecke/download/frequenzen_hardbruecke_2022.csv"


#Google
URL_google <- "https://storage.googleapis.com/covid19-open-data/v2/CH_ZH/main.csv"

#Wetter
#Wetter 2020
URL_wetter_2020 <- "https://data.stadt-zuerich.ch/dataset/ugz_meteodaten_tagesmittelwerte/download/ugz_ogd_meteo_d1_2020.csv"
#Wetter 2021
URL_wetter_2021 <- "https://data.stadt-zuerich.ch/dataset/ugz_meteodaten_tagesmittelwerte/download/ugz_ogd_meteo_d1_2021.csv"
#Wetter 2022
URL_wetter_2022 <- "https://data.stadt-zuerich.ch/dataset/ugz_meteodaten_tagesmittelwerte/download/ugz_ogd_meteo_d1_2022.csv"

#Funktionen
# downloadData <- function(url, destination){
#   download.file(url, destination, method = "auto", quiet = TRUE)
# }


#________________________________________________________________
#####                 Haupteil                              #####
#________________________________________________________________

#### > MIV ####
#### >> Daten beziehen/verarbeiten ####
data_miv_raw20 <- read.csv(URL_MIV_2020,
                           stringsAsFactors = FALSE,
                           encoding = "UTF-8")

data_miv_raw21 <- read.csv(URL_MIV_2021,
                           stringsAsFactors = FALSE,
                           encoding = "UTF-8")

data_miv_raw22 <- read.csv(URL_MIV_2022,
                           stringsAsFactors = FALSE,
                           encoding = "UTF-8")

#in einem Datensatz bündeln
data_miv_raw<-rbind(data_miv_raw20,
                    data_miv_raw21,
                    data_miv_raw22)
rm(data_miv_raw20)
rm(data_miv_raw21)
rm(data_miv_raw22)

# Datenformat ändern
data_miv_raw$datetime <- as.Date(as.POSIXct(data_miv_raw$MessungDatZeit, 
                                    format = "%Y-%m-%dT%H:%M:%S"))

# Datensätze während Zeitumstellung löschen "2020-03-29T02:00:00"
data_miv_reduced1 <- data_miv_raw[!is.na(data_miv_raw$datetime),]


#### >> Aufbau Referenzwerte ####
#Möglichst aktueller "Pre-corona"-Zeitraum in der Schweiz als Referenz
#2020 KW 1-9, je Wochentag, je Zählstelle

#Welche Zählstellen hatten "Fehlend" als Status im Zeitraum?
#ca. 20 Zählstellen haben in diesem Zeitraum mal keine Daten geliefert
#rigoros aussortieren als Referenzwert. Bei insg. über 90 Zählstellen ok.
data_miv_fehlend <- data_miv_reduced1%>%
  #Kalenderwoche, Jahr, Wochentage (ohne Feiertagsberücksichtigung)
  mutate(KW=as.integer(strftime(datetime, format="%V")),
         Jahr=as.integer(strftime(datetime, format="%Y")),
         Wochentag=strftime(datetime, format="%A"))%>%
  #Auf Refernezwochen filtern
  filter(KW<=9 & Jahr==2020)%>%
  #filter der fehlenden
  filter(AnzFahrzeugeStatus=="Fehlend")%>%
  select(ZSID)
#liste kreieren
data_miv_fehlend <- as.list(unique(data_miv_fehlend$ZSID))

#mit bereinigten Daten Referenzwert rechnen
data_miv_ref<- data_miv_reduced1%>%
  #Kalenderwoche, Jahr
  mutate(KW=as.integer(strftime(datetime, format="%V")),
         Jahr=as.integer(strftime(datetime, format="%Y")),
         date=as.Date(format(datetime, "%Y-%m-%d")),
         Wochentag=strftime(datetime, format="%A"))%>%
  #Auf Refernezwochen filtern
  filter(KW<=9 & Jahr==2020)%>%
  #fehlerhafte Zählstellen aussortieren
  filter(!ZSID %in% data_miv_fehlend)%>%
  #gruppieren
  group_by(ZSID, date, Wochentag)%>%
  #Summe je Tag
  summarise(AnzFahrzeuge=sum(AnzFahrzeuge))%>%
  ungroup()%>%
  #gruppieren
  group_by(ZSID, Wochentag)%>%
  #Mittelwert in KW1-9
  summarise(avg_ref_AnzFahrzeuge=mean(AnzFahrzeuge))%>%
  ungroup()



#### >> Wert je Tag rechnen ####
data_miv_tag <- data_miv_reduced1%>%
  #fehlerhafte Zählstellen aussortieren
  filter(!ZSID %in% data_miv_fehlend)%>%
  #Kalenderwoche, Jahr
  mutate(KW=as.integer(strftime(datetime, format="%V")),
         Jahr=as.integer(strftime(datetime, format="%Y")),
         date=as.Date(format(datetime, "%Y-%m-%d")),
         Wochentag=strftime(datetime, format="%A"))%>%
  #identifizierung ob "Fehlend" an einem Tag aufgetreten
  mutate(fehlerhaft=ifelse(AnzFahrzeugeStatus=="Fehlend", 1, 0))%>%
  #gruppieren
  group_by(ZSID, date)%>%
  #max bilden -> wenn ein Datensatz fehlerhaft, dann gesamten Tag markieren
  mutate(fehlerhaft=max(fehlerhaft))%>%
  ungroup()%>%
  #und aussortieren
  filter(fehlerhaft==0)%>%
  #gruppieren
  group_by(ZSID, date, Wochentag)%>%
  #Wert berechnen
  summarise(AnzFahrzeuge=sum(AnzFahrzeuge))%>%
  ungroup()


#### >> Indexwert berechnen ####
#u.a. Mittelwert über alle Indexierten Werte um auszugleichen,
# dass ggf. einzelne Zählstellen an einzelnen Tagen ausfallen
data_miv_index <- data_miv_tag%>%
  #Referenzwerte hinzufügen
  left_join(data_miv_ref,
            by=c("ZSID"="ZSID",
                 "Wochentag"="Wochentag"))%>%
  #berechnen Indexwert
  mutate(Indexwert=AnzFahrzeuge/avg_ref_AnzFahrzeuge)%>%
  #gruppieren
  group_by(date, Wochentag)%>%
  #mittelwert/gew. Mittel nach Anzahl Fahrzeuge im Referenzzeitraum
  summarise(avg_index_miv=mean(Indexwert),
            avg_index_miv_w=weighted.mean(Indexwert, avg_ref_AnzFahrzeuge))%>%
  ungroup()%>%
  #sortieren nach Datum
  arrange(date)


#### >> Standorte in Liste ####
geo_miv <- data_miv_reduced1%>%
  #fehlerhafte Zählstellen aussortieren
  filter(!ZSID %in% data_miv_fehlend)%>%
  select(ZSName, EKoord, NKoord)%>%
  mutate(vsys="miv")%>%
  unique()




#### > Velo/Fuss ####

#### >> Daten beziehen/verarbeiten ####
#Nur bestimmte spalten
# LV: Specify data types explicitly. Do not import attributes <OST> and <NORD>.
COL_SPEC_COUNTING_DATA <- cols_only(FK_ZAEHLER = col_character(),
                                    FK_STANDORT = col_character(),
                                    DATUM = col_character(),
                                    VELO_IN = col_integer(),
                                    VELO_OUT = col_integer(),
                                    FUSS_IN = col_integer(),
                                    FUSS_OUT = col_integer(),
                                    OST = col_integer(),
                                    NORD = col_integer()
                                    )


data_velo_raw20 <- read_csv(URL_Velo_2020,
                           col_types = COL_SPEC_COUNTING_DATA)

data_velo_raw21 <- read_csv(URL_Velo_2021,
                           col_types = COL_SPEC_COUNTING_DATA)

data_velo_raw22 <- read_csv(URL_Velo_2022,
                            col_types = COL_SPEC_COUNTING_DATA)

#in einem Datensatz bündeln
data_velo_raw<-rbind(data_velo_raw20,
                     data_velo_raw21,
                     data_velo_raw22)
rm(data_velo_raw20)
rm(data_velo_raw21)
rm(data_velo_raw22)


#### >> Aufbau Referenzwerte ####

data_velo_ref<- data_velo_raw%>%
  #Ausschluss zweier Zählstelle, da diese Probleme bereitet seit 14.02.2021
  filter(!FK_ZAEHLER=='U15G3063864' &
           !FK_ZAEHLER=='U15G3063869')%>%
  #Kalenderwoche, Jahr
  mutate(date=as.Date(as.POSIXct(DATUM)),
         KW=as.integer(format(date,"%V")),
         Jahr=as.integer(format(date,"%Y")),
         Wochentag=strftime(date, format="%A"))%>%
  #Auf Refernezwochen filtern
  filter(KW<=9 & Jahr==2020)%>%
  #In/Out als Frequenz Zeilenweise zusammenfassen, na entfernen
  rowwise()%>%
  mutate(VELO=sum(VELO_IN, VELO_OUT, na.rm = TRUE),
         FUSS=sum(FUSS_IN, FUSS_OUT, na.rm = TRUE))%>%
  ungroup()%>%
  select(-VELO_IN, -VELO_OUT, -FUSS_IN, -FUSS_OUT)%>%
  #gruppieren
  group_by(FK_ZAEHLER, date, Wochentag)%>%
  #Summe je Tag
  summarise(AnzVelo=sum(VELO),
            AnzFuss=sum(FUSS))%>%
  ungroup()%>%
  #gruppieren
  group_by(FK_ZAEHLER, Wochentag)%>%
  #Mittelwert in KW1-9
  summarise(avg_ref_AnzVelo=mean(AnzVelo),
            avg_ref_AnzFuss=mean(AnzFuss))%>%
  ungroup()


#### >> Werte je Tag ####

data_velo_tag<- data_velo_raw%>%
  #Ausschluss zweier Zählstelle, da diese Probleme bereitet seit 14.02.2021
  filter(!FK_ZAEHLER=='U15G3063864' &
           !FK_ZAEHLER=='U15G3063869')%>%
  #Kalenderwoche, Jahr
  mutate(date=as.Date(as.POSIXct(DATUM)),
         KW=as.integer(format(date,"%V")),
         Jahr=as.integer(format(date,"%Y")),
         Wochentag=strftime(date, format="%A"))%>%
  #In/Out als Frequenz Zeilenweise zusammenfassen, na entfernen
  rowwise()%>%
  mutate(VELO=sum(VELO_IN, VELO_OUT, na.rm = TRUE),
         FUSS=sum(FUSS_IN, FUSS_OUT, na.rm = TRUE))%>%
  ungroup()%>%
  select(-VELO_IN, -VELO_OUT, -FUSS_IN, -FUSS_OUT)%>%
  #gruppieren
  group_by(FK_ZAEHLER, date, Wochentag)%>%
  #Summe je Tag
  summarise(AnzVelo=sum(VELO),
            AnzFuss=sum(FUSS))%>%
  ungroup()


#### >> Indexwert berechnen ####
#u.a. Mittelwert über alle Indexierten Werte um auszugleichen,
# dass ggf. einzelne Zählstellen an einzelnen Tagen ausfallen
data_velo_index <- data_velo_tag%>%
  #Referenzwerte hinzufügen
  left_join(data_velo_ref,
            by=c("FK_ZAEHLER"="FK_ZAEHLER",
                 "Wochentag"="Wochentag"))%>%
  #berechnen Indexwert
  mutate(Indexwert_Velo=AnzVelo/avg_ref_AnzVelo,
         Indexwert_Fuss=AnzFuss/avg_ref_AnzFuss)%>%
  #0-Werte und Anteile unter 5%-Filtern (unplausibilitäten) als NA setzen
  mutate(Indexwert_Velo=ifelse(Indexwert_Velo<0.05 | is.nan(Indexwert_Velo),
                               NA,
                               Indexwert_Velo),
         Indexwert_Fuss=ifelse(Indexwert_Fuss<0.05 | is.nan(Indexwert_Fuss),
                               NA,
                               Indexwert_Fuss))%>%
  #gruppieren
  group_by(date, Wochentag)%>%
  #mittelwert/gew. Mittel
  summarise(avg_index_velo=mean(Indexwert_Velo, na.rm = TRUE),
            avg_index_velo_w=weighted.mean(Indexwert_Velo, 
                                           avg_ref_AnzVelo,
                                           na.rm = TRUE),
            avg_index_fuss=mean(Indexwert_Fuss, na.rm = TRUE),
            avg_index_fuss_w=weighted.mean(Indexwert_Fuss,
                                           avg_ref_AnzFuss,
                                           na.rm = TRUE))%>%
  ungroup()%>%
  #sortieren nach Datum
  arrange(date)


#### >> Standorte in Liste ####

geo_velo <- data_velo_raw%>%
  #Ausschluss zweier Zählstelle, da diese Probleme bereitet seit 14.02.2021
  filter(!FK_ZAEHLER=='U15G3063864' &
           !FK_ZAEHLER=='U15G3063869')%>%
  select(FK_STANDORT, OST, NORD)%>%
  mutate(ZSName = FK_STANDORT,
         EKoord = OST,
         NKoord = NORD,
         vsys = "fuss/velo")%>%
  select(-FK_STANDORT,
         -OST,
         -NORD)%>%
  unique()

geo_velo <- as.data.frame(geo_velo)



#### > öV VBZ ####

#### >> Daten beziehen/verarbeiten ####

data_oev_raw20 <- read.csv(URL_oev_2020,
                           stringsAsFactors = FALSE)

data_oev_raw21 <- read.csv(URL_oev_2021,
                           stringsAsFactors = FALSE)

data_oev_raw22 <- read.csv(URL_oev_2022,
                           stringsAsFactors = FALSE)

#in einem Datensatz bündeln
data_oev_raw<-rbind(data_oev_raw20,
                    data_oev_raw21,
                    data_oev_raw22)
rm(data_oev_raw20)
rm(data_oev_raw21)
rm(data_oev_raw22)

#Nur Zählstelle "VBZ"
data_oev_reduced <- data_oev_raw%>%
  filter(Name=='Ost-VBZ Total' | Name=='West-VBZ total')

#### >> Aufbau Referenzwerte ####

data_oev_ref<- data_oev_reduced%>%
#Kalenderwoche, Jahr
mutate(date=as.Date(as.POSIXct(Timestamp)),
       KW=as.integer(format(date,"%V")),
       Jahr=as.integer(format(date,"%Y")),
       Wochentag=strftime(date, format="%A"))%>%
  #Auf Refernezwochen filtern
  filter(KW<=9 & Jahr==2020)%>%
  #In/Out als Frequenz Zeilenweise zusammenfassen, na entfernen
  rowwise()%>%
  mutate(oev=sum(In, Out, na.rm = TRUE))%>%
  ungroup()%>%
  select(-In, -Out)%>%
  #gruppieren
  group_by(Name, date, Wochentag)%>%
  #Summe je Tag
  summarise(AnzOev=sum(oev))%>%
  ungroup()%>%
  #gruppieren
  group_by(Name, Wochentag)%>%
  #Mittelwert in KW1-9
  summarise(avg_ref_AnzOev=mean(AnzOev))%>%
  ungroup()


#### >> Werte je Tag ####

data_oev_tag<- data_oev_reduced%>%
  #Kalenderwoche, Jahr
  mutate(date=as.Date(as.POSIXct(Timestamp)),
         KW=as.integer(format(date,"%V")),
         Jahr=as.integer(format(date,"%Y")),
         Wochentag=strftime(date, format="%A"))%>%
  #In/Out als Frequenz Zeilenweise zusammenfassen, na entfernen
  rowwise()%>%
  mutate(oev=sum(In, Out, na.rm = TRUE))%>%
  ungroup()%>%
  select(-In, -Out)%>%
  #gruppieren
  group_by(Name, date, Wochentag)%>%
  #Summe je Tag
  summarise(AnzOev=sum(oev))%>%
  ungroup()


#### >> Indexwert berechnen ####
#u.a. Mittelwert über alle Indexierten Werte um auszugleichen,
# dass ggf. einzelne Zählstellen an einzelnen Tagen ausfallen
data_oev_index <- data_oev_tag%>%
  #Referenzwerte hinzufügen
  left_join(data_oev_ref,
            by=c("Name"="Name",
                 "Wochentag"="Wochentag"))%>%
  #berechnen Indexwert
  mutate(Indexwert_Oev=AnzOev/avg_ref_AnzOev)%>%
  #gruppieren
  group_by(date, Wochentag)%>%
  #mittelwert/gew. Mittel
  summarise(avg_index_oev=mean(Indexwert_Oev, na.rm = TRUE),
            avg_index_oev_w=weighted.mean(Indexwert_Oev, 
                                           avg_ref_AnzOev,
                                           na.rm = TRUE))%>%
  ungroup()%>%
  #sortieren nach Datum
  arrange(date)

#### >> Standort in Liste ####
#Es gibt nur einen Standort und dieser ist nicht in den Daten enthalten, deshalb manuel
#eingetragen

geo_oev <- tibble(vsys="oev",
                  ZSName="Hardbrücke",
                  EKoord = 2681416,
                  NKoord = 1248789)

#### > Wetter ####
data_wetter_2020_raw <- read.csv(URL_wetter_2020,
                        encoding = "UTF-8")

data_wetter_2021_raw <- read.csv(URL_wetter_2021,
                        encoding = "UTF-8")

data_wetter_2022_raw <- read.csv(URL_wetter_2022,
                        encoding = "UTF-8")

data_wetter_raw<-rbind(data_wetter_2020_raw,
                       data_wetter_2021_raw,
                       data_wetter_2022_raw)
rm(data_wetter_2020_raw,
   data_wetter_2021_raw,
   data_wetter_2022_raw)

#### Format Datum

data_wetter_raw$date <- as.Date(data_wetter_raw$X.U.FEFF.Datum, 
                            format = "%Y-%m-%d")


#### Filtern, aus 3 Stationen einen MW bilden und Tabelle umordnen

# 2020
data_wetter<-data_wetter_raw%>%
  filter(Parameter %in% c("T","RainDur"))%>%
  select(date,Standort,Parameter, Wert)%>%
  group_by(date,Parameter)%>%
  summarise(MW_Stationen=round(mean(Wert,na.rm = T),1))%>%
  select(date,Parameter,MW_Stationen)

data_wetter<-dcast(data_wetter, date~Parameter )%>%
  select(date,T,RainDur)%>%
  dplyr::rename(Lufttemperatur=T,Niederschlagsdauer=RainDur)


#### > Google Mobilität ####

google_mobility <- read.csv(URL_google,
                            stringsAsFactors = FALSE,
                            encoding = 'UTF-8')

#Divese umformatierungen/selektionen
google_mobility_reduced <- google_mobility%>%
  #Datum umwandeln
  mutate(date=as.Date(as.POSIXct(date)))%>%
  #Jede Menge Spalten welche hierfür uninteressant sind
  select(date,
         mobility_retail_and_recreation,
         mobility_grocery_and_pharmacy,
         mobility_parks,
         mobility_transit_stations,
         mobility_workplaces,
         mobility_residential)%>%
  #prozentuale Veränderung als Indexwerte
  mutate(mobility_retail_and_recreation=(100+mobility_retail_and_recreation)/100,
         mobility_grocery_and_pharmacy=(100+mobility_grocery_and_pharmacy)/100,
         mobility_parks=(100+mobility_parks)/100,
         mobility_transit_stations=(100+mobility_transit_stations)/100,
         mobility_workplaces=(100+mobility_workplaces)/100,
         mobility_residential=(100+mobility_residential)/100
         )
  



#### > Daten zusammenfassen als Output-file ####
#### >> Eindeutige time-series erzeugen ####T
Date <- as.Date(seq(as.POSIXct('2020-01-01 12:00:00'),
                    as.POSIXct(Sys.Date()), by="day"))
Date <- as.data.frame(Date)


#### >> matchen ####
mobility_index <- Date%>%
  #Wochentage
  mutate(Wochentag=strftime(Date, format="%A"))%>%
  left_join(data_miv_index,
            by=c("Date"="date",
                 "Wochentag"="Wochentag"))%>%
  left_join(data_velo_index,
            by=c("Date"="date",
                 "Wochentag"="Wochentag"))%>%
  left_join(data_oev_index,
            by=c("Date"="date",
                 "Wochentag"="Wochentag"))%>%
  left_join(google_mobility_reduced,
            by=c("Date"="date"))%>%
  left_join(data_wetter,
            by=c("Date"="date"))%>%
  #7-Tage-Wert https://stackoverflow.com/questions/26198551/rolling-mean-moving-average-by-group-id-with-dplyr
  arrange(Date)%>%
  mutate(avg_7day_index_miv=rollapply(avg_index_miv_w,7,mean,align='right',fill=NA),
         avg_7day_index_velo=rollapply(avg_index_velo_w,7,mean,align='right',fill=NA),
         avg_7day_index_fuss=rollapply(avg_index_fuss_w,7,mean,align='right',fill=NA),
         avg_7day_index_oev=rollapply(avg_index_oev,7,mean,align='right',fill=NA),
         avg_7day_index_retail=rollapply(mobility_retail_and_recreation,7,mean,align='right',fill=NA),
         avg_7day_index_grocery=rollapply(mobility_grocery_and_pharmacy,7,mean,align='right',fill=NA),
         avg_7day_index_parks=rollapply(mobility_parks,7,mean,align='right',fill=NA),
         avg_7day_index_transit=rollapply(mobility_transit_stations,7,mean,align='right',fill=NA),
         avg_7day_index_work=rollapply(mobility_workplaces,7,mean,align='right',fill=NA),
         avg_7day_index_residential=rollapply(mobility_residential,7,mean,align='right',fill=NA),
         avg_7day_temp=rollapply(Lufttemperatur,7,mean,align='right',fill=NA),
         avg_7day_rain=rollapply(Niederschlagsdauer,7,mean,align='right',fill=NA))

#### >> Geo-Info ####
geo_info <- rbind(geo_miv, 
                  geo_velo,
                  geo_oev)

#### >> schreiben ####
fwrite(mobility_index,
          "data/mobility_index.csv",
          row.names = FALSE)

fwrite(geo_info,
          "data/geo_info.csv",
          row.names = FALSE,
       quote = TRUE)
