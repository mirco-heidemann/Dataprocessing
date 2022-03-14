## ----
## Aufbereitung der Liste Hagelschutz fuer rw
## 
## Mirco Heidemann, Feb 2018
## ----

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pth_data <- ("./data/")
pth_port <- ("../GVZ_Portfolio/")

## funktionen laden
library(dplyr)
library(stringr)
library(tidyverse)

## csv file einlesen: Geb. Eigentuemer, Verwalter
dat_orig <- read.csv2(paste0(pth_port, 'Gebaeude_Eigentuemer_201801.csv'),
                     stringsAsFactors = FALSE)

## erste sichtung der daten
# str(dat_orig)
# summary(dat_orig)

## koordinaten
koord <- read.csv2(paste0(pth_port, 'gebaeudebestand_201801_georef.csv'),
                   stringsAsFactors = FALSE) %>%
  select(c(objektId, geoxLV95, geoyLV95))

## daten aufbereitung, bereinigung, feldtypbestimmung, benennung
# dat <- datOrig %>% 
dat <- unite(dat_orig, Adresse, Strasse, HausNr, PLZ, Ort,
             sep = " ", remove = TRUE) %>% 
  left_join(koord, c("ObvId_Geb" = "objektId")) %>% 
  mutate(objekt_Id = as.integer(ObvId_Geb),
         geb_nr = as.character(ObvBenId_Geb),
         gvz_nr = as.integer(Geb_Nummer),
         gemeinde_nr = as.integer(Gemeinde),
         strasse = as.character(G_Strasse),
         haus_nr = as.character(G_Nr),
         plz = as.integer(G_PLZ),
         ort = as.character(G_Ort),
         bezirk = as.character(TgmBezirk),
         zweckcode = as.integer(str_extract(Zweckcode, "\\d+")),
         zwcode_beschreib = as.character(str_extract(Zweckcode, "[A-Z][a-z]+")),
         zwcode_vkf = as.integer(naNutzungsbeschreibung),
         lage = as.character(Lage),
         baujahr = as.integer(Jahr),
         vers_Summe = as.numeric(Schaetzwert),
         volumen = as.integer(Kubus),
         pozPerId = as.integer(PozPerId),
         jurist_person  = as.character(Nat.Jur),
         geox_95 = as.character(geoxLV95),
         geoy_95 = as.character(geoyLV95),
         funktion = as.character(PotBezDt),
         name = as.character(Eigentuemer.Vertreter),
         zusatz = as.character(Name.Zusatz),
         adresse = as.character(Adresse),
         adress_zusatz = as.character(Zusatz)) %>% 
  select(objekt_Id:adress_zusatz) %>% 
  ## Rechnungsempfaenger, BS-Korrespondenzperson, Feuerwehr interessieren nicht:
  ## rausnehmen
  filter(funktion == "Eigentuemer" | funktion == "Verwalter")

## Mehrfache GebNr, mehrere Zeilen fuer dasselbe Gebaeude, da
## mehrere Funktionen - Eigentuemer, Verwalter, usw. - moeglich sind
## Loesung: mehrere eingentuemer in einer zelle zusammenfassen, mit
## komma trennen verwaltungsfunktion aus der zeile in spalten spreaden

# ## mehrere eigentuemer behalten, vermutlich nicht sinnvoll
# tmp <- dat %>%
#   group_by(objekt_Id, funktion) %>%
#   summarise(eigentuemer  = paste(funktion, collapse =", ")) %>%
#   spread(funktion, eigentuemer)

## spread and gather funktioniert nicht: wenn eigentuemer und verwalter
## unterschiedliche adressen besitzen, werden pro objekt_Id zwei zeilen
## generiert und eine davon mit NA gefuellt.
# tmp <- df %>%
#   gather(variable, value, -(objekt_Id:funktion)) %>%
#   unite(temp, funktion, variable) %>%
#   spread(temp, value)

## df verwalter
df1 <- dat %>%
  filter(funktion == "Verwalter") %>% 
  rename(verwalter = name,
         verwalt_zusatz = zusatz,
         verwalt_adresse = adresse,
         verwalt_adr_zusatz = adress_zusatz)

## df eigentuemer
df2 <- dat %>%
  filter(funktion == "Eigentuemer") %>%
  ## noch offen: mehrere eigentuemer behalten
  ## zur zeit wird der zweite eintrag geloescht
  distinct(objekt_Id, .keep_all = TRUE) %>% 
  rename(eigentuemer = name,
         eigentuem_zusatz = zusatz,
         eigentuem_adresse = adresse,
         eigentuemt_adr_zusatz = adress_zusatz)

## full join df1 and df2 by objekt_Id
df_out <- full_join(df2, df1[,-c(2:21)], by = "objekt_Id") %>% 
  select(-funktion) %>% 
  replace(., is.na(.), "") %>% 
  mutate(plz = as.integer(plz))

## Kontrolle: doppelte objekt_Ids? Wenn nicht, gut so.
tmp_dpl <- df_out %>%
  filter(objekt_Id %in% unique(.[["objekt_Id"]][duplicated(.[["objekt_Id"]])]))

# write.csv2(df_out, paste0(pth_data, "tbl_eigentuemer_verwalter_201801.csv"),
#            row.names = FALSE)



