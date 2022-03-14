## ----------------------------------------------------------------------------
##  Datenaufbereitung für den IRV - Erdbebenrisikomodell und PML Studie
##  
##  Stand 1.1.2020
##  Attribute:
##  - Identifikator: (Versicherungs-, Policenoder Gebäudenummer), Text, keine Duplikate
##  - Adresse (Strasse, Hausnummer, PLZ, Ortschaft)
##  - E-Koordinate, Koordinate im Format LV95
##  - N-Koordinate, Koordinate im Format LV95
##  - Baujahr
##  - Nutzungscode
##  - Versicherungssumme
##  - Gebäudevolumen
##  - Bauweise: nicht verfügbar bei der GVZ
##  - Versicherungsstatus: Versichert oder Bauzeit
##
##  Mirco Heidemann, Mai 2020
## ----------------------------------------------------------------------------

library(tidyverse)
library(stringr)

## daten einlesen
input_csv <- "I:/Climostatisticians/Data_Science/GVZ_Data/2020/SQL_Portfolio_20200511.csv"

dat <- read_delim(input_csv, delim = ";",
                  locale = locale(encoding = 'UTF-8'),
                  col_names = TRUE,
                  col_types = cols_only(
                    GebaeudeId = col_character(),
                    svStrassenname = col_character(),
                    egPolizeiNr = col_double(),
                    egPLZ = col_integer(),
                    egPLZ_Ort = col_character(),
                    naNutzungscode = col_integer(),
                    fdSchaetzwert = col_double(),
                    geErstellungsjahr = col_integer(),
                    fdGebaeudevolumen = col_double(),
                    ObvNrdKLok_Geb = col_character(),
                    ObvOstKLok_Geb = col_character()
                  )
) %>% 
  rename(
    GVZNr = GebaeudeId,
    Strasse = svStrassenname,
    HausNr = egPolizeiNr,
    PLZ = egPLZ,
    Ort = egPLZ_Ort,
    Nutzungscode = naNutzungscode,
    Versicherungssumme = fdSchaetzwert,
    Baujahr = geErstellungsjahr,
    Gebaeudevolumen = fdGebaeudevolumen,
    N_Koord_LV95 = ObvNrdKLok_Geb,
    E_Koord_LV95 = ObvOstKLok_Geb
  ) %>% 
  # Stand 01.01.2020
  filter(Baujahr < 2020)

## Identifiziere und entferne allfällige doppelten Gebäudenummern
df_duplicates <- dat %>%
  group_by(GVZNr) %>%
  filter(n() > 1) %>%
  ## duplikate nur einmal anzeigen
  filter(row_number() == 1)

# ## doppelte schaden ID Zeilen nur einmal
# df <- dat %>%
#   ## duplikate nur einmal
#   distinct(GVZNr, .keep_all = TRUE)

summary(dat)

# csv file schreiben
name_str <- paste0("GVZ_Gebaeudebestand_Stand_20200101.csv")
write_excel_csv(dat, name_str, delim = ";")
