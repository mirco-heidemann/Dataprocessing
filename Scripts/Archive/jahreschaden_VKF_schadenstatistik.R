## ---
## Schadendaten Lieferung an Markus Imhof, IRV per Ereignisdatum (Schadendatum)
## GESCHAETZTE SCHADENSUMME VERWENDEN (NICHT DIE AUSBEZAHLTE)
##
## Email: markus.imhof@irv.ch
##
## Gebaeudeversicherung Kanton Zuerich, Bereich Naturgefahren
## Mirco Heidemann, 06/2015

## Modified: 03/2018
## ---

## Schadenjahr waehlen:
jahr <- 2015

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- ("../data/")
pth_out <- ("../out/")

library(tidyverse)

## turn off scientific notation
options(scipen=999)

## Schaeden einlesen aus dem Schadenexport Tool
df <- read.csv2(paste0(pth_data, 'A_Schadendatenper04.06.2018_84217.csv'),
                stringsAsFactors = FALSE) %>% 
  mutate(schad_id = as.integer(SchadenId),
         gemeinde_nr = as.integer(Ausdr1),
         schad_datum = as.Date(SchadenDatum,"%d.%m.%Y"),
         schad_jahr = as.integer(format(schad_datum, "%Y")),
         schad_nr =  as.character(SchadenNr),
         zweckcode_vkf = as.integer(str_sub(str_extract(GebaeudeZweckText,
                                                        "[[:digit:]]+"), 1, 2)),
         schad_art = as.character(SchadenArtBezeichnung),
         schad_code = as.integer(str_extract(CodTextDt, "[[:digit:]]+")),
         schad_ursache = as.character(str_trim(gsub("\\d", "", CodTextDt),
                                                "left")),
         schad_geschaetzt = round(as.numeric(SchadenSumme)),
         vers_summe = as.numeric(SchadenVersicherungWert)) %>% 
  #filter(!is.na(schad_code)) %>% 
  dplyr::select(schad_id:vers_summe)

## doppelten Schaden ID's nur einmal beruecksichtigen!
## welche schaden ID sind doppelt vorhanden?
df_duplicates <- df %>%
  group_by(schad_id) %>%
  filter(n() > 1) %>%
  ## duplikate nur einmal anzeigen
  filter(row_number() == 1)

## doppelte schaden ID Zeilen nur einmal
df <- df %>%
  ## duplikate nur einmal
  distinct(schad_id, .keep_all = TRUE)

## Nach Elementar-, Feuerschaeden und Schadenjahr filtern
df_es <- df %>% 
  filter(schad_art == "Elementar",
         schad_jahr == jahr) %>% 
  select(gemeinde_nr = gemeinde_nr, schaden_datum = schad_datum,
         schaden_nr = schad_nr, zweckcode_vkf = zweckcode_vkf,
         schadencode_gvz = schad_code, schadenursache = schad_ursache,
         schadensumme = schad_geschaetzt, vers_summe = vers_summe)

df_fs <- df %>% 
  filter(schad_art == "Feuer",
         schad_jahr == jahr) %>% 
  select(gemeinde_nr = gemeinde_nr, schaden_datum = schad_datum,
         schaden_nr = schad_nr, zweckcode_vkf = zweckcode_vkf,
         schadencode_gvz = schad_code, schadenursache = schad_ursache,
         schadensumme = schad_geschaetzt, vers_summe = vers_summe)

# ## schreiben der tabellen
# write.csv2(df_es, paste0(pth_out, "tbl_elementarschaeden_gvz_",
#                          jahr, ".csv"), row.names = FALSE)
# 
# write.csv2(df_fs, paste0(pth_out, "tbl_feuerschaeden_gvz_",
#                          jahr, ".csv"), row.names = FALSE)

## Summarize fuer uebersicht
df %>% group_by(schad_art) %>%
  summarise(schadensumme = sum(schad_geschaetzt, na.rm = TRUE),
            anzahl = n())

df %>% group_by(schad_art) %>%
  filter(schad_geschaetzt > 0) %>% ## ohne Nuller - Schaeden
  summarise(schadensumme = sum(schad_geschaetzt, na.rm = TRUE),
            anzahl_verguetet = n())




