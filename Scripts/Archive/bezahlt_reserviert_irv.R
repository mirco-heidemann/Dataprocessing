## ----
## Schadenzusammenstellung: bezahlte und reservierte Elementar- und 
## Feuerschaeden im SCHADENjahr (seit 1982)
##  bis 2004: geschaetze Schaeden (Selbstbehalt im GemDat schon abgezogen)
##  ab 2005:  Aufteilung nach bezahlt und reserviert
##  Bezahlte Schaeden ab CHF 20'000 sind inklusive Verzinsung im GemDat
##  erfasst
##  
## Wird fuer die IRV-Praemienberechnung verwendet, Daten gehen an Ewa Kozlowski.
## Email: ewa.kozlowski@irv.ch
##
## Geb?udeversicherung Kanton Z?rich, Bereich Naturgefahren
## Feb 2018, Mirco Heidemann
## ----

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_port <- ("../GVZ_Portfolio/")
pth_out <- ("../out/")

library(tidyverse)

## csv file einlesen
dat <- read.csv2(paste0(pth_port, "schaeden_201801.csv"),
                 stringsAsFactors = FALSE)
# summary(dat)
df <- dat %>% mutate(geb_id = as.integer(GebaeudeId),
                     geb_nr =  as.character(GebaeudeSuchbegriff),
                     schad_id = as.integer(SchadenId),
                     schad_nr =  as.character(SchadenNr),
                     schad_datum = as.Date(SchadenDatum,"%d.%m.%Y"),
                     schad_geschaetzt = as.numeric(SchadenSumme),
                     bezahlt_datum = as.Date(FkoVersDa,"%d.%m.%Y"),
                     schad_bezahlt = as.numeric(FkoZaTot),
                     schad_art = as.character(SchadenArtBezeichnung),
                     schad_code = as.character(CodTextDt),
                     vers_summe = as.numeric(SbwVerWert),
                     zwk_code_text = as.character(GebaeudeZweckText),
                     status =  as.character(T_Tab_Jse_1_StcTextDt)) %>%
  select(geb_id:status)


## TEILAUSZAHLUNGEN: die ausbezahlte summe muss zu einem total zusammengezaehlt
## werden, die dazugehoerige geschaetzte schadensumme ist jedesmal wieder
## aufgefuert, darf aber nur einmal gezaehlt werden.

# # Alle doppelten werte: teilauszahlungen
# tmp.teilZahl <- df %>%
#   filter(schad_id %in% unique(.[["schad_id"]][duplicated(.[["schad_id"]])]))
# 
# # nur ein duplikat, nur die letzte auszahlung
# tmp <- df %>% filter(duplicated(.[["schad_id"]]))

## summieren der teilauszahlungen zu einer gesamtsumme ...
tmp <- df %>% group_by(schad_id) %>%
  summarise(schad_bezahlt_tot = sum(schad_bezahlt))

## df mit summierten teilauszahlungen und mit reservierten schaeden ergaenzen
df <- left_join(df, tmp, by = 'schad_id') %>% # join der daten, inkl. den duplikaten
  distinct(schad_id, .keep_all = TRUE) %>%   ## remove duplicates
  select(-schad_bezahlt) %>% 
  ## Reservierte Schaeden: geschaetzte Schadensumme, keine Auszahlung
  mutate(schad_reserviert = ifelse(is.na(schad_bezahlt_tot), schad_geschaetzt, 0),
         schad_bezahlt_tot = ifelse(is.na(schad_bezahlt_tot), 0, schad_bezahlt_tot),
         ## Geschaetzte Schaeden:
         ## Bis Mitte 2004 wurden im GemDat die Netto Summen als geschaetzte
         ## Schadensummen verbucht (Werte in GemDat ohne Selbestbehalt) danach
         ## die Brutto-Schadenschaetzung (Werte in GemDat inkl. Selbestbehalt)
         schad_geschaetzt = ifelse(schad_datum > as.Date('30.06.2004', "%d.%m.%Y"),
                              schad_geschaetzt - 500, schad_geschaetzt),
         schad_geschaetzt = ifelse(schad_geschaetzt < 0, 0, schad_geschaetzt))

rm(tmp)

## Schadenjahr Feuerschaeden
df_feuer <- df %>% filter(schad_art == "Feuer") %>% 
  group_by(schad_jahr = format(schad_datum, "%Y")) %>%
  summarise(schad_geschaetzt_tot = sum(schad_geschaetzt),
            schad_bezahlt_tot = sum(schad_bezahlt_tot),
            schad_reserviert_tot= sum(schad_reserviert)) %>% 
  mutate(schad_jahr = as.integer(schad_jahr)) %>% 
  ## fuer Schadenjahre bis 2004, nur geschaetzte Schadensumme
  mutate(schad_reserviert_tot = round(ifelse(schad_jahr < 2005, 0,
                                       schad_reserviert_tot), 0),
         schad_bezahlt_tot = round(ifelse(schad_jahr < 2005, 0,
                                      schad_bezahlt_tot), 0),
         schad_geschaetzt_tot = round(ifelse(schad_jahr > 2004, 0,
                                       schad_geschaetzt_tot), 0)) %>% 
  ## ohne das aktuelle schadenjahr, erst ab 1982
  filter(!(schad_jahr < 1982 | schad_jahr == as.numeric(format(Sys.Date(), "%Y"))))


## Schadenjahr Elementarschaeden
df_element <- df %>% filter(schad_art == "Elementar") %>% 
  group_by(schad_jahr = format(schad_datum, "%Y")) %>%
  summarise(schad_geschaetzt_tot = sum(schad_geschaetzt),
            schad_bezahlt_tot = sum(schad_bezahlt_tot),
            schad_reserviert_tot= sum(schad_reserviert)) %>% 
  mutate(schad_jahr = as.integer(schad_jahr)) %>% 
  ## fuer Schadenjahre bis 2004, nur geschaetzte Schadensumme
  mutate(schad_reserviert_tot = round(ifelse(schad_jahr < 2005, 0,
                                 schad_reserviert_tot), 0),
         schad_bezahlt_tot = round(ifelse(schad_jahr < 2005, 0,
                                schad_bezahlt_tot), 0),
         schad_geschaetzt_tot = round(ifelse(schad_jahr > 2004, 0,
                                 schad_geschaetzt_tot), 0)) %>% 
  ## ohne das aktuelle schadenjahr, erst ab 1982
  filter(!(schad_jahr < 1982 | schad_jahr == as.numeric(format(Sys.Date(), "%Y"))))

## CSV Files schreiben
# write.csv2(df_feuer, paste0(pth_out, 'jahresschad_feuer_2017.csv'), row.names = F)
# write.csv2(df_element, paste0(pth_out, 'jahresschad_elementar_2017.csv'), row.names = F)


