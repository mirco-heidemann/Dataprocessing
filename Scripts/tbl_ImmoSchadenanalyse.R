## ----
## Immo Schadendaten
## Datenmanipulation fuer corina
## 
## Mirco Heidemann, Nov 2017
## ----

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- ("./data/")
pth_port <- ("../GVZ_Portfolio/")
pth_funct <- ("../R_functions/")
pth_out <- ("./out/")

## funktionen laden
library(dplyr)
library(stringr)
source(paste0(pth_funct, 'Fun_SchadPrepAll.R'))

## csv file einlesen: immo liste
immo <- read.csv2(paste0(pth_data, 'Gebaeude Immo.csv'), stringsAsFactors = FALSE)

## erste analyse der daten
# str(immo)
# summary(immo)

## ---- 
## aufbereitung und indexierung der gvz schaeden
gvz_index <- paste0(pth_port, 'versicherungsindex_gvz.csv')
dat_schaden <- paste0(pth_port, 'schaeden_201801_georef.csv')
## funktion 'Fun_SchadenPrep.R':
## Input: - Pfad des csv schaden files
##        - Pfad des csv index file
## Output: data.frame der aufbereiteten schaeden, inkl. indexierung

schad <- schadPrep(dat_schaden, gvz_index) %>%
  filter(str_detect(schad_code, 'Hagel')) ## nur hagelschaeden 

## join schaden zu immo ueber 'geb_nr'. Achtung: NON UNIQUE-KEY!
## (da mehrere schaeden pro gebaeude moeglich)

## VARIANTE A)
## wenn mehrere schaeden pro geb_nr, dann mehrere zeilen mit derselben geb_nr
#immo <- immo %>% mutate(geb_nr = ObvBenId_Geb)
# immo_join <- left_join(immo, schad, by = 'geb_nr')
immo_join <- left_join(immo, schad, by = c('ObvBenId_Geb' = 'geb_nr')) %>% 
  mutate(schad_index = replace(schad_index, is.na(schad_index), 0)) %>% 
  rename(geb_nr = ObvBenId_Geb) ## 'ObvBenId_Geb' in 'geb_nr' umbenennen

## Welche geb_nr haben mehrere hagelschaeden?
immo_join %>% group_by(geb_nr) %>% 
  filter(n()>1) %>% summarise(n=n())

## VARIANTE B)
## schaende in zelle zusammenfassen und mit komma separieren
schad_aggr <- immo_join %>%
  group_by(geb_nr) %>% arrange(schad_index) %>% ## nach geb_nr zusammenfassen
  ## mehrere hagelschaeden in eine zelle schreiben und mit komma trennen
  summarise(hagel_schad  = paste(schad_index, collapse =", "), 
            n_hagel_schad = ifelse(hagel_schad!=0, length(schad_index), 0)) %>%
            # n_hagel_schad = length(schad_index)) %>% ## anzahl hagelschaeden
  arrange(desc(n_hagel_schad), hagel_schad) ## absteigen nach anzahl schaeden und summe

## Wieder alle immo- und schadenangaben anheften
immo_schad_aggr <- left_join(schad_aggr, immo_join, by = 'geb_nr') %>% 
  ## doppelte zeilen (geb_nr) raus, alle anderen variablen behalten
  distinct(geb_nr, .keep_all = TRUE)

## Liste aufraeumen, nicht gebrauchte spalten loeschen
immo_schad_aggr <- immo_schad_aggr %>%
  select(-c(gwz_Cod_1_CodTextDt, schad_art, schad_code, schad_index))
    
## neues csv files schreiben
# write.csv2(immo_schad_aggr, 'immoHagelschaeden.csv', row.names = FALSE)

