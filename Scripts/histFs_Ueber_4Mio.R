## ----
## Aufbereitung Feuer Schadendaten (indexiert) ueber 4 Mio
## zusammenfassen nach Ereignis:
## - gleiches Schadendatum
## - gleiche Gemeinde
## - gleiche Ursache
##
## Mirco Heidemann
## Maerz 2018: Datenaufbereitung fuer IRV, Ewa Kozlowski
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

## ---- 
## aufbereitung und indexierung der gvz schaeden
gvz_index <- paste0(pth_port, 'versicherungsindex_gvz.csv')
dat_schaden <- paste0(pth_port, 'schaeden_201801_georef.csv')
## funktion 'Fun_SchadPrep.R':
## Input: - Pfad des csv schaden files
##        - Pfad des csv index file
## Output: data.frame der aufbereiteten schaeden, inkl. indexierung

df <- schadPrep(dat_schaden, gvz_index) %>%
  ## filter nach feuerschaeden groesser 4 Mio index. Schadsumme
  filter(schad_art == "Feuer",
         schad_index > 4e6) %>% 
  arrange(schad_datum) %>% 
  group_by(schad_datum, schad_code, gemeinde) %>%  ## pro ereignis zusammenfassen (falls noetig)
  summarise(schaden = sum(schad_index))

# write.csv2(df, paste0(pth.out, "GVZ_Feuerschaeden_ueber_4Mio.csv"), row.names = FALSE)

