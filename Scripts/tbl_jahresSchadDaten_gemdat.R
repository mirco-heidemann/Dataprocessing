## Data Manipulation mit dplyr and stringr
## Schadendaten aus Gemdat aufbereiten
## GemDat Schaeden seit 1981

## Mirco Heidemann, Januar 2018

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_port <- ("../GVZ_Portfolio/")

library(tidyverse)

## Alle gemdat schaeden - nicht referenziert - seit 1981
## laden, aufbereiten und indexieren

dat <- read.csv2(paste0(pth_port, "schaeden_201801.csv"),
                 stringsAsFactors = F)
## Fuer Indexierung
gvz_index <- read.csv2(paste0(pth_port, 'versicherungsindex_gvz.csv'),
                       stringsAsFactors = FALSE) %>% 
  mutate(jahr = as.integer(format(as.Date(Jahr,"%d.%m.%Y"), "%Y")),
         index = as.integer(Versicherungsindex.GVZ)) %>% 
  select(c(5, 6))

## relevanten felder...
df <- dat %>% mutate(geb_Id = as.integer(GebaeudeId),
                     geb_nr = GebaeudeSuchbegriff,
                     schad_id = as.integer(SchadenId),
                     schad_nr = SchadenNr,
                     schad_datum = as.Date(SchadenDatum,"%d.%m.%Y"),
                     schad_jahr = as.integer(format(schad_datum, "%Y")),
                     schad_sum = as.numeric(SchadenSumme),
                     gemeinde = as.character(GebaeudeGemeindeName),
                     schad_art = as.character(SchadenArtBezeichnung),
                     schad_code = as.character(CodTextDt),
                     #zweck_code_text = as.character(GebaeudeZweckText),
                     zwk_code = as.integer(str_extract(GebaeudeZweckText, "\\d+")),
                     zwk_code_text = as.character(str_extract(GebaeudeZweckText,
                                                              "[A-Z][a-z]+")),
                     status = as.character(Tab_Stc_StcTextDt),
                     baujahr = as.integer(GebaeudeBaujahr)) %>% 
  dplyr::select(geb_Id:baujahr)

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
  distinct(schad_id, .keep_all = TRUE) %>% 
  ## schaeden indexieren
  left_join(gvz_index, by = c("schad_jahr" = "jahr")) %>% 
  mutate(schad_index = round(max(gvz_index$index) / index * schad_sum))

# ## nur elementarschaeden, schadensumme ueber 0
# df_es <- df %>% 
#   filter(schad_art == "Elementar",
#          schad_index > 0)

## nur...
## ueberschwemmungsschaeden,
## schadensumme ueber 0,
## schadendatum zwischen 1982 und 2017
df <- df %>% 
  filter(str_detect(schad_code, "Hagel"),
         # schad_index > 0,
         # schad_jahr > 1981 & schad_jahr < 2018)
         schad_datum >= as.Date("1993-07-05") &
           schad_datum <= as.Date("1993-07-07"))

## aggregiere die einzenlschaeden zu jahresschaden
aggr_anz <- aggregate(df$schad_index,
                      by = list(as.numeric(format(df$schad_datum,'%Y'))),
                      'length')
aggr_sum <- aggregate(df$schad_index,
                      by = list(as.numeric(format(df$schad_datum,'%Y'))),
                      'sum')
tbl_df <- as.data.frame(cbind(aggr_sum, aggr_anz$x))
names(tbl_df) <- c('jahr', 'schadensumme_indexiert', 'schadenanzahl')

# ## schreiben der tabellen
# write.csv2(tbl_df, "tbl_ueberschwemmungsschaeden_indexiert_gvz.csv",
#            row.names = FALSE)
