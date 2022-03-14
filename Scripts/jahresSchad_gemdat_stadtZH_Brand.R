## Data Manipulation mit dplyr and stringr:
## Brandschadendaten der Stadt ZH aus Gemdat aufbereiten
## GemDat Schaeden 2017 und 2018

## Mirco Heidemann, Januar 2018

## pfade spezifizieren
pth_port <- ("../Portfolio_GVZ/")

library(tidyverse)

input_csv <- paste0(pth_port, "schaden_201901.csv")

df <- read_delim(input_csv, delim = ";", col_names = TRUE,
                 locale = locale(encoding = 'ISO-8859-1'),
                 col_types = cols_only(
                   GebaeudeId = col_integer(),
                   GebaeudeSuchbegriff = col_character(),
                   SchadenId = col_integer(),
                   SchadenNr = col_character(),
                   SchadenDatum = col_date("%d.%m.%Y"),
                   Ausdr3 = col_integer(),
                   SchadenSumme = col_double(),
                   GebaeudeGemeindeName = col_character(),
                   SchadenArtBezeichnung = col_character(),
                   CodTextDt = col_character(),
                   GebaeudeZweckText = col_character(),
                   Tab_Stc_StcTextDt = col_character(),
                   GebaeudeBaujahr = col_integer()))

## doppelten Schaden ID's nur einmal beruecksichtigen!
## welche schaden ID sind doppelt vorhanden?
df_duplicates <- df %>%
  group_by(SchadenId) %>%
  filter(n() > 1) %>%
  ## duplikate nur einmal anzeigen
  filter(row_number() == 1)

## doppelte schaden ID Zeilen nur einmal
df <- df %>%
  ## duplikate nur einmal
  distinct(SchadenId, .keep_all = TRUE)

# ## nur elementarschaeden, schadensumme ueber 0
# df_es <- df %>% 
#   filter(schad_art == "Elementar",
#          schad_index > 0)

## nur...
## feuerschäden, schadensumme über 0, schadendatum zwischen 2017 und 2018, in der Stadt ZH
df <- df %>% 
  filter(str_detect(SchadenArtBezeichnung, "Feuer"),
         # schad_index > 0,
         # schad_jahr > 1981 & schad_jahr < 2018)
         SchadenDatum >= as.Date("2017-01-01") &
           SchadenDatum <= as.Date("2019-01-01"),
         str_detect(GebaeudeGemeindeName, "Zürich-"))

df_2017 <- df %>%
  filter(Ausdr3 == '2017',
         # nur schaden ueber 0
         SchadenSumme > 0) %>% 
  group_by(GebaeudeGemeindeName) %>% 
  summarize(n_brandschaden = n(),
            schadensumme = sum(SchadenSumme))

df_2018 <- df %>%
  filter(Ausdr3 == '2018',
         # nur schaden ueber 0
         SchadenSumme > 0) %>% 
  group_by(GebaeudeGemeindeName) %>% 
  summarize(n_brandschaden = n(),
            schadensumme = sum(SchadenSumme))

## Export des CSV
write_excel_csv2(df_2018, "brandschaden_stadtZH_2018.csv")
