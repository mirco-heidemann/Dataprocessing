## Data Manipulation mit dplyr and stringr
## Schadendaten aus Gemdat aufbereiten

## Bemerkung: In GemDat wird immer die gesamte Schadensumme für den Eigentümer 
## erfasst (inklusive Selbstbehalt). Z.B. Geschätzte Schadensumme in GemDat von 
## CHF 1'000, Auszahlung von CHF 500.

## Mirco Heidemann, Januar 2018

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- ("./data/")

library(dplyr)
library(stringr)

dat <- read.csv2(paste0(pth_data, "A_Schadendatenper09.10.2018_13654.csv"),
                 stringsAsFactors = F)

## relevanten felder...
df <- dat %>% mutate(geb_Id = as.integer(GebaeudeId),
                     geb_nr = GebaeudeSuchbegriff,
                     schad_id = as.integer(SchadenId),
                     schad_nr = SchadenNr,
                     scha_dat = as.Date(SchadenDatum,"%d.%m.%Y"),
                     schad_sum = as.numeric(SchadenSumme),
                     erledigt_date = as.Date(EdiAfDat, "%d.%m.%Y"),
                     gemeinde = as.character(GebaeudeGemeindeName),
                     schad_art = as.character(SchadenArtBezeichnung),
                     schad_code = as.character(CodTextDt),
                     zweck_code = as.integer(GebaeudeZweckCode),
                     zweck_code_text = as.character(GebaeudeZweckText),
                     status = as.character(StcTextDt),
                     baujahr = as.integer(GebaeudeBaujahr),
                     volumen = as.integer(GebaeudeVolumen)) %>% 
  dplyr::select(geb_Id:volumen)

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

# df_select <- df %>% 
#   filter(#schad_art == 'Elementar',
#          str_detect(schad_code, "Blitz"),
#          # str_detect(status, paste(c('erledigt', 'pendent'), collapse="|")),
#          ## Wintersturm Burglind vom 3. Januar 2018
#          # scha_dat == as.Date("03.01.2018", "%d.%m.%Y"))
#          scha_dat >= as.Date("01.01.2017", "%d.%m.%Y") &
#            scha_dat <= as.Date("28.05.2017", "%d.%m.%Y"))
# 
#          # between(scha_dat, as.Date("21.05.2018", "%d.%m.%Y"),
#          #         as.Date("27.05.2018", "%d.%m.%Y")))

df_select <- df %>%
  filter(schad_art == 'Elementar',
         # str_detect(status, paste(c('erledigt', 'pendent'), collapse="|")),
         ## Wintersturm Burglind vom 3. Januar 2018
         # scha_dat == as.Date("03.01.2018", "%d.%m.%Y"))
         scha_dat >= as.Date("01.01.2018", "%d.%m.%Y") &
           scha_dat <= as.Date("17.10.2018", "%d.%m.%Y"))


(tbl <- df_select %>% 
  group_by(schad_code) %>% 
  summarise(anzahl_schad = n(), schad_sum = sum(schad_sum)))

library(ggpubr)
# Basic density plot
# Add mean line and marginal rug
gghistogram(df_select, x = "schad_sum", color = "#08519c", fill = "#9ecae1",
            rug = TRUE, bins = 60, # add = "median", 
            title = "Häufigkeitsverteilung der Schadensummen bis 100'000",
            xlab = "Schadensumme", ylab = "Anzahl Schäden", xlim=c(0, 100000))

  
