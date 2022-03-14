## VLookup in R to join Coordinates

library(tidyverse)
library(stringr)

## Read data
path <- paste0("J:/Naturgefahren/Administration/Behörden, Komissionen, Verbände/",
               "MeteoSchweiz/Hagelklima Schweiz/20200911 Erfa mit MESHS/",
               "Hageltage 2016 bis 2020/")
input_csv <- "Hagelereignis_Bauteile_20200728.csv"
input_file <- paste0(path, input_csv)

dat_schad <- read_delim(input_file, delim = ";",
                  locale = locale(encoding = 'UTF-8'),
                  col_names = TRUE,
                  col_types = cols_only(
                    Schadennummer = col_character(),
                    Bauteil = col_character()
                  )
)

# Georef file
input_csv <- "SQL_Schaden_LV95.csv"
input_file <- paste0(path, input_csv)

dat_lv95 <- read_delim(input_file, delim = ";",
                        locale = locale(encoding = 'UTF-8'),
                        col_names = TRUE,
                        col_types = cols_only(
                          SchadenNr = col_character(),
                          KoordinateNord = col_character(),
                          KoordinateOrd = col_character(),
                          GebaeudeStatus = col_character(),
                          SchadenDatum = col_date("%d.%m.%Y"),
                          SchadenStatus = col_character()
                        )
)


## Georef with LV95
df_schad_lv95 <- dat_schad %>%
  left_join(dat_lv95, by = c("Schadennummer" = "SchadenNr")) %>%
  filter(!is.na(KoordinateNord)) %>%
  rename(LV95_N = KoordinateNord,
         LV95_E = KoordinateOrd,)

## write a UTF-8 CSV
name_str <- paste0(path, "20200728_LV95.csv")
write_excel_csv(df_schad_lv95, name_str, delim = ";")
