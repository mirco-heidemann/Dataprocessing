## Data Manipulation mit dplyr and stringr:
## Versicherungssumme der Stadt ZH aus Gemdat aufbereiten

## Mirco Heidemann, Januar 2018

## pfade spezifizieren und library laden
pth_port <- ("../Portfolio_GVZ/")

library(tidyverse)

input_csv <- paste0(pth_port, "portfolio_20190205.csv")

df <- read_delim(input_csv, delim = ";", col_names = TRUE,
                 locale = locale(encoding = 'ISO-8859-1'),
                 col_types = cols_only(
                   egPLZ_Ort = col_character(),
                   fdSchaetzwert = col_double()))

## nur...
## VerSum der Stadt ZH
df_zh <- df %>% 
  filter(egPLZ_Ort == "Zürich") %>% 
  group_by(egPLZ_Ort) %>% 
  summarize(Versicherungssumme = sum(fdSchaetzwert))


## Export des CSV
write_excel_csv2(df_zh, "versicherungssumme_stadtZH_2018.csv")