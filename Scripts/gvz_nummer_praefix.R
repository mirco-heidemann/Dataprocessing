## Get new GVZ Number with Präfix for Zurich and Winterthur
## from old Number and Kataster number

library(tidyverse)
library(stringr)
library(openxlsx)

## Read data
path <- "J:/Naturgefahren/FG2_Datenanalysen/GIS_Projekte/Revisionsplanung/Daten/2021/"

input_csv <- "ARE_GEO_GIS_2021_ohne_Bless.csv"
input_file <- paste0(path, input_csv)

dat <- read_delim(input_file, delim = ";",
                        locale = locale(encoding = 'UTF-8'),
                        col_names = TRUE,
                        col_types = cols_only(
                          Revisionsjahr = col_integer(),
                          Gemeinde = col_integer(),
                          Geb_Nummer = col_character(),
                          G_Strasse = col_character(),
                          G_Nr = col_character(),
                          G_PLZ = col_integer(),
                          G_Ort = col_character(),
                          Zweckcode = col_character(),
                          Jahr = col_integer(),
                          Versicherungswert = col_number(),
                          Kataster = col_character()
                          ))

df <- dat %>%
  mutate(
    praefix = as.character(str_trim(gsub("\\d", "", Kataster), "left")),
    tmp_gvz_nr = as.character(str_pad(Geb_Nummer, 5, pad = "0")),
    gvz_nr_praefix = ifelse(Gemeinde == 261 | Gemeinde == 230, paste0(praefix, tmp_gvz_nr), tmp_gvz_nr),
    Geb_Nummer = as.character(gvz_nr_praefix)) %>%
  select(Revisionsjahr:Kataster)

out_csv <- paste0(path, "ARE_GEO_GIS_2021_ohne_Bless_Modified_Praefix.csv")
out_xlsx <- paste0(path, "ARE_GEO_GIS_2021_ohne_Bless_Modified_Praefix.xlsx")

write_excel_csv(df, out_csv, delim = ";") 

## write an excel file, it will keep the leading zeros
# sheet_name <- str_split(input_csv, ".csv", simplify = TRUE)[,1]
# write.xlsx(df, out_xlsx, sheetName = sheet_name)
