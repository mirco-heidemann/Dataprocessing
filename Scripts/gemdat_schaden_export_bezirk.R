## Data Manipulation mit dplyr and stringr
## Schadendaten aus Gemdat für ganze Bezirke aufbereiten

## Bemerkung: Ab Juni 2004 wird in GemDat die gesamte Schadensumme für den Eigentümer 
## erfasst (inklusive Selbstbehalt). Z.B. Geschätzte Schadensumme in GemDat von 
## CHF 1'000, Auszahlung von CHF 500.

## Mirco Heidemann, Oktober 2018

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- "I:/Datadrop/"

library(dplyr)
library(stringr)

dat <- read.csv2(paste0(pth_data, "A_Schadendatenper20.12.2018_18740.csv"),
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
  filter(str_detect(schad_code,
                    # paste(c('Sturm', 'Hagel', 'Hochwasser'), collapse="|")),
                    'Sturm'),
         scha_dat >= as.Date("03.01.2018", "%d.%m.%Y") &
           scha_dat <= as.Date("04.01.2018", "%d.%m.%Y"),
         # gesamter bezirk Horgen:
         gemeinde == 'Adliswil' | gemeinde == 'R?schlikon' |
           gemeinde == 'Thalwil' | gemeinde == 'Langnau am Albis' |
           gemeinde == 'Oberrieden' | gemeinde == 'Horgen' |
           gemeinde == 'Hirzel'| gemeinde == 'Sch?nenberg'| gemeinde == 'H?tten' |
           gemeinde == 'W?denswil' | 
           # gesamter Bezirk Meilen
           gemeinde == 'Hombrechtikon' |
           gemeinde == 'St?fa' | gemeinde == 'Oetwil am See' |
           gemeinde == 'M?nnedorf' | gemeinde == 'Uetikon am See' |
           gemeinde == 'Meilen' | gemeinde == 'Herrliberg' | gemeinde == 'Erlenbach' |
           gemeinde == 'K?snacht' | gemeinde == 'Zumikon' | gemeinde == 'Zollikon')
         #schad_sum > 0)

tbl_schad <- df_select %>% 
  group_by_('gemeinde') %>% 
  summarize(schadenanzahl = n(), schadensumme = sum(schad_sum))
write.csv2(tbl_schad, 'tbl_schad.csv', row.names = F)

## Klassen fuer bar chart definieren
tbl_cluster <- data.frame(schad_sum = seq(0, ceiling(max(df_select$schad_sum)/1e5)*1e5, by = 1))
tbl_cluster$cluster <- cut(tbl_cluster$schad_sum,
                           breaks = c(0, 5e3, 20e3, 50e3, 100e3, 250e3,
                                      500e3, Inf),
                           include.lowest = TRUE,
                           labels=c(as.character(1:7)))
## Schadensummen klassieren
df_select <- left_join(df_select, tbl_cluster, by = "schad_sum")

## group by clusters
tbl <- df_select %>% 
  group_by(cluster) %>% 
  summarise(schadenanzahl = n(), schadensumme = sum(schad_sum))

# library(RColorBrewer)
# display.brewer.all(colorblindFriendly = TRUE)

library(ggpubr)
x_labels = c("bis 5'000", "5'001 - 20'000", "20'001 - 50'000", "50'001 - 100'000",
             "100'001 - 250'000", "250'001 - 500'000", "500'001 - 1'000'000")
text_labels = paste0("Schadenanzahl: ", formatC(length(df_select$schad_sum),
                                                format="f", big.mark="'", digits=0),
                     "\nSchadensumme total: ", formatC(sum(df_select$schad_sum),
                                                       format="f", big.mark="'", digits=0),
                     "\nGrösster Schaden: ", formatC(max(df_select$schad_sum),
                                                      format="f", big.mark="'", digits=0),
                     "\nKleinster Schaden: ", formatC(min(df_select$schad_sum),
                                                      format="f", big.mark="'", digits=0),
                     "\nSchadenmittel: ", formatC((mean(df_select$schad_sum)),
                                                  format="f", big.mark="'", digits=0),
                     "\nMedian: ", formatC(median(df_select$schad_sum),
                                           format="f", big.mark="'", digits=0))

(ggbar <- ggbarplot(tbl, x = "cluster", y = "schadenanzahl",
                    fill = "cluster", color = "cluster",
                    palette = rev(brewer.pal(9, "Blues")),
                    label = TRUE,
                    
                    position = position_dodge(),
                    xlab = "Schadensumme [CHF]", ylab = "Anzahl Schäden") +
    scale_x_discrete(labels = x_labels) +
    labs(title = "Schadensummenverteilung des Gewitters vom 30.05.2018",
         subtitle = "Durch die GVZ vergütete Elementarschäden im Bezirk Dielsdorf",
         caption = paste0("Quelle: GVZ, ", format(Sys.Date(), "%d/%m/%Y"))) +
    theme(axis.text.x = element_text(size = 10),
          legend.position="none",
          plot.caption = element_text(size = 9, face = "italic")) +
    annotate("text",  x=Inf, y = Inf, vjust=1, hjust=1,
             label = text_labels))

## --- Save the ggplot's
width.plot = 12
# height.plot = (3/4) * width.plot
height.plot = (9/16) * width.plot

# ## save a png
# ggsave(ggbar, filename = "ggpubr_schadSum_verteilung.png",
#        dpi = 1000, type = "cairo",
#        width = width.plot, height = height.plot, units = "in")
## save a pdf
# ggsave(ggbar, filename = "ggpubr_schadSum_verteilung.pdf", device = cairo_pdf,
#        width = width.plot, height = height.plot, units = "in")


# # Basic density plot
# # Add mean line and marginal rug
# gghistogram(df_select, x = "schad_sum", color = "#08519c", fill = "#9ecae1",
#             rug = TRUE, bins = 60, # add = "median", 
#             title = "Häufigkeitsverteilung der Schadensummen bis 100'000",
#             xlab = "Schadensumme", ylab = "Anzahl Schäden", xlim=c(0, 100000))


