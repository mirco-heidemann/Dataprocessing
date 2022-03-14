## ----
## Monitoring Hagelschadenpraevention
## "Welche Gebaeude fuer Hagelpraevention,
## zeitnah nach einem Ereignis?"
##
## Definiere automatisierbare Kriterien
## fuer moegliche Praeventionsobjekte
##
## Mirco Heidemann, Nov 2017
## ----

## Definiere Anfangs- und End-Datum (TT.MM.JJJJ) der Ereignisauswertung
## Hagelevents: 08.07.2004, 12.08.2004, 07.07.2011, 01.07.2012, 18.06.2013
event_date <- '01.08.2017'
## definiere schadenart
event_art <- 'Hagel'

## relative pfade spezifizieren
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

pth_data <- ("../../Datadrop/")
pth_port <- ("../Portfolio_GVZ/")
pth_funct <- ("../R_functions/")

## Funktionen laden
library(dplyr)
library(stringr)
library(plotly)

gvz_index <- paste0(pth_port, 'versicherungsindex_gvz.csv')

# ## --- aufbereitung und indexierung aller gvz schaeden
# source(paste0(pth_funct, 'Fun_SchadPrepAll.R'))
# dat_schaden <- paste0(pth_port, 'schaeden_201801_georef.csv')
# 
# ## funktion 'Fun_SchadPrepAll.R':
# ## Input: - Pfad des csv schaden files
# ##        - Pfad des csv index file
# ## Output: data.frame der aufbereiteten schaeden, inkl. indexierung
# schad <- schadPrep(dat_schaden, gvz_index)
## ---- 

## --- aufbereitung und indexierung der gvz schaeden aus dem 
## GVZ-Auswertungstool-V2-0.accdb
source(paste0(pth_funct, 'Fun_SchadPrepTool.R'))
dat_schaden <- paste0(pth_data, 'A_Schadendatenper09.10.2018_13654.csv')

## funktion 'Fun_SchadPrepTool.R':
## Input: - Pfad des csv schaden files
##        - Pfad des csv index file
## Output: data frame der aufbereiteten schaeden, inkl. indexierung
schad <- schadPrep(dat_schaden, gvz_index)
## ---- 

## Datum der Schadenstatistik
event_date <- as.Date(event_date, '%d.%m.%Y')
event_end <- event_date + 1

## Datenaufbereitung mit dplyr
## ... nur hagelschaeden ...
schad <- schad %>% filter(str_detect(schad_code, event_art)) %>%
  ## ... nur  am schadendatum
  filter(schad_datum >= event_date & schad_datum <= event_end)

schad <- schad %>% mutate(code = floor(schad$zwk_code/1000),
                          fact_code = as.factor(code))


## Bubble Plot mit library(plotly)
bubblePlot <- plot_ly(schad, x = ~schad_index, y = ~vers_summe,
                      text = ~paste('geb_nr:', geb_nr, '<br>SchadenNr:',
                                    schad_index, '<br>Zweckcode:', zwk_code,
                                    '<br>Baujahr:', baujahr),
                      type = 'scatter', mode = 'markers', size = ~schad_index,
                      color = ~fact_code, colors = 'Paired',
                      marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = paste0('Hagelschaden ', format(event_date,'%d.%m.%Y'), '\nAnzahl Gebäude = ',
                        dim(schad)[1]),
         xaxis = list(title = 'indexierte Schadensumme',
                      #range = c(5000, max(schad$schadin)),
                      type = 'log',
                      zerolinewidth = 1,
                      gridwidth = 1),
         yaxis = list(title = 'Versicherungssumme',
                      #range = c(36.12621671352166, 91.72921793264332),
                      type = 'log',
                      zerolinewidth = 1,
                      gridwith = 1))

## suche faktoren aus den Gebaeudedaten, 
## welche zu grossen schaeden fuehren...

vers_summe <- schad %>%
  filter(code == 1 | code == 2 | code == 4 | code == 5 | code == 8 | code == 9,
         vers_summe >= 150e6)

landwirt <- schad %>%
  ## zweckcode 3 nur gewaechshaeuser oder gaertnereigeb: 3535, 3501
  filter(zwk_code == 3535 | zwk_code == 3501,
         vers_summe >=100e3)

industrie <- schad %>%
  filter(code == 7 | code == 6,
         vers_summe >= 20e6)

praevention <- rbind(vers_summe, landwirt, industrie) %>%
  arrange(-vers_summe)

## prozentsatz der anzuschauende gebaeude bezogen auf gesamt schadenzahl
prozSatz <- round((dim(praevention)[1] / dim(schad)[1]) * 100, 1)

message(paste0("Prozentsatz auf alle gemeldeten Hagelschaeden: ", prozSatz))

## Bubble Plot mit library(plotly)
(bubblePlot.final <- plot_ly(praevention, x = ~schad_index, y = ~vers_summe,
                             text = ~paste('geb_nr:', geb_nr, '<br>SchadenNr:',
                                           schad_index, '<br>Zweckcode:', zwk_code,
                                           '<br>Baujahr:', baujahr),
                             type = 'scatter', mode = 'markers', size = ~schad_index,
                             color = ~fact_code, colors = 'Paired',
                             marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
    layout(title = paste0('Hagelschaden ', format(event_date,'%d.%m.%Y'), ': ',
                          dim(praevention)[1],' Geb?ude',
                          '<br>(',prozSatz, '% aller gemeldeten Schaeden)'),
           xaxis = list(title = 'indexierte Schadensumme',
                        #range = c(5000, max(schad$schadin)),
                        type = 'log',
                        zerolinewidth = 1,
                        gridwidth = 1),
           yaxis = list(title = 'Versicherungssumme',
                        #range = c(36.12621671352166, 91.72921793264332),
                        type = 'log',
                        zerolinewidth = 1,
                        gridwith = 1)))

## liste der gebaeude ausgeben, muessen manuell auf
## hagelschadenpotenzial gepueft werden
file_name <- paste0('monitoring_hagelpraevention_',
                   format(Sys.time(), '%Y%m%d_%H%M'), '.csv')

dfOut <- praevention %>% select(geb_nr, schad_datum, schadschaetz_datum,
                                vers_summe, baujahr, zwk_code)


# ## "turn off" scientific notation in write.csv 
# dfOut$versSumme <-format(dfOut$versSumme, scientific = FALSE)
# write.csv2(dfOut, fileName, row.names = FALSE)

