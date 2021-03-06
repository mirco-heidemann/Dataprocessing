## -------------------------------------------------------------------------------
## Schadenstatistik eines Elementarereignisses
##
## Folgende Ausgaben werden erzeugt:
## - Schadenmeldungen nach Ereignis pro Gemeinde und Sch�tzkreis
## - Entwicklung der gemeldeten und erfassten Sch�den
## - Schadentabelle mit Anzahl gemeldeter und erfasster Sch�den pro Sch�tzkreis
##
## Geb�udeversicherung Kanton Z�rich, Bereich Naturgefahren
## Mirco Heidemann, 08/2017
##
## Modified by Christoph Welker, 10/2017
## -------------------------------------------------------------------------------

## Schadenfile aus der Access Anwendung "GVZ-Auswertungstool-V2-0.accdb"
schadfile <- 'A_Schadendatenper27.10.2017_84459.csv'

# Definiere Anfangs- und End-Datum (TT.MM.JJJJ) der Ereignisauswertung
event.date <- '01.08.2017'
# event.date <- '02.08.2017'
# event.date <- '01.01.2015'

#   ###### ###### ######
#   # ++++
#   ## Winterst�rme
#   # event.date      <- '21.11.2016'
#   # event.date      <- '13.01.2017'
#   # event.date      <- '04.02.2017'
#   # event.date      <- '23.02.2017'
#   # event.date      <- '28.02.2017'
#   # event.date      <- '04.03.2017'
#   # ++++
# 
#   # ++++
#   ## Sommerst�rme
#   # event.date      <- '23.06.2017'
#   # event.date      <- '29.06.2017'
#   # event.date      <- '19.07.2017'
#   # event.date      <- '21.07.2017'
#   # event.date      <- '01.08.2017'
#   # event.date      <- '02.08.2017'
#   # event.date      <- '18.08.2017'
#   event.date      <- '24.08.2017'
#   # ++++
#   ###### ###### ######

## End-Datum der Schadenstatistik, standardm�ssig auf event.date gesetzt
# date.end <- event.date
date.end <- '02.08.2017'
# date.end <- '20.08.2017'

## Ursache des Elementarereignisses:
## elementar: Hagel-, Sturm- und Ueberschwemmungsschaeden. Bei Hagelereignis,
##            da Schadentrennung schwierig.
## gewitter: Wie elementar inkl. Blitzschaeden
## sturm: Nur Sturmschaeden
## ueberschwemmung: Nur Ueberschwemmungsschaeden
event.Art <- 'elementar'
# event.Art <- 'sturm'
# event.Art <- 'hagel'

# title.schadPlot <- 'Schadenmeldungen durch Sturm vom 01. und 02.08.2017'
# title.verlaufPlot <- paste('Sturm vom 01. und 02.08.2017 - Erfasste ',
#                            'Schadenmeldungen und abgesch�tzte Sch�den',sep="")

title.schadPlot <- 'Schadenmeldungen durch Unwetter vom 01. und 02.08.2017'
title.verlaufPlot <- paste('Unwetter vom 01. und 02.08.2017 - Erfasste ',
                           'Schadenmeldungen und abgesch�tzte Sch�den',sep="")

# title.schadPlot <- 'Schadenmeldungen durch Unwetter vom 01.08.2017'
# title.verlaufPlot <- paste('Unwetter vom 01.08.2017 - Erfasste ',
#                            'Schadenmeldungen und abgesch�tzte Sch�den',sep="")

# title.schadPlot <- 'Schadenmeldungen durch Hagel vom 01.08.2017'
# title.verlaufPlot <- paste('Hagel vom 01.08.2017 - Erfasste ',
#                            'Schadenmeldungen und abgesch�tzte Sch�den',sep="")

# title.schadPlot <- 'Schadenmeldungen durch Unwetter vom 02.08.2017'
# title.verlaufPlot <- paste('Unwetter vom 02.08.2017 - Erfasste ',
#                            'Schadenmeldungen und abgesch�tzte Sch�den',sep="")

# title.schadPlot <- 'Schadenmeldungen durch Unwetter vom 24.08.2017'
# title.verlaufPlot <- paste('Unwetter vom 24.08.2017 - Erfasste ',
#                            'Schadenmeldungen und abgesch�tzte Sch�den',sep="")

# title.schadPlot <- paste('Schadenmeldungen durch Sturm vom ',event.date,sep="")
# title.verlaufPlot <- paste('Sturm vom ',event.date,' - Erfasste ',
#                            'Schadenmeldungen und abgesch�tzte Sch�den',sep="")

# title.schadPlot <- 'Schadenmeldungen durch Sturm 01.01.2015-20.08.2017'
# title.verlaufPlot <- paste('Sturm 01.01.2015-20.08.2017 - Erfasste ',
#                            'Schadenmeldungen und abgesch�tzte Sch�den',sep="")
## -------------------------------------------------------------------------------

## Arbeitsverzeichnis definieren
setwd('J:/Naturgefahren/FG2_Datenanalysen/Ereignisanalysen/rData/')

## Funktionen laden
## Funktion fuer die Aufbereitung der Elementarschaeden laden
#### source('J:/Naturgefahren/FG2_Datenanalysen/Statistische Analysen/Rfunctions/f.schadPlot.R')
source('J:/Naturgefahren/FG2_Datenanalysen/Ereignisanalysen/Rfunctions/f.schadPlot - leaflet.R')
source('J:/Naturgefahren/FG2_Datenanalysen/Ereignisanalysen/Rfunctions/f.schadVerlauf.R')

## Schadenfile aus der Access Anwendung "GVZ-Auswertungstool-V2-0.accdb"
dat <- read.csv2(schadfile, stringsAsFactors = FALSE)

# ## Anzahl Schaeden pro Gemeinde und Schaetzkreis
## Fkt. schadPlot = function(schadFile, eventArt, eventStart, eventEnd, graphTitle)
schadGem <- schadPlot(schadfile, event.Art, event.date, date.end, title.schadPlot)

## Schadenentwicklung erfasster und abgeschaetzter Schaeden
## Fkt. schadVerlauf = function(schadFile, eventArt, eventStart, eventEnd)
df <- schadVerlauf(schadfile, event.Art, event.date, date.end, title.verlaufPlot)

###################################





## Tabelle erfasster und abgeschaetzter Schaeden
## mit DT - An R interface to the DataTables library
dt.colnames <- c('Sch�tzkreis', 'Erfasste Schadenmeldungen',
                 'Erfasste Absch�tzungen', 'Offene Sch�den',
                 'Offene Sch�den [%]')

dt.title <- paste('Schadenmeldungen Elementar seit dem ',
                  event.date, sep='')

dt.tbl <- DT::datatable(df, class = 'cell-border stripe',
                        extensions = 'Buttons',
                        options = list(pageLength = dim(df)[1],
                                       dom = 'r'), ## Ohne Filter, suchfunktion etc.
                        caption = htmltools::tags$caption(
                          style = 'caption-side: bottom; text-align: left;',
                          htmltools::em(dt.title)), rownames = FALSE,
                        colnames = dt.colnames) %>% formatPercentage('PendentProzent',0)
dt.tbl
# # save as html
# DT::saveWidget(dt.tbl, 'tbl.schad.html')
