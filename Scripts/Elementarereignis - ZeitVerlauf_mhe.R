## -------------------------------------------------------------------------------
## Schadenstatistik eines Elementarereignisses
##
## Folgende Ausgaben werden erzeugt und zusammen in einm pdf gespeichert:
## - Schadentabelle: Anzahl gemeldeter und erfasster Schäden pro Schätzkreis
## - Zeitlicher Verlauf der gemeldeten und erfassten Schäden
## - Kummulierte Anzahl der gemeldeten und erfassten Schäden
## - Geografische Verteilung der gemeldeten Schäden pro PLZ im Kanton Zürich
##
## Gebäudeversicherung Kanton Zürich, Bereich Naturgefahren
## Mirco Heidemann, 07/2012 - 09/2012
## -------------------------------------------------------------------------------

## EINZULESENDES SCHADENFILE (.CSV):
in.file <- 'A_Schadendatenper08.08.2017_17312.csv'

# Definiere Anfangs- und End-Datum (TT.MM.JJJJ) der Ereignisauswertung
date.start <- '01.08.2017'
event.string <- 'elementar'

## End-Datum der Schadenstatistik.
## ist standardmässig auf den heutigen Tag gesetzt
date.end <- format(Sys.time(), '%d.%m.%Y')
# date.end <- '31.07.2012'

## Bemerkung:
## Das Schadenfile wird aus der Access Anwendung "GVZ-Auswertungstool-V2-0.accdb"
## generiert und standartmässig unter folgendem Pfad gespeichert: 
## "K:\GemDat\Auswertungen\Exporte" 
## -------------------------------------------------------------------------------

## Arbeitsverzeichnis definieren
setwd('J:/Naturgefahren/FG2_Datenanalysen/Ereignisanalysen/rData/')

require(reshape2)
require(ggplot2)
require(gridExtra)
require(grid)
require(gtable)
require(tcltk)

## "Stand-per-Datum" aus dem Schadenfile extrahieren (fuer Grafik)
per.string <- unlist(strsplit(in.file, '\\per'))[2]
per.string <- unlist(strsplit(per.string, '\\_'))[1]
per <- as.Date(per.string, format='%d.%m.%Y')
## zeit für Grafik angeben
time.char <- paste(as.character(format(Sys.time(), '%R')), 'Uhr')

event.date <- date.start
event.date <- as.Date(event.date, '%d.%m.%Y')
event.date.seq <- seq(as.Date(event.date),
                      as.Date(event.date) + 3, by="day")

date.start.gmt <- date.start
date.end.gmt <- date.end
# Konvertiere Anfangs- und End-Datum in ISO-Format
date.start <- as.integer(unlist(strsplit(date.start, '\\.')))
date.start <- ISOdatetime(date.start[3], date.start[2], date.start[1], 0, 0, 0)
date.end <- as.integer(unlist(strsplit(date.end, '\\.')))
date.end <- ISOdatetime(date.end[3], date.end[2], date.end[1], 23, 59, 59)

filename <- paste('pdf/es.', format(date.start, '%Y%m%d'), '.', 'Stand', 
                  format(per, '%y%m%d'), sep = '')

## Einlesen der files:
## Schadenfile:
data <- read.csv2(in.file, stringsAsFactors = FALSE)

## Bei Hagelereignis Sturm und Wasserschaeden auch mitnehmen,
## da Schadentrennung schwierig: Nur Elementarschaeden
if(event.string == 'elementar'){
  ind <- which(data$SchadenArtBezeichnung == 'Elementar')
  data <- data[ind,]
}
## Bei Gewitter auch Blitzschaeden
if(event.string == 'gewitter'){
  ind <- which(data$SchadenArtBezeichnung == 'Elementar' |
                 data$CodTextDt == '61 Blitzschlag')
  data <- data[ind,]
  ind <- which(data$CodTextDt == '.' |
                 data$CodTextDt == '0  unbekannt')
  data <- data[-ind,]
}
## Nur Sturmschaeden
if(event.string == 'sturm'){
  ind <- which(data$CodTextDt == '1 Sturm')
  data <- data[ind,] 
}
## Nur Ueberschwemmungsschaeden
if(event.string == 'ueberschwemmung'){
  ind <- which(data$CodTextDt == '3 Hochwasser, Ueberschwemmung')
  data <- data[ind,] 
}

## plz-, gemeinden-, bezirks- tabellen
csv.string <- 'J:/Naturgefahren/FG2_Datenanalysen/Portfolio gvz/'
ortschaft_plz <- read.csv2(paste(csv.string,'Ortschaft_PLZ.csv',sep=''))
bezirke_gemeinde <- read.csv2(paste(csv.string,'Bezirk_Gemeinde_Liste.csv', sep=''))
bezirke_gemeinde[,3] <- as.character((bezirke_gemeinde[,3]))

# Schadendaten pro Kategorie selectieren und sortieren
data <- data[, c('SchadenId', 'GebaeudeSuchbegriff', 'GebaeudeGemeindeName',
                 'Ausdr1', 'GebaeudePlz', 'SchadenDatum', 'Erstellung', 'EdiAfDat',
                 'CodTextDt', 'SchadenArtBezeichnung', 'SchadenSumme', 'EdiStcDis')]

names(data) <- c('SchadenID', 'gebNr', 'Gemeinde', 'GemNr', 'PLZ',
                 'EreignisDat', 'ErfassDatGemDat', 'SchaetzungsDat',
                 'SchadenUrsache', 'SchadenArt','SchadenSumme', 'Abschaetz.Status' )

## Zahlen und Datumsfelder umwandeln
data$SchadenID <- as.integer(data$SchadenID)
data$GemNr <- as.integer(data$GemNr)
data$PLZ<- as.integer(data$PLZ)
data$SchadenSumme<- as.numeric(data$SchadenSumme)
data$EreignisDat <- as.Date(data$EreignisDat, format='%d.%m.%Y')
data$ErfassDatGemDat <- as.Date(data$ErfassDatGemDat, format='%d.%m.%Y')
data$SchaetzungsDat <- as.Date(data$SchaetzungsDat, format='%d.%m.%Y')
data$Abschaetz.Status <- as.character(data$Abschaetz.Status)

# Anzahl erledigte (erfasste) / pendente Schadenmeldungen ermitteln
data$SchadGesch[data$Abschaetz.Status == "DIS       RAP"]<-as.numeric(1)
data$SchadGesch[(data$Abschaetz.Status == "DIS       ERL")]<-as.numeric(1)
data$SchadOffen[(data$Abschaetz.Status == "DIS       DIS")]<-as.numeric(1)
data$SchadOffen[is.na(data$SchadOffen)]<-0
data$SchadGesch[is.na(data$SchadGesch)]<-0

# Daten wählen innerhalb der gewünschten Zeit --> date.start, date.end
ind.dat <- which(format(data$EreignisDat, '%Y%m%d') >= format(date.start, '%Y%m%d')
                 & format(data$EreignisDat, '%Y%m%d') <= format(date.end, '%Y%m%d'))
if (length(ind.dat) > 0) data <- data[ind.dat,]

# Anzahl Schäden, sowie Status in data summieren
unique.GemNr <- sort(unique(data$GemNr))
anzahl <- GemNr <- pendent <- erledigt <- pendent.prozent <- 
  erledigt.prozent <- schadenSumme <- numeric(length(unique.GemNr))

for (g in 1:length(unique.GemNr)) {
  ind <- which(data$GemNr == unique.GemNr[g])
  GemNr[g] <- as.character(data$GemNr[ind[1]])
  anzahl[g] <- length(ind)
  pendent[g] <- sum(data$SchadOffen[ind])
  erledigt[g] <- sum(data$SchadGesch[ind])
  schadenSumme[g] <- sum(as.numeric(data$SchadenSumme[ind]))
}

summarized.data <- data.frame(GemNr = unique.GemNr,
                              AnzahlSchaeden = anzahl,
                              Erledigt = erledigt,
                              ErledigtProzent = erledigt.prozent,
                              Pendent = pendent,
                              PendentProzent = pendent.prozent,
                              SchadenSumme = schadenSumme,
                              stringsAsFactors = FALSE)

## -------------------------------------------------------------------------------
## Schadentabelle
## -------------------------------------------------------------------------------
# Summierte Tabelle den Schätzkreisen zuordnen
m = match(summarized.data$GemNr, bezirke_gemeinde$GemeindeNr)
summarized.data$Schaetzkreis <- bezirke_gemeinde$Schaetzkreis[m]

## Anzahl gemeldeter und erfasster Schäden pro Schätzkreis:
## Summierte Tabelle pro GemeindeNr aggregieren (--> Pivot Prozedere),
## inkl. "Total-Zeile" ('margins')
melt.dat <- melt(summarized.data,
                 measure = c('AnzahlSchaeden', 'Erledigt', 'ErledigtProzent', 
                                              'Pendent', 'PendentProzent'))
aggr.data.anzahl <- dcast(melt.dat, Schaetzkreis ~ variable, sum,
                          margins="Schaetzkreis")
aggr.data.anzahl$ErledigtProzent <- 
  round(aggr.data.anzahl$Erledigt/aggr.data.anzahl$AnzahlSchaeden*100)
aggr.data.anzahl$PendentProzent <- 100-aggr.data.anzahl$ErledigtProzent
names(aggr.data.anzahl) <- c('Schätzkreis', 'Erfasste Schäden',
                             'Erfasste Abschätzungen',
                             'Erfasste Abschätzungen [%]', 'Offene Schäden',
                             'Offene Schäden [%]')

## -------------------------------------------------------------------------------
## Anzahl Schäden-Tabelle als Grafik plotten
## -------------------------------------------------------------------------------
# pivot.anzahl <- tableGrob(aggr.data.anzahl, row.just = "center",
#                           col.just = "center", core.just = "right",
#                           separator = "white", show.box = TRUE,
#                           show.vlines = TRUE, show.hlines = TRUE,
#                           show.namesep = FALSE, show.csep = FALSE,
#                           show.rsep = TRUE, equal.width = FALSE,
#                           equal.height = FALSE, padding.h = unit(4, "mm"),
#                           padding.v = unit(4, "mm"), gp = NULL,
#                           # Zellen Schrift:
#                           gpar.coretext = gpar(col = "black", cex = 1),
#                           # Spalten-Titel Schrift:
#                           gpar.coltext = gpar(col = "black",
#                                               cex = 1, fontface = "bold"),
#                           gpar.rowtext = gpar(col = "transparent",
#                                               cex = 1, fontface = "italic"),
#                    # Zeilen Schrift (links aussen, wenn vorhanden)
#                    h.odd.alpha = 1, h.even.alpha = 1, v.odd.alpha = 1,
#                           v.even.alpha = 1,
#                    # Zellen Füllfarbe:
#                    gpar.corefill = gpar(fill = "grey95",
#                                         col = "white"),
#                    # Zellen Füllfarbe (links aussen)
#                    gpar.rowfill = gpar(fill = "transparent",
#                                        col = "white"),
#                    # Spalten-Titel Zell-Füllfarbe
#                    gpar.colfill = gpar(fill = "slategray3",
#                                        col = "white"),
#                    theme = NULL)

# pivot.anzahl <- grid.table(aggr.data.anzahl, theme = ttheme_minimal())

tb <- tableGrob(aggr.data.anzahl, rows = NULL)
tb <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
tb <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
#grid.draw(tb)

# grid.newpage()
# h <- grobHeight(g)
# w <- grobWidth(g)

tbl.footnote <- "GVZ, Bereich Naturgefahren\n  Mirco Heidemann"
tbl.title <- textGrob(paste('Schadenmeldungen Elementar seit dem ',
                            format(date.start, '%d.%m.%Y'), sep=''),
                      gp=gpar(fontsize=20))
padding <- unit(5,"mm")
table <- gtable_add_rows(tb, heights = grobHeight(tbl.title) + padding, pos = 0)
table <- gtable_add_grob(table, tbl.title, 1, 1, 1, ncol(table))
grid.newpage()
grid.draw(table)

g <- arrangeGrob(table, bottom = textGrob(footnote, x = 0, hjust = -0.1, vjust=0.1, gp = gpar(fontface = "italic", fontsize = 12)))
grid.draw(g)


title <- textGrob(paste('Schadenmeldungen Elementar seit dem ',
                        format(date.start, '%d.%m.%Y'), sep=''),gp=gpar(fontsize=20))
footnote <- textGrob("GVZ, Bereich Naturgefahren\nMirco Heidemann",
                     x=unit(0.5,"npc") - 0.665*w,
                     y=unit(0.5,"npc") - 0.9*h,
                     vjust=1, hjust=0,gp=gpar( fontface="italic", fontsize=8))
subtitle <- textGrob(paste("GemDat:",
                           per.string),
                     x=unit(0.5,"npc") - 0.468*w,
                     y=unit(0.5,"npc") + 0.585*h,
                     vjust=1, hjust=0,gp=gpar( fontface="italic"))
pivot.picture.anzahl <- gTree(children=gList(g, title,
                                             footnote, subtitle))
grid.draw(pivot.picture.anzahl)

# # ... und als Bild abspeichern
# filename.plot.pdf <- paste(filename, '.pdf', sep = "")
# pdf(file=paste(filename.plot.pdf), width=20, heigh=10, paper= "a4r")
# grid.draw(pivot.picture.anzahl)
# dev.off()

## Schadensumme pro Schätzkreis
## Summierte Tabelle pro GemeindeNr aggregieren (--> Pivot Prozedere),
## inkl. "Total-Zeile" ('margins')
melt.dat.schadensumme <- melt(summarized.data, measure = c('SchadenSumme'))
aggr.data.schadensumme <- dcast(melt.dat.schadensumme, Schaetzkreis ~ 
  variable, sum, margins="Schaetzkreis")
names(aggr.data.schadensumme) <- c('Schätzkreis', 'Schadensumme')

## -------------------------------------------------------------------------------
# Schadensumme-Tabelle als Grafik plotten
## -------------------------------------------------------------------------------
# pivot.schadensumme <- tableGrob(aggr.data.schadensumme, row.just = "center",
#                                 col.just = "center", core.just = "right",
#                                 separator = "white", show.box = TRUE, show.vlines = TRUE,
#                                 show.hlines = TRUE,
#                                 show.namesep = FALSE, show.csep = FALSE, show.rsep = TRUE,
#                                 equal.width = FALSE,
#                                 equal.height = FALSE, padding.h = unit(4, "mm"),
#                                 padding.v = unit(4, "mm"), gp = NULL,
#                                 ## Zellen Schrift
#                                 gpar.coretext = gpar(col = "black", cex = 1),
#                                 ## Spalten-Titel Schrift
#                                 gpar.coltext = gpar(col = "black", cex = 1,
#                                                     fontface = "bold"),
#                                 gpar.rowtext = gpar(col = "transparent", cex = 1,
#                                                     fontface = "italic"),
#                                 ## Zeilen Schrift (links aussen, wenn vorhanden)
#                                 h.odd.alpha = 1, h.even.alpha = 1, v.odd.alpha = 1,
#                                 v.even.alpha = 1,
#                                 #3 Zellen Füllfarbe
#                                 gpar.corefill = gpar(fill = "grey95", col = "white"),
#                                 ## Zellen Füllfarbe (links aussen)
#                                 gpar.rowfill = gpar(fill = "transparent", col = "white"),
#                                 ## Spalten-Titel Zell-Füllfarbe
#                                 gpar.colfill = gpar(fill = "slategray3", col = "white"),
#                                 theme = NULL)

g <- tableGrob(aggr.data.schadensumme, rows = NULL)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
grid.draw(g)

grid.newpage() 
h <- grobHeight(g) 
w <- grobWidth(g)

title.schadensumme <- 
  textGrob(paste('Schadensummen Elementar seit dem ',
                 format(date.start, '%d.%m.%Y'), sep=''),
           x=unit(0.5,"npc") - 0.276*w,
           y=unit(0.5,"npc") + 0.48*h,  
           vjust=0, gp=gpar(fontsize=20))
pivot.picture.schadensumme <- gTree(children=gList(g,
                                                   title.schadensumme,
                                                   footnote, subtitle))

grid.draw(pivot.picture.schadensumme)

## -------------------------------------------------------------------------------
## Zeitlicher Verlauf der gemeldeten und erfassten Schäden.
## Schäden pro Woche zusammengefasst
## -------------------------------------------------------------------------------

# # Einlesen des Schadenmeldungs-Datum - Falsche Daten aus dem GemDat
# ## werden nicht berücksichtigt
# ind <- which(format(data$ErfassDatGemDat,'%Y%m%d') >= 
#                format(date.start,'%Y%m%d') &
#                format(data$ErfassDatGemDat,'%Y%m%d') <= 
#                format(date.end,'%Y%m%d'))
# schadenmeldungen.datum <- data[ind,]
# 
# # schadenmeldungen.woche <- format(schadenmeldungen.datum[c(6)], '%Y%W')
# # schadenmeldungen.pro.woche <- 
# #   as.matrix(as.data.frame(table(schadenmeldungen.woche[,1])))
# 
# # Einlesen des Schadenschätzungs-Datum, aber nur jene mit einem Schätzungsdatum
# # Falsche Daten aus dem GemDat werden nicht berücksichtigt
# ind <- which(format(data$SchaetzungsDat,'%Y%m%d') > 
#                format(date.start,'%Y%m%d') & 
#                format(data$SchaetzungsDat,'%Y%m%d') <= 
#                format(date.end,'%Y%m%d'))
# schaetzungen.woche <- data[ind,]
# schaetzungen.woche <- format(schaetzungen.woche[c(7)], '%Y%W')
# schaetzungen.pro.woche <- as.matrix(as.data.frame(table(schaetzungen.woche[,1])))
# 
# ## Konvertiere Anfangs- und End-Datum in ISO-Format der Zeitzone GMT.
# ## Damit kein Fehler mit derUmstellung von Sommer- auf Winter Zeit.
# date.start.gmt <- as.integer(unlist(strsplit(date.start.gmt, '\\.')))
# date.start.gmt <- ISOdatetime(date.start.gmt[3], date.start.gmt[2],
#                               date.start.gmt[1], 0, 0, 0, tz="GMT")
# date.end.gmt <- as.integer(unlist(strsplit(date.end.gmt, '\\.')))
# date.end.gmt <- ISOdatetime(date.end.gmt[3], date.end.gmt[2],
#                             date.end.gmt[1], 23, 59, 59, tz="GMT")
# 
# s <- seq(date.start.gmt, date.end.gmt, by = "day")
# ## Jeder Woch wird ein Tag zugeordnet. Wochenwerte pro Montag (=="1").
# ## D.h. der WochenANFANG wird mit dem Montag raepresentiert.
# serie.woche<-data.frame(day.montag=s[format(s, "%w") == "0"],
#                         week.val=as.integer(format(s[format(s, "%w") == "0"],
#                                                    "%Y%W")))
# wochenNr<- as.numeric(format(serie.woche$day.montag, '%Y%W'))
# wochenNr.date<-serie.woche$day.montag
# ## Für grafische Beschriftung das Ende der Woche dartstellen:
# ## Wochenanfangsdatum + 6
# tag.label <- as.Date(wochenNr.date, format="%d.%m.%Y") +6 
# 
# ## Matrix zusammenbastel, auch wenn unterschiedliche Anzahl an Wochen
# ## mit Schadenmeldungen und Wochen mit Schätzungen:
# ## Wenn in einer Woche Schadenmeldungen vorhanden sind, aber keine 
# ## Abschätzungen, soll "schaetzungen.pro.woche" in dieser Wochen den
# ## Wert "0" erhalten. Liegen in einer Woche Abschätzungen, aber keine
# ## Schadenmeldungen vor, soll in dieser Woche bei 
# ## "schadenmeldungen.pro.woche" der Wert "0" eingetragen werden.
# 
# i.schadenmeldung = match(wochenNr, schadenmeldungen.pro.woche)
# i.schaetzungen = match(wochenNr, schaetzungen.pro.woche)
# schadenmeldungen.pro.woche <- 
#   as.numeric(schadenmeldungen.pro.woche[i.schadenmeldung, 2])
# schaetzungen.pro.woche <- 
#   as.numeric(schaetzungen.pro.woche[i.schaetzungen, 2])
# 
# matrix.time <- cbind(wochenNr, schadenmeldungen.pro.woche,
#                      schaetzungen.pro.woche)
# matrix.time[is.na(matrix.time)]<-0
# schaetzungen.pro.woche[is.na(schaetzungen.pro.woche)]<-0
# schadenmeldungen.pro.woche[is.na(schadenmeldungen.pro.woche)]<-0
# 
# df.time <- data.frame(wochenNr.date, matrix.time[,2], matrix.time[,3])
# names(df.time) <- c('Woche','Schadenmeldung', 'Schaetzung')
# 
# df.time$Schadenmeldung <- as.numeric(df.time$Schadenmeldung)
# df.time$Schaetzung <- as.numeric(df.time$Schaetzung)

## Erfasste -und abgeschaetze Schaeden pro Tag
tbl.schad.erfasst <- as.data.frame(table(data$ErfassDatGemDat))
names(tbl.schad.erfasst) <- c('tag','anz')
tbl.schad.erfasst$tag <- as.Date(tbl.schad.erfasst$tag)
tbl.schad.geschaetz <- as.data.frame(table(data$SchaetzungsDat))
names(tbl.schad.geschaetz) <- c('tag','anz')
tbl.schad.geschaetz$tag <- as.Date(tbl.schad.geschaetz$tag)
min.tag <- min(min(tbl.schad.erfasst$tag), min(tbl.schad.geschaetz$tag))
max.tag <- max(max(tbl.schad.erfasst$tag), max(tbl.schad.geschaetz$tag))
s.tag <- seq(min.tag-1, max.tag, by = "day")
df.time <- as.data.frame(s.tag)

## in gemdat erfasste schaeden pro tag
i <- match(s.tag, tbl.schad.erfasst$tag)
df.time$erfasst <- as.numeric(tbl.schad.erfasst$anz[i])
## abgeschaetzte schaeden pro tag
i <- match(s.tag, tbl.schad.geschaetz$tag)
df.time$gesch <- as.numeric(tbl.schad.geschaetz$anz[i])
names(df.time) <- c('tag', 'erfasst', 'geschaetzt')
df.time[is.na(df.time)] <- 0

## -------------------------------------------------------------------------------
## Kummulierte Anzahl der gemeldeten und erfassten Schäden
## -------------------------------------------------------------------------------
# # i<-as.numeric(length(schadenmeldungen.pro.woche))
# i<-length(wochenNr)
# schadenmeldungen.pro.woche.kumm <- as.numeric(schadenmeldungen.pro.woche)
# 
# for (g in 1:i) {
#   if (g == 1){schadenmeldungen.pro.woche.kumm[g] <- 
#     schadenmeldungen.pro.woche.kumm[g]}
#   if (g>1){schadenmeldungen.pro.woche.kumm[g] <- 
#     (schadenmeldungen.pro.woche.kumm[g] + 
#     schadenmeldungen.pro.woche.kumm[(g-1)])
#             if(schadenmeldungen.pro.woche.kumm[g]==0){
#               schadenmeldungen.pro.woche.kumm[g]<-
#                 schadenmeldungen.pro.woche.kumm[(g-1)]}}
# }
# 
# # i<-as.numeric(length(schaetzungen.pro.woche))
# schaetzungen.pro.woche.kumm <- as.numeric(schaetzungen.pro.woche)
# 
# for (g in 1:i) {
#   if (g == 1){schaetzungen.pro.woche.kumm[g] <- schaetzungen.pro.woche.kumm[g]}
#   if (g>1){schaetzungen.pro.woche.kumm[g] <- (schaetzungen.pro.woche.kumm[g] + 
#     schaetzungen.pro.woche.kumm[(g-1)])
#            if(schaetzungen.pro.woche.kumm[g]==0){
#              schaetzungen.pro.woche.kumm[g]<-
#                schaetzungen.pro.woche.kumm[(g-1)]}}
# }
# 
# df.kumm <- data.frame(wochenNr.date, schadenmeldungen.pro.woche.kumm, 
#                       schaetzungen.pro.woche.kumm)
# names(df.kumm) <- c('Woche','Kumm.Schadenmeldung', 'Kumm.Schaetzung')
# df.kumm$Kumm.Schadenmeldung <- as.numeric(df.kumm$Kumm.Schadenmeldung)
# df.kumm$Kumm.Schaetzung <- as.numeric(df.kumm$Kumm.Schaetzung)

t <- cumsum(df.time[,c(2:3)])
df.time$kum.erfasst <- t[,1]
df.time$kum.geschaetzt <- t[,2]

## -------------------------------------------------------------------------------
## Plots (ggplot2) der Schadenentwicklung
## -------------------------------------------------------------------------------
ggtheme = theme_bw(base_size = 12) +
  theme(legend.position = c(0.93, 0.8),
        legend.background = element_rect(colour = NA, fill = NA),
        plot.margin=unit(c(1,1,1,1),"mm")) #("left", "right", "bottom", "top")) +
  #+ theme(axis.text.x  = element_text(angle=45, vjust=1,hjust=1))

footnote <- "GVZ, Bereich Naturgefahren\n  Mirco Heidemann"

## 1. Plot kummulierte Anzahl
dfm <- melt(df.time[,-c(2:3)], id="tag")
line.plot <- 
  ggplot(data=dfm, aes(x=tag, y=value, colour=variable)) +
  geom_line(size = 0.5, alpha = 1) +
  geom_point(size=2) +
  scale_colour_manual(name = '',
                      labels=c("Erfasst","Abgeschätzt"),
                      values=c("#fc8d59", "#99d594")) +

  ggtitle(paste('Entwicklung der erfassten Schadenmeldungen und abgeschätzten Schäden\n',
                'GemDat: ', per.string, ', ',time.char, sep='')) +
  ylab("Anzahl Schäden") + xlab("") + 
  scale_x_date(breaks = seq(from=min(df.time$tag), to=max(df.time$tag), by=1),
               date_labels = "%d.%m.%y") +
  ggtheme

# Fussnote / Text hinzufügen
grid.newpage()
p.line <- arrangeGrob(line.plot,
                      bottom = textGrob(footnote,
                                        x = 0, hjust = -0.1, vjust=0.1,
                                        gp = gpar(fontface = "italic",
                                                  fontsize = 8)))
# grid.draw(p.line)

## 2. Verlauf erfasster Schaeden und Abschaetzungen
dfm.bar <- melt(df.time[,-c(4:5)], id="tag")

barplot <- 
  ggplot(dfm.bar,aes(x = tag,y = value)) + 
  geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
  scale_fill_manual(name = '',
                     labels=c("Erfasst","Abgeschätzt"),
                     values=c("#fc8d59", "#99d594")) +
  ylab("Anzahl Schäden") + xlab("") +
  ggtitle(paste('Erfasste Schadenmeldungen und abgeschätzte Schäden pro Tag\n',
                'GemDat: ', per.string, ', ',time.char, sep='')) +
  scale_x_date(breaks = seq(from=min(df.time$tag), to=max(df.time$tag), by=1),
               date_labels = "%d.%m.%y") +
  ggtheme

# Fussnote / Text hinzufügen
grid.newpage()
p.bar <- arrangeGrob(barplot,
                 bottom = textGrob(footnote,
                                   x = 0, hjust = -0.1, vjust=0.1,
                                   gp = gpar(fontface = "italic",
                                             fontsize = 8)))
# grid.draw(p.bar)

## Kombinierter Plot
kombi.plot <- 
  ggplot(dfm.bar,aes(x = tag,y = value)) + 
  geom_bar(aes(fill = variable), position = "dodge", stat="identity", alpha = 0.5) +
  scale_fill_manual(name = '',
                    labels=c("Erfasst","Abgeschätzt"),
                    values=c("#fc8d59", "#99d594")) +
  
  geom_line(data=dfm, aes(x=tag, y=value, colour=variable), size=0.8) +
  geom_point(data=dfm, aes(x=tag, y=value, colour=variable), size=2) +
  scale_colour_manual(name = '',
                      labels=c("Erfasst","Abgeschätzt"),
                      values=c("#fc8d59", "#99d594")) +
  
  ylab("Anzahl Schäden") + xlab("") +
  ggtitle(paste('Entwicklung der erfassten Schadenmeldungen und abgeschätzten Schäden\n',
                'GemDat: ', per.string, ', ',time.char, sep='')) +
  scale_x_date(expand=c(0,0), breaks = seq(from=min(df.time$tag),
                                         to=max(df.time$tag), by=1),
               date_labels = "%d.%m.%y") +
  ggtheme

# Fussnote / Text hinzufügen
grid.newpage()
p.kombi <- arrangeGrob(kombi.plot,
                     bottom = textGrob(footnote,
                                       x = 0, hjust = -0.1, vjust=0.1,
                                       gp = gpar(fontface = "italic",
                                                 fontsize = 8)))
grid.draw(p.kombi)
## -------------------------------------------------------------------------------
## Abspeichern der Ausgaben in einem gemeinsamen .pdf - File
## -------------------------------------------------------------------------------
# save.path <- 
#   paste('J:/Naturgefahren/Datenanalysen/Ereignisanalysen/', 
#                    sep = "", collapse = NULL)
# 1. und 2. Seite: Tabellen
# pdf(file=filename.plot.pdf, width=20, heigh=10, paper= "a4")
filename.plot.pdf <- paste(filename, '.pdf', sep = "")
pdf(file=paste(save.path, filename.plot.pdf), width=20, 
    heigh=10, paper= "a4r")
#grid.draw(pivot.picture.anzahl)
grid.arrange(pivot.picture.anzahl) 
grid.arrange(pivot.picture.schadensumme)
#dev.off()
# 3. Seite: time.plot
print(t.plot)
# 4. Seite: time.plot
print(k.plot)
# 5. und 6. Seite: Map
# pdf(file=filename.plot.pdf, width=20, heigh=10, paper= "a4")
# print(map.anzahl)
# print(map.schadensumme)
dev.off()

## -------------------------------------------------------------------------------
## Zusätzliche Ausgabe der Anzahl-Schäden-Tabelle in ein .txt File
## -------------------------------------------------------------------------------
write.table(aggr.data.anzahl, paste(save.path, filename, '.txt',
                                    sep = "", collapse = NULL),
            sep='\t', col.names=NA)

# ## -------------------------------------------------------------------------------
# ## Abspeichern des Plots als .svg - File (Scalar Vector Graphics)
# ## -------------------------------------------------------------------------------
# # Titelgrösse Anpassen für die .svg Grafik
# tmp.map <- spplot(Kanton, zcol = "Schaden", main = list(Haupttitel, cex=1.2, font = 1.5, col="black"), 
#                   sub = list(Untertitel, cex=0.9, font = 0.9, col="black"),
#                   col.regions = br.palette(100), col = "grey40", 
#                   par.settings = list(axis.line = list(col =  NA)),      
#                   panel = function(...) {
#                     panel.polygonsplot(...) 
#                     sp.polygons(Seen, col = "lightblue", fill = "lightblue")
#                     sp.polygons(Bezirksgrenzen, col = "black", lwd = 2)
#                     sp.text(c(677200, 225000), 'GG25©swisstopo', col = "grey40", cex = 0.6)
#                   })
# 
# filename.plot.svg <- paste(filename, '.svg', sep = '')
# svg(file=paste(save.path, filename.plot.svg))
# print(tmp.map)
# dev.off()

# Message Box zur Info
tk_messageBox(type = "ok", paste('Folgende Files wurden erstellt: \n', '- ',
                                 filename.plot.pdf, '\n- ',
                                 paste(filename, '.txt', sep = ""), '\n',
                                 '\n... und unter folgendem Pfad abgelegt: \n',
                                 save.path,
                                 sep = ''), 
              caption = "Schadenstatistik erstellt", default = "")
