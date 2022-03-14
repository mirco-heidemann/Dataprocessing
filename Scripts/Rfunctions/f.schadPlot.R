## ---------------------------------------------------------------------
## Schadenschaetzung nach Ereignis pro Gemeinde darstellen
##
## Mirco Heidemann, 04/2015
## ---------------------------------------------------------------------

schadPlot = function(schadFile, eventArt, eventStart, eventEnd, graphTitle) {
  
  eventStart <- as.Date(eventStart, '%d.%m.%Y')
  eventEnd <- as.Date(eventEnd, '%d.%m.%Y')
  
  ## Schadenfile aus der Access Anwendung "GVZ-Auswertungstool-V2-0.accdb"
  data <- read.csv2(schadFile, stringsAsFactors = FALSE)
  ## "Stand-per-Datum" aus dem Schadenfile extrahieren (fuer Grafik)
  per.string <- unlist(strsplit(schadFile, '\\per'))[2]
  per.string <- unlist(strsplit(per.string, '\\_'))[1]
  per <- as.Date(per.string, format='%d.%m.%Y')
  ## zeit für Grafik angeben
  time.char <- paste(as.character(format(Sys.time(), '%R')), 'Uhr')
  
  ## Bei Hagelereignis Sturm und Wasserschaeden auch mitnehmen,
  ## da Schadentrennung schwierig: Nur Elementarschaeden
  if(eventArt == 'elementar'){
    ind <- which(data$SchadenArtBezeichnung == 'Elementar')
    data <- data[ind,]
  }
  ## Bei Gewitter auch Blitzschaeden
  if(eventArt == 'gewitter'){
    ind <- which(data$SchadenArtBezeichnung == 'Elementar' |
                   data$CodTextDt == '61 Blitzschlag')
    data <- data[ind,]
    ind <- which(data$CodTextDt == '.' |
                   data$CodTextDt == '0  unbekannt')
    data <- data[-ind,]
  }
  ## Nur Sturmschaeden
  if(eventArt == 'sturm'){
    ind <- which(data$CodTextDt == '1 Sturm')
    data <- data[ind,] 
  }
  ## Nur Ueberschwemmungsschaeden
  if(eventArt == 'ueberschwemmung'){
    ind <- which(data$CodTextDt == '3 Hochwasser, Ueberschwemmung')
    data <- data[ind,] 
  }
  
  ## Daten pro Kategorie selectieren, sortieren und aufbereiten
  data$zwCode <- as.integer(substr(data$GebaeudeZweckCode, 1,1))
  
  data <- data[, c('GebaeudeSuchbegriff', 'Ausdr1', 'GebaeudeGemeindeName',
                   'SchadenSumme', 'SchadenDatum','SchadenVersicherungWert','zwCode')]
  names(data) <- c('gebNr', 'gemID', 'gemeinde', 'schadSum', 'schadDat', 'versSum','zwCode')
  data$schadSum <- as.numeric(data$schadSum)
  data$versSum <- as.numeric(data$versSum)
  data$schadDat <- as.Date(data$schadDat, '%d.%m.%Y')
  
  ## tabelle schaetzkreise einlesen
  file.in <- 'J:/Naturgefahren/FG2_Datenanalysen/Portfolio gvz/Bezirk_Gemeinde_Liste.csv'
  tbl.schaetzkr <- read.csv2(file.in)
  # Gemeinden den Schaetzkreisen zuordnen
  m = match(data$gemID, tbl.schaetzkr$GemeindeNr)
  data$schaetzkrID <- tbl.schaetzkr$SchaetzkrID[m]
  data$schaetzkreis <- tbl.schaetzkr$Schaetzkreis[m]
  
  ## remove some data frames
  rm(tbl.schaetzkr)
  
  # # Ohne Nuller-Schaeden (ablehnung, falsch erfasst)
  # ind <- which(data$schadSum > 0)
  # if (length(ind) > 0) data <- data[ind,]
  
  ## Shapefiles einlesen
  library(maptools)
  #### shape.path <- 'D:/Users/mheidemann/GIS/Grunddaten_KantonZH/'
  shape.path <- 'J:/Naturgefahren/FG2_Datenanalysen/Daten-Grundlagen GIS/Grunddaten_KantonZH/'
  sh.gemeinde <- readShapeSpatial(paste(shape.path,'gemeinden.2016.shp',sep=""))
  sh.seen <-readShapeSpatial(paste(shape.path,'seendet_250.shp',sep=""))
  sh.fluss <-readShapeSpatial(paste(shape.path,'Fluesse_gross.shp',sep=""))
  ## Vermessungsbezirke der Stadt Zuerich lesen
  # zhwinti.umriss <- readShapeSpatial(paste(shape.path,
  #                                          'Vermessungsbezirke_STZH_Winti_Umriss',
  #                                          sep=""))
  sh.bezirke <- readShapeSpatial(paste(shape.path, 'Bezirke', sep=""))
  sh.schaetzkreis <- readShapeSpatial(paste(shape.path, 'gvz_schaetzkreise', sep=""))
  
  ## min-max coordinates
  xmin <- sh.gemeinde@bbox[1,1]
  xmax <- sh.gemeinde@bbox[1,2]
  ymin <- sh.gemeinde@bbox[2,1]
  ymax <- sh.gemeinde@bbox[2,2]
  
  ## ymax so anpassen, dass es zum Verhaeltnis von A4-Papier passt
  ymax <- ymin + (29.7 * (xmax - xmin) / 21)
  
  txt.x <- xmin + 0.02 * (xmax - xmin)
  txt.y <- ymin + 0.98 * (ymax - ymin)
  # txt.y2 <- ymin + 0.01 * (ymax - ymin)
  txt.y2 <- ymin - 0.1 * (ymax - ymin)
  
  ## define colors for data
  cols.YlOrRd <- c('#ffffd4', '#fed976', '#fd8d3c',
                   '#e31a1c', '#bd0026', '#67000d')
  
  ## definiere Funktion zum Plotten per GEMEINDE
  plot.gem <- function(variable = variable, breaks = breaks, dat) {
    ## grenze zeichnen, damit nichts abgeschnitten ist
    plot(sh.bezirke, lwd = 1)
    for (i in 2:length(breaks)) {
      ind <- which(dat[,variable] > breaks[i-1] &
                     dat[,variable] <= breaks[i])
      if (length(ind) > 0) {
        ind.plot <- is.finite(match(sh.gemeinde@data$BFS, dat$GemNr[ind]))
        if (sum(ind.plot) > 0) {
          poly.plot <- subset(sh.gemeinde, ind.plot)
          plot(poly.plot, add = TRUE, col = cols.YlOrRd[i-1], lwd = 0.5)
        }
      }
    }
    ## Gemeinden des Kantons ZH
    plot(sh.gemeinde, border='gray80', lwd=0.5, add = TRUE)
    ## Bezirke des Kantons ZH
    plot(sh.bezirke, lwd = 0.5, add = TRUE)
    ## Seen plotten
    plot(sh.seen, col = '#b9cfdd', border='#b9cfdd', add = TRUE)
    plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
  }
  
  ## definiere Funktion zum Plotten per SCHAETZKREIS
  plot.schaetzkr <- function(variable = variable, breaks = breaks, dat) {
    ## grenze zeichnen, damit nichts abgeschnitten ist
    plot(sh.schaetzkreis, lwd = 1)
    for (i in 2:length(breaks)) {
      ind <- which(dat[,variable] > breaks[i-1] &
                     dat[,variable] <= breaks[i])
      if (length(ind) > 0) {
        ind.plot <- is.finite(match(sh.schaetzkreis@data$schatzkrID,
                                    dat$SchaetzkrID[ind]))
        if (sum(ind.plot) > 0) {
          poly.plot <- subset(sh.schaetzkreis, ind.plot)
          plot(poly.plot, add = TRUE, col = cols.YlOrRd[i-1], lwd = 0.5)
        }
      }
    }
    ## Seen plotten
    plot(sh.seen, col = '#b9cfdd', border='#b9cfdd', add = TRUE)
    plot(sh.fluss, col = '#b9cfdd', border= '#b9cfdd', add = TRUE)
  }
  
  # Daten wählen innerhalb der gewünschten Zeit --> date.start, date.end
  ind.dat <- which(format(data$schadDat, '%Y%m%d') >= format(eventStart, '%Y%m%d')
                   & format(data$schadDat, '%Y%m%d') <= format(eventEnd, '%Y%m%d'))
  if (length(ind.dat) > 0) data.event <- data[ind.dat,]
  
  ## summarize data per gemeinde
  unique.gemID <- sort(unique(data.event$gemID))
  anzahl <- summe <- durchschn <- median <- schadensatzE <-
    schadensatzTot <- numeric(length(unique.gemID))
  gemeinde <- character(length(unique.gemID))
  for (g in 1:length(unique.gemID)) {
    ind <- which(data.event$gemID == unique.gemID[g])
    gemeinde[g] <- as.character(data.event$gemeinde[ind[1]])
    anzahl[g] <- length(ind)
    summe[g] <- sum(data.event$schadSum[ind])
    durchschn[g] <- mean(data.event$schadSum[ind])
    median[g] <- median(data.event$schadSum[ind])
    ind.VS <- which(data.event$versSum[ind] > 0)
    schadensatzE[g] <- mean(data.event$schadSum[ind[ind.VS]] /
                              data.event$versSum[ind[ind.VS]])
    schadensatzTot[g] <- sum(data.event$schadSum[ind]) / sum(data.event$versSum[ind])
  }
  
  data.gem <- data.frame(GemNr = unique.gemID,
                         Gemeinde = gemeinde,
                         AnzahlSchaeden = anzahl,
                         Schadensumme = summe,
                         Durchschnitt = durchschn,
                         Median = median,
                         SchadensatzE = schadensatzE,
                         SchadensatzTot = schadensatzTot,
                         stringsAsFactors = FALSE)
  
  ## summarize data per schaetzkreis
  unique.schaetzkrID <- sort(unique(data.event$schaetzkrID))
  anzahl <- summe <- durchschn <- median <- schadensatzE <-
    schadensatzTot <- numeric(length(unique.schaetzkrID))
  schaetzkr <- character(length(unique.schaetzkrID))
  for (g in 1:length(unique.schaetzkrID)) {
    ind <- which(data.event$schaetzkrID == unique.schaetzkrID[g])
    schaetzkr[g] <- as.character(data.event$schaetzkreis[ind[1]])
    anzahl[g] <- length(ind)
    summe[g] <- sum(data.event$schadSum[ind])
    durchschn[g] <- mean(data.event$schadSum[ind])
    median[g] <- median(data.event$schadSum[ind])
    ind.VS <- which(data.event$versSum[ind] > 0)
    schadensatzE[g] <- mean(data.event$schadSum[ind[ind.VS]] /
                              data.event$versSum[ind[ind.VS]])
    schadensatzTot[g] <- sum(data.event$schadSum[ind]) / sum(data.event$versSum[ind])
  }
  
  data.schaetzkr <- data.frame(SchaetzkrID = unique.schaetzkrID,
                               Schaetzkreis = schaetzkr,
                               AnzahlSchaeden = anzahl,
                               Schadensumme = summe,
                               Durchschnitt = durchschn,
                               Median = median,
                               SchadensatzE = schadensatzE,
                               SchadensatzTot = schadensatzTot,
                               stringsAsFactors = FALSE)
  
  ## summarize data per zwCode (fuer VKF)
  unique.zwCode <- sort(unique(data.event$zwCode))
  anzahl <- summe <- durchschn <- median <- maxSchad <- schadensatzE <-
    schadensatzTot <- numeric(length(unique.zwCode))
  for (g in 1:length(unique.zwCode)) {
    ind <- which(data.event$zwCode == unique.zwCode[g])
    anzahl[g] <- length(ind)
    summe[g] <- sum(data.event$schadSum[ind])
    durchschn[g] <- mean(data.event$schadSum[ind])
    median[g] <- median(data.event$schadSum[ind])
    maxSchad[g] <- max(data.event$schadSum[ind])
    ind.VS <- which(data.event$versSum[ind] > 0)
    schadensatzE[g] <- mean(data.event$schadSum[ind[ind.VS]] /
                              data.event$versSum[ind[ind.VS]])
    schadensatzTot[g] <- sum(data.event$schadSum[ind]) / sum(data.event$versSum[ind])
  }
  
  data.zwCode <- data.frame(ZweckCode = unique.zwCode,
                            AnzahlSchaeden = anzahl,
                            Schadensumme = summe,
                            Durchschnitt = durchschn,
                            Median = median,
                            MaxSchaden = maxSchad,
                            SchadensatzE = schadensatzE,
                            SchadensatzTot = schadensatzTot,
                            stringsAsFactors = FALSE)
  data.zwCode <- rbind(data.zwCode, c('Total',sum(anzahl), sum(summe),
                                      mean(durchschn), median(median), max(maxSchad),
                                      mean(schadensatzE), mean(schadensatzTot)))
  
  # ## schadentabelle pro Schaetzkreis
  # csv.file <- paste('pdf/',eventArt,'.',
  #                   format(eventStart, '%Y%m%d'), '.csv', sep = '')
  # write.csv(data.schaetzkr[,-1], csv.file, row.names=F)
  
  ## ----------------------------------------------------------------
  ## Karten erstellen
  ## ----------------------------------------------------------------
  
  ## Schadenmeldungen und -summe darstellen und als pdf-Seite:
  
  ## prepare the pdf-plot
  # pdf(pdf.file, width = 8.2, height = 11.6, version = '1.6',
  #     paper = 'a4', encoding = 'ISOLatin1.enc')
  # par(mar = c(1, 1, 1.5, 1), mfrow = c(2, 2))
  
  # pdf.file <- paste('pdf/',eventArt,'.',
  #                   format(eventStart, '%Y%m%d'),'.pdf', sep = '')
  
  # ## A) Anzahl Schaeden pro Gemeinde ploten
  # leg.x <- xmin
  # leg.y <- ymin + 0.7 * (ymax - ymin)
  # ## klassenwahl fuer die anzahl schaeden und schadensumme
  # if(sum(data.gem$AnzahlSchaeden)>=1000) {
  #   ## groesseres Ereignis
  #   leg.text <- c('< 10', '10 - 50', '50 - 100', '100 - 150', '150 - 200', '> 200')
  #   breaks <- c(0, 10, 50, 100, 150, 200, 9e9)
  #   ## klein-Ereignis
  # } else {
  #   leg.text <- c('bis 5', '5 - 10', '10 - 15', '15 - 20', '> 20')
  #   breaks <- c(0, 5, 10, 15, 20, 9e9)
  # }
  # 
  # plot.gem('AnzahlSchaeden', breaks, data.gem)
  # # title(main = paste(eventArt,', ', format(eventStart, '%d.%m.%Y'),sep=''),
  # #       line = 0.5, cex.main=1)
  # title(main = 'Schadenmeldungen durch Gewitter vom 01. und 02.08.2017',
  #       line = 0.5, cex.main=1)
  # text(txt.x, txt.y, paste('Anzahl erfasster Schäden pro Gemeinde',
  #                          '\n(GemDat per ', per.string, ')',sep=''),
  #      adj = c(0, 1), cex = 0.8, font = 1)
  # text(txt.x, txt.y2, paste('GG25 © swisstopo, bearbeitet durch die GVZ - Mirco Heidemann,',
  #                           format(Sys.time(), "%b %Y")),
  #      adj = c(0, 0), cex = 0.5)
  # legend('topright', leg.text, fill = cols.YlOrRd, bg = 'white', bty = "n",
  #        xjust = 0, yjust = 1, cex=0.7)
  # box()
  
  # ## B) Schadensumme pro Gemeinde ploten
  # leg.x <- xmin
  # leg.y <- ymin + 0.7 * (ymax - ymin)
  # ## klassenwahl fuer die anzahl schaeden und schadensumme
  # if(sum(data.gem$AnzahlSchaeden)>=1000) {
  #   ## groesseres Ereignis
  #   leg.text <- c("bis 10", "10 - 100", "100 - 200",
  #                 "200 - 500", "> 500")
  #   breaks <- c(0, 1e4, 1e5, 2e5, 5e5, 9e9)
  #   ## klein-Ereignis
  # } else {
  #   leg.text <- c("bis 1", "1 - 10", "10 - 20",
  #                 "20 - 50", "> 50")
  #   breaks <- c(0, 1e3, 1e4, 2e4, 5e4, 9e9)
  # }
  # 
  # plot.gem('Schadensumme', breaks, data.gem)
  # title(main = paste(eventArt,', ', format(eventStart, '%d.%m.%Y'),sep=''),
  #       line = 0.5, cex.main=1)
  # text(txt.x, txt.y, paste('Geschätzte Schadensumme pro Gemeinde',
  #                          '\nin Tausend CHF (GemDat per ', per.string, ')',sep=''),
  #      adj = c(0, 1), cex = 0.8, font = 1)
  # text(txt.x, txt.y2, paste('GG25 © swisstopo, bearbeitet durch die GVZ - Mirco Heidemann,',
  #                           format(Sys.time(), "%b %Y")),
  #      adj = c(0, 0), cex = 0.5)
  # legend('topright', leg.text, fill = cols.YlOrRd, bg = 'white', bty = "n",
  #        xjust = 0, yjust = 1, cex=0.7)
  # box()
  
  # ## C) Anzahl Schaeden pro SCHAETZKREISE ploten
  # leg.x <- xmin
  # leg.y <- ymin + 0.7 * (ymax - ymin)
  # ## klassenwahl fuer die anzahl schaeden und schadensumme
  # if(sum(data.gem$AnzahlSchaeden)>=1000) {
  #   ## groesseres Ereignis
  #   # leg.text <- c("bis 50", "50 - 200", "200 - 500",
  #   #               "500 - 1000", "> 1000")
  #   # breaks <- c(0, 50, 200, 500, 1e3, 9e9)
  #   leg.text <- c("< 10", "10 - 100", "100 - 200", "200 - 500",
  #                 "500 - 1000", "> 1000")
  #   breaks <- c(0, 10, 100, 200, 500, 1e3, 9e9)
  #   ## klein-Ereignis
  # } else {
  #   leg.text <- c('< 10', '50', '50 - 100', '100 - 150', '150 - 200', '> 200')
  #   breaks <- c(0, 10, 50, 100, 150, 200, 9e9)
  # }
  # 
  # plot.schaetzkr('AnzahlSchaeden', breaks, data.schaetzkr)
  # # title(main = paste(eventArt,', ', format(eventStart, '%d.%m.%Y'),sep=''),
  # #       line = 0.5, cex.main=1)
  # title(main = 'Schadenmeldungen durch Gewitter vom 01. und 02.08.2017',
  #       line = 0.5, cex.main=1)
  # text(txt.x, txt.y, paste('Anzahl erfasster Schäden pro Schätzungskreis',
  #                          '\n(GemDat per ', per.string, ')',sep=''),
  #      adj = c(0, 1), cex = 0.8, font = 1)
  # text(txt.x, txt.y2, paste('GG25 © swisstopo, bearbeitet durch die GVZ - Mirco Heidemann,',
  #                           format(Sys.time(), "%b %Y")),
  #      adj = c(0, 0), cex = 0.5)
  # legend('topright', leg.text, fill = cols.YlOrRd, bg = 'white', bty = "n",
  #        xjust = 0, yjust = 1, cex=0.7)
  # box()
  # 
  # ## D) Schadensumme pro SCHAETZKREISE ploten
  # leg.x <- xmin
  # leg.y <- ymin + 0.7 * (ymax - ymin)
  # ## klassenwahl fuer die anzahl schaeden und schadensumme
  # if(sum(data.gem$AnzahlSchaeden)>=1000) {
  #   ## groesseres Ereignis
  #   leg.text <- c("bis 0.5", "0.5 - 1", "1 - 2",
  #                 "2 - 3", "> 3")
  #   breaks <- c(0, 5000, 1e6, 2e6, 3e6, 9e9)
  #   ## klein-Ereignis
  # } else {
  #   leg.text <- c('bis 0.1', '0.1 - 0.2', '0.2 - 0.3', '0.3 - 0.4', '> 0.3')
  #   breaks <- c(0, 100e3, 200e3, 300e3, 400e3, 9e9)
  # }
  # 
  # plot.schaetzkr('Schadensumme', breaks, data.schaetzkr)
  # title(main = paste(eventArt,', ', format(eventStart, '%d.%m.%Y'),sep=''),
  #       line = 0.5, cex.main=1)
  # text(txt.x, txt.y, paste('Geschätzte Schadensumme pro Schätzungskreis',
  #                          '\nin Millionen CHF (GemDat per ',
  #                          per.string, ')',sep=''),
  #      adj = c(0, 1), cex = 0.8, font = 1)
  # text(txt.x, txt.y2, paste('GG25 © swisstopo, bearbeitet durch die GVZ - Mirco Heidemann,',
  #                           format(Sys.time(), "%b %Y")),
  #      adj = c(0, 0), cex = 0.5)
  # legend('topright', leg.text, fill = cols.YlOrRd, bg = 'white', bty = "n",
  #        xjust = 0, yjust = 1, cex=0.7)
  # box()
  # 
  # ## pdf schliessen
  # dev.off()
  
  
  ## Nur Anzahl Schaden als pdf-Seite
  ## prepare the pdf-plot
  pdf.file <- paste('pdf/', format(eventStart, '%Y%m%d'), '.schadAnz.',
                    eventArt, '.pdf', sep = '')
  pdf(pdf.file, width = 11.6, height = 8.2, version = '1.6',
      paper = 'a4r', encoding = 'ISOLatin1.enc')
  par(mar = c(1, 1, 1.5, 1), mfrow = c(1, 2))
  
  ## ymax so anpassen, dass es zum Verhaeltnis von A4r-Papier passt
  ymax <- ymin + (29.7 * (xmax - xmin) / 21)
  
  txt.x <- xmin + 0.02 * (xmax - xmin)
  txt.y <- ymin + 0.96 * (ymax - ymin)
  # txt.y2 <- ymin + 0.01 * (ymax - ymin)
  txt.y2 <- ymin - 0.08 * (ymax - ymin)
  
  ## A) Anzahl Schaeden pro Gemeinde ploten
  leg.x <- xmin
  leg.y <- ymin + 0.7 * (ymax - ymin)
  ## klassenwahl fuer die anzahl schaeden und schadensumme
  if(sum(data.gem$AnzahlSchaeden)>=1000) {
    ## groesseres Ereignis
    leg.text <- c('< 10', '10 - 50', '50 - 100', '100 - 150', '150 - 200', '> 200')
    breaks <- c(0, 10, 50, 100, 150, 200, 9e9)
    ## klein-Ereignis
  } else {
    leg.text <- c('bis 5', '5 - 10', '10 - 15', '15 - 20', '> 20')
    breaks <- c(0, 5, 10, 15, 20, 9e9)
  }
  
  plot.gem('AnzahlSchaeden', breaks, data.gem)
  title(main = graphTitle, line = 0.5, cex.main=1)
  text(txt.x, txt.y, paste('Anzahl erfasster Schäden pro Gemeinde',
                           '\n(GemDat per ', per.string, ', ', time.char, ')',sep=''),
       adj = c(0, 1), cex = 0.8, font = 1)
  
  text(txt.x, ymin - 0.01 * (ymax - ymin),
       paste('Anzahl erfasster Schäden:',
             prettyNum(as.character(sum(data.gem$AnzahlSchaeden)),
                       big.mark="'"),sep=' '), adj = c(0, 1), cex = 0.8, font = 1)
  
  text(txt.x, txt.y2,
       paste('GG25 © swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, Christoph Welker,',
             format(Sys.time(), "%b %Y")),
       adj = c(0, 0), cex = 0.5)
  legend('topright', leg.text, fill = cols.YlOrRd, bg = 'white', bty = "n",
         xjust = 0, yjust = 1, cex=0.7)
  box()
  
  ## B) Anzahl Schaeden pro SCHAETZKREISE ploten
  leg.x <- xmin
  leg.y <- ymin + 0.7 * (ymax - ymin)
  ## klassenwahl fuer die anzahl schaeden und schadensumme
  if(sum(data.gem$AnzahlSchaeden)>=1000) {
    ## groesseres Ereignis
    # leg.text <- c("bis 50", "50 - 200", "200 - 500",
    #               "500 - 1000", "> 1000")
    # breaks <- c(0, 50, 200, 500, 1e3, 9e9)
    leg.text <- c("< 10", "10 - 100", "100 - 200", "200 - 500",
                  "500 - 1000", "> 1000")
    breaks <- c(0, 10, 100, 200, 500, 1e3, 9e9)
    ## klein-Ereignis
  } else {
    leg.text <- c('< 10', '50', '50 - 100', '100 - 150', '150 - 200', '> 200')
    breaks <- c(0, 10, 50, 100, 150, 200, 9e9)
  }
  
  plot.schaetzkr('AnzahlSchaeden', breaks, data.schaetzkr)
  title(main = graphTitle, line = 0.5, cex.main=1)
  text(txt.x, txt.y, paste('Anzahl erfasster Schäden pro Schätzungskreis',
                           '\n(GemDat per ', per.string, ', ', time.char, ')',sep=''),
       adj = c(0, 1), cex = 0.8, font = 1)
  
  text(txt.x, txt.y2,
       paste('GG25 © swisstopo, bearbeitet durch die GVZ - Mirco Heidemann, Christoph Welker,',
             format(Sys.time(), "%b %Y")),
       adj = c(0, 0), cex = 0.5)
  legend('topright', leg.text, fill = cols.YlOrRd, bg = 'white', bty = "n",
         xjust = 0, yjust = 1, cex=0.7)
  box()
  
  # ## pdf schliessen
  dev.off()
  
  # print('Total Anzahl erfasster Schäden:'); sum(data.gem$AnzahlSchaeden)
  # print('Total Schadensumme:'); sum(data.gem$Schadensumme)
  return(data.gem);
}
