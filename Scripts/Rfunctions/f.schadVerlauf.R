## -------------------------------------------------------------------------------
## Schadenstatistik eines Elementarereignisses
##
## Folgende Ausgaben werden erzeugt und zusammen in einm pdf gespeichert:
## - Schadentabelle: Anzahl gemeldeter und erfasster Schäden pro Schätzkreis
## - Zeitlicher Verlauf der gemeldeten und erfassten Schäden
## - Kummulierte Anzahl der gemeldeten und erfassten Schäden
##
## Gebäudeversicherung Kanton Zürich, Bereich Naturgefahren
## Mirco Heidemann, 07/2012, 09/2012, 08/2017
##
## Modified by Christoph Welker, 08/2017
## -------------------------------------------------------------------------------

schadVerlauf = function(schadFile, eventArt, eventStart, eventEnd, graphTitle) {
  require(reshape2)
  require(ggplot2)
  require(gridExtra)
  require(grid)
  require(gtable)
  require(tcltk)
  require(DT)
  
  ## "Stand-per-Datum" aus dem Schadenfile extrahieren (fuer Grafik)
  per.string <- unlist(strsplit(schadFile, '\\per'))[2]
  per.string <- unlist(strsplit(per.string, '\\_'))[1]
  per <- as.Date(per.string, format='%d.%m.%Y')
  ## zeit für Grafik angeben
  time.char <- paste(as.character(format(Sys.time(), '%R')), 'Uhr')
  
  eventStart <- as.Date(eventStart, '%d.%m.%Y')
  eventEnd <- as.Date(eventEnd, '%d.%m.%Y')
  
  filename <- paste('pdf/es.', format(eventStart, '%Y%m%d'), '.', 'Stand', 
                    format(per, '%y%m%d'), sep = '')
  
  ## Einlesen der files:
  ## Schadenfile:
  data <- read.csv2(schadFile, stringsAsFactors = FALSE)
  
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
  
  ## plz-, gemeinden-, bezirks- tabellen
  csv.string <- 'J:/Naturgefahren/FG2_Datenanalysen/Portfolio gvz/'
  #ortschaft_plz <- read.csv2(paste(csv.string,'Ortschaft_PLZ.csv',sep=''))
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
  
  # Daten wählen innerhalb der gewünschten Zeit --> date.start, eventEnd
  ind.dat <- which(format(data$EreignisDat, '%Y%m%d') >= format(eventStart, '%Y%m%d')
                   & format(data$EreignisDat, '%Y%m%d') <= format(eventEnd, '%Y%m%d'))
  if (length(ind.dat) > 0) data <- data[ind.dat,]
  
  # Anzahl Schäden, sowie Status in data summieren
  unique.GemNr <- sort(unique(data$GemNr))
  anzahl <- GemNr <- pendent <- erledigt <- schadenSumme <- numeric(length(unique.GemNr))
  
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
                                Pendent = pendent,
                                SchadenSumme = schadenSumme,
                                stringsAsFactors = FALSE)
  
  ## -------------------------------------------------------------------------------
  ## Visulisieren der Tabelle 
  ## -------------------------------------------------------------------------------
  
  # Summierte Tabelle den Schätzkreisen zuordnen
  m = match(summarized.data$GemNr, bezirke_gemeinde$GemeindeNr)
  summarized.data$Schaetzkreis <- bezirke_gemeinde$Schaetzkreis[m]
  
  ## Anzahl gemeldeter und erfasster Schäden pro Schätzkreis:
  ## Summierte Tabelle pro GemeindeNr aggregieren (--> Pivot Prozedere),
  ## inkl. "Total-Zeile" ('margins')
  melt.dat <- melt(summarized.data,
                   measure = c('AnzahlSchaeden', 'Erledigt',
                               'Pendent'))
  dcast.anzahl <- dcast(melt.dat, Schaetzkreis ~ variable, sum,
                        margins="Schaetzkreis")
  dcast.anzahl[,1] <- as.character(dcast.anzahl[,1])
  #dcast.anzahl[12,1] <- as.character('Total')
  
  dcast.anzahl$PendentProzent <- dcast.anzahl$Pendent/dcast.anzahl$AnzahlSchaeden
  
  ## Liste aller Schaetzkreise, auch der nicht betroffenen
  sk <- sort(unique(bezirke_gemeinde$Schaetzkreis))
  m = match(sk, dcast.anzahl$Schaetzkreis)
  df <- as.data.frame(sk)
  names(df) <- 'Schaetzkreis'
  df$AnzahlSchaeden <- dcast.anzahl$AnzahlSchaeden[m]
  df$Erledigt <- dcast.anzahl$Erledigt[m]
  df$Pendent <- dcast.anzahl$Pendent[m]
  df$PendentProzent <- dcast.anzahl$PendentProzent[m]
  df <- rbind(df, dcast.anzahl[dim(dcast.anzahl)[1],])
  df[is.na(df)] <- 0
  
  # dt.colnames <- c('Schätzkreis', 'Erfasste Schadenmeldungen',
  #                  'Erfasste Abschätzungen', 'Offene Schäden',
  #                  'Offene Schäden [%]')
  # 
  # dt.title <- paste('Schadenmeldungen Elementar seit dem ',
  #                   format(eventStart, '%d.%m.%Y'), sep='')
  
  
  # ## mit DT - An R interface to the DataTables library
  # dt.tbl <- DT::datatable(df, class = 'cell-border stripe',
  #                         extensions = 'Buttons',
  #                         options = list(pageLength = dim(df)[1],
  #                                        dom = 'r'), ## Ohne Filter, suchfunktion etc.
  #                         caption = htmltools::tags$caption(
  #                           style = 'caption-side: bottom; text-align: left;',
  #                           htmltools::em(dt.title)), rownames = FALSE,
  #                         colnames = dt.colnames) %>% formatPercentage('PendentProzent',0)
  # dt.tbl
  # # # save as html
  # # DT::saveWidget(dt.tbl, 'tbl.schad.html')
  
  ## -------------------------------------------------------------------------------
  ## Plots (ggplot2) der Schadenentwicklung
  ## -------------------------------------------------------------------------------
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
  
  ## kummulierte schaeden
  t <- cumsum(df.time[,c(2:3)])
  df.time$kum.erfasst <- t[,1]
  df.time$kum.geschaetzt <- t[,2]
  
  aktStand.erfasst <- as.numeric(df.time[dim(df.time)[1],4])
  aktStand.gesch <- as.numeric(df.time[dim(df.time)[1],5])
  
  ## ggPlot
  ggtheme = theme_bw(base_size = 12) +
    theme(legend.justification=c(1,0.7),legend.position=c(1,0.7),legend.title=element_blank(),
          legend.background = element_rect(colour = NA, fill = NA),
          plot.margin=unit(c(1,1,1,1),"mm")) #("left", "right", "bottom", "top")) +
  #+ theme(axis.text.x  = element_text(angle=45, vjust=1,hjust=1))
    
  footnote <- "GVZ, Bereich Naturgefahren"
  gtitle <- paste(graphTitle,
                  '\nGemDat: ', per.string, ', ',time.char, sep='')
  
  ## 1. Plot kummulierte Anzahl
  dfm <- melt(df.time[,-c(2:3)], id="tag")
  line.plot <- 
    ggplot(data=dfm, aes(x=tag, y=value, colour=variable)) +
    # geom_line(size = 0.5, alpha = 1) +
    # geom_point(size=2) +
    geom_line(size = 1, alpha = 1) +
    geom_point(size=2.5) +
    scale_colour_manual(name = '',
                        labels=c("Erfasst","Abgeschätzt"),
                        values=c("#fc8d59", "#99d594")) +
    
    ggtitle(gtitle) +
    ##### annotate("text", x = dfm$tag[1], y = floor(max(dfm$value/100))*100,
    annotate("text", x = as.Date(eventStart), y = floor(max(dfm$value/100))*100,
             label=paste('Aktuell erfasste Schäden: ',
                         format(aktStand.erfasst, big.mark="'", scientific=F),
                         '\nAktuell abgeschätzte Schäden: ',
                         format(aktStand.gesch, big.mark="'", scientific=F), sep=''),
             size=4, hjust = 0) +
    ylab("Anzahl Schäden") + xlab("") + 
#     scale_x_date(breaks = seq(from=min(df.time$tag), to=max(df.time$tag), by=1),
#                  date_labels = "%d.%m.%y") +
    scale_x_date(breaks = seq(from=as.Date(eventStart)-1, to=max(df.time$tag)+1, by=7),
                 limits = c(as.Date(eventStart)-1, max(df.time$tag)+1),
                 date_labels = "%b %d",expand=c(0,0)) +
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
    ggtitle(gtitle) +
    ##### annotate("text", x = dfm$tag[1], y = floor(max(dfm$value/100))*100,
    annotate("text", x = as.Date(eventStart), y = floor(max(dfm$value/100))*100,         
             label=paste('Aktuell erfasste Schäden: ',
                         format(aktStand.erfasst, big.mark="'", scientific=F),
                         '\nAktuell abgeschätzte Schäden: ',
                         format(aktStand.gesch, big.mark="'", scientific=F), sep=''),
             size=4, hjust = 0) +
#     scale_x_date(breaks = seq(from=min(df.time$tag), to=max(df.time$tag), by=1),
#                  date_labels = "%d.%m.%y")
    scale_x_date(breaks = seq(from=as.Date(eventStart)-1, to=max(df.time$tag)+1, by=1),
                 limits = c(as.Date(eventStart)-1, max(df.time$tag)+1),
                 date_labels = "%b %d",expand=c(0,0))
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
    ggtitle(gtitle) +
    #### annotate("text", x = dfm$tag[1], y = floor(max(dfm$value/100))*100,
    annotate("text", x = as.Date(eventStart), y = floor(max(dfm$value/100))*100,
             label=paste('Aktuell erfasste Schäden: ',
                         format(aktStand.erfasst, big.mark="'", scientific=F),
                         '\nAktuell abgeschätzte Schäden: ',
                         format(aktStand.gesch, big.mark="'", scientific=F), sep=''),
             size=4, hjust = 0) +
    scale_y_continuous(breaks=seq(0,ceiling(aktStand.erfasst/500)*500,500)) +   # !!! Change y-ticks here !!!
#     scale_x_date(expand=c(0,0), breaks = seq(from=min(df.time$tag),
#                                              to=max(df.time$tag), by=1),
#                  date_labels = "%d.%m.%y") +
    scale_x_date(expand=c(0,0), breaks = seq(from=as.Date(eventStart)-1,to=max(df.time$tag)+1, by=2),
                 limits = c(as.Date(eventStart)-1, max(df.time$tag)+1),
                 date_labels = "%b %d") +
    ggtheme
  
  # Fussnote / Text hinzufügen
  grid.newpage()
  p.kombi <- arrangeGrob(kombi.plot,
                         bottom = textGrob(footnote,
                                           x = 0, hjust = -0.1, vjust=0.1,
                                           gp = gpar(fontface = "italic",
                                                     fontsize = 8)))
  #grid.draw(p.kombi)
  
  ## -------------------------------------------------------------------------------
  ## Abspeichern der Ausgaben in einem gemeinsamen .pdf - File
  ## -------------------------------------------------------------------------------
  pdf.file <- paste('pdf/',eventArt,'.',
                    'Start.', format(eventStart, '%Y%m%d'), '.Ende.', format(eventEnd, '%Y%m%d'), '.schadEntwicklung','.pdf', sep = '')
  pdf(pdf.file, width = 11.6, height = 8.2, version = '1.6',
      paper = 'a4r', encoding = 'ISOLatin1.enc')
  
  #grid.arrange(p.kombi)
  grid.arrange(p.line)
  
  # ## pdf schliessen
  dev.off()
  
  ## csv file ausgeben fuer excel tabelle
  write.csv(df, paste('pdf/', 'Start.', format(eventStart, '%Y%m%d'), '.Ende.', format(eventEnd, '%Y%m%d'), '.tbl.schad', '.csv',
                      sep=""), row.names = F)
  
  return(df);
}