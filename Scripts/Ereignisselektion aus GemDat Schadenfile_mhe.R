## ------------------------------------------------------------------------
## Selektion eines Elementarereignis aus dem GemDat Schaden-Export-File

## Gebäudeversicherung Kanton Zürich, Bereich Naturgefahren
## Mirco Heidemann, 10/2014
## ------------------------------------------------------------------------

## Arbeitsverzeichnis definieren
setwd('J:/MHE/10 Risikokarten/RIKA Hochwasser/Haselbach/')

## Schadenfile (von Stefan Hug erstellt) einlesen:
file.name <- 'alle_Schaeden_stand201402_MHE.csv'
file.path <- 'J:/MHE/03 Statistische Analysen/GemDat Datenanalysen/GemDat Export/'
d.schaden <- read.csv(paste(file.path, file.name,sep=""),
                     header=TRUE, sep=";")
## File "GVZ_Geb_GemDat_georef_201312" fuer die Georeferenzierung
d.geoRef <- read.csv('J:/MHE/04 GVZ Portfolio Referenzierung/GemDat_georef_2014.csv',
                     header=TRUE, sep=";")

## Ursache, Schadendatum und Gemeinde, waehlen
## ------------------------------------------------------------------------
# ## Sturm:
# ursache <- '1 Sturm'
# ## Hagel:
# ursache <- '2 Hagel'
## Ueberschwemm: 
ursache <- '3 Hochwasser, ueberschwemmung'
## Schadendatum
datum.ereignis <- '10.06.2008'
## Gemeinde waehlen
## Ganzer Kanton = 'KntZH'
gem <- c('Mettmenstetten', 'Knonau', 'Maschwanden')
## ------------------------------------------------------------------------

## Schadendatenaufbereitung
d.schaden$SchadenDatum <- as.Date(d.schaden$SchadenDatum,"%d.%m.%Y")
d.schaden$FkoVersDa <- as.Date(d.schaden$FkoVersDa,"%d.%m.%Y")
d.schaden$FkoZaTot <- as.numeric(d.schaden$FkoZaTot)
d.schaden$SchadenSumme <- as.numeric(d.schaden$SchadenSumme)

## Nach Gemeinde selektieren
ind <- which(d.schaden$GebaeudeGemeindeName %in% gem)
d.ereignis <- d.schaden[ind,]
## remove d.schaden
rm(d.schaden)

## Nach Ursache selektieren
d.ereignis <- d.ereignis[d.ereignis$CodTextDt==ursache,]

## Nach Ereignisdatum selektieren
ereignis.datum <- as.Date(datum.ereignis, '%d.%m.%Y')
ereignis.datum.seq <- seq(ereignis.datum, ereignis.datum+3, by="day")

d.ereignis <- d.ereignis[d.ereignis$SchadenDatum>=ereignis.datum.seq[1]&
  d.ereignis$SchadenDatum<=ereignis.datum.seq[4],]

## Abgewiesene Meldungen loeschen
t.logisch <- which(d.ereignis$T_Tab_Jse_1_StcTextDt=='abgewiesen')
d.ereignis <- d.ereignis[-t.logisch,]

## TEILAUSZAHLUNGEN: die ausbezahlte summe muss zu einem total zusammengezaehlt 
## werden, die dazugehoerige geschaetzte schadensumme ist jedesmal wieder
## aufgefuert, darf aber nur einmal gezaehlt werden:
schad.ausbez <- aggregate(d.ereignis$FkoZaTot,
                          by=list(d.ereignis$SchadenNr),
                          sum, simplify = TRUE)

## doppelte werte der schadenschaetzungen eliminieren
schad.schaetz <- d.ereignis[,c("SchadenSumme",
                            "SchadenNr",
                            "GebaeudeSuchbegriff")]
schad.gemdat <- schad.schaetz[!duplicated(schad.schaetz$SchadenNr),]

## Abzug des Selbsbehaltes von CHF 500.- bei geschaetzten Schaeden:
## Bis Mitte 2004 wurden im GemDat die Netto Summen als geschaetzte
## Schadensummen verbucht (Werte in GemDat ohne Selbestbehalt)
## danach die Brutto-Schadenschaetzung (Werte in GemDat inkl. Selbestbehalt)
t.logisch <- which(schad.gemdat$schadat <= as.Date('30.06.2004', "%d.%m.%Y"))
schad.gemdat$SchadenSumme[t.logisch] <- schad.gemdat$SchadenSumme[t.logisch]
t.logisch <- which(schad.gemdat$schadat > as.Date('30.06.2004', "%d.%m.%Y"))
schad.gemdat$SchadenSumme[t.logisch] <- schad.gemdat$SchadenSumme[t.logisch]-500
ind.negativ <- which(schad.gemdat$SchadenSumme<0)
schad.gemdat$SchadenSumme[ind.negativ] <- 0

## data frame mit bezahlten schaeden erstellen
schad.gemdat$schadbez <- schad.ausbez$x[
  match(schad.gemdat$SchadenNr,schad.ausbez$Group.1)]
names(schad.gemdat) <- c("schadgesch", "schadenNr", "gebnr", "schadbez")
## unter selbstbehalt: ausbezahlt = 0
ind <- which(schad.gemdat$schadgesch<=500)
schad.gemdat$schadbez[ind] <- 0

## wenn keine schadenzahlung vorhanden, dann schadenschaetzung
## -> pendente Schaeden
schad.gemdat$schadbez[is.na(schad.gemdat$schadbez)] <- 
  schad.gemdat$schadgesch[is.na(schad.gemdat$schadbez)]

## ausbezahlte schaeden kleiner Null: nehme die geschaetzte schadensumme
ind <- which(schad.gemdat$schadbez < 0)
schad.gemdat$schadbez[ind] <- schad.gemdat$schadgesch[ind]

## data frame zusammensetzten
## schadendatum
schad.gemdat$schadat <- d.ereignis$SchadenDatum[
  match(schad.gemdat$gebnr, d.ereignis$GebaeudeSuchbegriff)]
schad.gemdat$schadat <- as.Date(schad.gemdat$schadat,"%d.%m.%Y")
## schaden ursache
schad.gemdat$ursache <- d.ereignis$CodTextDt[
  match(schad.gemdat$gebnr, d.ereignis$GebaeudeSuchbegriff)]
## VersSumme
schad.gemdat$vs <- d.ereignis$SbwVerWert[
  match(schad.gemdat$gebnr, d.ereignis$GebaeudeSuchbegriff)]
## Baujahr
schad.gemdat$baujahr <- d.ereignis$GebaeudeBaujahr[
  match(schad.gemdat$gebnr, d.ereignis$GebaeudeSuchbegriff)]
## Zweckcode von Zweck-Text trennen
schad.gemdat$zwcode <- 
  as.numeric(sapply(strsplit(as.character(d.ereignis$GebaeudeZweckText[
    match(schad.gemdat$schadenNr,
          d.ereignis$SchadenNr)]), ","), "[[", 1))
## Gemeinde
schad.gemdat$gemeinde <- d.ereignis$GebaeudeGemeindeName[
  match(schad.gemdat$gebnr, d.ereignis$GebaeudeSuchbegriff)]
## Status vermerken:
schad.gemdat$status <- d.ereignis$T_Tab_Jse_1_StcTextDt[
  match(schad.gemdat$gebnr, d.ereignis$GebaeudeSuchbegriff)]
## koordinaten
schad.gemdat$geox <- d.geoRef$X_CH[match(schad.gemdat$gebnr,
                                         d.geoRef$GebNr)]
schad.gemdat$geoy <- d.geoRef$Y_CH[match(schad.gemdat$gebnr,
                                         d.geoRef$GebNr)]

# ## CSV Files schreiben
# string.name <- strsplit(getwd(),
#                         split='/')[[1]][length(strsplit(getwd(),
#                                                         split='/')[[1]])]
# write.csv(schad.gemdat,paste(string.name, '.ereignisschaden.',
#                       format(ereignis.datum,'%Y%m%d'),
#                       '.csv', sep=""), row.names = F)

## Schadensumme total
(tot.ereignis.schad <- sum(schad.gemdat$schadbez))
## Anzahl Schaeden total
(anz.ereignis.schad <- dim(schad.gemdat)[1])
## Schadensumme pro Gemeinde
(schadsum.gem <- aggregate(schad.gemdat$schadbez,
                           by=list(schad.gemdat$gemeinde),
                           sum, simplify = TRUE))
## Anzahl Schaeden pro Gemeinde
(schadanz.gem <- aggregate(schad.gemdat$schadbez,
                           by=list(schad.gemdat$gemeinde),
                           length, simplify = TRUE))

