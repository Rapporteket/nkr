rm(list = ls())
setwd("C:/ResultattjenesteGIT/nkr/")
library(nkr)

NKRdata <-
      read.table('C:/Registre/nkr/data/NKR2010_2015.csv', sep = ';', header = T, encoding = 'UTF-8')

Innbyggere2007_2015kjonn <-
      read.table('./Innbyggere2007_2015kjonn.csv', sep = ';', header = T, encoding = 'UTF-8')
Innbyggere2007_2015kjonn$BydelNum <-
      factor(as.character(Innbyggere2007_2015kjonn$BydelNum), exclude = "")
BoStederInnb <- aggregate(Innbyggere2007_2015kjonn$AntInnb,
                          by = Innbyggere2007_2015kjonn[,c('Kommune','KommNr', 'BydelNum','BoRHF',"BoHF", 'Fylke', 'Aar')], 
                          FUN = 'sum')#'BoHF',
RHFInnb <-
      aggregate(BoStederInnb$x, by = BoStederInnb[,c('KommNr', 'BoRHF','Aar')], FUN = 'sum')
#RegData <- merge(NKRdata, BoStederInnb, by.x = c("Kommunenr", "OpAar", "Bydelkode"), by.y = c("KommNr", "Aar", "BydelNum"), all.x = TRUE, all.y = FALSE)
#RegData1 <- merge(NKRdata, BoStederInnb, by.x = c("Kommunenr", "OpAar", "Bydelkode"),
#                by.y = c("KommNr", "Aar", "BydelNum"), all.x = TRUE, all.y = FALSE)
RegData1 <-
      merge(NKRdata, BoStederInnb[,c('BoHF', 'Fylke', "KommNr", "Aar", "BydelNum")],
            by.x = c("Kommunenr", "OpAar", "Bydelkode"), by.y = c("KommNr", "Aar", "BydelNum"), all.x = TRUE, all.y = FALSE)
RegData <-
      merge(RegData1, RHFInnb[,c('BoRHF', "KommNr", "Aar")], by.x = c("Kommunenr", "OpAar"),
            by.y = c("KommNr", "Aar"), all.x = TRUE, all.y = FALSE)
RegData <- RegData[which(RegData$OpAar %in% 2013:2015),]

#write.table(RegData, file='RegDataTilTest.csv', sep=';', row.names = F)
#Mister BoHF for registreringer som mangler bydelkode for Oslo. Disse kan få BoRHF. Legger derfor til BoRHF og BoHF separat


#__Inndata til funksjon:
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2013-01-01'
datoTil <- '2015-12-31'
erMann <- ''
tidlOp <- 99
minald <- 20
maxald <- 85
opKat <- 99  #Hastegrad
tidlOp <- 99 #
ktr <- 1
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
siste3aar <- 1
AKjust <- 1
enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'BoHF'  #ShNavn, Fylke, BoHF, BoRHF
valgtVar <- 'SympVarighUtstr'   #BeinsmEndrLav', SympVarighUtstr, OswEndr13, OswEndr20, OswEndr30pst, Osw48, Verre
outfile <- paste0(valgtVar, '_', grVar, AKjust,'Aar.png')

RyggFigAndelerGrVarAar(
      RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil,
      minald = minald, maxald = maxald, erMann = erMann, hovedkat = hovedkat,ktr = ktr, opKat=opKat, tidlOp=tidlOp,
      preprosess = 1, enhetsUtvalg = 10, reshID = reshID, outfile = outfile, grVar = grVar,  siste3aar=siste3aar, AKjust=AKjust)

AKjustDum <- 1 #Settes automatisk til 0 hvis grVar ulik BoRHF eller BoHF
grupperingInd <- c('ShNavn', 'BoHF') #c('Fylke', 'ShNavn', 'BoRHF', 'BoHF')
for (grVar in grupperingInd) {
      ifelse (grVar %in% c('BoHF', 'BoRHF'), AKjust <- AKjustDum, AKjust <- 0)
      valgtVar <- 'SympVarighUtstr' 
            outfile <- paste0(valgtVar, '1_1', grVar, AKjust,'Aar.png')
            hovedkat <- 1
            opKat <- 1  #Bare elektive pasienter
            RyggFigAndelerGrVarAar(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil, minald = minald, 
                  maxald = maxald, erMann=erMann, hovedkat = hovedkat, ktr = ktr,preprosess=1, opKat=opKat, tidlOp=tidlOp,
                  enhetsUtvalg = 10, reshID = reshID, outfile = outfile, grVar = grVar, siste3aar=siste3aar, AKjust=AKjust)
      }      

AKjustDum <- 1 #Settes automatisk til 0 hvis grVar ulik BoRHF eller BoHF
grupperingInd <- c('ShNavn', 'BoHF') #c('Fylke', 'ShNavn', 'BoRHF', 'BoHF')
for (grVar in grupperingInd) {
      ifelse (grVar %in% c('BoHF', 'BoRHF'), AKjust <- AKjustDum, AKjust <- 0)
      for (valgtVar in c('BeinsmLavPre', 'SympVarighUtstr')) {
            outfile <- paste0(valgtVar, '1_1', grVar, AKjust,'Aar.pdf')
            hovedkat <- 1
            opKat <- 1  #Bare elektive pasienter
            RyggFigAndelerGrVarAar(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil, minald = minald, 
                  maxald = maxald, erMann=erMann, hovedkat = hovedkat, ktr = ktr,preprosess=1, opKat=opKat, tidlOp=tidlOp,
                  enhetsUtvalg = 10, reshID = reshID, outfile = outfile, grVar = grVar, siste3aar=siste3aar, AKjust=AKjust)
      }
      valgtVar <- 'KpInf3Mnd'
            hovedkat <- 1:3
            outfile <- paste0(valgtVar, '123_', grVar, AKjust,'Aar.pdf')
            RyggFigAndelerGrVarAar(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil,hovedkat=hovedkat,
                  minald = minald, maxald = maxald, erMann = erMann, ktr = ktr,  siste3aar=siste3aar, AKjust=AKjust,
                  preprosess = 1, reshID = reshID, outfile = outfile, grVar = grVar)
     
}

#Kommune 301 Oslo har fire BoHF. For øvrig kommunenummer bestemmer kommunenummer entydig BoHF.
#BoStederInnb2015 <- BoStederInnb[which(BoStederInnb$Aar == '2015'),]
#save(BoStederInnb2015, file="./data/BostederInnb2015.Rdata")
#write.table(BoStederInnb2015, file="./data/BostederInnb2015.csv", sep=';')



#----------------Alders-og kjønnsstandardisering:-----------------------
#Gruppere på alder og kjønn. Velger aldersgrupper ut fra hendelsesfordeing, dvs. fra
#aldersfordelinga i registerpopulasjonen

NKRdata <-
      read.table(
            'C:/Registre/nkr/data/NKR2010_2015.csv', sep = ';', header = T, encoding = 'UTF-8'
      )
NKRdata <- RyggPreprosess(NKRdata)
NKRUtvalg <-
      RyggUtvalg(
            RegData = NKRdata, datoFra = datoFra, datoTil = datoTil, minald = minald, maxald =
                  maxald,
            erMann = erMann, hovedkat = hovedkat
      )
RegData <- NKRUtvalg$RegData
NKRUtvalg$utvalgTxt

#Velger 4 aldersgrupper
aldKvant <- quantile(RegData$Alder, c(25, 50, 75) / 100, na.rm = T)
#25% 50% 75% Hele populasjonen gir 43,57,67
#37  46  57
#aldKvant <- c(36,45,56)
aldgr <- c(minald - 1,aldKvant,85)
RegData$AlderGr <-
      cut(RegData$Alder,aldgr) #grensene er øverste grense i aldersintervallet
table(RegData$AlderGr)

#Må finne andel av normalpopulasjonen i disse gruppene ut fra befolkningsfil
#For alders-og kjønnsstandardisering:
Innb2015aldkj <-
      read.table(
            './Innbyggere2015aldkj.csv', sep = ';', header = T, encoding = 'UTF-8'
      )
Innb2015aldkj$AlderGr <- cut(Innb2015aldkj$Alder,aldgr)
#Innbyggere2015alder <- read.table('C:/VariasjonKvalitet/Innbyggere2015.csv', sep=';', header = T)
#Innbyggere <- with(Innbyggere2015alder, aggregate('AntInnb', by=list('KommNr', 'BoHF', 'BoRHF','Fylke'), FUN='sum')
PopAldKjGr <-
      aggregate(AntInnb ~ erMann + AlderGr, data = Innb2015aldkj,FUN = sum)
PopAldKjGr$Vekt <-
      prop.table((PopAldKjGr$AntInnb))#PopAldKjGr$AntInnb/sum(PopAldKjGr$AntInnb)

#Finne resultat i hver alder-og kjønnsgruppe

#Beregne kjønns- og aldersstandardisert resultat, dvs. vekte resultatene i gruppene
#ut fra normal-populasjon