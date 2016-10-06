rm(list = ls())
setwd("C:/ResultattjenesteGIT/nkr/")

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
      merge(            NKRdata, BoStederInnb[,c('BoHF', 'Fylke', "KommNr", "Aar", "BydelNum")],
            by.x = c("Kommunenr", "OpAar", "Bydelkode"), by.y = c("KommNr", "Aar", "BydelNum"), all.x = TRUE, all.y = FALSE)
RegData <-
      merge(RegData1, RHFInnb[,c('BoRHF', "KommNr", "Aar")], by.x = c("Kommunenr", "OpAar"),
            by.y = c("KommNr", "Aar"), all.x = TRUE, all.y = FALSE)

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
opKat <- 99
ktr <- 1
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
enhetsUtvalg <- 10 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'BoHF'  #ShNavn, Fylke, BoHF, BoRHF
valgtVar <- 'BeinsmLavPre'   #BeinsmEndrLav', BeinsmLavPre, OswEndr13, OswEndr20, OswEndr30pst, Osw48, Verre
outfile <- paste0(valgtVar, '_', grVar,'AarAK.pdf')

RyggFigAndelerGrVarAar(
      RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil,
      minald = minald, maxald = maxald, erMann = erMann, ktr = ktr, hovedkat = hovedkat, grVar = grVar,
      preprosess = 1, enhetsUtvalg = 10, reshID = reshID, outfile = outfile)

variableInd <- c('BeinsmLavPre', 'OswEndr20','SympVarighUtstr')
grupperingInd <- c('Fylke', 'ShNavn', 'BoRHF', 'BoHF')
for (grVar in grupperingInd) {
      for (valgtVar in variableInd) {
            outfile <- paste0(valgtVar, '_', grVar, 'Aar.pdf')
            hovedkat <- 1
            RyggFigAndelerGrVarAar(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil, minald = minald, 
                  maxald = maxald, erMann=erMann, hovedkat = hovedkat, ktr = ktr,preprosess = 1, 
                  enhetsUtvalg = 10, reshID = reshID, outfile = outfile, grVar = grVar)
      }
      for (valgtVar in c('PeropKompDura', 'KpInf3Mnd')) {
            outfile <- paste0(valgtVar, '_', grVar, 'Aar.pdf')
            hovedkat <- 99
            RyggFigAndelerGrVarAar(
                  RegData = RegData, valgtVar = valgtVar, datoFra = datoFra, datoTil = datoTil,
                  minald = minald, maxald = maxald, erMann = erMann, hovedkat = hovedkat,ktr = ktr,
                  preprosess = 1, enhetsUtvalg = 10, reshID = reshID, outfile = outfile, grVar = grVar)
      }
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