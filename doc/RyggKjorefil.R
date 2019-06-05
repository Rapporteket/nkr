
#------NB NB: Alle variabele for fordeling må testes!!
#________________________________________________________________________________________
##_________________________________________________________________________________________

#					S A M L E R A P P O R T E R, 
#_________________________________________________________________________________________
#_________________________________________________________________________________________
#For å klargjøre R for bruk av JDBC ved spørringer mot databasen, så må
#noen ekstra pakker installeres. Siden JDBC er basert på Javateknologi trenger
#vi R klargjort for java:

install.packages('rJava')
install.packages('RJDBC')
install.packages('xtable')

#Trenger pakkene:
library(grid)
library(gridExtra)
library(rJava)
library(xtable)
library(tools)
library(knitr)
library(nkr)
#------------------------------------

rm(list=ls())
setwd('C:/ResultattjenesteGIT/nkr/inst/')

reshID <- 601161 #Haukeland nevr.kir: 105588, NIMI:  104279, Unn: 601161, St Olav: 105783 Namsos:105899, 
	#Lillehammer: 111185, Gjøvik: 111150, Rana ortopediske avd. 102224, Ullevål, ortopediske avd. 999995
#RyggDataALLE <- read.table('C:/Registre/nkr/data/NKR2015-03-02.csv', header = T, sep = ";")
#RegData <- read.table('A:/Rygg/NKR2010-2016aarsrapp.csv', sep=';', header=T, encoding = 'UTF-8') 
#fil <- 'A:/Rygg/NKR2017-11-20'
fil <- 'A:/Rygg/NKR2010-2017aarsrapp'
#RegData <- read.table(paste0(fil, '.csv'), sep=';', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE)  # na.strings = "NULL", 
#save(RegData, file=paste0(fil, '.Rdata'))
load(file=paste0(fil, '.Rdata'))
#RegData <- RegData[sample(1:dim(RegData)[1],10000), ]
knit('SamleRappNKR.Rnw') #, encoding = 'UTF-8')
texi2pdf('SamleRappNKR.tex')

library(knitr)
knit('C:/ResultattjenesteGIT/nkr/AarsrappOff/ResultaterAarsrapp.Rnw') #, encoding = 'UTF-8')
library(tools)
texi2pdf('ResultaterAarsrapp.tex')
#_________________________________________________________________________________________
#Registreringsoversikter for 2019-data
#_________________________________________________________________________________________
SkjemaOversikt <- read.table('A:/Rygg/NKR_Degenerativ_Rygg_SkjemaOversikt_datadump.csv_10.05.2019.csv', sep=';', header=T, encoding = 'UTF-8')
SkjemaOversikt$Skjemanavn <- SkjemaOversikt$X.U.FEFF.Skjemanavn
SkjemaOversikt$MndAar <- format(as.Date(SkjemaOversikt$HovedDato), '%y.%m')
table(SkjemaOversikt$MndAar)

indPasientskjema <- which(SkjemaOversikt$SkjemaRekkeflg==5)
table(SkjemaOversikt[indPasientskjema, c('Sykehusnavn','MndAar', "SkjemaStatus")]) # ,


#_________________________________________________________________________________________
#_________________________________________________________________________________________
#_________________________________________________________________________________________
#
#""""""""""""""""""""""""""""" F I G U R F U N K S J O N E R """"""""""""""""""""""""""""""""""""" 
#_________________________________________________________________________________________

library(nkr)
#  Laste data og parametre
rm(list=ls())
NKRdata <- read.table('A:/Rygg/NKR2010-2017aarsrapp.csv', sep=';', header=T, encoding = 'UTF-8')
RegData <- NKRdata
save(RegData, file=paste0(fil, '.Rdata'))
#load('A:/Nakke/NakkeAarsrapp2016.Rdata') #Preprossesserte data
#__Inndata til RyggFigAndeler.R:
tittel=1
reshID <- 601161 #999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
datoFra <- '2017-01-01'
datoTil <- '2019-12-31'
minald <- 0		#alder, fra og med
maxald <- 130	#alder, til og med
erMann <- 99			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
hovedkat <- 1 		#HovedInngrep, 0-7, Standard: 99, dvs alle op
opKat <- 1 #Hastegrad, 1-3
enhetsUtvalg <- 1 #	0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					3–egen enhet mot egen shusgruppe, 4–egen shusgruppe, 5–egen shusgruppe mot resten
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
ktr <- 2			#1. el 2. kontroll. '3mnd' - 3mnd kontroll, '12mnd' - 12 mnd kontroll
tittel <- 1
tidlOp <- 4			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF
valgtMaal <- 'Gjsn'
aar <- 2010:2016	#Standard: 0, dvs. alle år
tidsenhet <- 'Mnd' #Oppløsning på tidsaksen: 'Aar' (standard), 'Halvaar', 'Kvartal','Mnd'
offData <- 0
Ngrense <- 10
medKI <- 1

#---------------------------Dekningsgrad-------------------------------------
RegData=0
preprosess=0 
outfile='' 
valgtVar='DeknNakke17' #DeknNakke17, 'DeknRygg17'
#RyggFigAndelerGrVarDeknGr(RegData=0, preprosess=0, outfile='', valgtVar='DeknNakke17')
RyggFigAndelerGrVarDeknGr(RegData=RegData, outfile='', valgtVar='SymptVarighRyggHof')
RyggFigAndelerGrVar(RegData = RegData, valgtVar='Morsmal', outfile = '')

#-------------------------------------------------------	
#---------AndelTid
#-------------------------------------------------------	
valgtVar <- 'OswEndr20'	#alder70, degSponFusj, KpInf3Mnd, OswEndr20
outfile <- ''#paste0(valgtVar, '.png')	#Navn angis av Jasper


RyggFigAndelTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, hovedkat=hovedkat, preprosess=1,
                      minald = minald, maxald = maxald, aar=aar, tidsenhet = tidsenhet,
                      erMann=erMann, ktr=ktr, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, tittel=1, 
                reshID=reshID)
variable <- c('alder70', 'degSponFusj', 'KpInf3Mnd', 'OswEndr20')
for (var in variable) {
      outfile <- paste0(var, 'ATid.png')
      RyggFigAndelTid(valgtVar=var, RegData=RegData, datoFra='2011-01-01', ktr=1, outfile=outfile)
}

RyggFigAndelTid(RegData=0, preprosess=1,valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, hovedkat=hovedkat, 
      ktr=ktr, tidlOp = tidlOp, minald=minald, maxald=maxald, erMann=erMann, reshID=reshID, outfile='', 
      enhetsUtvalg=enhetsUtvalg, hentData=0)
      

#----------------------------------------------------------------
#			FigAndeler (RyggFigAndeler.r
#----------------------------------------------------------------
valgtVar <- 'Underkat'	#Må velge...  'Alder', 'Antibiotika', 'AntNivOpr', 'ArbstatusPre', 'Arbstatus3mnd', 
#      'Arbstatus12mnd', 'ASA', 'BMI', 'EqangstPre', 'EqgangePre', 'ErstatningPre', 'Fornoyd12mnd','Fornoyd3mnd', 
#      'HovedInngrep', 'Komorbiditet', 'KomplPer', 'KomplPost', 'Liggedogn', 'Morsmal', 'Nytte3mnd', 'Nytte12mnd', 
#      'OpIndPareseGrad', 'OpInd', 'OpIndSmeType', 'OpKat', 'RadUnders', 
#      'Roker', 'SivilStatus','SmStiPre', 'SmHyppPre', 'SymptVarighRyggHof', 
#      'SympVarighUtstr', 'Saardren', 'TidlOpr', 'TidlOprAntall','UforetrygdPre', 'Utd', 'Underkat'
#NB: Hvis variabel='Underkat', MÅ hovedkat velges, dvs. ikke 99.
setwd("C:/ResultattjenesteGIT/nkr")
outfile <- ''	#paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper ''
FordUt <- RyggFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann, hovedkat=hovedkat, preprosess=1,
		 enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
hovedkat <-2 		#HovedInngrep, 0-7, Standard: 99, dvs alle op


variable <- c('Alder', 'Antibiotika', 'AntNivOpr', 'ArbstatusPre', 'Arbstatus3mnd', 
'Arbstatus12mnd', 'ASA', 'BMI', 'EqangstPre', 'EqgangePre', 'ErstatningPre', 'Fornoyd12mnd','Fornoyd3mnd', 
'HovedInngrep', 'Komorbiditet', 'KomplPer', 'KomplPost', 'Liggedogn', 'Morsmal', 'Nytte3mnd', 'Nytte12mnd', 
'OpIndPareseGrad', 'OpInd', 'OpIndSmeType', 'OpKat', 'RadUnders', 
'Roker', 'SivilStatus','SmStiPre', 'SmHyppPre', 'SymptVarighRyggHof', 
'SympVarighUtstr', 'Saardren', 'TidlOpr', 'TidlOprAntall','UforetrygdPre', 'Utd', 'Underkat')


for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '.png')
      utdata <- FigAndeler(RegData <- NKRdata, valgtVar = valgtVar, hovedkat=1, outfile = outfile, reshID=reshID)
}
		 
#                   RyggFigGjsnBox
#----------------------------------------------------------
#Endring i effektmål som funksjon av tid eller prescore

valgtVar <- 'Liggedogn' #
outfile <- ''#paste0(valgtVar,enhetsUtvalg, '.png')	#paste0(valgtVar,enhetsUtvalg, '.pdf')

utdata <- RyggFigGjsnBox(RegData=RegData, outfile=outfile, valgtVar=valgtVar, tidlOp=tidlOp, erMann=erMann, 
		hovedkat=hovedkat, minald=minald, maxald=maxald, ktr=ktr, tittel=tittel, valgtMaal=valgtMaal, 
		datoFra=datoFra, datoTil=datoTil, enhetsUtvalg=1, reshID=reshID) #aar=aar, 
#RyggFigGjsnBox
#RegData=0, outfile=outfile, valgtVar=valgtVar,valgtMaal=valgtMaal, enhetsUtvalg=enhetsUtvalg, 
#datoFra=datoFra, datoTil=datoTil, hovedkat=hovedkat, tidlOp=tidlOp,  ktr=ktr, erMann=erMann, 
#minald=minald, maxald=maxald, hentData=1, preprosess=1, reshID=reshID


variable <- c('EQ5DPre', 'OswTotPre', 'SmBePre', 'SmRyPre', 
              'EQ5DEndr', 'Liggedogn', 'OswEndr', 'SmRyggEndr', 'SmBeinEndr', 
              'EQ5DEndrPre', 'OswEndrPre', 'SmRyggEndrPre', 'SmBeinEndrPre')
for (var in variable) {
      outfile <- paste0(var, 'GjsnTid.png')
      RyggFigGjsnBox(valgtVar=var, RegData=RegData, datoFra='2010-01-01', ktr=1, outfile=outfile)
}

#----------------------------------------------------------
#                   RyggFigGjsnGrVar
#----------------------------------------------------------

valgtVar <- 'SmBeinEndr' #'alder' 'Liggedogn', 'OswEndr', 'SmBeinEndr', 'SmRyggEndr'
outfile <- ''#paste0(valgtVar,enhetsUtvalg, '.png')	#paste0(valgtVar,enhetsUtvalg, '.pdf')

RyggFigGjsnGrVar(RegData=RegData, outfile=outfile, valgtVar=valgtVar, tidlOp=tidlOp, erMann=erMann, 
                         hovedkat=hovedkat, minald=minald, maxald=maxald, ktr=ktr, tittel=tittel, valgtMaal='Gjsn', 
                         datoFra=datoFra, datoTil=datoTil, aar=aar, enhetsUtvalg=enhetsUtvalg)
setwd('C:/ResultattjenesteGIT/nkr/test/')
variable <- c('alder', 'Liggedogn', 'OswEndr', 'SmBeinEndr', 'SmRyggEndr')
for (var in variable) {
      outfile <- paste0(var, 'GjsnSh.png')
      RyggFigGjsnGrVar(valgtVar=var, RegData=RegData, datoFra='2016-01-01', ktr=1, outfile=outfile)
}


valgtMaal <- 'Med'
#----------------------------------------------------------------------------------------------------------------
#-------- Andel per sykehus eller annen gr.variabel (AndelGrVar)-----------------------------------------
#----------------------------------------------------------------------------------------------------------------
valgtVar <- 'SympVarighUtstr'
outfile <- '' #paste0(valgtVar, 'Sh.pdf')
RyggFigAndelerGrVar(valgtVar=valgtVar, RegData=RegData, hovedkat = hovedkat, tidlOp=tidlOp,  Ngrense=20, opKat=opKat,
                    datoFra='2017-01-01', ktr=1, outfile=outfile)

variable <- c('alder70', 'Antibiotika', 'ArbstatusPre', 'Arbstatus', 'ASA', 'BeinsmLavPre', 
              'BeinsmEndrLav', 'BMI', 'degSponFusj', 'degSponSSSten', 'ErstatningPre', 'Fornoyd', 
              'KpInf3Mnd', 'Kp3Mnd', 'Misfornoyd', 'Nytte', 'OswEndrLav', 'OswEndr20', 'OswEndr30pst', 
              'Osw22', 'Osw48', 'PeropKompDura', 'Roker', 'Saardren', 'SmStiPre', 'SymptVarighRyggHof',
              'SympVarighUtstr', 'tidlOp3', 'UforetrygdPre', 'Utd', 'Verre')
for (var in variable) {
      outfile <- paste0(var, 'Sh.png')
      RyggFigAndelerGrVar(valgtVar=var, RegData=RegData, datoFra='2016-01-01', ktr=1, outfile=outfile)
}

#----------------------------------------------------------------------------------------------------------------
#-------- Andel per sykehus eller annen gr.variabel (AndelGrVar), samt siste 3 år-----------------------------------------
#----------------------------------------------------------------------------------------------------------------
#Lage bostedfil:
Innbyggere2007_2015kjonn <- read.table('./Innbyggere2007_2015kjonn.csv', sep=';', header = T, encoding = 'UTF-8')
BoStederInnb <- aggregate(Innbyggere2007_2015kjonn$AntInnb, 
                          by=Innbyggere2007_2015kjonn[ ,c('Kommune','KommNr', 'BoRHF','Fylke', 'Aar')], FUN='sum')#'BoHF', 
#Kommune 301 Oslo har fire BoHF. For øvrig kommunenummer bestemmer kommunenummer entydig BoHF.
BoStederInnb2015 <- BoStederInnb[which(BoStederInnb$Aar == '2015'),]
save(BoStederInnb2015, file="./data/BostederInnb2015.Rdata")
write.table(BoStederInnb2015, file="./data/BostederInnb2015.csv", sep=';')


rm(list=ls())
setwd("C:/ResultattjenesteGIT/nkr/")
library(nkr)

NKRdata <- read.table('C:/Registre/nkr/data/NKR2010_2015.csv', sep=';', header=T)

load(file="./data/BostederInnb2015.Rdata")
#For alders-og kjønnsstandardisering:
Innbyggere2015aldkj <- read.table('./Innbyggere2015aldkj.csv', sep=';', header = T, encoding = 'UTF-8')
#Innbyggere2015alder <- read.table('C:/VariasjonKvalitet/Innbyggere2015.csv', sep=';', header = T) 
#Innbyggere <- with(Innbyggere2015alder, aggregate('AntInnb', by=list('KommNr', 'BoHF', 'BoRHF','Fylke'), FUN='sum')
RegData <- merge(NKRdata, BoStederInnb2015, by.x = "Kommunenr", by.y = "KommNr", all.x = TRUE, all.y = FALSE)



valgtVar <- 'SympVarighUtstr'   #, #BeinsmEndrLav', BeinsmLavPre, DegSponSSSten, KpInf3Mnd
                              # OswEndr13, OswEndr20, OswEndr30pst, Osw48, SympVarighUtstr, Verre, 

#Indikatorprosjekt: BeinsmLavPre, OswEndr20
outfile <- paste0(valgtVar, '_', grVar,'Aar.pdf')

RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
#RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, siste3aar = 1,
                minald=minald, maxald=maxald, erMann=erMann, ktr=ktr, hovedkat=hovedkat, grVar=grVar,
                preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

variableInd <- c('BeinsmLavPre', 'OswEndr20','SympVarighUtstr')
variable <- c('Alder', 'Antibiotika', 'ArbstatusPre', 'Arbstatus3mnd', 'Arbstatus12mnd', 'ASA', 'BMI', 
             'ErstatningPre', 'Fornoyd3mnd','Fornoyd12mnd', 'Kp3Mnd', 'Misfor3mnd', 'Misfor12mnd', 
          'Nytte3mnd', 'Nytte12mnd', 'PeropKomp', 'Osw30_3mnd', 'Osw30_12mnd', 'PeropKompDura', 'Roker', 
          'Saardren', 'SmStiPre', 'SymptVarighRyggHof', 'SympVarighUtstr', 'UforetrygdPre', 'Utd', 'Verre3mnd', 'Verre12mnd')
#for (grVar in c('Fylke', 'ShNavn', 'BoRHF')){          
grVar <- 'ShNavn' #'BoRHF'
  for (valgtVar in variableInd) {
      outfile <- paste0(valgtVar, '_', grVar, 'Aar.pdf')
      RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                        minald=minald, maxald=maxald, erMann=erMann, hovedkat=hovedkat,ktr=ktr,
                        preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile,
                      grVar = grVar)
}
#}

#-------------------------------------------------------	
#---------AndelStabelTid
#-------------------------------------------------------	
#query = 'select Kjonn, HovedInngrep, HovedInngreptxt, Fornoyd3mnd, Fornoyd12mnd, TidlOpr,
#			Nytte3mnd, Nytte12mnd,
#			AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from nkr_test.dbo.Uttrekk_Rapport'

valgtVar <- 'TidlOp'	#velge TidlOp, Fornoyd eller Nytte
outfile <- ''#paste0(valgtVar, '.png')	#Navn angis av Jasper

RyggFigAndelStabelTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, hovedkat=hovedkat, preprosess=1,
                   minald = minald, maxald = maxald,
                        erMann=erMann, ktr=ktr, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, tittel=1, reshID=reshID)
AndelTidlOp <- RyggAndelStabelTid(RegData=RegDataLand, outfile='TidlOp.pdf', valgtVar='TidlOp', hovedkat=hovedkat,
                                  minald = minald, maxald = maxald,
                                  erMann=erMann, enhetsUtvalg=enhetsUtvalg, tittel=0, reshID=reshID)


for (valgtVar in c('TidlOp', 'Fornoyd', 'Nytte')) {
      outfile <- paste0(valgtVar, '.png')
      RyggAndelStabelTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, hovedkat=hovedkat, preprosess=1,
                         erMann=erMann, ktr=ktr, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, tittel=1, reshID=reshID)
}


#----------------------------------------------------------
#-----------------AndelStabelEgetLand-----------------------------
#----------------------------------------------------------
#Variabel: ASA, OpKat(Hastegrad) 
library(gridExtra)

rm(list=ls())
query = 'select ASA, OpKat, Kjonn, TidlOpr, HovedInngrep, HovedInngreptxt, 
Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato from nkr_test.dbo.Uttrekk_Rapport'

#__Inndata til funksjon:
reshID <- 100133	#999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
variabel <- 'OpKat'		#Må velges. 'ASA', 'OpKat'
kjonn <- 1			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
outfile <- paste(variabel, '.png', sep='')	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/RAndelStabelEgetLand/")

source("fun_AndelStabelEgetLand.R", encoding="UTF-8")
AndelStabelEgetLand(opdata=opdata, outfile=outfile, variabel=variabel,
                    kjonn=kjonn, hovedkat=hovedkat, aar=aar, reshID=reshID)




#----------------------------------------------------------
#-----------------AndelTidLinjer -----------------------------
#----------------------------------------------------------
#Variabel: Antibiotika, Friskmeldt (ArbStatus), Komplikasjoner

rm(list=ls())
KomplPer <- c('PeropKompDura, PeropKompFeilnivSide, PeropKompNerve, PeropKompTransfuBlodning,
			PeropKompKardio, PeropKompFeilplassImp, PeropKompResp,PeropKompAnafy, PeropKomp,')
KomplPost <- c('KpInfOverfla3Mnd,KpInfDyp3Mnd,KpMiktProb3Mnd,KpUVI3Mnd,
			KpLungebet3Mnd, KpBlod3Mnd,KpDVT3Mnd,KpLE3Mnd, Kp3Mnd,')
query = paste('select ', KomplPer, KomplPost, 'Utfylt3Mnd, ArbstatusPre, Arbstatus3mnd, Arbstatus12mnd,
		Antibiotika, Kjonn, HovedInngrep, TidlOpr, HovedInngreptxt, Inngrep, 
		Inngreptxt, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport', sep=' ')


#__Inndata til funksjon:
#(opdata - fra SQLspørring)
#Mulige variable: Antibiotika,KpBlod3Mnd, KpDVT3Mnd, KpInfDyp3Mnd,  KpInfOverfla3Mnd, 
#	KpLE3Mnd, KpLungebet3Mnd, KpMiktProb3Mnd, KpUVI3Mnd,
#	PeropKomp, PeropKompAnafy, PeropKompDura, PeropKompFeilnivSide,
#	PeropKompFeilplassImp, PeropKompKardio, PeropKompNerve, PeropKompResp, PeropKompTransfuBlodning,
#Ut: 'KpInfDyp12Mnd', 'KpSarinfUspesType3Mnd' 
rm(list=ls())
load('C:/RyggRegister BACKUP/DataUttrekk/NKR2013nov13.Rdata')
opdata$Aar <- opdata$OpAar
variabel <- 'Friskmeldt'	#'Antibiotika', 'Friskmeldt', hver kompl.variabel
reshID <- 601161	# #999999	#100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
tidlOp <- 4			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
ktr = 1
mT = 1			#Om man ønsker å vise antall for hvert år. 1 - ja.
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- paste(variabel, '1.png')

setwd("C:/registre/nkr/trunk/RAndelTidLinjer/")
source("fun_AndelTidLinjer.R", encoding="UTF-8")
AndelTidLinjer(opdata=opdata, variabel, outfile=outfile, reshID=reshID, 
		 tidlOp=tidlOp, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr, medTall=mT)	

		 KomplPer <- c('PeropKompDura', 'PeropKompFeilnivSide', 'PeropKompNerve', 'PeropKompTransfuBlodning',
			'PeropKompKardio','PeropKompFeilplassImp','PeropKompResp','PeropKompAnafy', 'PeropKomp')
KomplPost <- c('KpInfOverfla3Mnd','KpInfDyp3Mnd','KpMiktProb3Mnd','KpUVI3Mnd',
			'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd', 'Kp3Mnd')
for (i in 1:length(c(KomplPer, KomplPost))) {
variabel <- c(KomplPer, KomplPost)[i]
outfile <- paste(variabel, '.pdf', sep='')	#Navn angis av Jasper
source("fun_AndelTidLinjer.R", encoding="UTF-8")
AndelTidLinjer(opdata=opdata, variabel, outfile=outfile, reshID=reshID, 
		 tidlOp=tidlOp, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr, medTall=mT)	
}

#-------------------------------------------------------------------
#			AntSoyler
#-------------------------------------------------------------------
query = 'select TidlOpr, Kjonn, HovedInngrep, HovedInngreptxt, 
		AvdReshID, AvdNavn, OpDato from nkr_test.dbo.Uttrekk_Rapport'
AntSoyler(opdata=opdata, outfile=outfile, egenavd=egenavd, variabel=variabel, 
		hovedkat=hovedkat, aar=aar, kjonn=kjonn, reshID=reshID)

#----------------------------------------------------------
#                FordPrePost
#----------------------------------------------------------
rm(list=ls())
setwd('C:/RyggRegister/Rapport')

query = 'select PID, Kjonn, HovedInngrep, HovedInngreptxt, 
		EQ5DPre, EQ5D3mnd, EQ5D12mnd, OswTotPre, OswTot3mnd, OswTot12mnd,
		SmRyPre, SmRy3mnd, SmRy12mnd, SmBePre, SmBe3mnd, SmBe12mnd, 
		Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'


#__Inndata til funksjon:
#(opdata - fra SQLspørring)
setwd("C:/RyggRegister/Rapport/jasper/nkr/trunk/RFordPrePost/")
reshID <- 601161 	#999999	#100407	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
egenavd <- 0
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
aar <- 2011
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
ktr <- 1			#1. el 2. kontroll. '3mnd' - 3mnd kontroll, '12mnd' - 12 mnd kontroll
plotType <- 'S'	#'L' el. 'S'. Søyler er standard.
valgtVar <- 'EQ5D'			#EQ5D, Oswestry, SmBein, SmeRygg

for (variabel in c('EQ5D', 'Oswestry', 'SmBein', 'SmRygg')) {
#variabel <- c(KomplPer, KomplPost)[i]
outfile <- paste(variabel,'FEs.pdf', sep='')
source("fun_FordPrePost.R", encoding="UTF-8")
FordPrePost(opdata=opdata, outfile=outfile, reshID=reshID, egenavd=egenavd,
		variabel=variabel, plotType=plotType, aar=aar, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr)
}

#----------------------------------------------------------
#--------SoyleSml, differanse m/konf.int. gj.sn., alle s-hus
#----------------------------------------------------------

rm(list=ls())
library(RODBC)
query = 'select OswTotPre, OswTot3mnd, OswTot12mnd, EQ5DPre, EQ5D3mnd, EQ5D12mnd,
		Alder, Kjonn, HovedInngrep, HovedInngreptxt, TidlOpr, Inngrep, Inngreptxt, 
			AvdID, AvdReshID, AvdNavn, OpDato, ASA, Utd from Uttrekk_Rapport'

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#111065 #999999	#105783	#103618	#102949	#   #Må sendes med til funksjon
variabel <- 'OswEndr'	#Må velge. EQ5DEndr, OswEndr
aar <- 0		#Standard: 0, dvs. alle år
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- ''	#paste(variabel,'.png', sep='')	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
tidlOp <- 0			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
#shgr <- 1			#0-hele landet (standard), 1-univ.sh, 2-off.sh., 3-pri
#just <- 0

setwd("C:/RyggRegister/Rapport/jasper/nkr/trunk/RSoyleSml/")
source("fun_SoyleSml.R", encoding="UTF-8")
test <- SoyleSml(opdata=opdata, outfile=outfile, reshID=reshID, variabel=variabel,
		tidlOp=tidlOp, aar=aar, kjonn=kjonn, shgr=0, hovedkat=hovedkat, ktr=ktr, just=0)
#For å kunne standardisere, må hele datamaterialet sendes med

#-----------------------------------------------------------------------------------------------------


#---------------------------------------------------------------
#				Kvalitetskontroll av data
#---------------------------------------------------------------
library(xtable)
require(RJDBC)
Sweave('KvalKtrAvData.Rnw')

#---------------------------KVALITETSSJEKK-----------------------------
RegData <- RyggPreprosess(RegData=RegData)
#Dobbeltregistrering
PIDop <- table(RegData$PID, RegData$OpDato)
names(PIDop[which(PIDop>1)])
testDato <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpDato')], drop=TRUE, FUN=length)
test[which(test$x >1), ]
testMnd <- aggregate(RegData$InnDato, by=RegData[ ,c('PID','Mnd','OpAar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
testAar <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpAar')], drop=TRUE, FUN=length)
sum(testAar$x >1)

#------------------------------ Data til NPR, Dekningsgradsanalyse-------------
load('A:/Rygg/NKR2010-2017aarsrapp.Rdata')
query <- 
      'SELECT loepenummer, Alder, Kjonn, HFNavn, HFID, HFReshID, AvdNavn, AvdID, AvdReshID, 
OpDato, Dagkirurgi, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, ProsKode1, ProsKode2
FROM Uttrekk_Rapport
WHERE OpAar=2017'
# variable <- c('PID', 'Alder', 'Kjonn', 'HFNavn', 'HFID', 'HFReshID', 
#               'AvdNavn', 'AvdID', 'AvdReshID', 'OpDato', 'Dagkirurgi', 
#               'HovedInngrep', 'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 'ProsKode1', 'ProsKode2')
NKR2017 <- read.table('A:/Rygg/NPR2017.csv', sep=';', header=T, encoding = 'UTF-8')
NKR2017 <- RegData[which(NKR2017$OpDato<2017), ]
sort(variable)[which(!(sort(variable) %in% sort(names(RegData))))]


