
> Prod heter tos-hreg-db-01.nreg.no\HELSEREG For å få kontakt, prøv en 
> av disse:
> tos-hreg-db-01.nreg.no\HELSEREG
> 172.29.180.20, port:49375
> 172.29.180.20\HELSEREG
>
>
> Test heter tos-hreg-db-01.nreg.no\TESTINSTANCE
> For å få kontakt, prøv en av disse:
> tos-hreg-db-01.nreg.no\TESTINSTANCE
> 172.29.180.20, port:50476
> 172.29.180.20\TESTINSTANCE


	
#Må laste inn fil når ikke koblet mot database:
#Eksempel:
setwd('C:/RyggRegister/DataUttrekk')
opdata <- read.table('Arbfil.csv', header=T,sep=';')
opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
load('C:/RyggRegister/NKR.RData')
opdata <- opdataALLE[,varnames]
opdata$Aar <- substr(opdata$OpDato,7,11)

require(RJDBC)
library(rJava)
library(RODBC)
_________________________________________________________________________________________
#_________________________________________________________________________________________

#					S A M L E R A P P O R T E R, 
#					dvs kobling av tekst og figurer, tabeller osv.
#_________________________________________________________________________________________
#_________________________________________________________________________________________
For å klargjøre R for bruk av JDBC ved spørringer mot databasen, så må
noen ekstra pakker installeres. Siden JDBC er basert på Javateknologi trenger
vi R klargjort for java:

install.packages('rJava')
install.packages('RJDBC')
install.packages('xtable')
#SweaveOpts{opt1=value1, opt2=value2, ..., optN=valueN}
#Funker ikke nå...: options(encoding='utf-8')
Trenger pakkene:
library(grid)
library(gridExtra)

#---------------------------------------------------------------
#				Kvalitetskontroll av data
#---------------------------------------------------------------
library(rJava)
library(RJDBC)
library(xtable)
----------------

rm(list=ls())
require(RJDBC)

svnRootPath <- 'C:/RyggRegister/Rapport/jasper/nkr/trunk/'
#setwd('C:/RyggRegister/Rapport/nkr/trunk/RKvalKtrAvData')
setwd(paste(svnRootPath, 'RKvalKtrAvData', sep=''))
source("C:/Registre/Rlib/trunk/fun_LeseInnFun.R", encoding="UTF-8")
LeseInnFun(server=0)
#setwd(svnRootPath)
reshID <- 105783
Sweave('KvalKtrAvData.Rnw')

#Direkte i R:
#texi2dvi('KvalKtrAvData.tex', pdf=T, clean=T)
#Sweave('test.Rnw')


#----------------------------------------------------------------------------
#				Samlerapport (sammendrag resultater for Rygg)
#-----------------------------------------------------------------------------
library(rJava)
#library(RJDBC)
library(xtable)
#library(RODBC)
#------------------------------------
#EKSEMPEL-------------------------------------
rm(list=ls())
require(RJDBC)
svnRootPath <- 'C:/RyggRegister/Rapport/jasper/nkr/trunk/'
setwd(paste(svnRootPath, 'RSamleRappNKR', sep=''))
Sweave('Eksempel.Rnw')
#------------------------------------

rm(list=ls())
setwd('C:/Registre/nkr/trunk/RSamleRappNKR')
library(tools)
svnRootPath <- 'C:/Registre/nkr/trunk/'
libkat <- 'C:/Registre/Rlib/trunk/'
server=0
setwd(paste(svnRootPath, 'RSamleRappNKR', sep=''))

source("C:/Registre/Rlib/trunk/fun_LeseInnFun.R", encoding="UTF-8")
LeseInnFun(server=0, libkat=libkat)

reshID <- 102224 #Haukeland nevr.kir: 105588, NIMI:  104279, Unn: 601161, St Olav: 105783 Namsos:105899, 
	#Lillehammer: 111185, Gjøvik: 111150, Rana ortopediske avd. 102224, Ullevål, ortopediske avd. 999995
funkat <- libkat
opdataALLE <- read.table('C:/Registre/nkr/data/NKR2015-03-02.csv', header = T, sep = ";")

figKat <-paste('Avd',reshID,'/',sep='')
dir.create(figKat, showWarnings = F)


Sweave('SamleRappNKR.Rnw')

#AlleResh <- c(100316, 100407, 100408, 100415, 100968, 102021, 102040, 102467, 102483, 102949, 103001, 
#103015, 103094, 103240, 103469, 103618, 103948, 104279, 105153, 105403, 105588, 105783, 105798, 105899, 
#106095, 107137, 107240, 107508, 107511, 107981, 108169, 108338, 110633, 110771, 111127, 111150, 111185, 
#111961, 114288, 420115, 601160, 601161, 701403, 707437, 999975, 999994, 999998, 999999, 4201352, 4205477, 
#4207427, 4208066)

#Oppdater for å unngå sykehus uten registreringer
AlleResh <- c(1491, 100133, 100407, 100415, 100968)
, 102224, 102467, 102483, 102484, 102949, 103094, 
	103240, 103469, 103618, 104279, 105153, 105403, 105588, 105783, 105798, 105899, 107137, 107240, 
	107508, 107511, 107981, 109820, 110633, 110771, 111065, 111127, 111150, 111185, 111961, 114288, 
	601161, 999900, 999920, 999975, 999976, 999978, 999994, 999995, 999998, 999999)
#Sweave('SamleRappNKR.Rnw')
#Avd <- opdataALLE$AvdNavn[match(reshID, opdataALLE$AvdResh)]
for (reshID in AlleResh ) {
	Sweave('SamleRappNKR.Rnw')
	texi2pdf(file='SamleRappNKR.tex')
	file.rename('SamleRappNKR.pdf', paste0('SamleRappNKR', reshID, '.pdf'))	#list.files(pattern="water_*.img", paste0("water_", 1:700))
}


#Hele landet:
rm(list=ls())
setwd('C:/Registre/nkr/trunk/RSamleRappNKR')
#svnRootPath <- 'C:/RyggRegister/Rapport/'	#På server må /jasper inn i tillegg
svnRootPath <- 'C:/Registre/nkr/trunk/'
setwd(paste(svnRootPath, 'RSamleRappNKR', sep=''))
libkat <- 'C:/Registre/Rlib/trunk/'
source("C:/Registre/Rlib/trunk/fun_LeseInnFun.R", encoding="UTF-8")
LeseInnFun(server=0, libkat=libkat)
source("C:/Registre/nkr/RyggFigURappSyst/fun_LinjerTidRHF.R", encoding="UTF-8")
source("C:/Registre/nkr/RyggFigURappSyst/fun_RyggAndelSoylerAvd.R", encoding="UTF-8")

opdataALLE <- read.table('C:/Registre/nkr/data/NKR2014-09-16.csv', header = T, sep = ";")
Sweave('SamleRappNKRHeleLandet.Rnw')



#_________________________________________________________________________________________
#_________________________________________________________________________________________
#
#""""""""""""""""""""""""""""" F I G U R F U N K S J O N E R """"""""""""""""""""""""""""""""""""" 
#_________________________________________________________________________________________


#----------------------------------------------------------------
#			FigAndeler (RyggFigAndeler.R) [erstatter AndelSoyler]
#----------------------------------------------------------------
rm(list=ls())
NKRdata <- read.table('C:/Registre/nkr/data/NKR2015-03-02.csv', sep=';', header=T)
RegData <- NKRdata
#__Inndata til RyggFigAndeler.R:
tittel=1
reshID <- 601161 #999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
datoFra <- '2007-01-01'
datoTil <- '2016-12-31'
minald <- 0		#alder, fra og med
maxald <- 130	#alder, til og med
erMann <- ''			#kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
hovedkat <- 1 		#HovedInngrep, 0-7, Standard: 99, dvs alle op
enhetsUtvalg <- 0 #	0-hele landet, 1-egen enhet mot resten av landet, 2-egen enhet
#					3–egen enhet mot egen shusgruppe, 4–egen shusgruppe, 5–egen shusgruppe mot resten
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten

valgtVar <- 'Alder'	#Må velge...  'Alder', 'Antibiotika', 'AntNivOpr', 'ArbstatusPre', 'Arbstatus3mnd', 
#      'Arbstatus12mnd', 'ASA', 'BMI', 'EqangstPre', 'EqgangePre', 'ErstatningPre', 'Fornoyd12mnd','Fornoyd3mnd', 
#      'HovedInngrep', 'Komorbiditet', 'KomplPer', 'KomplPost', 'Liggedogn', 'Morsmal', 'Nytte3mnd', 'Nytte12mnd', 
#      'OpIndPareseGrad', 'OpInd', 'OpIndSmeType', 'OpKat', 'RadUnders', 
#      'Roker', 'SivilStatus','SmStiPre', 'SmHyppPre', 'SymptVarighRyggHof', 
#      'SympVarighUtstr', 'Saardren', 'TidlOpr', 'TidlOprAntall','UforetrygdPre', 'Utd', 'Underkat'
#NB: Hvis variabel='Underkat', MÅ hovedkat velges, dvs. ikke 99.

setwd("C:/ResultattjenesteGIT/nkr")
outfile <- ''	#paste(valgtVar, '.pdf', sep='')	#Navn angis av Jasper ''
FigAndeler(RegData=NKRdata, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
		minald=minald, maxald=maxald, erMann=erMann, hovedkat=hovedkat, preprosess=1,
		 enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

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
		 

#----------------------------------------------------------
#                   RyggFigGjsnTid 
#----------------------------------------------------------
rm(list=ls())
setwd("C:/ResultattjenesteGIT/nkr/")

NKRdata <- read.table('C:/Registre/nkr/data/NKR2015-03-02.csv', sep=';', header=T)
RegData <- NKRdata

#__Inndata til funksjon:
valgtVar <- 'Liggedogn' #EQ5DPre, OswTotPre, SmBePre, SmRyPre, 
						#EQ5DEndr, Liggedogn, OswEndr, SmRyggEndr, SmBeinEndr,  
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2009-01-03'
datoTil <- '2015-11-03'
erMann <- ''
minald <- 0
maxald <- 130
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
ktr <- 2			#1. el 2. kontroll. '3mnd' - 3mnd kontroll, '12mnd' - 12 mnd kontroll
tittel <- 1
tidlOp <- 0			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
valgtMaal <- ''
enhetsUtvalg <- 1 # 0-hele landet, 1-egen enhet mot resten av landet (standard), 2-egen enhet
#					3–egen enhet mot egen shusgruppe, 4–egen shusgruppe, 5–egen shusgruppe mot resten
#					6–egen enhet mot egen region, 7–egen region, 8–egen region mot resten
outfile <- paste0(valgtVar,enhetsUtvalg, '.png')	#paste0(valgtVar,enhetsUtvalg, '.pdf')

GjsnTid(RegData=RegData, outfile=outfile, valgtVar=valgtVar, tidlOp=tidlOp, erMann=erMann, 
		hovedkat=hovedkat, minald=minald, maxald=maxald, ktr=ktr, tittel=tittel, valgtMaal=valgtMaal, 
		datoFra=datoFra, datoTil=datoTil, enhetsUtvalg=enhetsUtvalg, reshID=reshID)
		
#query = 'select PID, Kjonn, Alder, Liggedogn, Dagkirurgi, TidlOpr, 
#		EQ5DPre, EQ5D3mnd, EQ5D12mnd, OswTotPre, OswTot3mnd, OswTot12mnd,
#		SmRyPre, SmRy3mnd, SmRy12mnd, SmBePre, SmBe3mnd, SmBe12mnd, 
#		HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, 
#		AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from nkr_test.dbo.Uttrekk_Rapport'
		
for (valgtVar in c('Liggedogn','EQ5DPre', 'OswTotPre', 'SmBePre', 'SmRyPre',
			'EQ5DEndr', 'OswEndr', 'SmBeinEndr', 'SmRyggEndr')) {
	outfile <- paste0(valgtVar,'Gjsn.png')
	GjsnTid(RegData = NKRdata, outfile=outfile, reshID=reshID, valgtVar=valgtVar, ktr=ktr)
}


#----------------------------------------------------------------------------------------------------------------
#-------- Andel per sykehus eller annen grupperingsvariabel (AndelGrVar)-----------------------------------------
#----------------------------------------------------------------------------------------------------------------

rm(list=ls())
setwd("C:/ResultattjenesteGIT/nkr/")

NKRdata <- read.table('C:/Registre/nkr/data/NKR2010_2016-05-18.csv', sep=';', header=T)
RegData <- NKRdata

#__Inndata til funksjon:
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2009-01-03'
datoTil <- '2015-11-03'
erMann <- ''
minald <- 0
maxald <- 130
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
tidlOp <- 4			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
valgtVar <- 'Alder'
outfile <- paste0(valgtVar, '.png')

FigAndelerGrVar(RegData=NKRdata, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                minald=minald, maxald=maxald, erMann=erMann, hovedkat=hovedkat,
                preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


variable <- c('Alder', 'Antibiotika', 'ArbstatusPre', 'Arbstatus3mnd', 'Arbstatus12mnd', 'ASA', 'BMI', 
              'ErstatningPre', 'Fornoyd3mnd','Fornoyd12mnd', 'Kp3Mnd', 'Misfor3mnd', 'Misfor12mnd', 
          'Nytte3mnd', 'Nytte12mnd', 'PeropKomp', 
          
variable <- c('PeropKompDura', 'Osw30_3mnd', 'Osw30_12mnd','Roker', 'Saardren', 'SmStiPre', 'SymptVarighRyggHof', 'SympVarighUtstr', 'UforetrygdPre', 
              'Utd', 'Verre3mnd', 'Verre12mnd') 
for (valgtVar in variable) {
      outfile <- paste0(valgtVar, '.png')
      FigAndelerGrVar(RegData=NKRdata, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                        minald=minald, maxald=maxald, erMann=erMann, hovedkat=hovedkat,
                        preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
}


#----------------------------------------------------------
#-----------------AndelStabelEgetLand-----------------------------
#----------------------------------------------------------
Variabel: ASA, OpKat(Hastegrad) 
library(gridExtra)

rm(list=ls())
library(RODBC)
#channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
channel = odbcConnect("nkr_test", "WebUser_nkr", "Querken8")
query = 'select ASA, OpKat, Kjonn, TidlOpr, HovedInngrep, HovedInngreptxt, 
			Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato from nkr_test.dbo.Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon
odbcCloseAll()

#__Inndata til funksjon:
reshID <- 100133	#999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
variabel <- 'OpKat'		#Må velges. 'ASA', 'OpKat'
aar <- 2012	#Standard: 0, dvs. alle år
kjonn <- 1			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
outfile <- paste(variabel, '.png', sep='')	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/RAndelStabelEgetLand/")

source("fun_AndelStabelEgetLand.R", encoding="UTF-8")
AndelStabelEgetLand(opdata=opdata, outfile=outfile, variabel=variabel,
		kjonn=kjonn, hovedkat=hovedkat, aar=aar, reshID=reshID)


		 
		 
#-------------------------------------------------------	
#---------AndelStabelTid
#-------------------------------------------------------	
variable: TidlOp, Fornoyd, Nytte
library(RODBC)
rm(list=ls())
channel = odbcConnect("nkr_test", "WebUser_nkr", "Querken8")
query = 'select Kjonn, HovedInngrep, HovedInngreptxt, Fornoyd3mnd, Fornoyd12mnd, TidlOpr,
			Nytte3mnd, Nytte12mnd,
			AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from nkr_test.dbo.Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon
close(channel)

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 100133	#601161 #999999	#100133	#111065 #105783	#103618	#   #Må sendes med til funksjon
variabel <- 'TidlOp'	#velge TidlOp, Fornoyd eller Nytte
egenavd <- 1		#0-hele landet, 1-egen avdeling
kjonn <- 0		#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs typer inngrep
tidlOp <- 0		#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
outfile <- paste(variabel, '.png', sep='')	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
setwd("C:/RyggRegister/Rapport/jasper/nkr/trunk/RAndelStabelTid/")

source("fun_AndelStabelTid.R", encoding="UTF-8")
for (variabel in c('TidlOp', 'Fornoyd', 'Nytte')) {
AndelStabelTid(opdata=opdata, outfile=paste(variabel, '.png', sep=''), variabel=variabel, 
kjonn=kjonn, hovedkat=hovedkat, tidlOp=tidlOp, egenavd=egenavd, ktr=ktr, reshID=reshID)
}


#----------------------------------------------------------
#-----------------AndelTidLinjer -----------------------------
#----------------------------------------------------------
Variabel: Antibiotika, Friskmeldt (ArbStatus), Komplikasjoner

rm(list=ls())
library(RODBC)
setwd("C:/RyggRegister/Rapport/jasper/nkr/trunk/RAndelTidLinjer/")
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
#channel = odbcConnect("nkr_test", "WebUser_nkr", "Querken8")
KomplPer <- c('PeropKompDura, PeropKompFeilnivSide, PeropKompNerve, PeropKompTransfuBlodning,
			PeropKompKardio, PeropKompFeilplassImp, PeropKompResp,PeropKompAnafy, PeropKomp,')
KomplPost <- c('KpInfOverfla3Mnd,KpInfDyp3Mnd,KpMiktProb3Mnd,KpUVI3Mnd,
			KpLungebet3Mnd, KpBlod3Mnd,KpDVT3Mnd,KpLE3Mnd, Kp3Mnd,')
query = paste('select ', KomplPer, KomplPost, 'Utfylt3Mnd, ArbstatusPre, Arbstatus3mnd, Arbstatus12mnd,
		Antibiotika, Kjonn, HovedInngrep, TidlOpr, HovedInngreptxt, Inngrep, 
		Inngreptxt, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport', sep=' ')
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon
close(channel)


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
library(RODBC)
rm(list=ls())
channel = odbcConnect("nkr_test", "WebUser_nkr", "Querken8")
query = 'select TidlOpr, Kjonn, HovedInngrep, HovedInngreptxt, 
		AvdReshID, AvdNavn, OpDato from nkr_test.dbo.Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon
close(channel)

#Inngrep, Inngreptxt, 

#__Inndata til funksjon:
reshID <- 601161    #Standard: alle data (egenavd = 0)	601161
variabel <- 'Mnd'	#Må velges. 'Mnd', 'HovedInngrep'
egenavd <- 1		#Angir om skal ha data for egen avdeling eller alle. Standard:0, dvs. alle sykehus
aar <- 0	#Standard: alle år (verdien 0)
kjonn <- 0			#1- menn, 2 - kvinner, Standard: 0, dvs. begge kjønn
hovedkat <- 1		#Hovedkategori, 0-7. Alle kategorier: 99
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- ''	#paste(variabel,'.png', sep='')	#Navn angis av Jasper

setwd("C:/RyggRegister/Rapport/jasper/nkr/trunk/RAntSoyler/")
source("fun_AntSoyler.R", encoding="UTF-8")
AntSoyler(opdata=opdata, outfile=outfile, egenavd=egenavd, variabel=variabel, 
		hovedkat=hovedkat, aar=aar, kjonn=kjonn, reshID=reshID)






#----------------------------------------------------------
#                FordPrePost
#----------------------------------------------------------
rm(list=ls())
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr_test", "WebUser_nkr", "Querken8")
query = 'select PID, Kjonn, HovedInngrep, HovedInngreptxt, 
		EQ5DPre, EQ5D3mnd, EQ5D12mnd, OswTotPre, OswTot3mnd, OswTot12mnd,
		SmRyPre, SmRy3mnd, SmRy12mnd, SmBePre, SmBe3mnd, SmBe12mnd, 
		Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon
close(channel)


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
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select OswTotPre, OswTot3mnd, OswTot12mnd, EQ5DPre, EQ5D3mnd, EQ5D12mnd,
		Alder, Kjonn, HovedInngrep, HovedInngreptxt, TidlOpr, Inngrep, Inngreptxt, 
			AvdID, AvdReshID, AvdNavn, OpDato, ASA, Utd from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

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





		 

#-----------------------------------------------------------------
#		EffDiffAvd
#----------------------------------------------------------------

rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query <- 'select AvdReshID, AvdNavn, right(OpDato, 4) as Aar, Kjonn, 
HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, EQ5DPre, EQ5D3mnd, EQ5D12mnd, 
OswTotPre, OswTot3mnd, OswTot12mnd, SmBePre, SmBe3mnd, SmBe12mnd, 
SmRyPre, SmRy3mnd, SmRy12mnd, Nytte3mnd, Nytte12mnd, Fornoyd3mnd, Fornoyd12mnd from Uttrekk_Rapport'  
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
aar <- 	2009	#Standard: alle år (verdien 0)
hovedkat <- 1		#Numerisk, 0-7, standard: 99 - operasjoner
inngrepstype <- 1
kjonn <- 0		#1-menn, 2-kvinner, Standard: 0, dvs. begge
reshID <- 601161    #Hentes fra innlogging	111185, 601161  
egen.resh.id <-    reshID
kontroll <- 1
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- "EffDiffAvd.png"

setwd("C:/RyggRegister/Rapport/nkr/trunk/EffDiffAvd/")
source("fun_EffDiffAvd.R", encoding="UTF-8")
EffDiffAvd(op.data=opdata, outfile=outfile, aar=aar, kjonn=kjonn, hovedinngrep=hovedkat, 
		inngrepstype=inngrepstype, kontroll=ktr, egen.resh.id=reshID) 




#----------------------------------------------------------
#-------- Endr i effektmål som funksjon av prescore--------------
#----------------------------------------------------------

rm(list=ls())
library(RODBC)
setwd("C:/RyggRegister/Rapport/jasper/nkr/trunk/RBoxTid/")
#channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
channel = odbcConnect("nkr_test", "WebUser_nkr", "Querken8")
query = 'select PID, Kjonn, Alder, TidlOpr, 
		EQ5DPre, EQ5D3mnd, EQ5D12mnd, OswTotPre, OswTot3mnd, OswTot12mnd,
		SmRyPre, SmRy3mnd, SmRy12mnd, SmBePre, SmBe3mnd, SmBe12mnd, 
		HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, 
		AvdID, AvdReshID, AvdNavn, OpDato, right(OpDato, 4) as Aar from nkr_test.dbo.Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon
close(channel)

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
variabel <- 'OswEndr'	#EQ5DEndr, OswEndr, SmRyggEndr, SmBeinEndr,  
reshID <- 601161	#999995	#999999	#100407	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
aar <- 0	#Standard: 0, dvs. alle Ã¥r
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
ktr <- 2			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
tidlOp <- 0			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner

for (variabel in c('EQ5DEndr', 'OswEndr', 'SmBeinEndr', 'SmRyggEndr')) {
	outfile <- paste(variabel,'Pre.pdf', sep='')	#Navn angis av Jasper
	source("fun_PreEndr.R", encoding="UTF-8")
	PreEndr(opdata=opdata, outfile=outfile, reshID=reshID, aar=aar,
		variabel=variabel, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr)
}






			
#-----------------------------------------------------------------------------------------------------
#""""""""""""""""""""""""""" UNDER UTVIKLING """"""""""""""""""""""""""""""""""""""""""""""""
#-----------------------------------------------------------------------------------------------------
		
#----------------- Reoperasjon innen 90 dager ---------------------------
Reop90d==1, unntatt diff(opdato)>90 for samme PID
UNION 
diff(opdato)<91 & diff(opdato)>0 for samme PID

query = 'select Kjonn, HovedInngrep, HovedInngreptxt, TidlOpr, ...
			AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'

rm(list=ls())
setwd('C:/RyggRegister/Rapport')
opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
varnames = c('Reop90d', 'TidlOpr', 'Kjonn', 'PID', 'HovedInngrep', 
		'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 'AvdReshID', 'AvdNavn', 'OpDato') 
opdata <- opdataALLE[,varnames]
opdata$Aar <- substr(opdata$OpDato,7,11)

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#999999	#601161 #111065 #105783	#103618	#   #Må sendes med til funksjon
egenavd <- 1		#0-hele landet, 1-egen avdeling
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
#tidlOp <- 0			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'Reop90.png'	#Navn angis av Jasper


setwd("C:/RyggRegister/Rapport/nkr/trunk/Reop90/")
source("fun_Reop90.R", encoding="UTF-8")
Reop90(opdata=opdata, outfile=outfile, kjonn=kjonn, hovedkat=hovedkat, 
	egenavd=egenavd, reshID=reshID)








---------Div  saker og ting---------
Unn: 601161, St Olav: 105783 og 105899, SKDE: 105678
Lillehammer: 111185, Gjøvik: 111150

hist(Inngrep[HovedInngrep==1][])

#__Hente sykehusnavn__
if (AvdReshID=='*') {Shusnavn <- 'Hele landet'} else {
	channel2 <- odbcConnect("HregUserDb", "RapportUser_nkr", "Pitkeep7")
	spShus <- paste('select Navn from Avdeling where RESHID=',AvdReshID)	#use[HregUserDb] 
	Shus <- sqlQuery(channel2, spShus) #...Det enkleste er om denne kunne komme som en tekststreng fra Jasper
	Shusnavn <- as.character(Shus$Navn)}

	query = 'select Kjoenn_Ia, HovedInngrep,Inst_Ia,InstNavn from Uttrekk_Rapport 
		where right(Operasjonsdato, 4)=2008'

		#Velg ut s-hus som har noen over 30, andre får 0 / eller vises ikke...
match(Osw_sh$names,Osw_sh30$names)
 [1]  1  2  3  4  5 NA  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20
#query = scan("C:/Rygg/nkr/hovedkatAnt/sql/hovedkatAnt.sql", what=raw, skip=0, quiet=TRUE)


*******************************************************************************************
*****************************ENDREDE FUNKSJONER
*******************************************************************************************

#---------------------------------------------------------------
#		Aldersfordeling, hist, eget s-hus kontra alle - ERSTATTET av AndelSoyler
#---------------------------------------------------------------
#varnames = c('Alder', 'Kjonn', 'HovedInngrep', 'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 
#			'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]

rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Alder, Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, ASA,
			AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon
odbcClose(channel)

#__Inndata til funksjon:
aar <- 	0	#Standard: alle år (verdien 0)
kjonn <- 0
hovedkat <- 1		#Numerisk, 0-7, standard: 99 - operasjoner
reshID <- 601161    #Hentes fra innlogging	111185, 601161     
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'Aldersfordeling.png'	#Navn angis av Jasper

setwd("C:/RyggRegister/Rapport/nkr/trunk/Aldersford/")
source('fun_Aldersford.R')	#, encoding="UTF-8")
Aldersford(opdata=opdata, kjonn=kjonn, outfile=outfile, 
		aar=aar, hovedkat=hovedkat, reshID=reshID)


		
#---------------------------------------------------------------------------------------
#-----------------Antibiotika  - ERSTATTET av AndelTidLinjer
#---------------------------------------------------------------------------------------
Variabel: Antibiotika
#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('Antibiotika', 'Kjonn', 'HovedInngrep', 'TidlOpr',
#		'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]
#opdata$Aar <- substr(opdata$OpDato,7,11)

rm(list=ls())
library(RODBC)
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Antibiotika, Kjonn, HovedInngrep, TidlOpr, HovedInngreptxt, Inngrep, 
		Inngreptxt, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon


#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #999999	#100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
tidlOp <- 4			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
mT = 1			#Om man ønsker å vise antall for hvert år. 1 - ja.
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'Antibiotika.png'	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/Antibiotika/")

source("fun_Antibiotika.R", encoding="UTF-8")
Antibiotika(opdata=opdata, outfile=outfile, reshID=reshID, 
		 tidlOp=tidlOp, kjonn=kjonn, hovedkat=hovedkat, medTall=mT)	

		 
		
#----------------------------------------------------------
#-----------------Arbeidsstatus - ERSTATTET av AndelTidLinjer
#----------------------------------------------------------


rm(list=ls())
library(RODBC)
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select ArbstatusPre, Arbstatus3mnd, Arbstatus12mnd, Kjonn, HovedInngrep, HovedInngreptxt, 
			Inngrep, Inngreptxt, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 100133	#601161 #102949	#999999	#111065 #105783	#103618	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 2 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
#(medTall = 1	#Mest for egen kontroll, trenger ikke være med
ktr = 2
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'ArbStatus.png'	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/ArbStatus/")

source("fun_ArbStatus.R", encoding="UTF-8")
ArbStatus(opdata=opdata, outfile=outfile, reshID=reshID, 
		 ktr=ktr, kjonn=kjonn, hovedkat=hovedkat) #, medTall=1)	



#----------------------------------------------------------
#-----------------ASA-grad - ERSTATTET av AndelStabelEgetLand
#----------------------------------------------------------
Variabel: ASA
#...
#opdata <- opdataALLE[,varnames]
#opdata$Aar <- substr(opdata$OpDato,7,11)

rm(list=ls())
library(RODBC)
library(gridExtra)
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
#query = 'select  ASA, Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, 
#		Inngreptxt, AvdReshID, AvdNavn, OpDato,  right(OpDato, 4) as Aar from Uttrekk_Rapport'
query = 'select  ASA, Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, 
		Inngreptxt, AvdReshID, AvdNavn, OpDato,  right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon
odbcCloseAll()

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161	#999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
aar <- 2010		#Standard: 0, dvs. alle år
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'ASA.png'	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/ASA/")

source("fun_ASA.R", encoding="UTF-8")
ASA(opdata, libkat, outfile, hovedkat=hovedkat, kjonn=kjonn, aar=aar, reshID)	


		 
#--------------------------------------------------------------
#				BMI, hist, eget s-hus kontra alle - ERSTATTET av AndelSoyler
#--------------------------------------------------------------

#setwd('C:/RyggRegister/Rapport')
#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('BMI', 'Kjonn', 'HovedInngrep', 'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 
#			'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]
#Spørring mot SQL:

rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select BMI, Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, 
			AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport' 
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon



#__Inndata til funksjon:
aar <- 	0	#Standard: alle år (verdien 0)
hovedkat <- 1		#Numerisk, 0-7, standard: 99 - operasjoner
kjonn <- 2		#1-menn, 2-kvinner, Standard: 0, dvs. begge
reshID <- 601161    #Hentes fra innlogging	111185, 601161     
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'BMI.png'	#Navn angis av Jasper

setwd("C:/RyggRegister/Rapport/nkr/trunk/BMI/")
source('fun_BMI.R')	#, encoding="UTF-8")
BMI(opdata=opdata, outfile=outfile, aar=aar, hovedkat=hovedkat, kjonn=kjonn, reshID=reshID)
setwd("C:/Rygg/")
}
		 
		 
#-------------------------------------------------------------------------------
#		Hastegrad (Operasjonskategori) - ERSTATTET av AndelStabelEgetLand
#-------------------------------------------------------------------------------

rm(list=ls())
setwd('C:/RyggRegister/Rapport')
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select OpKat, Kjonn, HovedInngrep, TidlOpr, HovedInngreptxt, 
			Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #999999	#100133	#111065 #105783	#103618	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs typer inngrep
aar <- 2012		#Standard: 0, dvs. alle år
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'Hastegrad.png'	#Navn angis av Jasper


setwd("C:/RyggRegister/Rapport/nkr/trunk/Hastegrad/")
source("fun_Hastegrad.R", encoding="UTF-8")
Hastegrad(opdata=opdata, outfile=outfile, kjonn=kjonn, hovedkat=hovedkat, 
		aar=aar, reshID=reshID)


				
#-------------------------------------------------------	
#-----------------Liggetid, fordeling - ERSTATTET av AndelSoyler
#-------------------------------------------------------	
Variabel: Liggedogn
#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('Liggedogn', 'Dagkirurgi', 'TidlOpr', 'Kjonn', 'HovedInngrep', 
#		'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]


rm(list=ls())
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Liggedogn, Dagkirurgi, TidlOpr, Kjonn, HovedInngrep, 
		HovedInngreptxt, Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#111065 #999999	#105783	#103618	#102949	#   #Må sendes med til funksjon
aar <- 0 #2006		#Standard: 0, dvs. alle år
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'LiggedFord.png'	#Navn angis av Jasper
tidlOp <- 4			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner

setwd("C:/RyggRegister/Rapport/nkr/trunk/LiggedFord/")
source("fun_LiggedFord.R", encoding="UTF-8")
LiggedFord(opdata=opdata, outfile=outfile, reshID=reshID, 
		tidlOp=tidlOp, aar=aar, kjonn=kjonn, hovedkat=hovedkat)

		


#-------------------------------------------------------	
#---------NYTTE av operasjonen, tidstrend - ERSTATTET av AndelStabelTid
#-------------------------------------------------------	

rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Kjonn, HovedInngrep, HovedInngreptxt, Nytte3mnd, Nytte12mnd, TidlOpr,
			AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#999999	#601161 #111065 #105783	#103618	#   #Må sendes med til funksjon
egenavd <- 0		#0-hele landet, 1-egen avdeling
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
tidlOp <- 4			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'Nytte.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll


setwd("C:/RyggRegister/Rapport/nkr/trunk/Nytte/")
source("fun_Nytte.R", encoding="UTF-8")
Nytte(opdata=opdata, outfile=outfile, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr, tidlOp=tidlOp,
	egenavd=egenavd, reshID=reshID)







#----------------------------------------------------------------
#			Utdanning - ERSTATTES	
#----------------------------------------------------------------
Variabel: Utd
rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
#query = 'select Utd, Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, 
		#AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'
query = 'select Utd, Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, 
		AvdReshID, AvdNavn, OpDato,  right(OpDato, 4) as Aar from Uttrekk_Rapport'
		
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
reshID <- 601161 #999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
aar <- 0		#Standard: 0, dvs. alle år
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'Utdanning.png'	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/Utdanning/")

source("fun_Utdanning.R", encoding="UTF-8")
Utdanning(opdata=opdata, outfile=outfile, reshID=reshID, 
		 aar=aar, kjonn=kjonn, hovedkat=hovedkat)

		 
		 
		 
#----------------------------------------------------------
#                    EQ5Dbox - inngår i BoxTid
#----------------------------------------------------------

rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select PID,Alder, Kjonn, EQ5DPre, EQ5D3mnd, EQ5D12mnd, HovedInngrep, HovedInngreptxt, 
			Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, 
			right(OpDato, 4) as Aar, ASA, Utd from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon


#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 105783	#100133	#601161 	#999999	#100407	#111065 #103618	#102949	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'EQ5Dbox.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
#just <- 0
setwd("C:/RyggRegister/Rapport/nkr/trunk/EQ5Dbox/")

source("fun_EQ5Dbox.R", encoding="UTF-8")
EQ5Dbox(opdata=opdata, outfile=outfile, reshID=reshID, 
		kjonn=kjonn, hovedkat=hovedkat, ktr=ktr)


#----------------------------------------------------------
#                    EQ5Dsml - inngår i SoyleSml
#----------------------------------------------------------
#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('Kjonn', 'EQ5DPre', 'EQ5D3mnd', 'EQ5D12mnd', 'HovedInngrep', 'HovedInngreptxt', 'Inngrep', 
#			'Inngreptxt', 'AvdID', 'AvdReshID', 'AvdNavn', 'OpDato', 'TidlOpr', 'ASA', 'Utd', 'Alder') 
#opdata <- opdataALLE[,varnames]

rm(list=ls())
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select TidlOpr, Kjonn, EQ5DPre, EQ5D3mnd, EQ5D12mnd, HovedInngrep, HovedInngreptxt,
		Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, OpDato, ASA, Utd, Alder 
		from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#111065 #999999	#105783	#103618	#102949	#   #Må sendes med til funksjon
aar <- 2011			#Standard: 0, dvs. alle år
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'EQ5Dsml.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
tidlOp <- 4		#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
shgr <- 0			#0-hele landet (standard), 1-univ.sh, 2-off.sh., 3-pri
#just <- 0
setwd("C:/RyggRegister/Rapport/nkr/trunk/EQ5Dsml/")

source("fun_EQ5Dsml.R", encoding="UTF-8")
EQ5Dsml(opdata=opdata, outfile=outfile, reshID=reshID, 
		tidlOp=tidlOp, aar=aar, kjonn=kjonn, shgr=0, hovedkat=hovedkat, ktr=ktr, just=0)


#-------------------------------------------------------	
#---------Grad av FORNØYDHET, tidstrend, ERSTATTET av AndelStabelTid
#-------------------------------------------------------	
library(RODBC)
rm(list=ls())
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Kjonn, HovedInngrep, HovedInngreptxt, Fornoyd3mnd, Fornoyd12mnd, TidlOpr,
			AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #999999	#100133	#111065 #105783	#103618	#   #Må sendes med til funksjon
egenavd <- 1		#0-hele landet, 1-egen avdeling
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs typer inngrep
tidlOp <- 0			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'Fornoyd.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll


setwd("C:/RyggRegister/Rapport/nkr/trunk/Fornoyd/")
source("fun_Fornoyd.R", encoding="UTF-8")
Fornoyd(opdata=opdata, outfile=outfile, kjonn=kjonn, hovedkat=hovedkat, tidlOp=tidlOp,
		egenavd=egenavd, ktr=ktr, reshID=reshID)


	
#---------------------------------------------------------------
#		Hovedkategorier søylediagram - ERSTATTET av AntSoyler
#---------------------------------------------------------------


#Spørring mot SQL:
rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Kjonn, HovedInngrep, HovedInngreptxt, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport' 
opdata <- sqlQuery(channel, query)	#Sendes med til funksjon

#---Inndata (som angis av bruker) til funksjon:
egenavd <- 0		#0-alle sykehus(default), 1-eget
aar <- 2010		#Default: alle år (verdien 0)
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'HovedkatAnt.png'	#Navn angis av Jasper
reshID <- 601161    #Hentes fra innlogging


setwd("C:/RyggRegister/Rapport/nkr/trunk/hovedkatAnt/")
source("fun_hovedkatAnt.R")
hovedkatAnt(opdata=opdata, outfile=outfile, aar=aar, egenavd=egenavd, 
	kjonn=kjonn, reshID=reshID)





#-------------------------------------------------------	
#-----------------LiggedBox - ERSTATTET av BoxTid
#-------------------------------------------------------	
Variabel: Liggedogn
Median liggetid per år
#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('Liggedogn', 'Dagkirurgi', 'TidlOpr', 'Kjonn', 'HovedInngrep', 
#		'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]
#opdata$Aar <- substr(opdata$OpDato,7,11)


rm(list=ls())
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Liggedogn, Dagkirurgi, TidlOpr, Kjonn, HovedInngrep, HovedInngreptxt, 
		Inngrep, Inngreptxt, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#111065 #999999	#105783	#103618	#102949	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'LiggedBox.png'	#Navn angis av Jasper
setwd("C:/RyggRegister/Rapport/nkr/trunk/LiggedBox/")

source("fun_LiggedBox.R", encoding="UTF-8")
LiggedBox(opdata=opdata, outfile=outfile, reshID=reshID, 
		kjonn=kjonn, hovedkat=hovedkat)



		
#-------------------------------------------------------------------
#			Måned for operasjon - ERSTATTET av AntSoyler
#-------------------------------------------------------------------
rm(list=ls())
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select TidlOpr, Kjonn, HovedInngrep, 
		HovedInngreptxt, Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
reshID <- 601161    #Standard: alle data (egenavd = 0)	601161
egenavd <- 1		#Angir om skal ha data for egen avdeling eller alle. Standard:0, dvs. alle sykehus
aar <- 2012		#Standard: alle år (verdien 0)
kjonn <- 0			#1- menn, 2 - kvinner, Standard: 0, dvs. begge kjønn
hovedkat <- 99		#Hovedkategori, 0-7. Alle kategorier: 99
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- "MndAntOp.pdf"	#Navn angis av Jasper

setwd("C:/RyggRegister/Rapport/nkr/trunk/mndAntOp/")
source("fun_mndAntOp.R", encoding="UTF-8")
mndAntOp(opdata=opdata, outfile=outfile, egenavd=egenavd, hovedkat=hovedkat, 
		reshID=reshID, aar=aar, kjonn=kjonn)
setwd("C:/Rygg/")


#----------------------------------------------------------
#                    OswBox - erstattet av BoxTid
#----------------------------------------------------------

#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('PID','Kjonn', 'OswTotPre', 'OswTot3mnd', 'OswTot12mnd', 'HovedInngrep', 'HovedInngreptxt', 
#			'Inngrep', 'Inngreptxt', 'AvdID', 'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]
#opdata$Aar <- substr(opdata$OpDato,7,11)

rm(list=ls())
setwd('C:/RyggRegister/Rapport')
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select PID, Kjonn, OswTotPre, OswTot3mnd, OswTot12mnd, HovedInngrep, HovedInngreptxt, 
			Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon


#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 	#111065 #100407	#100133	#999999	#100407	#601161 #105783	#103618	#102949	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'OswBox.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
#just <- 0

setwd("C:/RyggRegister/Rapport/nkr/trunk/OswBox/")
source("fun_OswBox.R", encoding="UTF-8")
OswBox(opdata=opdata, outfile=outfile, reshID=reshID, 
		kjonn=kjonn, hovedkat=hovedkat, ktr=ktr)

		
#----------------------------------------------------------
#--------Oswestery, differanse m/konf.int. median, alle s-hus - INNGÅR I SoyleSml
#----------------------------------------------------------

rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Alder, Kjonn, OswTotPre, OswTot3mnd, OswTot12mnd, HovedInngrep, HovedInngreptxt, TidlOpr,
			Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, OpDato, ASA, Utd from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#111065 #999999	#105783	#103618	#102949	#   #Må sendes med til funksjon
aar <- 2011		#Standard: 0, dvs. alle år
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'OswDiff.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
tidlOp <- 0			#Tidl.operert: 1-sm, 2-annet, 3, sm+annet, 4-primær, Standard: 0, dvs. alle operasjoner
#shgr <- 1			#0-hele landet (standard), 1-univ.sh, 2-off.sh., 3-pri
#just <- 0

setwd("C:/RyggRegister/Rapport/nkr/trunk/OswDiff/")
source("fun_OswDiff.R", encoding="UTF-8")
#For å kunne standardisere, må hele datamaterialet sendes med
OswDiff(opdata=opdata, outfile=outfile, reshID=reshID, 
		tidlOp=tidlOp, aar=aar, kjonn=kjonn, shgr=0, hovedkat=hovedkat, ktr=ktr, just=0)



#----------------------------------------------------------
#                    SmBox - ERSTATTES
#----------------------------------------------------------

#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('PID', 'Kjonn', 'SmRyPre', 'SmRy3mnd', 'SmRy12mnd', 'SmBePre', 'SmBe3mnd', 'SmBe12mnd',
#			'HovedInngrep', 'HovedInngreptxt', 
#			'Inngrep', 'Inngreptxt', 'AvdID', 'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]
#opdata$Aar <- substr(opdata$OpDato,7,11)

rm(list=ls())
library(RODBC)
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select PID, Kjonn, SmRyPre, SmRy3mnd, SmRy12mnd, SmBePre, SmBe3mnd, SmBe12mnd, HovedInngrep, HovedInngreptxt, 
			Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 	#999995	#999999	#100407	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'SmBox.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
smsted <- 2			#Smertested: 1- ryggen (standard), 2-beina

setwd("C:/RyggRegister/Rapport/nkr/trunk/SmBox/")
source("fun_SmBox.R", encoding="UTF-8")
SmBox(opdata=opdata, outfile=outfile, reshID=reshID, 
		smsted=smsted, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr)


#----------------------------------------------------------
#                SmFord - ERSTATTET av FordPrePost
#----------------------------------------------------------

#opdataALLE <- read.table('../AllePIDarbfil.csv', header = T, sep = ";")
#varnames = c('PID', 'Kjonn', 'SmRyPre', 'SmRy3mnd', 'SmRy12mnd', 'SmBePre', 'SmBe3mnd', 'SmBe12mnd',
#			'HovedInngrep', 'HovedInngreptxt', 'Inngrep', 'Inngreptxt', 
#			'AvdID', 'AvdReshID', 'AvdNavn', 'OpDato') 
#opdata <- opdataALLE[,varnames]

rm(list=ls())
setwd('C:/RyggRegister/Rapport')
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select PID, Kjonn, SmBePre, SmBe3mnd, SmBe12mnd, SmRyPre, SmRy3mnd, SmRy12mnd, HovedInngrep, HovedInngreptxt, 
			Inngrep, Inngreptxt, AvdID, AvdReshID, AvdNavn, OpDato from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon


#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 	#999999	#100407	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
egenavd <- 0
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
aar <- 2011
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'SmFord.png'	#Navn angis av Jasper
ktr <- 1			#1. el 2. kontroll. 1 - 3mnd kontroll, 2 - 12 mnd kontroll
smsted <- 1			#Smertested: 1- ryggen (standard), 2-beina

setwd("C:/RyggRegister/Rapport/nkr/trunk/SmFord/")
source("fun_SmFord.R", encoding="UTF-8")
SmFord(opdata=opdata, outfile=outfile, reshID=reshID, egenavd=egenavd,
		smsted=smsted, aar=aar, kjonn=kjonn, hovedkat=hovedkat, ktr=ktr)


		
		
		

#-------------------------------------------------------------
#		Tidligere operasjon, tidstrend - ERSTATTET av AndelStabelTid
#-------------------------------------------------------------

#Spm: Tidligere ryggoperert?
rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Kjonn, HovedInngrep, HovedInngreptxt, TidlOpr,
			AvdID, AvdReshID, AvdNavn, right(OpDato, 4) as Aar from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
#(opdata - fra SQLspørring)
reshID <- 601161 #100133	#601161 #999999	#601161 #111065 #105783	#103618	#   #Må sendes med til funksjon
egenavd <- 1		#0-hele landet, 1-egen avdeling
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'TidlOp.png'	#Navn angis av Jasper


setwd("C:/RyggRegister/Rapport/nkr/trunk/TidlOperasjon/")
source("fun_TidlOperasjon.R", encoding="UTF-8")
TidlOperasjon(opdata=opdata, outfile=outfile, kjonn=kjonn, hovedkat=hovedkat, 
	egenavd=egenavd, reshID=reshID)





#------------------------------------------------------------------
#		SØYLEDIAGRAM, underkategorier, antall - TAS UT!!
#------------------------------------------------------------------

rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato
		from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon

#__Inndata til funksjon:
reshID <- 601161 #100133	#999999	#601161 #111065 #105783	#103618	#601161    #Alltid med
hovedkat <- 1 # Bare 1,2,5 og 7 har underkat. 1-standard
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
aar <- 2011	#Standard: 0, dvs. alle år
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'underkatAnt.png'	#Navn angis av Jasper

setwd("C:/RyggRegister/Rapport/nkr/trunk/underkatAnt/")
source("fun_underkatAnt.R", encoding="UTF-8")
underkatAnt(opdata=opdata, outfile=outfile, hovedkat=hovedkat, aar=aar, 
		kjonn=kjonn, reshID=reshID)




#-------------------------------------------------------------------------------------
#			UnderkatAndel - Inngår i AndelSoyler
#-------------------------------------------------------------------------------------
#SØYLEDIAGRAM(%), andel av underkat, ett S-hus, kontra alle
rm(list=ls())
library(RODBC)
channel = odbcConnect("nkr", "RapportUser_nkr", "Pitkeep7")
query = 'select Kjonn, HovedInngrep, HovedInngreptxt, Inngrep, Inngreptxt, AvdReshID, AvdNavn, OpDato
		from Uttrekk_Rapport'
opdata <- sqlQuery(channel, query)		#Må sendes med til R-funksjon


#__Inndata til funksjon:
reshID <- 601161    #Må sendes med til funksjon, 111185
hovedkat <- 1 # Kan velge bare 1,2,5 og 7. 1-standard
aar <- 0		#Standard: 0, dvs. alle år
kjonn <- 0			#1-menn, 2-kvinner, Standard: 0, dvs. begge
libkat <- 'C:/Registre/Rlib/trunk/'		#del av sti til bibliotekkatalog, før /lib/r/<funksjon.R>
outfile <- 'underkatAndel.png'	#Navn angis av Jasper

#.libPaths("C:/RyggRegister/Rapport/nkr/trunk/underkatAndel/")
setwd("C:/RyggRegister/Rapport/nkr/trunk/underkatAndel/")
source("fun_underkatAndel.R", encoding="UTF-8")
underkatAndel(opdata=opdata, outfile=outfile, hovedkat=hovedkat, aar=aar, kjonn=kjonn, reshID=reshID)
setwd("C:/Rygg/")
}


		


**************************************************
