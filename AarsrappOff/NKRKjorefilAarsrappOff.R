#**************************************************
#********************* Årsrapport****************************

#------------------Resultatkapittel--------------------------------
library(nkr)
rm(list=ls())
library(nkr)
setwd('C:/ResultattjenesteGIT/nkr/AarsrappOff/')

library(knitr)
library(tools)
knit('ResultaterAarsrappTore25sept.Rnw', encoding = 'UTF-8')
texi2pdf('ResultaterAarsrappTore25sept.tex') 

#rm(list=ls())
#NKRdata <- read.table('A:/Rygg/NKR2010-2017aarsrapp.csv', sep=';', header=T, encoding = 'UTF-8')
#RegData <- NKRdata
#save(RegData, file=paste0('A:/Rygg/NKR2010-2017aarsrapp', '.Rdata'))

load('A:/Rygg/NKR2010-2017aarsrapp.Rdata') #IKKE preprossessert
preprosess <- 0
#save(RegData, file='C:/Registre/nkr/data/NKR2010-2016aarsrapp.Rdata')
#__Inndata til funksjon:
datoFra <- '2010-01-01'
datoTil <- '2017-12-31'
aarsRappAar <- 2017
aarsStart <- paste0(aarsRappAar,'-01-01')
aar <- aarsRappAar
aar2 <- (aarsRappAar-1):aarsRappAar  #2015:2016
tidlAar <- aarsRappAar-1
tidlAar2 <- (aarsRappAar-3):(aarsRappAar-2) #2013:2014
#aar <- 0 #2010:2017
minald <- 0
maxald <- 110
erMann <- 99
hovedkat <- 99 		#Hovedinngrep, 0-9, Standard: 99, dvs alle operasjoner
opKat <- 99  #Bare elektive pasienter
tidlOp <- 99 #4 - Bare primæroperasjoner
enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF
ktr <- 2
Ngrense <- 10
AKjust <- 0
reshID <- 0


#Nakke
valgtVar <- 'KomplStemme3mnd'
myelopati=99
fremBak=0
ktr=0
hentData=0
outfile=''

#----------------------------Til offentliggjøring:
#---NAKKE----------
library(Nakke)
load('A:/Nakke/NakkeAarsrapp2017.Rdata')
library(nkr)


library(Nakke)
#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='KomplStemme3mnd',
                   myelopati=0, fremBak=1, Ngrense=20,
                   ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='')

#Svelgvansker, 3 mnd (ikke-myelopati, fremre tilgang) – lav
NakkeFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='KomplSvelging3mnd',
                   myelopati=0, fremBak=1, Ngrense=20,
                   ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='')

#Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav
NakkeFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='Komplinfek',
                   fremBak=2, Ngrense=20,
                   ktr=0,aar=aar2,tidlAar=tidlAar2, outfile='')

# 
#----- RYGG ---------------------
#KpInf3Mnd, #15 mot 16
#Sårinfeksjon, pasientrapportert (prolaps) – lav
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='KpInf3Mnd', hovedkat=1,  
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='KpInf3MndPro.png') 
#Sårinfeksjon, pasientrapportert (spinal stenose) – lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=8,  
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='KpInf3MndSS.png') 


#Komplikasjon durarift ved operasjon (prolaps, elektiv, primærop.), - lav
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraPro.png') 
#Komplikasjon durarift ved operasjon (spinal stenose, elektiv, primærop.) – lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraSS.png') 

#Degen. spondylolistese operert med fusjonskirurgi, hele tidsperioden
RyggFigAndelerGrVar(RegData=RegData, preprosess=0, valgtVar='degSponFusj', aar = (aarsRappAar-4):aarsRappAar,#
                       Ngrense=30, outfile='DegSponFusj.png') #aar=2016, 

#--sml. to og to år. Siste året blir året før årsrapport
#Forbedring av Oswestry-skår <13p, 12mnd etter. (prolaps, elektiv, primærop.) – lav
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='OswEndrLav', ktr = 2, hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='OswEndrLav.png') 

#Oswestry-skår =<22p, 12 mnd. etter (prolaps, elektiv, primærop.) – høy
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='Osw22', ktr = 2, hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='Osw22Pro.png') 
#Oswestry-skår =<22p, 12 mnd. etter (spinal stenose, elektiv, primærop.), 12 og 13 mot 14 og 15 – høy
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='Osw22', ktr = 2, hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='Osw22SS.png') 

#Helt fornøyde pasienter 12 mnd. etter (spinal stenose, elektiv, primær) - høy
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='Fornoyd', ktr = 2, hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=30, aar=aar2-1, tidlAar=tidlAar2-1, outfile='Fornoyd.png') 

#Andel durarift, prolaps
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='PeropKompDura', hovedkat=1, opKat=1, tidlOp=4, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraPro.png')
#Andel durarift, prolaps
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='PeropKompDura', hovedkat=8, opKat=1, tidlOp=4, 
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='PeropKompDuraSS.png')


#Gjennomsnittlig liggetid (spinal stenose)
# Ett år mot forrige
RyggFigGjsnBox(valgtVar='Liggedogn', RegData=RegData, preprosess=0, datoFra='2010-01-01', hovedkat=8, 
               outfile='LiggedognTidSS.png')

RyggFigGjsnGrVar(valgtVar='Liggedogn', Ngrense=20, valgtMaal = 'Gjsn', 
                 RegData=RegData, preprosess=0, datoFra=aarsStart, hovedkat=8, outfile='') #LiggedognSh.png')

#Andel varighet av utstrålende smerter mer enn ett år (SympVarighUtstr). 
#Utvalg: prolapskirurgi og  foraminotomi + laminectomi slått sammen i en gruppe.
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar='SympVarighUtstr', hovedkat=1,  
                       Ngrense=30, aar=aar2, tidlAar=tidlAar2, outfile='SympVarighUtstrPro.png')


#--------------------------------------N>30: ---------------------------
#??Andel fusjonskirurgi over 75 år. Andel foramenotomi over 75 år
valgtVar <- 'Alder'
ktr <- 1
outfile <- paste0(valgtVar='alder75', '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel prolapsoperte uten parese (OpIndParese != 1) med NRS bensmerte < 2,5 preoperativt
valgtVar <- 'BeinsmLavPre'
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, 
                    reshID=reshID, outfile=outfile)
     

#Andel pasienter med Degenerativ spondylolistese (RfSpondtypeDegen=1) og sentral spinal stenose (RfSentr=1). 
#Utvalg: Andel  operert med fusjonskirurgi. 
valgtVar <- 'DegSponSSSten'
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Fremmedspråklige
valgtVar <- 'Morsmal'    
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
					


#--------------------------------------N>50: ---------------------------

#Andel med  bensmerte endring < 1,5 (failure)
#Utvalg: prolaps
valgtVar <- 'BeinsmEndrLav'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#KpInf3Mnd: Andel sårinfeksjon (enhver). Variabel: KpInf3Mnd 
#Utvalg: prolaps, foraminotomi+ laminectomi slått sammen i en gruppe og fusjonskirurgi)
valgtVar <- 'KpInf3Mnd'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#Andel med 12 mnd ODI> 48 (forverring). (Dvs. Osw12mnd>48, uavhengig av prescore.)
#Utvalg: prolaps
valgtVar <- 'Osw48'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 20 (suksess)
#Utvalg: prolaps
valgtVar <- 'OswEndr20'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 30% (suksess)
#Utvalg: foramenotomi + laminectomi
valgtVar <- 'OswEndr30pst'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel prolapsopererte ODI endring < 13 (failure)
valgtVar <- 'OswEndrLav'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#Andel  mye verre/verre enn noen sinne. 
valgtVar <- 'Verre'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.png')
RyggFigAndelerGrVarAar(RegData=RegData, preprosess=0, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#---------------------------------------------------------------
#				Kvalitetskontroll av data
#---------------------------------------------------------------
# RYGG
rm(list=ls())
library(nkr)
load('A:/Rygg/NKR2010-2017aarsrapp.Rdata') #IKKE preprossessert

RegData <- RyggPreprosess(RegData=RegData)
#Dobbeltregistrering
PIDop <- table(RegData$PID, RegData$OpDato)
testDato <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpDato')], drop=TRUE, FUN=length)
testDato[which(testDato$x >1), ]
testMnd <- aggregate(RegData$InnDato, by=RegData[ ,c('PID','Mnd','OpAar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
testAar <- aggregate(RegData$PID, by=RegData[ ,c('PID','OpAar')], drop=TRUE, FUN=length)
sum(testAar$x >1)


# NAKKE
library(Nakke)
setwd('C:/ResultattjenesteGIT/nkr/inst/')
NakkeData <- read.table('A:/Nakke/AlleVarNum2018-06-21.csv', sep=';', header=T, encoding = 'UTF-8', stringsAsFactors = FALSE)  # na.strings = "NULL", 


load('A:/Nakke/NakkeAarsrapp2017.Rdata')
RegData <- NakkePreprosess(RegData=NakkeData)
#Dobbeltregistrering
#PIDop <- table(RegData$PasientID, RegData$OprDato)
testDato <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','OprDato')], drop=TRUE, FUN=length)
testDato[which(testDato$x >1), ]
RegData$Mnd <- RegData$InnDato$mon +1
RegData$Mnd <- RegData$Mnd-min(RegData$Mnd[RegData$Aar==min(RegData$Aar)])+1
testMnd <- aggregate(RegData$OprDato, by=RegData[ ,c('PasientID','Mnd','Aar')], drop=TRUE, FUN=length)
duplMnd <- testMnd[which(testMnd$x >1), ]
testAar <- aggregate(RegData$PasientID, by=RegData[ ,c('PasientID','Aar')], drop=TRUE, FUN=length)
sum(testAar$x >1)

#---------------------------Dekningsgrad-----------------------------------------
DekningsgradRygg2017
DeknData <- read.table('P:/Registerinfo og historie/nkr/AarsrappOff/DekningsgradRygg2017.csv', sep=';', header=T, encoding = 'UTF-8')
head(DeknData)
RegData <- DeknData

RyggFigAndelerGrVar(RegData=DeknData, preprosess = 0, valgtVar='deknGrad', datoFra='2016-01-01',outfile=outfile)











