#**************************************************
#********************* Årsrapport, 2016****************************

#------------------Resultatkapittel--------------------------------
library(nkr)
rm(list=ls())
setwd('C:/ResultattjenesteGIT//nkr/aarsrapp')

library(knitr)
library(tools)
knit('ResultaterAarsrapp.Rnw', encoding = 'UTF-8')
texi2pdf('ResultaterAarsrapp.tex')

load('A:/Rygg/NKR2010-2016aarsrapp.Rdata')
#save(RegData, file='C:/Registre/nkr/data/NKR2010-2016aarsrapp.Rdata')
#__Inndata til funksjon:
datoFra <- '2010-01-01'
datoTil <- '2016-12-31'
minald <- 0
maxald <- 130
erMann <- 99
aar <- 2010:2016
hovedkat <- 99 		#Hovedinngrep, 0-9, Standard: 99, dvs alle operasjoner
opKat <- 99  #Bare elektive pasienter
tidlOp <- 99 #4 - Bare primæroperasjoner
enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF
ktr <- 2
AKjust <- 0
reshID <- 0
outfile <- ''


#----------------------------Til offentliggjøring for 2016-tall:
#---NAKKE----------
#Stemmevansker, 3 mnd etter (ikke-myelopati, fremre tilgang) – lav
#Svelgvansker, 3 mnd (ikke-myelopati, fremre tilgang) – lav
#Infeksjon, pasientrapp., 3 mnd etter (bakre tilgang) – lav
# 
#----- RYGG ---------------------
#Sårinfeksjon, pasientrapportert (prolaps) – lav
#15 mot 16
valgtVar='KpInf3Mnd'
hovedkat=1
grVar='ShNavn'
tidlAar=2015
aar=2016
Ngrense <- 20




RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1, grVar='ShNavn', #tittel=1, ktr=0, 
                                   Ngrense=20, aar=2016, tidlAar=2015, outfile='') #EksempelShusAar.png') 
      
#tapply(RegData$Variabel, RegData[ ,c('OpAar', 'ShNavn')], sum, na.rm=T)
setwd('C:/ResultattjenesteGIT/nkr/AarsrappOff/2016')
#KpInf3Mnd, #15 mot 16
#Sårinfeksjon, pasientrapportert (prolaps) – lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=1,  
                       Ngrense=20, aar=2016, tidlAar=2015, outfile='KpInf3MndPro.pdf') 
#Sårinfeksjon, pasientrapportert (spinal stenose) – lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='KpInf3Mnd', hovedkat=8,  
                       Ngrense=20, aar=2016, tidlAar=2015, outfile='KpInf3MndSS.pdf') 


#Komplikasjon durarift ved operasjon (prolaps, elektiv, primærop.), 15 mot 16 - lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=20, aar=2016, tidlAar=2015, outfile='PeropKompDuraPro.pdf') 
#Komplikasjon durarift ved operasjon (spinal stenose, elektiv, primærop.) – lav
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='PeropKompDura', hovedkat=8, tidlOp=4, opKat=1, 
                       Ngrense=20, aar=2016, tidlAar=2015, outfile='') #PeropKompDuraSS.pdf') 

#Degen. spondylolistese operert med fusjonskirurgi, 15 mot 16
RyggFigAndelerGrVar(RegData=RegData, valgtVar='degSponFusj', 
                       Ngrense=20, outfile='DegSponFusj.pdf') #aar=2016, 

#Forbedring av Oswestry-skår <13p, 12mnd etter. (prolaps, elektiv, primærop.) – lav
# 12 og 13 mot 14 og 15
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar='OswEndrLav', hovedkat=1, tidlOp=4, opKat=1, 
                       Ngrense=20, aar=2014:2015, tidlAar=2012:2013, outfile='') #PeropKompDuraPro.pdf') 

#Oswestry-skår =<22p, 12 mnd. etter (prolaps, elektiv, primærop.) – høy
# 12 og 13 mot 14 og 15

#Oswestry-skår =<22p, 12 mnd. etter (spinal stenose, elektiv, primærop.) – høy
# 12 og 13 mot 14 og 15

#Helt fornøyde pasienter 12 mnd. etter (spinal stenose, elektiv, primær) - høy
# 12 og 13 mot 14 og 15

#Gjennomsnittlig liggetid (spinal stenose)
#15 mot 16










#--------------------------------------N>30: ---------------------------
#Andel fusjonskirurgi over 75 år. Andel foramenotomi over 75 år
valgtVar <- 'Alder'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel prolapsoperte uten parese (OpIndParese != 1) med NRS bensmerte < 2,5 preoperativt
valgtVar <- 'BeinsmLavPre'
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
     

#Andel pasienter med Degenerativ spondylolistese (RfSpondtypeDegen=1) og sentral spinal stenose (RfSentr=1). 
#Utvalg: Andel  operert med fusjonskirurgi. 
valgtVar <- 'DegSponSSSten'
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Fremmedspråklige
valgtVar <- 'Morsmal'    
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
					
#Andel varighet av utstrålende smerter mer enn ett år (SympVarighUtstr). 
#Utvalg: prolapskirurgi og  foraminotomi + laminectomi slått sammen i en gruppe.
valgtVar <- 'SympVarighUtstr' 
outfile <- paste0(valgtVar, '_1',grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#--------------------------------------N>50: ---------------------------

#Andel med  bensmerte endring < 1,5 (failure)
#Utvalg: prolaps
valgtVar <- 'BeinsmEndrLav'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#KpInf3Mnd: Andel sårinfeksjon (enhver). Variabel: KpInf3Mnd 
#Utvalg: prolaps, foraminotomi+ laminectomi slått sammen i en gruppe og fusjonskirurgi)
valgtVar <- 'KpInf3Mnd'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#Andel med 12 mnd ODI> 48 (forverring). (Dvs. Osw12mnd>48, uavhengig av prescore.)
#Utvalg: prolaps
valgtVar <- 'Osw48'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 20 (suksess)
#Utvalg: prolaps
valgtVar <- 'OswEndr20'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 30% (suksess)
#Utvalg: foramenotomi + laminectomi
valgtVar <- 'OswEndr30pst'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel prolapsopererte ODI endring < 13 (failure)
valgtVar <- 'OswEndrLav'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel durarift, alle typer inngrep. 
valgtVar <- 'PeropKompDura'
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')

RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel  mye verre/verre enn noen sinne. 
valgtVar <- 'Verre'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)




#-----------------Årsvariasjon, offentliggjøring:
rm(list=ls())
RegData <- read.table('C:/Registre/nkr/data/NKR2010_2015.csv', sep=';', header=T, encoding = 'UTF-8')
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2010-01-01'
datoTil <- '2015-12-31'
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
opKat <- 1  #Bare elektive pasienter
tidlOp <- 4 #Bare primæroperasjoner
ktr <- 1
enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
siste3aar <- 0 # siste 3 år, årsvariasjon
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF
setwd('C:/ResultattjenesteGIT/nkr/aarsrapp/Offentliggjoring')


valgtVar <- 'Fornoyd'
outfile <- paste0(valgtVar, hovedkat, grVar,'.pdf')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                      ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile,
                      siste3aar=siste3aar) 





siste3aar <- 1 # siste 3 år, årsvariasjon
variabelvalg <- c('BeinsmLavPre', 'Fornoyd', 'KpInf3Mnd', 'Morsmal', 'PeropKompDura', 'OswEndr20', 
                  'OswEndr30pst','SympVarighUtstr')
#Lav beinsmerte, fig 9
for (valgtVar in variabelvalg) {
outfile <- paste0(valgtVar, hovedkat, grVar,'AarN20.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                      ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile,
                      siste3aar=siste3aar) }
                  
#Fornøydhet, ny fig
#Sårinfeksjon, fig 12
#Morsmål, fig 14
#Durarift, fig 13
#Oswestryforbedring, fig 11
