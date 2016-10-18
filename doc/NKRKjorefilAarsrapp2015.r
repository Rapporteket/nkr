#**************************************************
#********************* Årsrapport, 2015****************************
rm(list=ls())
RegData <- read.table('C:/Registre/nkr/data/NKR2010_2015.csv', sep=';', header=T, encoding = 'UTF-8')
setwd('C:/ResultattjenesteGIT//nkr/aarsrapp')
#__Inndata til funksjon:
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2010-01-01'
datoTil <- '2015-12-31'
ktr <- 2
minald <- 0
maxald <- 130
erMann <- 99
hovedkat <- 1 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
opKat <- 1  #Bare elektive pasienter
tidlOp <- 4 #Bare primæroperasjoner
enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF
#valgtVar <- 'KpInf3Mnd'   #BeinsmEndrLav', BeinsmLavPre, DegSponSSSten,OswEndr13, OswEndr20, OswEndr30pst, Osw48
#Verre, KpInf3Mnd
#outfile <- paste0(valgtVar, '_', grVar,'.png')

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


#-----------------Årsvariasjon:
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2010-01-01'
datoTil <- '2015-12-31'
ktr <- 2
enhetsUtvalg <- 10 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF


#Lav beinsmerte
valgtVar <- 'BeinsmLavPre'   #BeinsmEndrLav', BeinsmLavPre, DegSponSSSten,OswEndr13, OswEndr20, OswEndr30pst, Osw48
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                      ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#Oswestryforbedring
valgtVar <- 'OswEndr20'   
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                       ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Oswestryforbedring
valgtVar <- 'OswEndr30pst'   
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                       ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Lang symptomvarighet
valgtVar <- 'SympVarighUtstr'   
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                       ktr=ktr, hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
