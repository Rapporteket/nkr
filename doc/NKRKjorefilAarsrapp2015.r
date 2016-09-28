#**************************************************
#********************* Årsrapport, 2015****************************
rm(list=ls())
RegData <- read.table('C:/Registre/nkr/data/NKR2010_2015.csv', sep=';', header=T)
setwd('C:/ResultattjenesteGIT//nkr/aarsrapp')
#__Inndata til funksjon:
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2010-01-01'
datoTil <- '2015-12-31'
ktr <- 2
hovedkat <- c(1,2) 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
enhetsUtvalg <- 0 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF
valgtVar <- 'KpInf3Mnd'   #BeinsmEndrLav', BeinsmLavPre, DegSponSSSten,OswEndr13, OswEndr20, OswEndr30pst, Osw48
            #Verre, KpInf3Mnd
outfile <- paste0(valgtVar, '_', grVar,'.png')
enhetsutvalg <- 0

#--------------------------------------N>30: ---------------------------

#Andel fusjonskirurgi over 75 år. Andel foramenotomi over 75 år
hovedkat <- c(2,3,5)
valgtVar <- 'Alder'
ktr <- 1
outfile <- paste0(valgtVar, '_25', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel prolapsoperte uten parese (OpIndParese != 1) med NRS bensmerte < 2,5 preoperativt
hovedkat <- 1
valgtVar <- 'BeinsmLavPre'
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
     

#Andel pasienter med Degenerativ spondylolistese (RfSpondtypeDegen=1) og sentral spinal stenose (RfSentr=1). 
#Utvalg: Andel  operert med fusjonskirurgi. 
hovedkat <- 5
valgtVar <- 'DegSponSSSten'
outfile <- paste0(valgtVar, '_5', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Fremmedspråklige
valgtVar <- 'Morsmal'    
hovedkat <- 99
outfile <- paste0(valgtVar, '_', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
					
#Andel varighet av utstrålende smerter mer enn ett år (SympVarighUtstr). 
#Utvalg: prolapskirurgi og  foraminotomi + laminectomi slått sammen i en gruppe.
valgtVar <- 'SympVarighUtstr' 
hovedkat <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
hovedkat <- 2:3
outfile <- paste0(valgtVar, '_23', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#--------------------------------------N>50: ---------------------------

#Andel med  bensmerte endring < 1,5 (failure)
#Utvalg: prolaps
hovedkat <- 1
valgtVar <- 'BeinsmEndrLav'
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#KpInf3Mnd: Andel sårinfeksjon (enhver). Variabel: KpInf3Mnd 
#Utvalg: prolaps, foraminotomi+ laminectomi slått sammen i en gruppe og fusjonskirurgi)
hovedkat <- c(1:3)
valgtVar <- 'KpInf3Mnd'
ktr <- 1
outfile <- paste0(valgtVar, '_123', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

hovedkat <- 5
outfile <- paste0(valgtVar, '_5', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med 12 mnd ODI> 48 (forverring). (Dvs. Osw12mnd>48, uavhengig av prescore.)
#Utvalg: prolaps
hovedkat <- 1
valgtVar <- 'Osw48'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 20 (suksess)
#Utvalg: prolaps
hovedkat <- 1
valgtVar <- 'OswEndr20'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel med ODI forbedring mer enn 30% (suksess)
#Utvalg: foramenotomi + laminectomi
hovedkat <- c(2,3)
valgtVar <- 'OswEndr30pst'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel prolapsopererte ODI endring < 13 (failure)
hovedkat <- 1
valgtVar <- 'OswEndrLav'
ktr <- 2
outfile <- paste0(valgtVar, '_1', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Andel durarift, alle typer inngrep. 
hovedkat <- 99
valgtVar <- 'PeropKompDura'
for (tidlOp in ) {
outfile <- paste0(valgtVar, '_', grVar,'.pdf')

RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
}

#Andel  mye verre/verre enn noen sinne. 
#Utvalg: foraminotomi + laminectomi slått sammen i en gruppe.
hovedkat <- 2:3
valgtVar <- 'Verre'
ktr <- 1
outfile <- paste0(valgtVar, '_23_3mnd', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

ktr <- 2
outfile <- paste0(valgtVar, '_23_12mnd', grVar,'.pdf')
RyggFigAndelerGrVar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                    ktr=ktr, hovedkat=hovedkat, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#-----------------Årsvariasjon:
reshID <- 601161	#999995	#999999	#100407 (Kristiansand)	#601161(UNN), 100133(Arendal),105783(StOlav),103618(Drammen)	#102949	#   #Må sendes med til funksjon
datoFra <- '2010-01-01'
datoTil <- '2015-12-31'
ktr <- 2
hovedkat <- 99 		#Hovedinngrep, 0-7, Standard: 99, dvs alle operasjoner
enhetsUtvalg <- 10 # 0-hele landet, 4–egen shusgruppe, 7–egen region
grVar <- 'ShNavn'  #ShNavn, Fylke, BoHF, BoRHF


#Lav beinsmerte
valgtVar <- 'BeinsmLavPre'   #BeinsmEndrLav', BeinsmLavPre, DegSponSSSten,OswEndr13, OswEndr20, OswEndr30pst, Osw48
hovedkat <- 1
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                      ktr=ktr, hovedkat=hovedkat, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)


#Oswestryforbedring
valgtVar <- 'OswEndr20'   
hovedkat <- 1
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                       ktr=ktr, hovedkat=hovedkat, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Oswestryforbedring
valgtVar <- 'OswEndr30pst'   
hovedkat <- 1
ktr <- 1
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                       ktr=ktr, hovedkat=hovedkat, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)

#Lang symptomvarighet
valgtVar <- 'SympVarighUtstr'   
hovedkat <- 1 #2:3
outfile <- paste0(valgtVar, '_1', grVar,'Aar.png')
RyggFigAndelerGrVarAar(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, 
                       ktr=ktr, hovedkat=hovedkat, grVar=grVar,
                       preprosess=1, enhetsUtvalg=enhetsUtvalg, reshID=reshID, outfile=outfile)
