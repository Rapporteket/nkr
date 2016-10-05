#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
#'        'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet' 
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Pasienter over 75år    
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?    
#'     \item ArbstatusPre: Mottar sykepenger, før operasjon?    
#'     \item Arbstatus: Mottar sykepenger, 3 mnd etter operasjon?    [ENDRET fra Arbstatus3mnd, Arbstatus12mnd]
#'     \item ASA: ASA-grad > II    
#'     \item BMI: Pasienter med fedme (BMI>30)    
#'     \item ErstatningPre: Søkt/planlegger å søke erstatning?    
#'     \item Fornoyd: Fornøyde pasienter [ENDRET fra Fornoyd3mnd, Fornoyd12mnd  ]
#'     \item Kp3Mnd: Pasientrapporterte komplikasjoner    
#'     \item Misfornoyd:  Andel med Misfornøyd/litt misfornøyd [ENDRET fra Misfor3mnd, Misfor12mnd]
#'     \item Nytte: Klart bedre    [ENDRET fra Nytte3mnd, Nytte12mnd]
#'	 \item OswEndr30pst: Mer enn 30% forbedring i Oswestry-skår, 3 mnd. [ENDRET fra Osw30_3mnd, Osw30_12mnd]
#'     \item PeropKomp: Komplikasjon ved operasjon
#'     \item PeropKompDura: Komplikasjon ved operasjon: Durarift
#'     \item Roker: Røyker du?    
#'     \item Saardren: Sårdren    
#'     \item SmStiPre: Bruker smertestillende før operasjonen    
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år    
#'     \item SympVarighUtstr: Varighet av utstrålende smerter > 1 år  
#'     \item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?    
#'     \item Utd: Andel høyskole-/universitetsutdannede    
#'     \item Verre Mye verre/verre enn noen gang, 3 mnd. [ENDRET fra Verre3mnd, Verre12mnd]
#'		\item ..
#'	 	\item BeinsmLavPre: Pasienter med preop. beinsmerte < 2.5 og ikke parese.
#'		\item BeinsmEndrLav: Forbedring av beinsmerter under 1.5 poeng
#'     \item DegSponSSSten: Pasienter med Degenerativ spondylolistese og sentral spinal stenose 
#'	 \item OswEndrLav: Mer enn 20 poeng forbedring i Oswestry-skår, 3 mnd/12mnd.
#' \item OswEndr20: 
#' \item Osw48: 
#' \item KpInf3Mnd: 
#'		}
#'
#' @inheritParams RyggFigAndeler
#' @param grVar Tekstvariabel som angir hva skal resultatene grupperes på. 
#'                ShNavn-sykehus/avdeling
#'                Fylke- Pasienten bor i det akutelle fylket
#'                BoHF - Pasienten bor i boområdene til det angitte HF.
#'                BoRHF - Pasienten bor i boområdene til det angitte RHF.
#' @param valgtVar Variabelen det skal vises resultat for. Se \strong{Details} for oversikt.
#'
#' @return Figur med...
#'
#' @export

RyggFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='3000-12-31', 
                            minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', hentData=0, preprosess=1,
                            opKat=99, enhetsUtvalg=0, grVar='ShNavn', tittel=1, ktr=0, reshID=0, outfile='') {

	if (hentData == 1) {		
	  RegData <- RyggRegDataSQL()
	}

     # Preprosessere data
     if (preprosess){
          RegData <- RyggPreprosess(RegData=RegData)
     }


     #----------- Figurparametre ------------------------------
     cexShNavn <- 1 #0.85

     #Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne,
     #trengs ikke data for hele landet:
     reshID <- as.numeric(reshID)
     indEgen1 <- match(reshID, RegData$ReshId)
     smltxt <- 'Hele landet'
     if (enhetsUtvalg == 7) {
          smltxt <- as.character(RegData$Region[indEgen1])
          RegData <- RegData[which(RegData$Region == smltxt), ]	#kun egen region
          cexShNavn <- 1
     }

     RegData[ ,grVar] <- factor(RegData[ ,grVar])
     Ngrense <- 30		#Minste antall registreringer for at ei gruppe skal bli vist

#if (valgtVar %in% c('OswEndr20', 'OswEndr30pst' )) {
#ktr kan ha verdiene 0, 1 eller 2
	ktrtxt <- c(', 3 mnd etter', ', 12 mnd. etter')[ktr]
#	}
      RegData$Variabel <- 0

     if (valgtVar == 'Alder') {
          #Andel over 75 år
          RegData$Variabel[which(RegData[ ,valgtVar] >= 75)] <- 1
          TittelUt <- 'Pasienter over 75 år'
     }

     if (valgtVar == 'Antibiotika') {
          #Komorbiditet
          RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Fått antibiotika'
     }
    if (valgtVar == 'Arbstatus') {
          # Andel i kategori 6 tom 9, mottar sykepenger Av 1-9, (ikke bare de som sykemeldt fra før)
          #  #grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
          #		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
 		  RegData$Arbstatus <- switch(as.character(ktr), 
						'1'= RegData$Arbstatus3mnd,
						'2'= RegData$Arbstatus12mnd)
		  RegData <- RegData[which(RegData$Arbstatus %in% 1:10), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 6:10)] <- 1
          TittelUt <- paste0('Mottar sykepenger' ,ktrtxt)
     }
     if (valgtVar == 'ArbstatusPre') {
          # Andel i kategori 6 tom 9, mottar sykepenger Av 1-9, (ikke bare de som sykemeldt fra før)
          #  #grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
          #		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
		RegData <- RegData[which(RegData[ ,valgtVar] %in% 1:10), ]
          TittelUt <- 'Mottar sykepenger, preoperativt?'
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 6:10)] <- 1
     }
     if (valgtVar == 'ASA') {
          RegData <- RegData[which(RegData[,valgtVar] %in% 1:5), ]
          RegData$Variabel[which(RegData[ ,valgtVar] > 2)] <- 1
          TittelUt <- 'ASA-grad > II'
     }
     if (valgtVar == 'BeinsmLavPre') {
          #Lav beinsmerte og ingen parese. (Først og fremst prolaps)
          RegData$Variabel[which(is.na(RegData$OpIndParese) & (RegData$SmBePre < 2.5))] <- 1
          TittelUt <- 'Beinsmerte < 2,5 og ingen parese'
     }
    if (valgtVar == 'BeinsmEndrLav') {
          #Mislykkede operasjoner
 		  RegData$BeinsmEndr <- switch(as.character(ktr), 
						'1'= (RegData$SmBePre - RegData$SmBe3mnd),
						'2'= (RegData$SmBePre - RegData$SmBe12mnd))
          RegData <- RegData[which(RegData$BeinsmEndr >= -10), ]	#Fjerne tomme og ugyldige
#          RegData$Variabel[which(RegData$BeinsmEndr <1.5)] <- 1
          RegData$Variabel[which(is.na(RegData$OpIndParese) & (RegData$BeinsmEndr < 1.5))] <- 1
          TittelUt <- paste0('Forbedring av beinsmerte-skår < 1.5 poeng', ktrtxt)
     }
     if (valgtVar == 'BMI') {
          #BMI > 30
          RegData <- RegData[which(RegData[,valgtVar] >10), ]
          RegData$Variabel[which(RegData[ ,valgtVar] > 30)] <- 1
          TittelUt <- 'Pasienter med fedme (BMI>30)'
     }
     if (valgtVar == 'DegSponSSSten') {
          #(Først og fremst fusjonskirurgi)
          RegData$Variabel[which((RegData$RfSentr==1) & (RegData$RfSpondtypeDegen == 1))] <- 1
          TittelUt <- 'Degenerativ spondylolistese og sentral spinal stenose'
     }
     if (valgtVar == 'ErstatningPre') {
          #Pasientskjema. Andel med ErstatningPre 1 el 3
          #Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
          RegData <- RegData[which(RegData$ErstatningPre %in% 1:4), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
          TittelUt <- 'Pasienten har søkt/planlegger å søke erstatning'
     }
     if (valgtVar =='Fornoyd') {	#%in% c('Fornoyd3mnd','Fornoyd12mnd')) {
          #3/12mndSkjema. Andel med Fornøyd/litt fornøyd (1,2)
          #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
 		  RegData$Fornoyd <- switch(as.character(ktr), 
						'1'= RegData$FornoydBeh3mnd,
						'2'= RegData$FornoydBeh12mnd)
          RegData <- RegData[which(RegData$Fornoyd %in% 1:5), ]
          RegData$Variabel[which(RegData$Fornoyd %in% 1:2)] <- 1
          TittelUt <- paste0('Fornøyde pasienter, 3 mnd.' ,ktrtxt)
     }
     if (valgtVar == 'Kp3Mnd') {
          #Komplikasjoner 0:nei, 1:ja
          RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Pasientrapporterte komplikasjoner'
     }
     if (valgtVar == 'KpInf3Mnd') {
          #Komplikasjoner 0:nei, 1:ja
          RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
          RegData$Variabel <- RegData[ ,valgtVar]
          TittelUt <- 'Pasientrapporterte komplikasjoner'
     }

     if (valgtVar == 'Misfornoyd') {	#%in% c('Misfor3mnd','Misfor12mnd')) {
          #3/12mndSkjema. Andel med Misfornøyd/litt misfornøyd (1,2)
          #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
		  RegData$Misfornoyd <- switch(as.character(ktr), 
						'1'= RegData$FornoydBeh3mnd,
						'2'= RegData$FornoydBeh12mnd)
          RegData <- RegData[which(RegData$Misfornoyd %in% 1:5), ]
          RegData$Variabel[which(RegData$Misfornoyd %in% 4:5)] <- 1
          TittelUt <- paste0('Misfornøyde pasienter, 3 mnd.' ,ktrtxt)
     }
      if (valgtVar == 'Morsmal') {
#           Kode 1:3:'Norsk', 'Samisk', 'Annet'
            RegData <- RegData[which(RegData$Morsmal %in% 1:3), ]
            RegData$Variabel[which(RegData$Morsmal %in% 2:3)] <- 1 
            TittelUt <- 'Fremmedspråklige (ikke norsk som morsmål)'
      }
      
     if (valgtVar == 'Nytte') {
          #Andel med helt bra/mye bedre (1:2)
          #Kode 1:7: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
          #				'Verre enn noen gang', 'Ukjent')
           RegData$Nytte <- switch(as.character(ktr), 
							'1'=RegData$Nytte3mnd,
							'2'=RegData$Nytte12mnd)
		  RegData <- RegData[which(RegData$Nytte %in% 1:7), ]
          RegData$Variabel[RegData$Nytte %in% 1:2] <- 1
          TittelUt <- paste0('Helt bra eller mye bedre' , ktrtxt)
     }

    if (valgtVar == 'OswEndrLav') {
          #Mislykkede operasjoner
		  RegData$OswEndr <- switch(as.character(ktr), 
						'1'= (RegData$OswTotPre - RegData$OswTot3mnd),
						'2'= (RegData$OswTotPre - RegData$OswTot12mnd))
          RegData <- RegData[which(RegData$OswEndr >= -100), ]
          RegData$Variabel[which(RegData$OswEndr <13)] <- 1
          TittelUt <- paste0('Forbedring av Oswestry-skår < 13 poeng', ktrtxt)
    }
      if (valgtVar == 'OswEndr20') {
            #Mislykkede operasjoner
            RegData$OswEndr <- switch(as.character(ktr), 
                                      '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                      '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
            RegData <- RegData[which(RegData$OswEndr >= -100), ]
            RegData$Variabel[which(RegData$OswEndr >20)] <- 1
            TittelUt <- paste0('Forbedring av Oswestry-skår > 20 poeng', ktrtxt)
      }
      
if (valgtVar == 'OswEndr30pst') {
         #Andel med klinisk signifikant forbedring i Oswestry-skår. 
		 #Forbedring = nedgang
         RegData$OswPst <- switch(as.character(ktr),
                             '1' = (RegData$OswTotPre - RegData$OswTot3mnd)/RegData$OswTotPre*100,
                             '2' = (RegData$OswTotPre - RegData$OswTot12mnd)/RegData$OswTotPre*100)
         RegData <- RegData[which(RegData$OswPst>=-1000), ]
        RegData$Variabel[which(RegData$OswPst >30)] <- 1
         TittelUt <- paste0('Mer enn 30% forbedring av Oswestry-skår', ktrtxt)
    }
if (valgtVar == 'Osw48') {
         #Andel med Oswestry-skår fortsatt over 48. 
         RegData$OswPost <- switch(as.character(ktr),
                             '1' = RegData$OswTot3mnd,
                             '2' = RegData$OswTot12mnd)
         RegData <- RegData[which(RegData$OswPost>=0), ]
        RegData$Variabel[which(RegData$OswPost >48)] <- 1
         TittelUt <- paste0('Oswestry-skår > 48 poeng', ktrtxt)
    }
	 
      if (valgtVar=='KpInf3Mnd') {
            #Infeksjoner
            #Kode 1:Ja,  0:Nei 
            RegData$Variabel[which(RegData$PeropKomp == 1)] <- 1
            TittelUt <- 'Sårinfeksjon'
      }
      if (valgtVar=='PeropKomp') {
          #Komplikasjoner ved operasjon
          #Kode 1:Ja,  tomme:Nei 
          RegData$Variabel[which(RegData$PeropKomp == 1)] <- 1
          TittelUt <- 'Komplikasjoner ved operasjon'
     }
     if (valgtVar=='PeropKompDura') {
          #Durarift ved operasjon
          #Kode 1:Ja,  tomme:Nei 
          RegData$Variabel[which(RegData$PeropKompDura == 1)] <- 1
          TittelUt <- 'Komplikasjon ved operasjon: Durarift'
     }
     if (valgtVar=='Roker') {
          #PasientSkjema. Andel med Roker=1
          #Kode 0,1,tom: Nei, Ja Ukjent
          RegData <- RegData[which(RegData$Roker %in% 0:1), ]
          RegData$Variabel <- RegData$Roker
          TittelUt <- 'Røykere'
     }

     if (valgtVar == 'Saardren') {
          #LegeSkjema. Andel med Saardren=1
          #Kode 0,1,tom: Nei, Ja Ukjent
          RegData <- RegData[which(RegData$Saardren %in% 0:1), ]
          RegData$Variabel <- RegData$Saardren
          TittelUt <- 'Andel som får sårdren'
     }

     if (valgtVar == 'SmStiPre') {
          #PasientSkjema. Andel med SmStiPre=1
          #Kode 0,1,tom: Nei, Ja Ukjent
          RegData <- RegData[which(RegData$SmStiPre %in% 0:1), ]
          RegData$Variabel <- RegData$SmStiPre
          TittelUt <- 'Bruker smertestillende, før operasjon'
     }

     if (valgtVar == 'SymptVarighRyggHof') {
          #PasientSkjema. Andel med SymptVarighRyggHof 4 el 5
          #Kode 1:5,tom: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
          RegData <- RegData[which(RegData$SymptVarighRyggHof %in% 1:5), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
          VarTxt <- 'med varighet minst 1 år'
          TittelUt <- 'Varighet av rygg-/hoftesmerter minst ett år'
     }

     if (valgtVar == 'SympVarighUtstr') {
          #PasientSkjema. Andel med SympVarighUtstr 4 el 5
          #Kode 1:5,tom: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
          RegData <- RegData[which(RegData$SympVarighUtstr %in% 1:5), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
          VarTxt <- 'med varighet minst 1år'
          TittelUt <- 'Varighet av utstrålende smerter minst ett år'
     }

     if (valgtVar == 'UforetrygdPre') {
          #PasientSkjema. Andel med UforetrygdPre 1 og 3
          #Kode 1:4,tom: 'Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
          RegData <- RegData[which(RegData$UforetrygdPre %in% 1:4), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
          VarTxt <- 'søkt/planlagt å søke'
          TittelUt <- 'Søkt eller planlegger å søke uføretrygd?'
     }
     if (valgtVar == 'Utd') {
          #PasientSkjema. Andel med Utdanning 4 el 5
          #Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
          #Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
          RegData <- RegData[which(RegData$Utd %in% 1:5), ]
          RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
          VarTxt <- 'med høyere utdanning'
          TittelUt <- 'Andel høyskole-/universitetsutdannede'
     }
     if (valgtVar == 'Verre') {		#%in% c('Verre3mnd','Verre12mnd')) {
          #3/12mndSkjema. Andel med helt mye verre og noen sinne (6:7)
          #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
          #				'Verre enn noen gang', 'Ukjent')
          RegData$Nytte <- switch(as.character(ktr), 
							'1'=RegData$Nytte3mnd,
							'2'=RegData$Nytte12mnd)
		  RegData <- RegData[which(RegData$Nytte %in% 1:7), ]
          RegData$Variabel[RegData$Nytte %in% 6:7] <- 1
          TittelUt <- paste0('Mye verre/verre enn noen gang' , ktrtxt)
     }


     #Gjør utvalg
     RyggUtvalg <- RyggUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, hovedkat=hovedkat, tidlOp=tidlOp, opKat=opKat)
     RegData <- RyggUtvalg$RegData
     utvalgTxt <- RyggUtvalg$utvalgTxt

     
     #Standardisere mht grupperingsvariabel. Først sykehus.
     

     dummy0 <- -0.001
     N <- dim(RegData)[1]
     Nvar <- tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)
     if(N > 0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}
     AntGr <- length(which(Ngr >= Ngrense))	#Alle som har gyldig resultat
     AndelerGr <- round(100*Nvar/Ngr,2)

     indGrUt <- as.numeric(which(Ngr < Ngrense))
     if (length(indGrUt)==0) { indGrUt <- 0}
     AndelerGr[indGrUt] <- dummy0
     sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE)
     Ngrtxt <- paste('N=', as.character(Ngr), sep='')	#
     Ngrtxt[indGrUt] <- paste('N<', Ngrense,sep='')	#paste(' (<', Ngrense,')',sep='')	#

     AndelerGrSort <- AndelerGr[sortInd]
     AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
     #	GrNavnSort <- paste(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd], sep='')
     GrNavnSort <- names(Ngr)[sortInd]

     andeltxt <- paste(sprintf('%.1f',AndelerGrSort), '%',sep='') 	#round(as.numeric(AndelerGrSort),1)
     if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}

     if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt}

     #-----------Figur---------------------------------------
     if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
          FigTypUt <- rapbase::figtype(outfile)
          farger <- FigTypUt$farger
          plot.new()
          if (dim(RegData)[1]>0) {
               tekst <- paste('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene', sep='')
          } else {tekst <- 'Ingen registrerte data for dette utvalget'}
          title(main=Tittel)
          text(0.5, 0.6, tekst, cex=1.2)
          legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
          if ( outfile != '') {dev.off()}

     } else {

          #--------------------------FIGUR---------------------------------------------------
          #Innparametre: ...


          FigTypUt <- figtype(outfile, height=3*800, fargepalett=RyggUtvalg$fargepalett)
          farger <- FigTypUt$farger
          #Tilpasse marger for å kunne skrive utvalgsteksten
          NutvTxt <- length(utvalgTxt)
          vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
          #NB: strwidth oppfører seg ulikt avh. av device...
          par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

          xmax <- min(max(AndelerGrSort),100)*1.15
          pos <- barplot(as.numeric(AndelerGrSort), horiz=T, border=NA, col=farger[3], #main=Tittel,
                         xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(Ngr), font.main=1, xlab='Andel (%)', las=1, cex.names=cexShNavn*0.9)
          ybunn <- 0.1
          ytopp <- pos[AntGr]+1	#-length(indGrUt)]
          lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
          legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
                 legend=paste(smltxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N,sep='' ),
                 bty='o', bg='white', box.col='white')
          mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
          text(x=0.005*xmax, y=pos, Ngrtxt[sortInd], las=1, cex=cexShNavn, adj=0, col=farger[4], lwd=3)	#c(Nshtxt[sortInd],''),
          title(Tittel, line=1, font.main=1, cex.main=1.3)

          text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
               las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus

          #Tekst som angir hvilket utvalg som er gjort
          mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))


          par('fig'=c(0, 1, 0, 1))
          if ( outfile != '') {dev.off()}
          #----------------------------------------------------------------------------------
     }
}
