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
#'     \item degSponFusj: Degenerativ spondylolistese operert  med fusjonskirurgi 
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
#' \item KpInf3Mnd: Sårinfeksjon
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

RyggFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='3000-12-31', aar=0,
                            minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', hentData=0, preprosess=1,
                            opKat=99, enhetsUtvalg=0, grVar='ShNavn', tittel=1, ktr=0, Ngrense=10, reshID=0, outfile='') {

	if (hentData == 1) {		
	  RegData <- RyggRegDataSQL()
	}

     # Preprosessere data
     if (preprosess){
          RegData <- RyggPreprosess(RegData=RegData)
     }
      #------- Tilrettelegge variable
            RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr) #, figurtype = 'andelGrVar')
            RegData <- RyggVarSpes$RegData
            sortAvtagende <- RyggVarSpes$sortAvtagende
            varTxt <- RyggVarSpes$varTxt
            KImaal <- RyggVarSpes$KImaal
            tittel <- RyggVarSpes$tittel
      

     #----------- Figurparametre ------------------------------
     cexShNavn <- 1 #0.85

     RegData[ ,grVar] <- factor(RegData[ ,grVar])
     #Ngrense <- 10		#Minste antall registreringer for at ei gruppe skal bli vist

     #------- Gjøre utvalg
     smltxt <- ''
     medSml <- 0
     
     if (reshID==0) {enhetsUtvalg <- 0}
     RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil, 
                                 minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                                 hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp,enhetsUtvalg=enhetsUtvalg)
     smltxt <- RyggUtvalg$smltxt
     medSml <- RyggUtvalg$medSml 
     hovedgrTxt <- RyggUtvalg$hovedgrTxt
     utvalgTxt <- RyggUtvalg$utvalgTxt
     ind <- RyggUtvalg$ind
     RegData <- RyggUtvalg$RegData
     
     
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
     Ngrtxt <- paste0('N=', as.character(Ngr))	#
     Ngrtxt[indGrUt] <- paste0('N<', Ngrense)	#paste(' (<', Ngrense,')',sep='')	#

     AndelerGrSort <- AndelerGr[sortInd]
     AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
     #	GrNavnSort <- paste(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd], sep='')
     GrNavnSort <- names(Ngr)[sortInd]

     andeltxt <- paste0(sprintf('%.1f',AndelerGrSort), '%') 	#round(as.numeric(AndelerGrSort),1)
     if (length(indGrUt)>0) {andeltxt[(AntGr+1):(AntGr+length(indGrUt))] <- ''}

     if (tittel==0) {Tittel<-''} else {Tittel <- RyggVarSpes$tittel}

     #-----------Figur---------------------------------------
     if 	( max(Ngr) < Ngrense)	{#Dvs. hvis ALLE er mindre enn grensa.
          FigTypUt <- rapbase::figtype(outfile)
          farger <- FigTypUt$farger
          plot.new()
          if (dim(RegData)[1]>0) {
               tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
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
                 legend=paste0(hovedgrTxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
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
