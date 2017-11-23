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
#'     \item alder70: Pasienter over 75år    
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?    
#'     \item ArbstatusPre: Mottar sykepenger, før operasjon?    
#'     \item Arbstatus: Mottar sykepenger, etter operasjon?   
#'     \item ASA: ASA-grad > II    
#'	 \item BeinsmLavPre: Pasienter med preop. beinsmerte < 2.5 og ikke parese.
#'	 \item BeinsmEndrLav: Forbedring av beinsmerter under 1.5 poeng
#'     \item BMI: Pasienter med fedme (BMI>30)    
#'     \item degSponFusj: Degenerativ spondylolistese operert  med fusjonskirurgi
#'     \item degSponSSSten: Degenerativ spondylolistese og sentral spinal stenose
#'     \item ErstatningPre: Søkt/planlegger å søke erstatning?    
#'     \item Fornoyd: Fornøyde pasienter 
#'     \item KpInf3Mnd: Sårinfeksjon, pasientrapportert    
#'     \item Kp3Mnd: Pasientrapporterte komplikasjoner    
#'     \item Misfornoyd:  Andel med Misfornøyd/litt misfornøyd
#'     \item Nytte: Klart bedre   
#'	 \item OswEndrLav: Forbedring av Oswestry-skår < 13 poeng
#'	 \item OswEndr20: Forbedring av Oswestry-skår > 20 poeng
#'	 \item OswEndr30pst: Mer enn 30% forbedring i Oswestry-skår
#'	 \item Osw22: Oswestry-skår < 22 poeng
#'	 \item Osw48: Oswestry-skår > 48 poeng
#'     \item PeropKomp: Komplikasjon ved operasjon
#'     \item PeropKompDura: Komplikasjon ved operasjon: Durarift
#'     \item Roker: Røyker du?    
#'     \item Saardren: Sårdren    
#'     \item SmStiPre: Bruker smertestillende før operasjonen    
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år    
#'     \item SympVarighUtstr: Varighet av utstrålende smerter > 1 år 
#'     \item tidlOp3: Mer enn to tidligere operasjoner 
#'     \item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?    
#'     \item Utd: Andel høyskole-/universitetsutdannede    
#'     \item Verre Mye verre/verre enn noen gang, 3 mnd. 
#'		}
#'
#' @inheritParams RyggFigAndeler
#' @param grVar Tekstvariabel som angir hva skal resultatene grupperes på. 
#'                ShNavn-sykehus/avdeling
#'                Fylke- Pasienten bor i det akutelle fylket
#'                BoHF - Pasienten bor i boområdene til det angitte HF.
#'                BoRHF - Pasienten bor i boområdene til det angitte RHF.
#' @param valgtVar Variabelen det skal vises resultat for. Se \strong{Details} for oversikt.
#' @param Ngrense Minste antall registreringer for at ei gruppe skal bli vist
#'
#' @return Figur med...
#'
#' @export

RyggFigAndelerGrVar <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='3000-12-31', aar=0,
                            minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', hentData=0, 
							preprosess=1, opKat=99, enhetsUtvalg=0, grVar='ShNavn', tittel=1, ktr=0, 
							Ngrense=10, reshID=0, outfile='') {

	if (hentData == 1) {		
	  RegData <- RyggRegDataSQL()
	}

     # Preprosessere data
     if (preprosess){
          RegData <- RyggPreprosess(RegData=RegData)
     }
      #------- Tilrettelegge variable
            RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr, figurtype = 'andelGrVar')
            RegData <- RyggVarSpes$RegData
            sortAvtagende <- RyggVarSpes$sortAvtagende
            varTxt <- RyggVarSpes$varTxt
            KImaal <- RyggVarSpes$KImaal
            
     
     RegData[ ,grVar] <- factor(RegData[ ,grVar])
     
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
     
     
#---------------Beregninger
# Variabelen Variabel er definert som indikatorvariabel for den valgte variabelen. 
     
     if(dim(RegData)[1] > 0) {
           RegData <- RegData[which(RegData[ ,grVar] != ''),] #Tar ut registreringer uten grupperingsnavn
           RegData[ ,grVar] <- as.factor(RegData[ ,grVar])	
           Ngr <- table(RegData[ ,grVar])
     }	else {Ngr <- 0}
     N <- dim(RegData)[1]
     AntGr <- length(which(Ngr >= Ngrense))	#Alle som har gyldig resultat
     AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
     AndelerGr <- round(100*tapply(RegData$Variabel, RegData[ ,grVar], sum, na.rm=T)/Ngr,2)

     Ngrtxt <- as.character(Ngr) 
     indGrUt <- 0
     GrNavn <- names(Ngr)
     if (sum(which(Ngr < Ngrense))>0) {
           indGrUt <- as.numeric(which(Ngr<Ngrense)) #} else {indGrUt <- 0}
           AndelGrUt <- sum(AndelerGr[indGrUt]*Ngr[indGrUt], na.rm = T)/sum(Ngr[indGrUt])
           AndelerGr <- c(AndelGrUt, AndelerGr[-indGrUt]) #AndelerGr[indGrUt] <- NA
           GrNavn <- c(paste0(length(indGrUt), ' avd. med N<',Ngrense), names(Ngr)[-indGrUt])
           Ngrtxt <- c(Ngrtxt[-indGrUt],sum(Ngr[indGrUt]))  #Ngrtxt[indGrUt] <- paste0('<', Ngrense)
     }
     sortInd <- order(as.numeric(AndelerGr), decreasing=sortAvtagende, na.last = FALSE) 
     AndelerGrSort <- AndelerGr[sortInd]
     GrNavnSort <- paste0(GrNavn[sortInd], ' (',Ngrtxt[sortInd], ')')
     
     andeltxtUsort <- paste0(sprintf('%.1f',AndelerGr), ' %') 	
     #andeltxtUsort[indGrUt] <- ''
     andeltxt <- andeltxtUsort[sortInd]

     
     #N = list(Hoved=N, Rest=0)
     #Ngr = list(Hoved=Ngr, Rest=0)
     #AggVerdier = list(Hoved=AndelerGrSort, Rest=0)
     xAkseTxt <- "Andel opphold (%)"	
     
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
           #----------- Figurparametre ------------------------------
           cexShNavn <- 1 #0.85
           
           
           FigTypUt <- figtype(outfile, height=3*800, fargepalett=RyggUtvalg$fargepalett)
           farger <- FigTypUt$farger
           #Tilpasse marger for å kunne skrive utvalgsteksten
           NutvTxt <- length(utvalgTxt)
           vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.7)
           #NB: strwidth oppfører seg ulikt avh. av device...
           par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
           
           xmax <- min(max(AndelerGrSort, na.rm=T),100)*1.15
           pos <- rev(barplot(rev(as.numeric(AndelerGrSort)), horiz=T, border=NA, col=farger[4], #main=Tittel,
                              xlim=c(0,xmax), ylim=c(0.05, 1.25)*length(GrNavnSort), font.main=1, #xlab='Andel (%)', 
                              las=1, cex.names=cexShNavn*0.9))
           mtext('Andel (%)', side=1, line=2)
           ybunn <- 0.1
           ytopp <- rev(pos)[AntGr]+1	
           #Linje for hele landet/utvalget:
           lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[2], lwd=2)
           legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
                  legend=paste0(hovedgrTxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
                  bty='o', bg='white', box.col='white')
           mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
           
           
           title(Tittel, line=1, font.main=1, cex.main=1.3)
           
           text(x=AndelerGrSort+xmax*0.01, y=pos+0.1, andeltxt,
                las=1, cex=0.9, adj=0, col=farger[1])	#Andeler, hvert sykehus
           
           mtext(at=max(pos)+0.4*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=1, adj=1, line=0.25)
           
           #Tekst som angir hvilket utvalg som er gjort
           mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
           
           # if (indGrUt[1]>0){
           # mtext(paste0('* ',length(indGrUt),  ' avdelinger har mindre enn ', Ngrense,' registreringer og er fjernet fra figuren'), 
           #              side=1, at=-0.2*xmax, las=1, cex=0.8, adj=0, col=farger[1], line=3)}
           
           par('fig'=c(0, 1, 0, 1))
           if ( outfile != '') {dev.off()}
           #----------------------------------------------------------------------------------
     }
}

