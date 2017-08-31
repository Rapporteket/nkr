#' Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med gjennomsnitt/median
#' for hvert sykehus og kan ta inn ulike numeriske variable.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
#'
#' @inheritParams RyggAndeler
#' @inheritParams RyggAndelerGrVar
#' @param valgtMaal 'Med' = median. Alt annet gir gjennomsnitt 
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Pasientens alders 
#'     \item SMR: Standardisert mortalitetsratio (Gir annen figurtype)
#'     \item liggetid: Liggetid 
#'     \item Nas: Skår for sykepleieraktiviteter. (Nursing Activities Score). Per døgn.
#'     \item NEMS: Skår for ressursbruk per opphold. (Nine Equivalents of Nursing Manpower Use Score)
#'     \item NEMS24: NEMS-skår per døgn. 
#'     \item respiratortid: Tid tilbrakt i respirator
#'     \item SAPSII: Skår for alvorlighetsgrad av sykdom.  (Simplified Acute Physiology Score II)
#'    }
#'
#' Detajer: Her bør man liste opp hvilke variable funksjonen benytter.
#'
#' @return Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
#'
#' @export


RyggGjsnGrVar <- function(RegData, valgtVar, preprosess=1, hentData=0, valgtMaal='Gjsn', 
                  minald=0, maxald=130, datoFra='2011-01-01', datoTil='3000-01-01', aar=0,
                  grType=99, InnMaate=99, dodInt='', erMann='', grVar='ShNavn', medKI=1, 
                  lagFig=1, outfile) {
      
      
if (hentData == 1) {		
  RegData <- RyggRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess){
       RegData <- RyggPreprosess(RegData=RegData) #, reshID=reshID)
     }

#------- Tilrettelegge variable
RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, figurtype = 'gjsnGrVar')
RegData <- RyggVarSpes$RegData

#------- Gjøre utvalg
RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, datoFra=datoFra, datoTil=datoTil, aar=aar, minald=minald, maxald=maxald, 
                          erMann=erMann, InnMaate=InnMaate, dodInt=dodInt, grType=grType) #overfPas=overfPas, 
RegData <- RyggUtvalg$RegData
utvalgTxt <- RyggUtvalg$utvalgTxt

Ngrense <- 10		
'%i%' <- intersect

RegData[ ,grVar] <- as.factor(RegData[ ,grVar])
#Grupper som ikke har registreringer vil nå ikke komme med i oversikta. Gjøres dette tidligere, vil alle
#grupper komme med uansett om de ikke har registreringer.

if(dim(RegData)[1]>0) {Ngr <- table(RegData[ ,grVar])}	else {Ngr <- 0}


t1 <- switch(valgtMaal,
			Med = 'Median ',
			Gjsn = 'Gjennomsnittlig ')


#tittel <- paste0(t1, valgtVar, ', ', RyggUtvalg$grTypeTxt, 'sykehus')
tittel <- paste0(t1, RyggVarSpes$tittel) 
			
if( valgtVar =='SMR') {tittel <- c(paste0('SMR, ', RyggUtvalg$grTypeTxt, 'sykehus'),
								'(uten reinnlagte og overflyttede pasienter)')}

Ngrtxt <- paste0(' (', as.character(Ngr),')') 
indGrUt <- which(Ngr < Ngrense)
if (length(indGrUt)==0) { indGrUt <- 0}
Ngrtxt[indGrUt] <- paste0(' (<', Ngrense,')')	
N <- dim(RegData)[1]

KIHele <- c(0,0)    
KIned <- c(0,0)
KIhele <- c(0,0)
		

dummy0 <- NA #-0.0001
#Kommer ut ferdig sortert!
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData[ ,grVar], RegData$Variabel, notch=TRUE, plot=FALSE)
	MedIQR$stats[ ,indGrUt] <- dummy0
	MedIQR$conf[ ,indGrUt] <- dummy0
	sortInd <- order( MedIQR$stats[3,], decreasing=RyggVarSpes$sortAvtagende, na.last = FALSE) 
	Midt <- as.numeric(MedIQR$stats[3, sortInd])
	KIned <- MedIQR$conf[1, sortInd]
	KIopp <- MedIQR$conf[2, sortInd]
	MedIQRHele <-  boxplot.stats(RegData$Variabel, do.conf = TRUE)
	MidtHele <- as.numeric(MedIQRHele$stats[3])	#median(RegData$Variabel)
	KIHele <- MedIQRHele$conf
	#Hvis vil bruke vanlige konf.int:
	#j <- ceiling(N/2 - 1.96*sqrt(N/4))
	#k <- ceiling(N/2 + 1.96*sqrt(N/4))
	#KIHele <- sort(RegData$Variabel)[c(j,k)]
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).) 
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared, 
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give 
#roughly a 95% confidence interval for the difference in two medians. 	
	} 
	
if (valgtMaal=='Gjsn') {	#Gjennomsnitt er standard, men må velges.
	if (valgtVar=='SMR') { #Bør tas ut av Gjsn...?
		medKI <- 0
		ObsGr <- tapply(RegData$Dod30, RegData[ ,grVar], mean, na.rm=T)
		EstGr <- tapply(RegData$SMR, RegData[ ,grVar], mean, na.rm=T)
		ind0 <- which(EstGr == 0)
		Gjsn <- 100*ObsGr/EstGr  
		if (length(ind0)>0) {Gjsn[ind0] <- 0}#Unngå å dele på 0
		#Vi benytter ikke konf.int for SMR. Setter alle SE lik 0
		     #TestPoGr <- which((Ngr*ObsGr-3*sqrt(Ngr*ObsGr*(1-ObsGr)) <= 0) | (Ngr*ObsGr+3*sqrt(Ngr*ObsGr*(1-ObsGr)) > Ngr))
		     #SE <- sqrt(ObsGr*(1-ObsGr))*100/(sqrt(Ngr)*EstGr)
		     #if (length(TestPoGr)>0) {SE[TestPoGr] <- 0}
		SE <- rep(0, length(Ngr))
		Obs <-  mean(RegData$Dod30)	#Kun 0 og 1
		Est <- mean(RegData$Variabel, na.rm=T)
		MidtHele <- ifelse(Est ==0, 0, 100*Obs/Est)
	} else {
	      Gjsn <- tapply(RegData$Variabel, RegData[ ,grVar], mean, na.rm=T)
		SE <- tapply(RegData$Variabel, RegData[ ,grVar], sd, na.rm=T)/sqrt(Ngr)
		MidtHele <- mean(RegData$Variabel)	#mean(RegData$Variabel)
		KIHele <- MidtHele + sd(RegData$Variabel)/sqrt(N)*c(-2,2)
	}
      
	Gjsn[indGrUt] <- dummy0
	SE[indGrUt] <- 0
	sortInd <- order(Gjsn, decreasing=RyggVarSpes$sortAvtagende, na.last = FALSE) 
	Midt <- Gjsn[sortInd] #as.numeric(Gjsn[sortInd])
	KIned <- Gjsn[sortInd] - 2*SE[sortInd]
	KIopp <- Gjsn[sortInd] + 2*SE[sortInd]
	}


#if (sum(which(Ngr < Ngrense))>0) {indGrUt <- as.numeric(which(Ngr<Ngrense))} else {indGrUt <- 0}
#AndelerGr[indGrUt] <- -0.0001

GrNavnSort <- paste0(names(Ngr)[sortInd], Ngrtxt[sortInd])
if (valgtVar == 'SMR') {AntDes <- 2} else {AntDes <- 1} 
soyletxt <- sprintf(paste0('%.', AntDes,'f'), Midt) 
#soyletxt <- c(sprintf(paste0('%.', AntDes,'f'), Midt[1:AntGr]), rep('',length(Ngr)-AntGr))
indUT <- which(is.na(Midt))  #Rydd slik at bare benytter indGrUt
soyletxt[indUT] <- ''
KIned[indUT] <- NA
KIopp[indUT] <- NA

AggVerdier <- list(Hoved=Midt, Rest=0, KIned=KIned, KIopp=KIopp, KIHele=KIHele)
Ngr <- list(Hoved=Ngr[sortInd], Rest=0)


#Se RyggFigSoyler for forklaring av innhold i lista GjsnGrVarData
GjsnGrVarData <- list(AggVerdier=AggVerdier, #Endres til Soyleverdi? Evt. AggVerdier
                        AggTot=MidtHele, #Til AggVerdiTot?
                         N=list(Hoved=N), 
                         Ngr=Ngr,
                         grtxt2='', 
				 medKI=medKI,
				 KImaal = RyggVarSpes$KImaal,
                         soyletxt=soyletxt,
                         grtxt=GrNavnSort,
				 valgtMaal=valgtMaal,
                         tittel=tittel,    #RyggVarSpes$tittel, 
                         #yAkseTxt=yAkseTxt, 
                         retn='H', 
                         xAkseTxt=RyggVarSpes$xAkseTxt,
                         grTypeTxt=RyggUtvalg$grTypeTxt,			 
                         utvalgTxt=RyggUtvalg$utvalgTxt, 
                         fargepalett=RyggUtvalg$fargepalett, 
                         medSml=RyggUtvalg$medSml, 
                         smltxt=RyggUtvalg$smltxt)


#FigDataParam skal inn som enkeltparametre i funksjonskallet
if (lagFig == 1) {
      cexgr <- 1-ifelse(length(soyletxt)>20, 0.25*length(soyletxt)/60, 0)
#---------------------------------------FRA FIGANDELER, FigGjsnGrVar og FigAndelGrVar--------------------------
#Hvis for få observasjoner..

if (dim(RegData)[1] < 10 )
    #|(grVar=='' & length(which(RegData$ReshId == reshID))<5 & enhetsUtvalg %in% c(1,3))) 
    {
	#-----------Figur---------------------------------------
      FigTypUt <-figtype(outfile)  #FigTypUt <- figtype(outfile)
	farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	if (valgtMaal=='Med' & grepl('SMR', tittel)) {tekst <- 'Ugyldig parameterkombinasjon'   #valgtVar=='SMR'
		} else {tekst <- 'For få registreringer'}
	text(0.5, 0.6, tekst, cex=1.2)
	if ( outfile != '') {dev.off()}
	
} else {
	

	#Plottspesifikke parametre:
	#Høyde må avhenge av antall grupper
	hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
	FigTypUt <- figtype(outfile, height=hoyde, fargepalett=fargepalett)	
	#Tilpasse marger for å kunne skrive utvalgsteksten
	NutvTxt <- length(utvalgTxt)
	vmarg <- switch(retn, V=0.04, H=min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75)))
	#NB: strwidth oppfører seg ulikt avh. av device...
	par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
	
	
	farger <- FigTypUt$farger
	fargeHoved <- ifelse(grVar %in% c('ShNavn'), farger[4], farger[1])
	fargeRest <- farger[3]
	graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
	antGr <- length(grtxt)
	lwdRest <- 3	#tykkelse på linja som repr. landet
	cexleg <- 0.9	#Størrelse på legendtekst
	
	
	#Horisontale søyler
if (retn == 'H') {
	#Definerer disse i beregningsfunksjonen?  
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
      xmax <- ifelse(valgtMaal=='Andel', min(xmax, 100), xmax) 	#100 som maks bare hvis andelsfigur..
	  ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
	  ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved) 

	  #Må def. pos først for å få strek for hele gruppa bak søylene
	  ### reverserer for å slippe å gjøre det på konf.int
	  pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=T, xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
	                     xlab=xAkseTxt, border=NA, col=fargeHoved)) #, col.axis='white', col='white'))
	  indOK <- which(AggVerdier$Hoved>=0)
	  posOK <- pos[indOK]
	  posOver <- max(pos)+0.35*log(max(pos))
	  posDiff <- 1.2*(pos[1]-pos[2])
	  posOK <- pos[indOK]
	  minpos <- min(posOK)-0.7
	  maxpos <- max(posOK)+0.7
	  
	  if (medKI == 1) {	#Legge på konf.int for hele populasjonen
	        #options(warn=-1)	#Unngå melding om KI med lengde 0
	        KIHele <- AggVerdier$KIHele
	        AntGr <- length(which(AggVerdier$Hoved>0))
	        polygon(c(rep(KIHele[1],2), rep(KIHele[2],2)), col=farger[3], border=farger[3],
	                c(minpos, maxpos, maxpos, minpos))
	  }

	if (grVar %in% c('ShNavn')) {	#Må si noe om den "gamle figurtypen"
	      #grtxt <- rev(grtxt)
	      grTypeTxt <- smltxt
	      mtext(at=posOver, paste0('(N)' ), side=2, las=1, cex=cexgr, adj=1, line=0.25)
	      #Linje for hele landet/utvalget:
	      lines(x=rep(AggTot, 2), y=c(minpos, maxpos), col=farger[1], lwd=2.5) #y=c(0, max(pos)+0.55), 
	      #Linje for kvalitetsindikatormål:
	      if (!is.na(KImaal)) { 
	            lines(x=rep(KImaal, 2), y=c(minpos, maxpos), col= '#FF7260', lwd=2.5) #y=c(0, max(pos)+0.55), 
	            text(x=KImaal, y=maxpos+0.6, paste0('Mål:', KImaaltxt), cex=0.9*cexgr, col= '#FF7260',adj=c(0.5,0)) 
	      }
	      barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, add=TRUE,
	              col=fargeHoved, border=NA, cex.names=cexgr) #, xlim=c(0, xmax), ylim=c(ymin,ymax)
	      soyleXpos <- 1.12*xmax*max(strwidth(soyletxt, units='figure')) # cex=cexgr
	      text(x=soyleXpos, y=pos+0.1, soyletxt, las=1, cex=cexgr, adj=1, col=farger[1])	#AggVerdier, hvert sykehus
	      }


	if (medKI == 1) {	#Legge på konf.int for hver enkelt gruppe/sykehus
	      arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIopp, y1=pos, 
	             length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	      arrows(x0=AggVerdier$Hoved, y0=pos, x1=AggVerdier$KIned, y1=pos, 
	             length=0.5/max(pos), code=2, angle=90, lwd=1, col=farger[1])
	}
	#------Tegnforklaring (legend)--------
	if (valgtMaal %in% c('Gjsn', 'Med')) { #Sentralmålfigur
#	if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) { #Sentralmålfigur
	  if (medKI == 0) { #Hopper over hvis ikke valgtMaal er oppfylt
	            TXT <- paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N$Hoved)
      	      legend(xmax/4, posOver+posDiff, TXT, fill=NA,  border=NA, lwd=2.5, xpd=TRUE, #inset=c(-0.1,0),
      	             col=farger[1], cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
	      } else {
	            TXT <- c(paste0('totalt: ', sprintf('%.1f', AggTot), ', N=', N$Hoved), 
	                     paste0('95% konf.int., ', grTypeTxt, 'sykehus (', 
	                            sprintf('%.1f', KIHele[1]), '-', sprintf('%.1f', KIHele[2]), ')'))
      	      legend(xmax/4, posOver+2*posDiff, TXT, fill=c(NA, farger[3]),  border=NA, lwd=2.5,  #inset=c(-0.1,0),
      	             col=c(farger[1], farger[3]), cex=cexleg, seg.len=0.6, merge=TRUE, bty='n')
	      }
	} 
	  #else { 
	  if (figurtype %in% c('andelGrVar', 'andelTid')) {
	      legend(xmax/4, posOver+2*posDiff, paste0(grTypeTxt, 'sykehus: ', sprintf('%.1f', AggTot), '%, N=', N$Hoved),
	             col=farger[1], border=NA, lwd=2.5, xpd=TRUE, bty='n', cex = cexleg) 
	}
	  #Fordelingsfigurer:
	  #if (grVar == '') {
	  if (figurtype == 'andeler') {
      	  if (medSml == 1) { #Legge på prikker for sammenlikning
      	        legend(xmax/4, posOver+0.8*posDiff, c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
      	               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
      	               lwd=lwdRest, lty=NA, ncol=1)
      	  } else {	
      	        legend(xmax/4, posOver+0.8*posDiff, paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
      	               border=NA, fill=fargeHoved, bty='n', ncol=1)
      	  }
	  }
	#--------------------------------------


      #Legge på gruppe/søylenavn
      if (figurtype  == 'andeler') {
            grtxt <- paste(grtxt, grtxt2, sep='\n')}
     
            mtext(at=pos+0.05, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25) 
      
	  
	  #Fordelingsfigurer:
      #if (grVar == '') {
      	if (medSml == 1) { #Legge på prikker for sammenlikning
      		  points(as.numeric(AggVerdier$Rest), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
      	}
       #}
      }		#Slutt horisontale søyler
      	
	
	
	if (retn == 'V' ) {
		  #Vertikale søyler. Det er bare andeler som har vertikale søyler.
		  ymax <- min(max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.25, 115)
		  pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1, ylab=yAkseTxt,	
						 sub=xAkseTxt,	col=fargeHoved, border='white', ylim=c(0, ymax))	
		  mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
		  mtext(at=pos, grtxt2, side=1, las=1, cex=0.9*cexgr, adj=0.5, line=1.5, col=graa[2])
		  mtext(at=0,  paste0(hovedgrTxt,': '), side=1, cex=0.9*cexgr, adj=0.9, line=1.5, col=graa[2])
		  #legend(x=0, y=-0.05*ymax, legend=paste0(hovedgrTxt,':'), col=fargeRest,pch=18,bty="n",ncol=2, cex=0.9*cexgr, xpd=TRUE) #pt.cex=0.7,
		  
		  if (medSml == 1) {
		        grtxt3 <- paste0(sprintf('%.1f',AggVerdier$Rest), '%') #paste0('(', sprintf('%.1f',AggVerdier$Rest), '%)')
		        mtext(at=pos, grtxt3, side=1, las=1, cex=0.9*cexgr, adj=0.5, line=2.5, col=graa[2])
		        mtext(at=0,  paste0(smltxt,': '), side=1, cex=0.9*cexgr, adj=0.9, line=2.5, col=graa[2])
		        points(pos, as.numeric(AggVerdier$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
				legend('top', legend=c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
					   border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
					   lwd=lwdRest, ncol=2, cex=cexleg)
		  } else {	
				legend('top', legend=paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
					   border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		  }
	} 
	
  
				
	title(tittel, line=1.5) #cex.main=1.3)

	#Tekst som angir hvilket utvalg som er gjort
	avst <- 0.8
	utvpos <- 3	#Startlinje for teksten
	mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
	
	par('fig'=c(0, 1, 0, 1)) 
	if ( outfile != '') {dev.off()}
	
}  #Figur
}

return(invisible(GjsnGrVarData))

}



