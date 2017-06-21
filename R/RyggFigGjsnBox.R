#' Utvikling over tid eller som funksjon av variabelen for gjennomsnitt/median av valgt variabel
#'
#' Figuren viser gjennomsnitt/median per år eller gruppe for valgt variabel.
#' I bakgrunn vises konfidensintervall for resten av landet. Eventuelt videreutvikle denne til å ha
#' en inn-parameter som sier noe om X-aksen skal være år, måned, pre-verdi eller noe annet.
#'
#' Skala for de ulike livskvalitetsmålene er definert slik:
#'    \itemize{
#'		\item EQ5D: Skala fra -0.594 tl 1, jo høyere jo bedre.
#'		\item Oswestry: Skala fra 0 til 100, hvor lavest er friskest
#'     	\item Smerte: Skalaen går fra 0 til 10, dvs. at differansen ligger mellom -10 og 10..
#'    }
#'		
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'		\item EQ5DEndr: Endring av EQ5D fra før til 3 eller 12 mnd. etter operasjonen 
#'		\item EQ5DPre: EQ5D før operasjon
#'          \item EQ5DEndrPre: EQ5D, endring vs. prescore
#'     	\item Liggedogn: Liggetid på sykehuset
#'     	\item OswEndrPre: Endring i Oswestry-skår fra før til etter operasjon
#'          \item OswEndrPre: Oswestry (ODI-Oswestry Disability Index.) Endring vs. prescore.
#'     	\item OswTotPre: Oswestry-skår før operasjon
#'     	\item SmBeinEndr: Endring i beinsmerter fra før til etter operasjon
#'          \item SmBeinEndrPre: Smerter i beina. Endring vs. prescore
#'     	\item SmBePre: Grad av beinsmerter før operasjon
#'     	\item SmRyggEndr: Endring av ryggsmerter fra før til etter operasjon
#'          \item SmRyggEndrPre: Smerter i ryggen. Endring vs. prescore 
#'     	\item SmRyPre: Ryggsmerter før operasjon
#'      \item TidOpReg Ønsker å se på tid fra operasjon til registrering
#'    }
#' @inheritParams RyggFigAndeler 
#' @param valgtMaal
#'        'Gjsn': gir middelverdi (standard)
#'        'Med': gir median
#' @param ktr Hvilken oppfølging man ønsker å se på. Valg: 3mnd - 3 mnd kontroll(standard), 12mnd - 12 mnd kontroll
#'          
#' @return Linjediagram som viser utvikling over tid for valgt variabel
#'
#' @export
RyggFigGjsnBox <- function(RegData, outfile, valgtVar, tidlOp='', erMann='', hovedkat=99, aar=0,
                    minald=0, maxald=130, ktr=0, tittel=1, datoFra='2007-01-01', datoTil='3000-01-01', 
                    valgtMaal='',enhetsUtvalg=1, hentData=0, preprosess=1, reshID=0){
      
  
      if (hentData == 1) {		
            RegData <- RyggRegDataSQL()   #(datoFra, datoTil)
      }
      
      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
      if (preprosess == 1){
            RegData <- RyggPreprosess(RegData=RegData)
      }
      

#------------ Figurparametre -------------------------
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
antDes <- 1

#Når bare skal sammenlikne med sykehusgruppe eller region, eller ikke sammenlikne, 
#trengs ikke data for hele landet:
reshID <- as.numeric(reshID)
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(2,3,4,6,7)) {	#Ta med 2,4 og 7? Oppr. 3 og 6
		RegData <- switch(as.character(enhetsUtvalg),
						'2' = RegData[which(RegData$ReshId == reshID),],	#kun egen enhet
						'3' = RegData[which(RegData$Sykehustype == RegData$Sykehustype[indEgen1]),],	#sml. shgruppe
						'4' = RegData[which(RegData$Sykehustype == RegData$Sykehustype[indEgen1]),],	#kun egen shgruppe
						'6' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),],	#sml region
						'7' = RegData[which(RegData$Region == as.character(RegData$Region[indEgen1])),])	#kun egen region
	}


	if (valgtVar == 'EQ5DPre') {KIekstrem <- c(0, 1.6)}
	if (valgtVar == 'OswTotPre') {KIekstrem <- c(0, 100)}
	if (valgtVar %in% c('SmBePre','SmRyPre')) {KIekstrem <- c(0, 10)}
	
	t1 <- switch(valgtVar,
		SmRyPre = 'Smerter i rygg', 
		SmBePre = 'Smerter i bein',
		OswTotPre = 'Oswestry',
		EQ5DPre = 'EQ5D')

IkkeTidsVar <- c('EQ5DEndrPre', 'OswEndrPre', 'SmBeinEndrPre', 'SmRyggEndrPre')

if (valgtVar %in% c('EQ5DPre', 'OswTotPre', 'SmBePre', 'SmRyPre')) {
	RegData$Variabel <- RegData[ ,valgtVar]
	TittelVar <- paste(t1, ' før operasjonen', sep='')
	ytxt1 <- paste('prescore av ', t1, sep='')
	} 	

	Xlab <- 'Operasjonsår'
	retn <- 1 #retning på aksetekst
	gr <- 0:10	
	GrNavn <- 0:10
	AntGr <- 11
	if (valgtVar %in% c('EQ5DEndr', 'EQ5DEndrPre')) {	
		t1 <- 'EQ5D '
		RegData$Pre <- RegData$EQ5DPre	#Forbedring= høyere EQ5D
		RegData$Post3mnd <- RegData$EQ5D3mnd
		RegData$Post12mnd <- RegData$EQ5D12mnd
		KIekstrem <- c(-1.6, 1.6)
		}
      if (valgtVar=='EQ5DEndrPre') {	
			Xlab <- 'EQ5D før operasjon'
            gr <- c(round(seq(-0.6,0.8,0.2),1),1.6)	#round(seq(-0.6,1.6,0.3),1)}
            RegData$Gr <- cut(RegData$Pre, gr, right=F)
            GrNavn <- levels(RegData$Gr)
            AntGr <- length(GrNavn)
            GrNavn[AntGr] <- '0.8+'
            retn <- 2	
      }
	if (valgtVar=='Liggedogn') {
		#For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
		dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
		RegData$Liggedogn[dagind]<-0
		RegData$Variabel <- RegData$Liggedogn
		TittelVar <- 'Liggetid ved operasjon'
		ytxt1 <- 'liggetid'
		KIekstrem <- c(0, 20)
		} 
      if (valgtVar %in% c('OswEndr', 'OswEndrPre')) {
		t1 <- 'Oswestry '
		RegData$Pre <- RegData$OswTotPre	#Forbedring=lavere Oswestry
		RegData$Post3mnd <- RegData$OswTot3mnd
		RegData$Post12mnd <- RegData$OswTot12mnd
		KIekstrem <- c(-100, 100)
		}
      if (valgtVar=='OswEndrPre') {
			Xlab <- 'Oswestry før operasjon'
            gr <- c(seq(0,90,10), 101)
            RegData$Gr <- cut(RegData$Pre, gr, right=F)
            GrNavn <- levels(RegData$Gr)
            AntGr <- length(GrNavn)
            GrNavn[AntGr] <- '[90,100]'
            retn <- 2}
	if (valgtVar %in% c('SmBeinEndr', 'SmBeinEndrPre')) {
		t1 <- 'beinsmerter '
		RegData$Pre <- RegData$SmBePre	#Forbedring = lavere smerte
		RegData$Post3mnd <- RegData$SmBe3mnd
		RegData$Post12mnd <- RegData$SmBe12mnd
		KIekstrem <- c(-10,10)
		}
	if (valgtVar %in% c('SmRyggEndr', 'SmRyggEndrPre')) {
	      t1 <- 'ryggsmerter '
	      RegData$Pre <- RegData$SmRyPre
	      RegData$Post3mnd <- RegData$SmRy3mnd
	      RegData$Post12mnd <- RegData$SmRy12mnd
	      KIekstrem <- c(-10,10)
	}
	if (valgtVar=='SmBeinEndrPre') {
			Xlab <- 'Beinsmerter før operasjon'
            RegData$Gr <- factor(RegData$Pre, levels=gr)
      }
      if (valgtVar=='SmRyggEndrPre') {
			Xlab <- 'Ryggsmerter før operasjon'
            RegData$Gr <- factor(RegData$Pre, levels=gr)
      }


if (valgtVar %in% c('EQ5DEndr', 'OswEndr', 'SmBeinEndr', 'SmRyggEndr',
						'EQ5DEndrPre', 'OswEndrPre', 'SmBeinEndrPre', 'SmRyggEndrPre')) {
	if (ktr==1) {RegData$Variabel <- (RegData$Post3mnd - RegData$Pre)
					ktrtxt <- '3 mnd etter'}
	if (ktr==2) {RegData$Variabel <- (RegData$Post12mnd - RegData$Pre)
					ktrtxt <- '12 mnd etter'}
      
	TittelVar <- paste('Forbedring av ', t1, ktrtxt, ' operasjon,', sep='')
	ytxt1 <- paste('endring av ', t1 ,sep='')
	}
 if (valgtVar %in% c('OswEndrPre', 'SmBeinEndrPre', 'SmRyggEndrPre',
						'OswEndr', 'SmBeinEndr', 'SmRyggEndr') ) {
            RegData$Variabel <- -RegData$Variabel}



#Gjør utvalg
RegData <- RegData[intersect(which(is.na(RegData$Variabel) == FALSE), 
							 which(is.nan(RegData$Variabel) == FALSE)), ]
RyggUtvalg <- RyggUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, hovedkat=hovedkat, tidlOp=tidlOp)
RegData <- RyggUtvalg$RegData
utvalgTxt <- RyggUtvalg$utvalgTxt

#Tar ut de med manglende registrering
	indMed <- intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != c('NaN')))
	if (dim(RegData)[1]>0) {RegData <- RegData[indMed, ]}


SykehustypeTxt <- c('universitetssykehus', 'lokalsykehus', 'private sykehus')				
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$ShNavn[indEgen1]) } else {
		shtxt <- switch(as.character(enhetsUtvalg), 	
			'0' = 'Hele landet',
			'4' = SykehustypeTxt[RegData$Sykehustype[indEgen1]],
			'5' = SykehustypeTxt[RegData$Sykehustype[indEgen1]],
			'7' = as.character(RegData$Region[indEgen1]),
			'8' = as.character(RegData$Region[indEgen1]))
			}
			
if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
			medSml <- 0
			indHoved <- 1:dim(RegData)	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
			indRest <- NULL
		} else {						#Skal gjøre sammenlikning
			medSml <- 1
			if (enhetsUtvalg %in% c(1,3,6)) {	#Involverer egen enhet
				indHoved <-which(as.numeric(RegData$ReshId)==reshID) } else {
				indHoved <- switch(as.character(enhetsUtvalg),
						'5' = which(RegData$Sykehustype == RegData$Sykehustype[indEgen1]),	#shgr
						'8' = which(RegData$Region == RegData$Region[indEgen1]))}	#region
			smltxt <- switch(as.character(enhetsUtvalg),
				'1' = 'landet forøvrig',
				'3' = paste(SykehustypeTxt[RegData$Sykehustype[indEgen1]], ' forøvrig', sep=''),	#RegData inneh. kun egen shgruppe
				'5' = 'alle andre typer sykehus',
				'6' = paste(RegData$Region[indEgen1], ' forøvrig', sep=''),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			indRest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
				'5' = which(RegData$Sykehustype != RegData$Sykehustype[indEgen1]),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$Region != RegData$Region[indEgen1]))
			}								
			
TittelUt <-  c(TittelVar, shtxt)	#c(TittelVar, hovedkattxt, paste(kjtxt, ', ', optxt, sep=''), shtxt)
if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt} 

		

if (length(indHoved)<5 | ((medSml == 1) & (length(indRest) < 5))) {
    #-----------Figur---------------------------------------
figtype(outfile)
	tekst <- 'Mindre enn 5 registreringer i egen eller sammenligningsgruppa'  
	plot.new()
	title(main=Tittel)
	text(0.5, 0.5, tekst,cex=1.5)	#, family="sans")
	if ( outfile != '') {dev.off()}
} else {

#GrNavn <- min(RegData$OpAar):max(RegData$OpAar)	
#RegData$OpAar <- factor(RegData$OpAar, levels=Aartxt)
#AntAar <- length(Aartxt)
#Tid endres til ordinal variabel og kalles Gr

if (!(valgtVar %in% IkkeTidsVar)) { #Må ha gjort utvalg før angir hvilke år som inkludert
	GrNavn <- min(RegData$OpAar):max(RegData$OpAar)
	RegData$Gr <- factor(RegData$OpAar, levels=GrNavn) #Denne gjelder når gr er år. 
}

 
#Resultat for hovedgruppe
N <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Gr'], length)
if (valgtMaal=='Med') {
	MedIQR <- plot(RegData$Gr[indHoved],RegData$Variabel[indHoved],  notch=TRUE, plot=FALSE)
	Midt <- as.numeric(MedIQR$stats[3, ])	#as.numeric(MedIQR$stats[3, sortInd])
	Konf <- MedIQR$conf
	#Hvis vil bruke vanlige konf.int:
	#j <- ceiling(N/2 - 1.96*sqrt(N/4))
	#k <- ceiling(N/2 + 1.96*sqrt(N/4))
	#KIHele <- sort(RegData$Variabel)[c(j,k)]
#The notches (if requested) extend to +/-1.58 IQR/sqrt(n). (Chambers et al. (1983, p. 62), given in McGill et al. (1978, p. 16).) 
#They are based on asymptotic normality of the median and roughly equal sample sizes for the two medians being compared, 
#and are said to be rather insensitive to the underlying distributions of the samples. The idea appears to be to give 
#roughly a 95% confidence interval for the difference in two medians. 	
} else {	#Gjennomsnitt blir standard.
	Midt <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Gr'], mean)
	SD <- tapply(RegData[indHoved ,'Variabel'], RegData[indHoved, 'Gr'], sd)
	Konf <- rbind(Midt - 2*SD/sqrt(N), Midt + 2*SD/sqrt(N))
}
	Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
	Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])

#Resten (gruppa det sammenliknes mot)
MidtRest <- NULL
KonfRest <- NULL
if (medSml ==  1) {
NRest <- tapply(RegData[indRest ,'Variabel'], RegData[indRest, 'Gr'], length)
	if (valgtMaal=='Med') {
		MedIQRrest <- plot(RegData$Gr[indRest],RegData$Variabel[indRest],  notch=TRUE, plot=FALSE)
		MidtRest <- as.numeric(MedIQRrest$stats[3, ])
		KonfRest <- MedIQRrest$conf
	} else {
	MidtRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Gr'], mean)	#indRest
	SDRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Gr'], sd)
	NRest <- tapply(RegData[indRest,'Variabel'], RegData[indRest, 'Gr'], length)
	KonfRest <- rbind(MidtRest - 2*SDRest/sqrt(NRest), MidtRest + 2*SDRest/sqrt(NRest))
	}
	KonfRest <- replace(KonfRest, which(KonfRest < KIekstrem[1]), KIekstrem[1])
	KonfRest <- replace(KonfRest, which(KonfRest > KIekstrem[2]), KIekstrem[2])
}
#-----------Figur---------------------------------------
#xmin <- 0.5
#xmax <- AntGr+0.5
cexgr <- 0.9	#Kan endres for enkeltvariable
ymin <- min(KonfRest, Konf, na.rm=TRUE)	#ymin1 - 2*h
ymax <- 1.1*max(KonfRest, Konf, na.rm=TRUE)	#ymax1 + 2*h
if (valgtMaal=='Med') {maaltxt <- 'Median ' } else {maaltxt <- 'Gjennomsnittlig '}
ytxt <- paste(maaltxt, ytxt1, sep='')
AntGr <- length(GrNavn)
AntGrVek <- 1:AntGr


#Plottspesifikke parametre:
FigTypUt <- rapbase::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
par('fig'=c(0, 1, 0, 1-0.02*(max((NutvTxt-1),0))))	
	
farger <- FigTypUt$farger
fargeHovedRes <- farger[1]
fargeRestRes <- farger[4]

plot(AntGrVek, Midt, ylim=c(ymin, ymax), type='n', frame.plot=FALSE, #xlim= c(xmin, xmax), ylim=c(ymin-0.05*ymax, ymax),
		#cex=0.8, cex.lab=0.9, cex.axis=0.9,	
		ylab=c(ytxt,'med 95% konfidensintervall'), 
		xaxt='n', xlab=Xlab, 
		sub='(Tall i boksene angir antall operasjoner)', cex.sub=cexgr)	#, axes=F)
axis(side=1, at = AntGrVek, labels=GrNavn, cex.axis=0.9, las=retn)	
#Sammenlikning:
if (medSml==1) {
	polygon( c(AntGrVek, AntGrVek[AntGr:1]), c(KonfRest[1,], KonfRest[2,AntGr:1]), 
			col=fargeRestRes, border=NA)
	legend('top', bty='n', fill=fargeRestRes, border=fargeRestRes, cex=cexgr,
		paste('95% konfidensintervall for ', smltxt, ', N=', sum(NRest, na.rm=T), sep=''))
}
h <- strheight(1, cex=cexgr)*0.7	#,  units='figure',
b <- 1.1*strwidth(max(N, na.rm=T), cex=cexgr)/2	#length(AntGrVek)/30
rect(AntGrVek-b, Midt-h, AntGrVek+b, Midt+h, border = fargeHovedRes, lwd=1)	#border=farger[4], col=farger[4]
text(AntGrVek, Midt, N, col=fargeHovedRes, cex=cexgr) 	

#Konfidensintervall:
ind <- which(Konf[1, ] > Midt-h) #Konfidensintervall som er tilnærmet 0
options('warn'=-1)
arrows(x0=AntGrVek, y0=Midt-h, x1=AntGrVek, length=0.08, code=2, angle=90, 
		y1=replace(Konf[1, ], ind, Midt[ind]-h), col=fargeHovedRes, lwd=1.5)
arrows(x0=AntGrVek, y0=Midt+h, x1=AntGrVek, y1=replace(Konf[2, ], ind, Midt[ind]+h), 
		length=0.08, code=2, angle=90, col=fargeHovedRes, lwd=1.5)
	
if (tittel==1) {title(main=Tittel, font.main=1, line=1)}
#Tekst som angir hvilket utvalg som er gjort
if (length(utvalgTxt)>0) {
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3-(1-tittel)+0.8*((NutvTxt-1):0)))}

if ( outfile != '') {dev.off()}

ResData <- round(rbind(Midt, Konf, MidtRest, KonfRest), 1)
rownames(ResData) <- c('Midt', 'KIned', 'KIopp', 'MidtRest', 'KIRestned', 'KIRestopp')[1:(3*(medSml+1))]
UtData <- list(paste(toString(TittelUt),'.', sep=''), ResData )
names(UtData) <- c('Tittel', 'Data')
return(invisible(UtData))

}	#end if statement for 0 observations
}	#end function
