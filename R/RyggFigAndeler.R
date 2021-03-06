#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et horisontalt eller vertikalt søylediagram som viser andeler (fordeling) 
#' av valgt variabel filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling 
#'     \item antDagerInnl: Liggetid 
#'     \item antibiotika: Er det gitt antibiotikaprofylakse?
#'     \item antNivOpr: Antall nivå operert'
#'     \item arbstatus: Arbeidsstatus før, 3 el. 12 mnd. etter operasjon
#'     \item ASA: ASA-grad
#'     \item BMI: Pasientenes BMI (Body Mass Index)
#'     \item EQangstPre: Helsetilstand: Angst
#'     \item EQgangePre: Helsetilstand: Gange
#'     \item erstatningPre: Har pasienten søkt erstatning?
#'     \item fornoydhet: Fornøydhet 3 eller 12 mnd etter operasjon
#'     \item hovedInngrep: Hovedinngrep
#'     \item komorbiditet: Komorbiditet
#'     \item komplPer: Peroperative komplikasjoner
#'     \item komplPost: Pasientrapporterte komplikasjoner
#'     \item liggedogn: Liggetid ved operasjon
#'     \item morsmal: Morsmål
#'     \item nytte: Hvilken nytte har du hatt av operasjonen? (svar 3 eller 12 måneder etter)
#'     \item opInd: Operasjonsindikasjon
#'     \item opIndPareseGrad: Operasjonsindikasjon, paresegrad
#'     \item opIndSmeType: Operasjonsindikasjon, smertetype
#'     \item opKat: Operasjonskategori 
#'     \item radUnders: Radiologisk undersøkelse
#'     \item roker: Røyker du?
#'     \item saardren: Sårdren
#'     \item sivilStatus: Sivilstatus
#'     \item smHyppPre: Hyppighet av smertestillende før operasjonen
#'     \item smStiPre: Bruk av smertestillende før operasjonen
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter
#'     \item SymptVarighUtstr: Varighet av utstrålende smerter
#'     \item tidlOpr: Tidligere ryggoperert?
#'     \item tidlOprAntall: Antall tidligere operasjoner
#'     \item uforetrygdPre: Har pasienten søkt uføretrygd?
#'     \item underkat: Fordeling av inngrepstyper. NB: hovedkategori MÅ velges
#'     \item utd: Høyeste fullførte utdanning
#'    }
#' Argumentet \emph{hovedkat} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Annet
#'     \item 1: Prolaps
#'     \item 2: Foramenotomi
#'     \item 3: Laminektomi
#'     \item 4: Eksp. intraspin. impl.
#'     \item 5: Fusjon
#'     \item 6: Skiveprotese
#'     \item 7: Fjerning/revisjon
#'    }
#'    Velges ingen av disse, vil alle data vises.
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'     \item 3: Egen enhet mot egen sykehustype
#'     \item 4: Egen sykehustype
#'     \item 5: Egen sykehustype mot resten av landet
#'     \item 6: Egen enhet mot egen region
#'     \item 7: Egen region
#'	   \item 8: Egen region mot resten
#'    	}							
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @inheritParams RyggUtvalgEnh
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.

#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param tittelMed Vise tittel i figuren eller ikke (0/1). standard:1
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.
#' @param preprosess Preprosesser data
#'                 0: Nei
#'                 1: Ja (Standard)
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#'				
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export
#'
RyggFigAndeler  <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='2999-12-31', aar=0, 
                            hentData=0, preprosess=1,minald=0, maxald=130, erMann='', hovedkat=99,
                            opKat=99, tidlOp='', ktr=0, tittelMed=1, outfile='', reshID=0, enhetsUtvalg=0)
{


if (hentData == 1) {		
  RegData <- RyggRegDataSQL()       #(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess == 1){
       RegData <- RyggPreprosess(RegData=RegData)
     }

#------------Parameterdefinisjon -------------------------
retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
grtxt <- ''		#Spesifiseres for hver enkelt variabel
grtxt2 <- NULL	#Spesifiseres evt. for hver enkelt variabel
subtxt <- ''	#Benevning
flerevar <- 0
antDes <- 1
cexgr <- 1	#Kan endres for enkeltvariable

#Noen variable settes som "Variabel" for å standardisere beregninga ytterligere:
#if (valgtVar %in% c('Alder', 'BMI', 'HovedInngrep', 'Liggedogn', 'Utd', 
#					'SymptVarighRyggHof','SympVarighUtstr')) {
#	RegData$Variabel <- RegData[ ,valgtVar] 
#	}

# 'Utd', 'SymptVarighRyggHof','SympVarighUtstr'
					
dato <- as.POSIXlt(RegData$OpDato,format="%d.%m.%Y")	#evt strptime()
NB <- ''
	
RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr, figurtype='andeler')
RegData <- RyggVarSpes$RegData
flerevar <- RyggVarSpes$flerevar
variable <- RyggVarSpes$variable
grtxt <- RyggVarSpes$grtxt
retn <- RyggVarSpes$retn
subtxt <- RyggVarSpes$subtxt



#Gjør utvalg
RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil, 
                            minald=minald, maxald=maxald, erMann=erMann, aar=aar,
                            hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp,enhetsUtvalg=enhetsUtvalg)
RegData <- RyggUtvalg$RegData
utvalgTxt <- RyggUtvalg$utvalgTxt
ind <- RyggUtvalg$ind
medSml <- RyggUtvalg$medSml
hovedgrTxt <- RyggUtvalg$hovedgrTxt
smltxt <- RyggUtvalg$smltxt
#Skal endres til ind$Hoved og ind$Rest

if (tittelMed==0) {tittel<-''} else {tittel <- RyggVarSpes$tittel
} 
			
#--------------- Gjøre beregninger ------------------------------
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
AggVerdier <- list(Hoved = 0, Rest =0)
N <- list(Hoved=0, Rest=0)
AntRest <- 0
AntHoved <- switch(as.character(flerevar), 
				'0' = table(RegData$VariabelGr[ind$Hoved]),
				'1' = colSums(sapply(RegData[ind$Hoved ,variable], as.numeric), na.rm=T))
N$Hoved <- switch(as.character(flerevar), 
				'0' = sum(AntHoved),	#length(ind$Hoved)- Kan inneholde NA
				'1' = length(ind$Hoved))
AggVerdier$Hoved <- 100*AntHoved/N$Hoved

if (medSml==1) {
	AntRest <- switch(as.character(flerevar), 
					'0' = table(RegData$VariabelGr[ind$Rest]),
					'1' = colSums(sapply(RegData[ind$Rest ,variable], as.numeric), na.rm=T))
	N$Rest <- switch(as.character(flerevar), 
					'0' = sum(AntRest),	#length(ind$Rest)- Kan inneholde NA
					'1' = length(ind$Rest))
	AggVerdier$Rest <- 100*AntRest/N$Rest
}

     FigDataParam <- list(AggVerdier=AggVerdier,
                           # N=Nfig,
                           # Ngr=Nfig,
                           # Nvar=Ngr,
                           #KImaal <- RyggVarSpes$KImaal,
                           #grtxt2=grtxt2,
                           grtxt=grtxt,
                           #grTypeTxt=grTypeTxt,
                           tittel= RyggVarSpes$tittel,
                           retn=retn,
                           subtxt=subtxt,
                           #yAkseTxt=yAkseTxt,
                           utvalgTxt=utvalgTxt,
                           fargepalett=RyggUtvalg$fargepalett,
                           medSml=medSml,
                           hovedgrTxt=hovedgrTxt,
                           smltxt=smltxt)

#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
if ((valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) | N$Hoved < 10 | 
		(medSml ==1 & N$Rest<10)) {
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
farger <- FigTypUt$farger
	plot.new()
	title(tittel)	#, line=-6)
	legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
	if (valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) {
		text(0.5, 0.6, 'Velg Hovedkategori: 
			Prolapskirurgi, Foramenotomi, Fusjonskirurgi eller 
		Fjerning/rev. av implantat for å se på inngrepstyper', cex=1.2)} else {
		text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)}
	if ( outfile != '') {dev.off()}

} else {

#-----------Figur---------------------------------------
#Innparametre: subtxt, grtxt, grtxt2, tittel, Andeler, utvalgTxt, retn, cexgr


#Plottspesifikke parametre:
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
antDesTxt <- paste0('%.', antDes, 'f')
grtxtpst <- paste0(rev(grtxt), ' \n(', rev(sprintf(antDesTxt, AggVerdier$Hoved)), '%)')
vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7))
#vmarg <- max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.7)
par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
	
farger <- FigTypUt$farger
fargeHoved <- farger[1]
fargeRest <- farger[3]
antGr <- length(grtxt)
lwdRest <- 3	#tykkelse på linja som repr. landet
cexleg <- 1	#Størrelse på legendtekst

#Horisontale søyler
if (retn == 'H') {
	xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
	pos <- barplot(rev(as.numeric(AggVerdier$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel, 
		col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#  
	mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

	if (medSml == 1) {
		points(as.numeric(rev(AggVerdier$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
		legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
						paste0(smltxt, ' (N=', N$Rest,')')), 
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {	
		legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	if (length(grtxt2) == 0) {grtxt2 <- paste0('(', sprintf(antDesTxt, AggVerdier$Hoved), '%)')}
	ymax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
		xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1) {
	points(pos, as.numeric(AggVerdier$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')), 
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {	
	legend('top', paste0(hovedgrTxt, ' (N=', N$Hoved,')'), 
		border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
	}
} 

if (tittelMed==1) {title(tittel, line=1, font.main=1)}

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3-(1-tittelMed)+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}
}


# AndelerUt <- rbind(AggVerdier$Hoved, AggVerdier$Rest)
# rownames(AndelerUt) <- c(hovedgrTxt, smltxt)
# AntallUt <- rbind(AntHoved, AntRest)
# rownames(AntallUt) <- c(hovedgrTxt, smltxt)
# 
# UtData <- list(paste(toString(TittelUt),'.', sep=''), AggVerdierUt, AntallUt, grtxt )
# names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
return(invisible(FigDataParam))

}
