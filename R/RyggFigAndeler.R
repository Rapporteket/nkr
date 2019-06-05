#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et horisontalt eller vertikalt søylediagram som viser andeler (fordeling) 
#' av valgt variabel filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item alder: Aldersfordeling 
#'     \item AntDagerInnl: Liggetid 
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?
#'     \item AntNivOpr: Antall nivå operert'
#'     \item arbstatus: Arbeidsstatus før, 3 el. 12 mnd. etter operasjon
#'     \item ASA: ASA-grad
#'     \item BMI: Pasientenes BMI (Body Mass Index)
#'     \item EqangstPre: Helsetilstand: Angst
#'     \item EqgangePre: Helsetilstand: Gange
#'     \item ErstatningPre: Har pasienten søkt erstatning?
#'     \item fornoydhet: Fornøydhet 3 eller 12 mnd etter operasjon
#'     \item HovedInngrep: Hovedinngrep
#'     \item Komorbiditet: Komorbiditet
#'     \item KomplPer: Peroperative komplikasjoner
#'     \item KomplPost: Pasientrapporterte komplikasjoner
#'     \item Liggedogn: Liggetid ved operasjon
#'     \item Morsmal: Morsmål
#'     \item nytte: Hvilken nytte har du hatt av operasjonen? (svar 3 eller 12 måneder etter)
#'     \item OpInd: Operasjonsindikasjon
#'     \item OpIndPareseGrad: Operasjonsindikasjon, paresegrad
#'     \item OpIndSmeType: Operasjonsindikasjon, smertetype
#'     \item OpKat: Operasjonskategori 
#'     \item RadUnders: Radiologisk undersøkelse
#'     \item Roker: Røyker du?
#'     \item Saardren: Sårdren
#'     \item SivilStatus: Sivilstatus
#'     \item SmHyppPre: Hyppighet av smertestillende før operasjonen
#'     \item SmStiPre: Bruk av smertestillende før operasjonen
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter
#'     \item SympVarighUtstr: Varighet av utstrålende smerter
#'     \item TidlOpr: Tidligere ryggoperert?
#'     \item TidlOprAntall: Antall tidligere operasjoner
#'     \item UforetrygdPre: Har pasienten søkt uføretrygd?
#'     \item Underkat: Fordeling av inngrepstyper. NB: hovedkategori MÅ velges
#'     \item Utd: Høyeste fullførte utdanning
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
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#' @param datoFra Tidligste operasjonsdato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste operasjonsdato i utvalget (vises alltid i figuren).
#' @param erMann Kjønn, standard: alt annet enn 0/1 gir begge kjønn
#'          0: Kvinner
#'          1: Menn
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 130)
#' @param tidlOp Tidligere operert, numerisk 1-4. Alt annet gir alle data uten utvalg.
#'                1: Tidl. operert samme nivå, 
#'                2: Tidl. operert annet nivå, 
#'			3: Tidl. operert annet og sm. nivå,
#'			4: Primæroperasjon
#' @param opKat Hastegrad av operasjon 1: Elektivt, 2: Akutt, 3: Halvøyeblikkelig
#' @param hovedkat Hvilken type hovedinngrep, numerisk 0-7, standard: 99, dvs. alle

#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param tittel Vise tittel i figuren eller ikke (0/1). standard:1
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
                            opKat=99, tidlOp='', tittel=1, outfile='', reshID=0, enhetsUtvalg=0)
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
	

TittelUt <- switch(valgtVar, 
				SymptVarighRyggHof = 'Varighet av rygg-/hoftesmerter',
				SympVarighUtstr = 'Varighet av utstrålende smerter',
				)
if (valgtVar %in% c('SymptVarighRyggHof','SympVarighUtstr')) {
	grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '> 2 år', 'Ukjent')
	#grtxt <- c('Ingen smerter', 'Under 3 mnd', '3-12 mnd', '1-2 år', 'Mer enn 2 år', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Variabel %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData$Variabel[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9)) 
	}

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

if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt} 
			
#--------------- Gjøre beregninger ------------------------------
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
NRest <- 0
AntRest <- 0
AntHoved <- switch(as.character(flerevar), 
				'0' = table(RegData$VariabelGr[ind$Hoved]),
				'1' = colSums(sapply(RegData[ind$Hoved ,variable], as.numeric), na.rm=T))
NHoved <- switch(as.character(flerevar), 
				'0' = sum(AntHoved),	#length(ind$Hoved)- Kan inneholde NA
				'1' = length(ind$Hoved))
Andeler$Hoved <- 100*AntHoved/NHoved

if (medSml==1) {
	AntRest <- switch(as.character(flerevar), 
					'0' = table(RegData$VariabelGr[ind$Rest]),
					'1' = colSums(sapply(RegData[ind$Rest ,variable], as.numeric), na.rm=T))
	NRest <- switch(as.character(flerevar), 
					'0' = sum(AntRest),	#length(ind$Rest)- Kan inneholde NA
					'1' = length(ind$Rest))
	Andeler$Rest <- 100*AntRest/NRest
}
#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
if ((valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) | NHoved < 10 | 
		(medSml ==1 & NRest<10)) {
FigTypUt <- rapbase::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
farger <- FigTypUt$farger
	plot.new()
	title(Tittel)	#, line=-6)
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
FigTypUt <- rapbase::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
antDesTxt <- paste0('%.', antDes, 'f')
grtxtpst <- paste0(rev(grtxt), ' \n(', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)')
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
	xmax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab="Andel pasienter (%)", #main=tittel, 
		col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#  
	mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)

	if (medSml == 1) {
		points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
		legend('top', c(paste0(hovedgrTxt, ' (N=', NHoved,')'), 
						paste0(smltxt, ' (N=', NRest,')')), 
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {	
		legend('top', paste0(hovedgrTxt, ' (N=', NHoved,')'), 
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	if (length(grtxt2) == 0) {grtxt2 <- paste0('(', sprintf(antDesTxt, Andeler$Hoved), '%)')}
	ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
		xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	legend('top', c(paste0(hovedgrTxt, ' (N=', NHoved,')'), paste0(smltxt, ' (N=', NRest,')')), 
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {	
	legend('top', paste0(hovedgrTxt, ' (N=', NHoved,')'), 
		border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
	}
} 

if (tittel==1) {title(Tittel, line=1, font.main=1)}

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3-(1-tittel)+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}
}


AndelerUt <- rbind(Andeler$Hoved, Andeler$Rest)
rownames(AndelerUt) <- c(hovedgrTxt, smltxt)
AntallUt <- rbind(AntHoved, AntRest)
rownames(AntallUt) <- c(hovedgrTxt, smltxt)

UtData <- list(paste(toString(TittelUt),'.', sep=''), AndelerUt, AntallUt, grtxt )
names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
return(invisible(UtData))

}
