#' Funksjon som gjør utvalg i datagrunnlaget til registreringene for Nakke
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams RyggFigAndeler
#' @param fargepalett - Velge fargepalett, standard:BlaaOff ("offentliggjøringsfargene")
#'
#' @export
RyggUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', hovedkat=99, 
           aar=0,
		tidlOp='', fargepalett='BlaaOff')	#insttype, 'BlaaOff'
{

# Definer intersect-operator
      "%i%" <- intersect
      
      
Ninn <- dim(RegData)[1]

indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indAar <- if (aar[1] > 2000) {which(RegData$OpAar %in% as.numeric(aar))} else {indAar <- 1:Ninn}
indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
#indHovedInngr <- if (hovedkat %in% 0:7){which(RegData$HovedInngrep == hovedkat)
#			} else {indHovedInngr <- 1:Ninn}
#Hovedkategori, flervalgsutvalg
indHovedInngr <- if (hovedkat[1] %in% 0:7) {which(RegData$HovedInngrep %in% as.numeric(hovedkat))
			} else {indHovedInngr <- 1:Ninn}

indTidlOp <- if (tidlOp %in% 1:4) {which(RegData$TidlOpr==tidlOp)} else {indTidlOp <- 1:Ninn}
indMed <- indAld %i% indDato %i% indAar %i% indKj %i% indHovedInngr %i% indTidlOp 
RegData <- RegData[indMed,]


hkatnavn <- c(
	'Operasjonskategori: "annet"',	#hkat=0
	'Prolapskirurgi',
	'Foramenotomi',
	'Laminektomi',
	'Interspinøst implantat',	
	'Fusjonskirurgi',
	'Skiveprotese',
	'Fjerning/rev. av implantat')

TidlOprtxt <-	c('Tidl. operert samme nivå', 'Tidl. operert annet nivå', 'Tidl. operert annet og sm. nivå', 'Primæroperasjon')

N <- dim(RegData)[1]

utvalgTxt <- c(paste('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra}, 
			' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}, sep='' ),
	#År, flervalgsutvalg, ikke ha med egen tekst for dette?
#	if (aar[1] > 2000 ){
#	      AarMed <- min(RegData$OpAar, na.rm=T):max(RegData$OpAar, na.rm=T)
#	      if (length(AarMed)>1) {paste0('År: ', AarMed[1], ':', max(AarMed))} else {paste0('År: ', AarMed)}},
	if ((minald>0) | (maxald<130)) {paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald}, 
						' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
	if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
	if (hovedkat[1] %in% 0:7) {paste0('Hovedinngrep: ', paste(hkatnavn[as.numeric(hovedkat)+1], collapse=','))},
	if (tidlOp %in% 1:4) {TidlOprtxt[tidlOp]}
	)
	


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
return(invisible(UtData)) 
}