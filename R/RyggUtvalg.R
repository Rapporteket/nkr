RyggUtvalg <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', hovedkat=99, 
		tidlOp='', fargepalett='BlaaOff')	#insttype, 
{
#Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
# Inndata:
#		erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#		minald - alder, fra og med
#		maxald - alder, til og med
#		datoFra <- 'YYYY-MM-DD'    # min og max dato i utvalget vises alltid i figuren.
#		datoTil <- 'YYYY-MM-DD'
#		hovedkat - 0:annet, 1:Prolapskirurgi, 2:Foramenotomi, 3:Laminektomi, 4:Interspinøst implantat,	
#					5:Fusjonskirurgi, 6:Skiveprotese, 7:Fjerning/rev. av implantat
#					standard: 99 (alt annet), dvs. alle


#VELGER Å IKKE TA BORT TOMME I valgtVar
#Hvis "Variabel" ikke definert
#if (length(which(names(RegData) == 'Variabel')) == 0 ) {RegData$Variabel <- 0}
Ninn <- dim(RegData)[1]
#indVarMed <- intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')), 
#				which(RegData$Variabel != ''))
#RegData <- RegData[which(is.na(RegData$Variabel) == FALSE), ]

indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$erMann == erMann)} else {indKj <- 1:Ninn}
indHovedInngr <- if (hovedkat %in% 0:7){which(RegData$HovedInngrep == hovedkat)
			} else {indHovedInngr <- 1:Ninn}
indTidlOp <- if (tidlOp %in% 1:4) {which(RegData$TidlOpr==tidlOp)} else {indTidlOp <- 1:Ninn}
indMed <- intersect(indAld, intersect(indDato, intersect(indKj, intersect(indHovedInngr, indTidlOp))))
#indMed <- intersect(setdiff(1:dim(RegData)[1], c(indAldUt, indDatoUt, indKjUt,indDiagUt,indInnl4tUt,indNIHSSinnUt)), 
#		indVarMed)
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
	if ((minald>0) | (maxald<130)) {paste('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald}, 
						' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år', sep='')},
	if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
	if (hovedkat %in% 0:7) {paste('Hovedinngrep: ', hkatnavn[hovedkat+1], sep='')},
	if (tidlOp %in% 1:4) {TidlOprtxt[tidlOp]}
	)
	


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
return(invisible(UtData)) 
}