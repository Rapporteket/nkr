#' Funksjon som gjør utvalg i datagrunnlaget til registreringene for Nakke
#'
#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams RyggFigAndeler
#' @param fargepalett - Velge fargepalett, standard:BlaaOff ("offentliggjøringsfargene")
#'
#' @export

RyggUtvalgEnh <- function(RegData, datoFra, datoTil, minald=0, maxald=130, erMann='', hovedkat=99, #insttype, 'BlaaOff'
           aar=0, opKat=99, tidlOp='', enhetsUtvalg=0, reshID=0, fargepalett='BlaaOff') {

# Definer intersect-operator
      "%i%" <- intersect
      

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
      
Ninn <- dim(RegData)[1]

indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
indAar <- if (aar[1] > 2000) {which(RegData$OpAar %in% as.numeric(aar))} else {indAar <- 1:Ninn}
indDato <- which(RegData$InnDato >= as.POSIXlt(datoFra) & RegData$InnDato <= as.POSIXlt(datoTil))
indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
#Hovedkategori, flervalgsutvalg
      indHovedInngr <- if (hovedkat[1] %in% 0:7) {which(RegData$HovedInngrep %in% as.numeric(hovedkat))
            } else {indHovedInngr <- 1:Ninn}
      ##Spinal stenose, beregnes for 8 og 9:
      if (length(intersect(c(8:9), a)>0)) {indSS <- which((RfSentr == 1 | RfLateral == 1) & is.na(RfSpondtypeIsmisk)
                    & (OpDeUlamin==1 | OpLaminektomi==1 | OpDeFasett==1)
                    & (HovedInngrep %in% c(2:5,7)))} 
      if (is.element(8, hovedkat)) {indHovedInngr <- union(indHovedInngr, indSS)}
      #Degenerativ spondylolistese:
      if (is.element(9, hovedkat)) {indHovedInngr <- union(indHovedInngr, intersect(indSS, which(RfSpondtypeDegen==1)))}
      

indTidlOp <- if (tidlOp %in% 1:4) {which(RegData$TidlOpr==tidlOp)} else {indTidlOp <- 1:Ninn}
indOpKat <- if (opKat %in% 1:3) {which(RegData$OpKat == opKat)} else {1:Ninn}
indMed <- indAld %i% indDato %i% indAar %i% indKj %i% indHovedInngr %i% indTidlOp %i% indOpKat
RegData <- RegData[indMed,]


hkatnavn <- c(
	'Operasjonskategori: "ukjent"',	#hkat=0
	'Prolapskirurgi',
	'Foramenotomi',
	'Laminektomi',
	'Interspinøst implantat',	
	'Fusjonskirurgi',
	'Skiveprotese',
	'Fjerning/rev. av implantat',
	'Spinal stenose',
	'Degen. spondylolistese')

TidlOprtxt <-	c('Tidl. operert samme nivå', 'Tidl. operert annet nivå', 'Tidl. operert annet og sm. nivå', 'Primæroperasjon')
OpKatTxt <- paste0('Operasjonskategori: ', c('Elektiv', 'Akutt', '1/2-Akutt'))

N <- dim(RegData)[1]

utvalgTxt <- c(paste0('Operasjonsdato: ', if (N>0) {min(RegData$InnDato, na.rm=T)} else {datoFra}, 
			' til ', if (N>0) {max(RegData$InnDato, na.rm=T)} else {datoTil}),
	#År, flervalgsutvalg, ikke ha med egen tekst for dette?
#	if (aar[1] > 2000 ){
#	      AarMed <- min(RegData$OpAar, na.rm=T):max(RegData$OpAar, na.rm=T)
#	      if (length(AarMed)>1) {paste0('År: ', AarMed[1], ':', max(AarMed))} else {paste0('År: ', AarMed)}},
	if ((minald>0) | (maxald<130)) {paste0('Pasienter fra ', if (N>0) {min(RegData$Alder, na.rm=T)} else {minald}, 
						' til ', if (N>0) {max(RegData$Alder, na.rm=T)} else {maxald}, ' år')},
	if (erMann %in% 0:1) {paste0('Kjønn: ', c('Kvinner', 'Menn')[erMann+1])},
	if (hovedkat[1] %in% 0:8) {paste0('Hovedinngrep: ', paste(hkatnavn[as.numeric(hovedkat)+1], collapse=','))},
      if (opKat %in% 1:3) {OpKatTxt[opKat]},
      if (tidlOp %in% 1:4) {TidlOprtxt[tidlOp]}
	)
	
SykehustypeTxt <- c('univ. sykehus', 'lokalsykehus', 'priv. sykehus')				
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
			smltxt <- 'Ingen sml'
			indHoved <- 1:dim(RegData)[1]	#Tidligere redusert datasettet for 2,4,7. (+ 3og6)
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
				'3' = paste0('andre ', SykehustypeTxt[RegData$Sykehustype[indEgen1]]),	#RegData inneh. kun egen shgruppe
				'5' = 'andre typer sykehus',
				'6' = paste0(RegData$Region[indEgen1], ' forøvrig'),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			indRest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
				'5' = which(RegData$Sykehustype != RegData$Sykehustype[indEgen1]),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$Region != RegData$Region[indEgen1]))
			}								


UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, 
			indHoved=indHoved, indRest=indRest, medSml=medSml, smltxt=smltxt, shtxt=shtxt) #GronnHNpms624,
return(invisible(UtData))
}
