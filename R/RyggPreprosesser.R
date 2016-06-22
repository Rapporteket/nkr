#' Preprosesser data fra Degenerativ Rygg
#'
#' Denne funksjonen definerer og formaterer variabler
#'
#' @inheritParams RyggFigAndeler
#'
#' @return RegData En dataramme med det preprosesserte datasettet
#'
#' @export

RyggPreprosess <- function(RegData=RegData)
{
  #Kun ferdigstilte registreringer: Det skal kun leveres ferdigstilte skjema til RapportUttrekk
	#Kjønnsvariabel:Kjonn 1:mann, 2:kvinne
	RegData$ErMann <- RegData$Kjonn
	RegData$ErMann[which(RegData$Kjonn == 2)] <- 0
	#Riktig datoformat og hoveddato
	RegData$InnDato <- as.POSIXlt(RegData$OpDato, format="%d.%m.%Y")
	RegData$Aar <- 1900 + strptime(RegData$InnDato, format="%Y")$year
	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'AvdReshID')] <- 'ReshId'
	names(RegData)[which(names(RegData) == 'AvdNavn')] <- 'ShNavn'
	class(RegData$ReshId) <- 'numeric'


  return(invisible(RegData))
}

