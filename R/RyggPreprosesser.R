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
	#RegData$Aar <- 1900 + strptime(RegData$InnDato, format="%Y")$year

	#Variabel som identifiserer avdelingas resh
	names(RegData)[which(names(RegData) == 'AvdReshID')] <- 'ReshId'
	names(RegData)[which(names(RegData) == 'AvdNavn')] <- 'ShNavn'
	class(RegData$ReshId) <- 'numeric'
	
	#Formatering
	RegData$Morsmal <- factor(RegData$Morsmal, levels=1:3)
	RegData$HovedInngrep <- factor(RegData$HovedInngrep, levels=0:7)
	RegData$Inngrep <- factor(RegData$Inngrep, levels=0:19)
	RegData$SivilStatus <- factor(RegData$SivilStatus, levels=1:3)
	RegData$ASA <- factor(RegData$ASA, levels=1:4)
	RegData$Utd <- factor(RegData$Utd, levels=1:5)
	RegData$ArbstatusPre <- factor(RegData$ArbstatusPre, levels=1:10)
	RegData$UforetrygdPre <- factor(RegData$UforetrygdPre, levels=1:4)
	RegData$ErstatningPre <- factor(RegData$ErstatningPre, levels=1:4)
	RegData$SymptVarighRyggHof <- factor(RegData$SymptVarighRyggHof, levels=1:5)
	RegData$SympVarighUtstr <- factor(RegData$SympVarighUtstr, levels=1:5)
	RegData$OpKat <- factor(RegData$OpKat, levels=1:3)



  return(invisible(RegData))
}

