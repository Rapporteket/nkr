#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg fra "staging" (?)
#'
#' @inheritParams RyggFigAndeler
#'
#' @return RegData data frame
#' @export
#'
RyggRegDataSQL <- function() {
      
#RyggRegDataSQL <- function(datoFra = '2007-01-01', datoTil = '2099-01-01') 
#Dette blir feil siden Rygg i staging har datoformat dd.mm.yyyy

  registryName <- "nkr"
  dbType <- "mssql"

  
#  query = 'select * from Uttrekk_Rapport'
  
  query <- paste0('SELECT
      SymptVarighRyggHof,
      SympVarighUtstr,
	Alder,
	AntBarn,
	Antibiotika,
	AntNivOpr,
	Arbstatus12mnd,
	Arbstatus3mnd,
	ArbstatusPre,
	ASA,
	AvdID,
	AvdNavn,
	AvdReshID,
	BMI,
      Bydelkode,
      Bydelsted,
	Dagkirurgi,
	ErstatningPre,
	Fornoyd12mnd,
	Fornoyd3mnd,
	Helsetilst12mnd,
	Helsetilst3mnd,
	HelsetilstPre,
	HFID,
	HFNavn,
	HFReshID,
	HovedInngrep,
	HovedInngreptxt,
	Hoyde,
	Inngrep,
	Inngreptxt,
	Kjonn,
	KnivtidTot,
	Liggedogn,
	Morsmal,
	Nytte12mnd,
	Nytte3mnd,
	OpAar,
	OpAndreSkiveprotese,
	OpDato,
	OpKat,
	OpLaminektomi,
	OpMikro,
	OpProlap,
	OpTilgang,
	OswTot12mnd,
	OswTot3mnd,
	OswTotPre,
	PID,
	Region,
	Reop90d,
	Roker,
	Saardren,
	SivilStatus,
	SkjemaIDIa,
	SkjemaIDIIa,
	TideOp12mnd,
	TideOp3mnd,
	TidlOpr,
	TidlOprAntall,
	TypeBen,
	TypeBenBensub,
	UforetrygdPre,
	Utd,
	Utfylt12Mnd,
	Utfylt3Mnd,
	Vekt
FROM Uttrekk_Rapport')
#  WHERE (OpAar>2009) AND (OpAar<2015)
#                  WHERE OpDato >= \'', datoFra, '\' AND OpDato <= \'', datoTil, '\'')

RegData <- rapbase::LoadRegData(registryName, query, dbType)

return(RegData)
}


