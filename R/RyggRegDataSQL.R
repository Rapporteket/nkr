#' Henter data registrert for Degenerativ Rygg
#'
#' Henter data for Degenerativ Rygg fra "staging" (?)
#'
#'
#' @return RegData data frame
#' @export
#'
RyggRegDataSQL <- function() {
      
#RyggRegDataSQL <- function(datoFra = '2007-01-01', datoTil = '2099-01-01') 
#Dette blir feil siden Rygg i staging har datoformat dd.mm.yyyy

  registryName <- "nkr"
  dbType <- "mssql"

  
  query <- paste0('SELECT
	Alder,
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
	EQ5D12mnd,
	EQ5D3mnd,
	EQ5DPre,
	Eqangst12mnd,
	Eqangst3mnd,
	EqangstPre,
	Eqgange12mnd,
	Eqgange3mnd,
	EqgangePre,
	ErstatningPre,
	Fornoyd12mnd,
	Fornoyd3mnd,
	HFID,
	HFNavn,
	HovedInngrep,
	HovedInngreptxt,
	Inngrep,
	Inngreptxt,
	Kjonn,
	KnivtidTot,
	Kommunenr,
	Kp3Mnd,
	KpBlod3Mnd,
	KpDVT3Mnd,
	KpInf3Mnd,
	KpInfDyp12Mnd,
	KpInfDyp3Mnd,
	KpInfOverfla3Mnd,
	KpLE3Mnd,
	KpLungebet3Mnd,
	KpMiktProb3Mnd,
	KpUVI3Mnd,
	Liggedogn,
	Morsmal,
	Nytte12mnd,
	Nytte3mnd,
	OpAar,
	OpDato,
      OpDeUlamin,
      OpDeFasett,
	OpIndCauda,
	OpIndParese,
	OpIndPareseGrad,
	OpIndSme,
	OpIndSmeType,
	OpKat,
	OpLaminektomi,
	OpMikro,
	OpProlap,
	OpTilgang,
	OswTot12mnd,
	OswTot3mnd,
	OswTotPre,
	PeropKomp,
	PeropKompAnafy,
	PeropKompDura,
	PeropKompFeilnivSide,
	PeropKompFeilplassImp,
	PeropKompKardio,
	PeropKompNerve,
	PeropKompResp,
	PeropKompTransfuBlodning,
	PID,
	Region,
	Reop90d,
	RfAnnet,
	RfDegen,
	RfDegskol,
	RfForamino,
	RfLateral,
	RfNormal,
	RfPseudom,
	RfSentr,
	RfSkive,
	RfSpondtypeDegen,
	RfSpondtypeIsmisk,
	RfSynovpre,
	Roker,
	RvCt,
	RvDiscogr,
	RvDpregblok,
	RvFunksjo,
	RvMr,
	RvRadigr,
	RvRtgLscol,
	Saardren,
	SivilStatus,
	SmBe12mnd,
	SmBe3mnd,
	SmBePre,
	SmHypp12mnd,
	SmHypp3mnd,
	SmHyppPre,
	SmRy12mnd,
	SmRy3mnd,
	SmRyPre,
	SmSti12mnd,
	SmSti3mnd,
	SmStiPre,
	Sykd,
	SykdAndreRelevanteSykdBechtrew,
	SykdAnnenendokrin,
	SykdAnnenreumatisk,
	SykdCerebrovaskular,
	SykdDepresjonAngst,
	SykdHjertekar,
	SykdHoftekneartose,
	SykdHypertensjon,
	SykdKreft,
	SykdKroniskLunge,
	SykdKroniskNevrologisk,
	SykdKroniskSmerterMuskelSkjelettsyst,
	SykdOsteoporose,
	SykDprebetesMellitus,
	SykdReumatoidartritt,
	SykdVaskulærClaudicatio,
	Sykehustype,
	SykemeldVarighPre,
	SymptVarighRyggHof,
	SympVarighUtstr,
	TideOp12mnd,
	TideOp3mnd,
	TidlOpr,
	TidlOprAntall,
	UforetrygdPre,
	Utd,
	Utfylt12Mnd,
	Utfylt3Mnd,
	Vekt
FROM Uttrekk_Rapport ')

RegData <- rapbase::LoadRegData(registryName, query, dbType)

return(RegData)
}


