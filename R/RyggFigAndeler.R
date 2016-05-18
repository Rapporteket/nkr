#' Søylediagram som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et horisontalt eller vertikalt søylediagram som viser andeler (fordeling) 
#' av valgt variabel filtrert på de utvalg som er gjort.
#'
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Aldersfordeling 
#'     \item AntDagerInnl: Liggetid 
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?
#'     \item AntNivOpr: Antall nivå operert'
#'     \item ArbstatusPre: Arbeidsstatus før operasjon'
#'     \item Arbstatus3mnd: Arbeidsstatus 3 mnd. etter operasjon
#'     \item Arbstatus12mnd: Arbeidsstatus 12 mnd. etter operasjon
#'     \item ASA: ASA-grad
#'     \item BMI: Pasientenes BMI (Body Mass Index)
#'     \item EqangstPre: Helsetilstand: Angst
#'     \item EqgangePre: Helsetilstand: Gange
#'     \item ErstatningPre: Har pasienten søkt erstatning?
#'     \item Fornoyd3mnd: Fornøydhet 3 mnd etter operasjon
#'     \item Fornoyd12mnd: Fornøydhet 12 mnd etter operasjon
#'     \item HovedInngrep: Hovedinngrep
#'     \item Komorbiditet: Komorbiditet
#'     \item KomplPer: Peroperative komplikasjoner
#'     \item KomplPost: Pasientrapporterte komplikasjoner
#'     \item Liggedogn: Liggetid ved operasjon
#'     \item Morsmal: Morsmål
#'     \item Nytte3mnd: Hvilken nytte har du hatt av operasjonen? (svar 3 måneder etter)
#'     \item Nytte12mnd: Hvilken nytte har du hatt av operasjonen? (svar 12 måneder etter)
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
#'    				
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.
#' @param datoFra Tidligste operasjonsdato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste operasjonsdato i utvalget (vises alltid i figuren).
#' @param erMann Kjønn, standard: alt annet enn 0/1 gir begge kjønn
#'          0: Kvinner
#'          1: Menn
#' @param minald Alder, fra og med (Standardverdi: 0)
#' @param maxald Alder, til og med (Standardverdi: 130)
#' @param tidlOp Tidligere operert, 
#'                0: alle, 
#'                1: Tidl. operert samme nivå, 
#'                2: Tidl. operert annet nivå, 
#'			3: Tidl. operert annet og sm. nivå,
#'			4: Primæroperasjon
#' @param hovedkat Hvilken type hovedinngrep, numerisk 0-7, standard: 99, dvs. alle

#' @param outfile Navn på fil figuren skrives til. Standard: '' (Figur skrives
#'    til systemets standard utdataenhet (som regel skjerm))
#' @param tittel Vise tittel i figuren eller ikke (0/1). standard:1
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Gjør gruppeutvalg med eller uten sammenlikning for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Standard)
#'                 2: Egen enhet
#'				   6: Egen enhet mot egen region 
#'				   7: Egen region 
#'				   8: Egen region mot resten
#' @param preprosess Preprosesser data
#'                 FALSE: Nei
#'                 TRUE: Ja (Standard)
#' @param hentData Gjør spørring mot database
#'                 0: Nei, RegData gis som input til funksjonen (Standard)
#'                 1: Ja
#'				
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export
#'
FigAndeler  <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='2999-12-31', hentData=0, preprosess=1,
		minald=0, maxald=130, erMann='', hovedkat=99, tittel=1, outfile='', reshID, enhetsUtvalg=1)
{


if (hentData == 1) {		
  RegData <- RyggRegDataSQL(datoFra, datoTil)
}

# Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
if (preprosess == 1){
       RegData <- RyggPreprosess(RegData=RegData)
     }

#------------Parameterdefinisjon -------------------------
retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
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

#Noen variable settes som "Variabel" for å standardisere beregninga ytterligere:
if (valgtVar %in% c('Alder', 'BMI', 'HovedInngrep', 'Liggedogn', 'Utd', 
					'SymptVarighRyggHof','SympVarighUtstr')) {
	RegData$Variabel <- RegData[ ,valgtVar] 
	}
	
dato <- as.POSIXlt(RegData$OpDato,format="%d.%m.%Y")	#evt strptime()
NB <- ''
	
if (valgtVar=='Underkat'){RegData$Variabel <- RegData$Inngrep }
	
if (valgtVar=='Liggedogn') {
	#For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
	dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
	RegData$Liggedogn[dagind]<-0
	gr <- c(0:7,100)	
	RegData$VariabelGr <- cut(RegData$Liggedogn, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c(0:6, '7+')
	subtxt <- 'Antall liggedøgn'
}
if (valgtVar=='Utd') {
	grtxt <- c('Grunnskole++, 7-10år','Real-, yrkes- el vg skole', #'Real-, yrkes-/yrkesfaglig vg skole',
		'Allmennfaglig vg skole','Høyskole/universitet, <4 år','Høyskole/universitet, 4år+')
	RegData$VariabelGr <- factor(RegData$Utd, levels = 1:5) 
	retn <- 'H'
	}

if (valgtVar=='HovedInngrep'){
	grtxt <- c('Annet', 'Prolapskirurgi', 'Foramenotomi', 'Laminektomi',
	'Interspin. implantat', 'Fusjonskirurgi', 'Skiveprotese', 'Rev. av implantat')
	RegData$VariabelGr <- factor(RegData$HovedInngrep, levels = 0:7) 
	retn <- 'H'
	}

if (valgtVar=='Underkat'){
	if (hovedkat %in% 0:7) {
		gr_nr <- c(0:19)
		txt <- c('Annet','Mikro','Makro','Tubekirurgi','Udefinert','Mikro','Makro','Tubekirurgi',
			'Udefinert','Laminektomi', 'Interspinøst impl.','PLF','PLIF','TLIF','ALIF',
			'Udefinert fusjon', 'Skiveprotese','Fjern interspinøst impl.','Fjerne ostemat.',
			'Revisjon ostemat.')
		hgr <- c(0,1,1,1,1,2,2,2,2,3,4,5,5,5,5,5,6,7,7,7)
		kat <- data.frame(hgr, gr_nr, txt)	#hkatnavn[hgr+1], 
		underkat_num <- kat$gr_nr[kat$hgr==hovedkat]
		
		RegData <- RegData[which(RegData$Inngrep %in% underkat_num), ]
		grtxt <- as.character(kat$txt[underkat_num+1])
		RegData$VariabelGr <- factor(RegData$Inngrep, levels = underkat_num) 
		}
	}
	
	
if (valgtVar=='Alder') {
	gr <- c(0,seq(20,90,10),150)
	#AndelSh <- table(cut(RegData$Alder[ind_sh], gr, right=F))/NSh*100
	#AndelLand <- table(cut(RegData$Alder[ind_resten], gr, right=F))/NLand*100
	RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c('0-19', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '80-89', '90+')	#c(levels(RegData$VariabelGr)[-length(gr)], '90+')	#c(names(AndelLand)[-length(gr)], '90+')
	subtxt <- 'Aldersgruppe'
}
if (valgtVar=='AntNivOpr') {
	gr <- c(0:5,1000)
	RegData$VariabelGr <- cut(RegData$AntNivOpr, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c(0:4,'5+')	#sort(unique(RegData$AntNivOpr))
	subtxt <- 'Antall'
}
if (valgtVar == 'Antibiotika') {
	grtxt <- c('Nei', 'Ja', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- RegData$Antibiotika %in% 0:1
	RegData$VariabelGr[indDum] <- RegData$Antibiotika[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9)) 
	}
if (valgtVar == 'ArbstatusPre') {
	retn <- 'H'
	grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt', 	
		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
	RegData$VariabelGr <- 99
	indDum <- which(RegData$ArbstatusPre %in% 1:10)
	RegData$VariabelGr[indDum] <- RegData$ArbstatusPre[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:10,99)) 
	}
if (valgtVar == 'Arbstatus3mnd') {
	datoTil <- min(datoTil, as.character(Sys.Date()-90))
	RegData <- RegData[which(RegData$Utfylt3Mnd==1), ]
	retn <- 'H'
	grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt', 	
		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
	RegData$VariabelGr <- 99
	indDum <- which(RegData$Arbstatus3mnd %in% 1:10)
	RegData$VariabelGr[indDum] <- RegData$Arbstatus3mnd[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:10,99)) 
	}
if (valgtVar == 'Arbstatus12mnd') {
	datoTil <- min(datoTil, as.character(Sys.Date()-365))
	RegData <- RegData[which(RegData$Utfylt12Mnd==1), ]
	retn <- 'H'
	grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt', 	
		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
	RegData$VariabelGr <- 99
	indDum <- which(RegData$Arbstatus12mnd %in% 1:10)
	RegData$VariabelGr[indDum] <- RegData$Arbstatus12mnd[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:10,99)) 
	}
if (valgtVar == 'ASA') {
	#retn <- 'H'
	grtxt <- c('I:Ingen','II:Moderat', 'III:Alvorlig', 'IV:Livstruende', 'Ukjent')
	subtxt <- 'Sykdomsgrad'
	RegData$VariabelGr <- 99
	indDum <- which(RegData$ASA %in% 1:4)	#Antar ikke opererer døde...
	RegData$VariabelGr[indDum] <- RegData$ASA[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,99)) 
	}
if (valgtVar=='BMI') {
	gr <- c(0, 18.5, 25, 30, 35, 40, 1000)
	RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
	#AndelSh <- table(cut(RegData$BMI[ind_sh], gr, right=F))/NSh*100
	#AndelLand <- table(cut(RegData$BMI[ind_resten], gr, right=F))/NLand*100
	grtxt <- c('<18,5', levels(RegData$VariabelGr)[2:(length(gr)-2)],'40+')
	grtxt2 <- c('Undervekt', 'Normalvekt', 'Overvekt', 'Fedme', 'Fedme kl II', 'Fedme kl III')
	subtxt <- '"Body Mass Index"'
}
if (valgtVar == 'EqangstPre') {
	RegData$VariabelGr <- 9
	grtxt <- c('Ingen', 'Litt', 'Svært', 'Ukjent')
	indDum <- which(RegData$EqangstPre %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$EqangstPre[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9)) 
	subtxt <- 'Grad av engstelighet/deprimerthet'	#Tilstand i forhold til angst'
	}
if (valgtVar == 'EqgangePre') {
	grtxt <- c('Ingen problemer', 'Litt problemer', 'Sengeliggende', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$EqgangePre %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$EqgangePre[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9)) 
	subtxt <- ''	#'Tilstand i forhold til gange'
	}
if (valgtVar == 'ErstatningPre') {
	grtxt <- c('Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$ErstatningPre %in% 1:4)
	RegData$VariabelGr[indDum] <- RegData$ErstatningPre[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9)) 
	}

if (valgtVar == 'Fornoyd3mnd') {
	datoTil <- min(datoTil, as.character(Sys.Date()-90))
	RegData <- RegData[which(RegData$Utfylt3Mnd==1), ]
	retn <- 'H'
	grtxt <- c('Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Fornoyd3mnd %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData$Fornoyd3mnd[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9)) 
	}
if (valgtVar == 'Fornoyd12mnd') {
	datoTil <- min(datoTil, as.character(Sys.Date()-365))
	RegData <- RegData[which(RegData$Utfylt12Mnd==1), ]
	retn <- 'H'
	grtxt <- c('Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Fornoyd12mnd %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData$Fornoyd12mnd[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9)) 
	}
if (valgtVar == 'Morsmal') {
	grtxt <- c('Norsk', 'Samisk', 'Annet', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Morsmal %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$Morsmal[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9)) 
	}
if (valgtVar == 'Nytte3mnd') {
	datoTil <- min(datoTil, as.character(Sys.Date()-90))
	retn <- 'H'
	grtxt <- c('Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre', 
					'Verre enn noen gang', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Nytte3mnd %in% 1:7)
	RegData$VariabelGr[indDum] <- RegData$Nytte3mnd[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:7,9)) 
	}
if (valgtVar == 'Nytte12mnd') {
	datoTil <- min(datoTil, as.character(Sys.Date()-365))
	RegData <- RegData[which(RegData$Utfylt12Mnd==1), ]
	retn <- 'H'
	grtxt <- c('Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre', 
					'Verre enn noen gang', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Nytte12mnd %in% 1:7)
	RegData$VariabelGr[indDum] <- RegData$Nytte12mnd[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:7,9)) 
	}
if (valgtVar == 'OpIndPareseGrad') {
	grtxt <- c(0:5, 'Ukjent')
	RegData <- RegData[which(RegData$OpIndParese ==1),]
	indDum <- which(RegData$OpIndPareseGrad %in% 0:5)
	RegData$VariabelGr <- 9
	RegData$VariabelGr[indDum] <- RegData$OpIndPareseGrad[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:5,9)) 
	}
if (valgtVar == 'OpIndSmeType') {
	grtxt <- c('Rygg/hofte', 'Bein', 'Begge deler', 'Ukjent')
	RegData <- RegData[which(RegData$OpIndSme ==1),]
	indDum <- which(RegData$OpIndSmeType %in% 1:3)
	RegData$VariabelGr <- 9
	RegData$VariabelGr[indDum] <- RegData$OpIndSmeType[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9)) 
}
if (valgtVar == 'OpKat') {
	#retn <- 'H'
	grtxt <- c('Elektiv', 'Akutt', '1/2-Akutt', 'Ukjent')
	indDum <- which(RegData$OpKat %in% 1:3)
	RegData$VariabelGr <- 9
	RegData$VariabelGr[indDum] <- RegData$OpKat[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9)) 
}
if (valgtVar == 'Roker') {
	grtxt <- c('Nei', 'Ja', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- RegData$Roker %in% 0:1
	RegData$VariabelGr[indDum] <- RegData$Roker[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0,1,9)) 
	}
if (valgtVar == 'Saardren') {
	#grtxt <- c('Nei', 'Ja', 'Ukjent')
	#RegData$VariabelGr <- 9
	#indDum <- which(RegData$Saardren %in% 0:1)
	#indDum <- which(RegData$Saardren %in% 0:1)
	#RegData$VariabelGr[indDum] <- RegData$Saardren[indDum]
	#RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:1,9)) 
	grtxt <- c('Nei', 'Ja')
	RegData$VariabelGr <- 0
	RegData$VariabelGr[which(RegData$Saardren==1)] <- 1
	}
if (valgtVar == 'SivilStatus') {
	grtxt <- c('Gift', 'Samboer', 'Enslig', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$SivilStatus %in% 1:3)
	RegData$VariabelGr[indDum] <- RegData$SivilStatus[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:3,9)) 
	}
if (valgtVar == 'SmStiPre') {
	grtxt <- c('Nei', 'Ja', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$SmStiPre %in% 0:1)
	RegData$VariabelGr[indDum] <- RegData$SmStiPre[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(0:1,9)) 
	}
if (valgtVar == 'SmHyppPre') {
	grtxt <- c('Sjeldnere', 'Månedlig', 'Ukentlig', 'Daglig', 'Oftere', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$SmHyppPre %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData$SmHyppPre[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9)) 
	}
if (valgtVar %in% c('SymptVarighRyggHof','SympVarighUtstr')) {
	grtxt <- c('Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '> 2 år', 'Ukjent')
	#grtxt <- c('Ingen smerter', 'Under 3 mnd', '3-12 mnd', '1-2 år', 'Mer enn 2 år', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$Variabel %in% 1:5)
	RegData$VariabelGr[indDum] <- RegData$Variabel[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:5,9)) 
	}
if (valgtVar == 'TidlOpr') {
	retn <- 'H'
	grtxt <- c('Samme nivå', 'Annet nivå', 'Annet og sm. nivå', 'Primæroperasjon', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$TidlOpr %in% 1:4)
	RegData$VariabelGr[indDum] <- RegData$TidlOpr[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9)) 
	}
if (valgtVar=='TidlOprAntall') {
	gr <- c(0:5, 1000)
	RegData$Variabel <- 0
	#indDum <- which(is.na(RegData$TidlOprAntall) == FALSE)
	indDum <- which(RegData$TidlOprAntall>0)
	RegData$Variabel[indDum] <- RegData$TidlOprAntall[indDum]
	RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c(0:4,paste0('5-', max(RegData$TidlOprAntall, na.rm=T)))
}
if (valgtVar == 'UforetrygdPre') {
	retn <- 'H'
	grtxt <- c('Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
	RegData$VariabelGr <- 9
	indDum <- which(RegData$UforetrygdPre %in% 1:4)
	RegData$VariabelGr[indDum] <- RegData$UforetrygdPre[indDum]
	RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4,9)) 
	}

#FIGURER SATT SAMMEN AV FLERE VARIABLE FRA SAMME TOTALUTVALG
if (valgtVar=='OpInd') {
	retn <- 'H'
	flerevar <- 1
	variable <- c('OpIndCauda', 'OpIndParese', 'OpIndSme')
	grtxt <- c('Cauda equlina', 'Parese', 'Smerter')
		}
if (valgtVar=='RadUnders') {
	retn <- 'H'
	flerevar <- 1
	variable <- c('RvCt', 'RvMr', 'RvRadigr', 'RvDiscogr', 'RvDpregblok', 'RvRtgLscol', 'RvFunksjo')
	grtxt <- c('CT', 'MR', 'Radikulografi', 'Diskografi', 'Diag.blokade', 'Rtg.LS-columna', 'Fleks./Ekst.')
		}
if (valgtVar=='Komorbiditet') {
	retn <- 'H'
	flerevar <- 1
	variable <- c('SykdAndreRelevanteSykdBechtrew', 'SykdAnnenendokrin', 'SykdAnnenreumatisk', 
			'SykdCerebrovaskular', 'SykdDepresjonAngst', 'SykdHjertekar', 'SykdHoftekneartose',
			'SykdHypertensjon', 'SykdKreft', 'SykdKroniskLunge', 'SykdKroniskNevrologisk', 
			'SykdKroniskSmerterMuskelSkjelettsyst', 'SykdOsteoporose', 'SykDprebetesMellitus', 
			'SykdReumatoidartritt', 'SykdVaskulærClaudicatio', 'Sykd')
	grtxt <- c('Bechterew', 'Endokrin', 'Reumatisk', 
			'Cerebrovaskulær', 'Depresjon/Angst', 'Hjerte-kar', 'Hoftekneartose',
			'Hypertensjon', 'Kreft', 'Lungesykd.', 'Nevrologisk sykd.', 
			'Muskel-/skjelettsm.', 'Osteoporose', 'Diabetes Mell.', 
			'Reumatoid artritt', 'Vask. Claudicatio', 'Tot. Komorb.')
			
		}
if (valgtVar=='KomplPer') {
	retn <- 'H'
	flerevar <- 1
	RegData <- RegData[which(dato > as.POSIXlt('2009-12-31')), ]
	NB <- '(Komplikasjoner rapporteres kun f.o.m. 2010)'
	variable <- c('PeropKompDura', 'PeropKompFeilnivSide', 'PeropKompNerve', 'PeropKompTransfuBlodning',
			'PeropKompKardio','PeropKompFeilplassImp','PeropKompResp','PeropKompAnafy')
	grtxt <- c('Durarift', 'Operert feil nivå/side', 'Nerveskade', 'Transfusjonskrevende blødning',
		'Kardiovaskulær komplikasjon', 'Feilplassert implantat', 'Respiratorisk komplikasjon', 'Anafylaksi')
	#RegData$Variabel <- rowSums(RegData[ ,variable], na.rm=T)
		}
if (valgtVar=='KomplPost') {
	datoTil <- min(datoTil, as.character(Sys.Date()-90))
	retn <- 'H'
	flerevar <- 1
	subtxt <- '(Komplikasjoner rapporteres kun f.o.m. 2010)'
	RegData <- RegData[which(dato > as.POSIXlt('2009-12-31')), ]
	#Andel kun av de som har svart på 3 mnd ktr:
	RegData <- RegData[which(RegData$Utfylt3Mnd==1), ]
	variable <- c('KpInfOverfla3Mnd','KpInfDyp3Mnd', 'KpMiktProb3Mnd','KpUVI3Mnd',
			'KpLungebet3Mnd', 'KpBlod3Mnd','KpDVT3Mnd','KpLE3Mnd', 'Kp3Mnd')
	grtxt <- c('Overfladisk sårinfeksjon', 'Dyp sårinfeksjon', 
			'Problem, vannlatning/avføring','Urinveisinfeksjon', 'Pneumoni', 
			'Transf./opr. pga. blødning', 'DVT','Lungeemboli', 'Tot. komplikasjoner')	
	#RegData$Variabel <- colSums(RegData[ ,kompl])
			}
TittelUt <- switch(valgtVar, 
				Alder = 'Aldersfordeling',
				Antibiotika = 'Er det gitt antibiotikaprofylakse?',
				AntNivOpr = 'Antall nivå operert',
				ArbstatusPre = 'Arbeidsstatus før operasjon',
				Arbstatus3mnd = 'Arbeidsstatus 3 mnd. etter operasjon',
				Arbstatus12mnd = 'Arbeidsstatus 12 mnd. etter operasjon',
				ASA = 'ASA-grad',
				BMI = 'Pasientenes BMI (Body Mass Index)',
				EqangstPre = 'Helsetilstand: Angst',
				EqgangePre = 'Helsetilstand: Gange',
				ErstatningPre = 'Har pasienten søkt erstatning?',
				Fornoyd3mnd = 'Fornøydhet, 3 mnd etter operasjon',
				Fornoyd12mnd = 'Fornøydhet, 12 mnd etter operasjon',
				HovedInngrep = 'Hovedinngrep',
				Komorbiditet = 'Komorbiditet',
				KomplPer = 'Peroperative komplikasjoner',
				KomplPost = 'Pasientrapporterte komplikasjoner',
				Liggedogn = 'Liggetid ved operasjon',
				Morsmal = 'Morsmål',
				Nytte3mnd = c('Hvilken nytte har du hatt av operasjonen?','(svar 3 måneder etter)'),
				Nytte12mnd = c('Hvilken nytte har du hatt av operasjonen?','(svar 12 måneder etter)'),
				OpInd = 'Operasjonsindikasjon',
				OpIndPareseGrad = 'Operasjonsindikasjon, paresegrad',
				OpIndSmeType = 'Operasjonsindikasjon, smertetype',
				OpKat = 'Operasjonskategori', 
				RadUnders = 'Radiologisk undersøkelse',
				Roker = 'Røyker du?',
				Saardren = 'Sårdren',
				SivilStatus = 'Sivilstatus',
				SmHyppPre = 'Hyppighet av smertestillende før operasjonen',
				SmStiPre = 'Bruk av smertestillende før operasjonen',
				SymptVarighRyggHof = 'Varighet av rygg-/hoftesmerter',
				SympVarighUtstr = 'Varighet av utstrålende smerter',
				TidlOpr = 'Tidligere ryggoperert?',
				TidlOprAntall = 'Antall tidligere operasjoner',
				UforetrygdPre = 'Har pasienten søkt uføretrygd?',
				Underkat = 'Fordeling av inngrepstyper',
				Utd = 'Høyeste fullførte utdanning')

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
RyggUtvalg <- RyggUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, maxald=maxald, 
		erMann=erMann, hovedkat=hovedkat)	#, tidlOp=tidlOp
RegData <- RyggUtvalg$RegData
utvalgTxt <- RyggUtvalg$utvalgTxt

SykehustypeTxt <- c('univ. sykehus', 'lokalsykehus', 'priv. sykehus')				
indEgen1 <- match(reshID, RegData$ReshId)
if (enhetsUtvalg %in% c(1,2,3,6)) {	#Involverer egen enhet
		shtxt <- as.character(RegData$AvdNavn[indEgen1]) } else {
		shtxt <- switch(as.character(enhetsUtvalg), 	
			'0' = 'Hele landet',
			'4' = SykehustypeTxt[RegData$Sykehustype[indEgen1]],
			'5' = SykehustypeTxt[RegData$Sykehustype[indEgen1]],
			'7' = as.character(RegData$Region[indEgen1]),
			'8' = as.character(RegData$Region[indEgen1]))
			}
			
if (enhetsUtvalg %in% c(0,2,4,7)) {		#Ikke sammenlikning
			medSml <- 0
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
				'3' = paste('andre ', SykehustypeTxt[RegData$Sykehustype[indEgen1]], sep=''),	#RegData inneh. kun egen shgruppe
				'5' = 'andre typer sykehus',
				'6' = paste(RegData$Region[indEgen1], ' forøvrig', sep=''),	#RegData inneh. kun egen region
				'8' = 'andre regioner')
			indRest <- switch(as.character(enhetsUtvalg),
				'1' = which(as.numeric(RegData$ReshId) != reshID),
				'3' = which(as.numeric(RegData$ReshId) != reshID),	#RegData inneh. kun egen shgruppe
				'5' = which(RegData$Sykehustype != RegData$Sykehustype[indEgen1]),
				'6' = which(as.numeric(RegData$ReshId)!=reshID),	#RegData inneh. kun egen region
				'8' = which(RegData$Region != RegData$Region[indEgen1]))
			}								
if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt} 
			
#--------------- Gjøre beregninger ------------------------------
#Gjør beregninger selv om det evt ikke skal vise figur ut. Trenger utdata.
Andeler <- list(Hoved = 0, Rest =0)
NRest <- 0
AntRest <- 0
AntHoved <- switch(as.character(flerevar), 
				'0' = table(RegData$VariabelGr[indHoved]),
				'1' = colSums(sapply(RegData[indHoved ,variable], as.numeric), na.rm=T))
NHoved <- switch(as.character(flerevar), 
				'0' = sum(AntHoved),	#length(indHoved)- Kan inneholde NA
				'1' = length(indHoved))
Andeler$Hoved <- 100*AntHoved/NHoved

if (medSml==1) {
	AntRest <- switch(as.character(flerevar), 
					'0' = table(RegData$VariabelGr[indRest]),
					'1' = colSums(sapply(RegData[indRest ,variable], as.numeric), na.rm=T))
	NRest <- switch(as.character(flerevar), 
					'0' = sum(AntRest),	#length(indRest)- Kan inneholde NA
					'1' = length(indRest))
	Andeler$Rest <- 100*AntRest/NRest
}
#-----------Figur---------------------------------------
#Hvis for få observasjoner..
#if (dim(RegData)[1] < 10 | (length(which(RegData$ReshId == reshID))<5 & egenavd==1)) {
if ((valgtVar=='Underkat' & all(hovedkat != c(1,2,5,7))) | NHoved < 10 | 
		(medSml ==1 & NRest<10)) {
FigTypUt <- figtype(outfile)
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
cexgr <- 1	#Kan endres for enkeltvariable


#Plottspesifikke parametre:
FigTypUt <- figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
#Tilpasse marger for å kunne skrive utvalgsteksten
NutvTxt <- length(utvalgTxt)
antDesTxt <- paste('%.', antDes, 'f', sep='')
grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf(antDesTxt, Andeler$Hoved)), '%)', sep='')
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
		legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), 
						paste(smltxt, ' (N=', NRest,')', sep='')), 
			border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, 
			lwd=lwdRest,	lty=NA, ncol=1, cex=cexleg)
		} else {	
		legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
			border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
		}
}

if (retn == 'V' ) {
#Vertikale søyler eller linje
	if (grtxt2 == '') {grtxt2 <- paste('(', sprintf(antDesTxt, Andeler$Hoved), '%)', sep='')}
	ymax <- max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.15
	pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",	
		xlab=subtxt, col=fargeHoved, border='white', ylim=c(0, ymax))	#sub=subtxt,	
	mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
if (medSml == 1) {
	points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"), 
	legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste(smltxt, ' (N=', NRest,')', sep='')), 
		border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA), 
		lwd=lwdRest, ncol=2, cex=cexleg)
	} else {	
	legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''), 
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
rownames(AndelerUt) <- c('Hoved', 'Rest')
AntallUt <- rbind(AntHoved, AntRest)
rownames(AntallUt) <- c('Hoved', 'Rest')

UtData <- list(paste(toString(Tittel),'.', sep=''), AndelerUt, AntallUt, grtxt )
names(UtData) <- c('Tittel', 'Andeler', 'Antall', 'GruppeTekst')
return(invisible(UtData))

}
