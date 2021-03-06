% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RyggFigAndelerGrVarAarGml.R
\name{RyggFigAndelerGrVarAarGml}
\alias{RyggFigAndelerGrVarAarGml}
\title{Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)}
\usage{
RyggFigAndelerGrVarAarGml(RegData, valgtVar, datoFra = "2007-01-01",
  datoTil = "3000-12-31", minald = 0, maxald = 130, erMann = "",
  hovedkat = 99, tidlOp = "", opKat = 99, hentData = 0,
  preprosess = 1, enhetsUtvalg = 0, grVar = "ShNavn", tittel = 1,
  ktr = 0, reshID, outfile = "", siste3aar = 0, AKjust = 0)
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Variabelen det skal vises resultat for. Se \strong{Details} for oversikt.}

\item{datoFra}{Tidligste operasjonsdato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste operasjonsdato i utvalget (vises alltid i figuren).}

\item{minald}{Alder, fra og med (Standardverdi: 0)}

\item{maxald}{Alder, til og med (Standardverdi: 130)}

\item{erMann}{Kjønn, standard: alt annet enn 0/1 gir begge kjønn
0: Kvinner
1: Menn}

\item{hovedkat}{Hvilken type hovedinngrep, numerisk 0-9, standard: 99, dvs. alle. Valgmuligheter:
\itemize{
 \item 0:'Operasjonskategori: "ukjent"',    #hkat=0
 \item 1:'Prolapskirurgi',
 \item 2:'Foramenotomi',
 \item 3:'Laminektomi',
 \item 4:'Interspinøst implantat',    
 \item 5:'Fusjonskirurgi',
 \item 6:'Skiveprotese',
 \item 7:'Fjerning/rev. av implantat',
 \item 8:'Spinal stenose',
 \item 9:'Degen. spondylolistese'
 \item 99: Alle
 }}

\item{tidlOp}{Tidligere operert, numerisk 1-4. Alt annet gir alle data uten utvalg.
       1: Tidl. operert samme nivå, 
       2: Tidl. operert annet nivå, 
3: Tidl. operert annet og sm. nivå,
4: Primæroperasjon}

\item{opKat}{Hastegrad av operasjon 1: Elektivt, 2: Akutt, 3: Halvøyeblikkelig. Slår sammen 1 og 3 til elektiv}

\item{hentData}{Gjør spørring mot database
0: Nei, RegData gis som input til funksjonen (Standard)
1: Ja}

\item{preprosess}{Preprosesser data
0: Nei
1: Ja (Standard)}

\item{enhetsUtvalg}{Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.}

\item{grVar}{Tekstvariabel som angir hva skal resultatene grupperes på. 
ShNavn-sykehus/avdeling
Fylke- Pasienten bor i det akutelle fylket
BoHF - Pasienten bor i boområdene til det angitte HF.
BoRHF - Pasienten bor i boområdene til det angitte RHF.}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives
til systemets standard utdataenhet (som regel skjerm))}

\item{siste3aar}{0:Viser resultatat for hele perioden samlet, 1: Viser resultat for hvert av de siste tre år}

\item{AKjust}{Alders-og kjønnsjustering når grVar er boområder. Basert på 3 aldersgrupper gruppert ut fra alderskvartilene.
0:ikke juster, 1:juster for alder og kjønn}
}
\value{
Figur med...
}
\description{
Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
f.eks. sykehus.
Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
}
\details{
Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
       'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet' 
Argumentet \emph{valgtVar} har følgende valgmuligheter:
   \itemize{
    \item Alder: Pasienter over 75år    
    \item Antibiotika: Er det gitt antibiotikaprofylakse?    
    \item ArbstatusPre: Mottar sykepenger, før operasjon?    
    \item Arbstatus: Mottar sykepenger, 3 mnd etter operasjon?    [ENDRET fra Arbstatus3mnd, Arbstatus12mnd]
    \item ASA: ASA-grad > II    
    \item BMI: Pasienter med fedme (BMI>30)    
    \item ErstatningPre: Søkt/planlegger å søke erstatning?    
    \item Fornoyd: Fornøyde pasienter [ENDRET fra Fornoyd3mnd, Fornoyd12mnd  ]
    \item Kp3Mnd: Pasientrapporterte komplikasjoner    
    \item Misfornoyd:  Andel med Misfornøyd/litt misfornøyd [ENDRET fra Misfor3mnd, Misfor12mnd]
    \item Nytte: Klart bedre    [ENDRET fra Nytte3mnd, Nytte12mnd]
 \item OswEndr30pst: Mer enn 30% forbedring i Oswestry-skår, 3 mnd. [ENDRET fra Osw30_3mnd, Osw30_12mnd]
    \item PeropKomp: Komplikasjon ved operasjon
    \item PeropKompDura: Komplikasjon ved operasjon: Durarift
    \item Roker: Røyker du?    
    \item Saardren: Sårdren    
    \item SmStiPre: Bruker smertestillende før operasjonen    
    \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år    
    \item SympVarighUtstr: Varighet av utstrålende smerter > 1 år  
    \item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?    
    \item Utd: Andel høyskole-/universitetsutdannede    
    \item Verre Mye verre/verre enn noen gang, 3 mnd. [ENDRET fra Verre3mnd, Verre12mnd]
	\item ..
 	\item BeinsmLavPre: Pasienter med preop. beinsmerte < 2.5 og ikke parese.
	\item BeinsmEndrLav: Forbedring av beinsmerter under 1.5 poeng
    \item DegSponSSSten: Pasienter med Degenerativ spondylolistese og sentral spinal stenose 
 \item OswEndrLav: Mer enn 20 poeng forbedring i Oswestry-skår, 3 mnd/12mnd.
\item OswEndr20: 
\item Osw48: Oswestry-skår fortsatt over 48
\item KpInf3Mnd: Sårinfeksjoner
\item Morsmal: Fremmedspråklige (ikke norsk som morsmål)
	}
}
