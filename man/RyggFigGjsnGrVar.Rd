% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RyggFigGjsnGrVar.R
\name{RyggFigGjsnGrVar}
\alias{RyggFigGjsnGrVar}
\title{Søylediagram med gjennomsnitt/median for hver grupperingsenhet (sykehus, RHF, ...)}
\usage{
RyggFigGjsnGrVar(RegData, valgtVar, valgtMaal = "Gjsn",
  datoFra = "2007-01-01", datoTil = "3000-12-31", aar = 0,
  minald = 0, maxald = 130, erMann = 99, hovedkat = 99,
  tidlOp = "", hentData = 0, preprosess = 1, opKat = 99,
  enhetsUtvalg = 0, grVar = "ShNavn", tittel = 1, ktr = 0,
  Ngrense = 10, medKI = 1, reshID = 0, outfile = "")
}
\arguments{
\item{RegData}{En dataramme med alle nødvendige variabler fra registeret}

\item{valgtVar}{Hvilken variabel som skal visualiseres. Se \strong{Details} for oversikt.}

\item{valgtMaal}{'Med' = median. Alt annet gir gjennomsnitt 

Argumentet \emph{valgtVar} har følgende valgmuligheter:
   \itemize{
    \item alder:  
    \item Liggedogn: Liggetid 
    \item OswEndr: Endring i Oswestry fra før til etter
    \item SmBeinEndr: Forbedring av beinsmerter fra før til etter
    \item SmRyEndr: Forbedring av ryggsmerter fra før til etter
   }

Detajer: Her bør man liste opp hvilke variable funksjonen benytter.}

\item{datoFra}{Tidligste operasjonsdato i utvalget (vises alltid i figuren).}

\item{datoTil}{Seneste operasjonsdato i utvalget (vises alltid i figuren).}

\item{aar}{- Operasjonsår}

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

\item{hentData}{Gjør spørring mot database
0: Nei, RegData gis som input til funksjonen (Standard)
1: Ja}

\item{preprosess}{Preprosesser data
0: Nei
1: Ja (Standard)}

\item{opKat}{Hastegrad av operasjon 1: Elektivt, 2: Akutt, 3: Halvøyeblikkelig. Slår sammen 1 og 3 til elektiv}

\item{enhetsUtvalg}{Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.}

\item{grVar}{Tekstvariabel som angir hva skal resultatene grupperes på. 
ShNavn-sykehus/avdeling
Fylke- Pasienten bor i det akutelle fylket
BoHF - Pasienten bor i boområdene til det angitte HF.
BoRHF - Pasienten bor i boområdene til det angitte RHF.}

\item{Ngrense}{Minste antall registreringer for at ei gruppe skal bli vist}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives
til systemets standard utdataenhet (som regel skjerm))}
}
\value{
Søylediagram med gjennomsnitt/median av valgt variabel for hvert sykehus
}
\description{
Funksjon som genererer en figur med gjennomsnitt/median
for hvert sykehus og kan ta inn ulike numeriske variable.
Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
}
\details{
Detajer: Her bør man liste opp hvilke variable funksjonen benytter...
}
