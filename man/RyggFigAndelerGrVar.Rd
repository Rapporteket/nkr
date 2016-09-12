% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RyggFigAndelerGrVar.R
\name{RyggFigAndelerGrVar}
\alias{RyggFigAndelerGrVar}
\title{Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)}
\usage{
RyggFigAndelerGrVar(RegData, valgtVar, datoFra = "2007-01-01",
  datoTil = "3000-12-31", minald = 0, maxald = 130, erMann = "",
  hovedkat = 99, tidlOp = "", hentData = 0, preprosess = 1,
  enhetsUtvalg = 0, grVar = "ShNavn", tittel = 1, reshID, outfile = "")
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

\item{hovedkat}{Hvilken type hovedinngrep, numerisk 0-7, standard: 99, dvs. alle}

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

\item{enhetsUtvalg}{Gjør gruppeutvalg med eller uten sammenlikning. Se \strong{Details} for oversikt.}

\item{grVar}{Tekstvariabel som angir hva skal resultatene grupperes på. 
ShNavn-sykehus/avdeling
Fylke- Pasienten bor i det akutelle fylket
BoHF - Pasienten bor i boområdene til det angitte HF.
BoRHF - Pasienten bor i boområdene til det angitte RHF.}

\item{tittel}{Vise tittel i figuren eller ikke (0/1). standard:1}

\item{reshID}{Parameter følger fra innlogging helseregister.no og angir
hvilken enhet i spesialisthelsetjenesten brukeren tilhører}

\item{outfile}{Navn på fil figuren skrives til. Standard: '' (Figur skrives
til systemets standard utdataenhet (som regel skjerm))}
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
    \item Alder: Pasienter over 80år    
    \item Antibiotika: Er det gitt antibiotikaprofylakse?    
    \item ArbstatusPre: Mottar sykepenger, før operasjon?    
    \item Arbstatus3mnd: Mottar sykepenger, 3 mnd etter operasjon?    
    \item Arbstatus12mnd: Mottar sykepenger, 12 mnd. etter operasjon    
    \item ASA: ASA-grad > II    
    \item BMI: Pasienter med fedme (BMI>30)    
    \item ErstatningPre: Søkt/planlegger å søke erstatning?    
    \item Fornoyd3mnd: Fornøyde pasienter, 3 mnd etter operasjon    
    \item Fornoyd12mnd: Fornøyde pasienter, 12 mnd etter operasjon    
    \item Kp3Mnd: Pasientrapporterte komplikasjoner    
    \item Misfor3mnd:  Andel med Misfornøyd/litt misfornøyd, 3 mnd
    \item Misfor12mnd: Andel med Misfornøyd/litt misfornøyd, 12 mnd
    \item Nytte3mnd: Klart bedre, 3 mnd.    
    \item Nytte12mnd: Klart bedre, 12 mnd. 
 \item Osw30_3mnd: Mer enn 30% forbedring i Oswestry-skår, 3 mnd.
 \item Osw30_12mnd: Mer enn 30% forbedring i Oswestry-skår, 12 mnd.
    \item PeropKomp: Komplikasjon ved operasjon
    \item PeropKompDura: Komplikasjon ved operasjon: Durarift
    \item Roker: Røyker du?    
    \item Saardren: Sårdren    
    \item SmStiPre: Bruker smertestillende før operasjonen    
    \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år    
    \item SympVarighUtstr: Varighet av utstrålende smerter > 1 år  
    \item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?    
    \item Utd: Andel høyskole-/universitetsutdannede    
    \item Verre3mnd Mye verre/verre enn noen gang, 3 mnd.
    \item Verre12mnd Mye verre/verre enn noen gang, 12 mnd.
	}
}
