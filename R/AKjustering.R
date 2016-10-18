#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
#'        'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet' 
#' @param RegData E dataramme som minimum må inneholde variabelen det skal beregnes alders- og 
#'                kjønnsstandardiserte rater for. (variabelen: Variabel)
#' RegData inneholder både alder, kjønn og bo-områder.
	#' Standardiserer mot variablene  Alder (år) og ErMann (0/1)
	#' RegData må inneholde variabelen Variabel som det skal beregnes AK-justerte andeler for.
#' @param valgtVar Variabelen det skal vises resultat for. Se \strong{Details} for oversikt.
#' @param siste3aar 0:Viser resultatat for hele perioden samlet, 1: Viser resultat for hvert av de siste tre år
#' @param AKjust Alders-og kjønnsjustering når grVar er boområder. Basert på 3 aldersgrupper gruppert ut fra alderskvartilene.
#'          0:ikke juster, 1:juster for alder og kjønn
#'
#' @return Figur med...
#'
#' @export

StandAlderKjonn  <- function(RegData, antAldgr, katVariable) {
{
      #Sjekk om RegData inneholder bo-områder.
		 #Legger aldersgrupper på RegData
         antAldgr <- 3
         kvantgr <- (1/antAldgr)*(1:(antAldgr-1))
		 aldInndel <- quantile(RegData$Alder, kvantgr, na.rm = T)  #c(25, 50, 75)
		 aldgr <- c(minald-1,aldInndel,85)
		 RegData$AlderGr <- cut(RegData$Alder,aldgr) #grensene er øverste grense i aldersintervallet
		 
		 #Må finne andel av normalpopulasjonen i disse aldersgruppene ut fra befolkningsfil
		 #For alders-og kjønnsstandardisering:
		 #Innb2015aldkj <- read.table('./Innbyggere2015aldkj.csv', sep=';', header = T, encoding = 'UTF-8')
		 #StdPop2015AK <- aggregate(AntInnb ~ ErMann+Alder, data=Innb2015aldkj, FUN=sum)
		 #write.table(StdPop2015AK, file = 'StdPop2015.csv', sep = ';', row.names = F, fileEncoding = 'UTF-8')
		 StdPopAK <- read.table('./StdPop2015AK.csv', sep=';', header = T, encoding = 'UTF-8')
		 StdPopAK$AlderGr <- cut(StdPopAK$Alder, aldgr)
		 PopAldKjGr <- aggregate(AntInnb ~ ErMann+AlderGr, data=StdPopAK, FUN=sum)
		 PopAldKjGr$Vekt <- prop.table((PopAldKjGr$AntInnb))#PopAldKjGr$AntInnb/sum(PopAldKjGr$AntInnb) 
		 
            #Må ha kategorivariable som input...
			katVariable <- c('OpAar', 'grVar')
			
		 #RegData$OpAar <- factor(RegData$OpAar, exclude = "") #Ikke nødvendig å lage faktor?
		 #RegData$ErMann <- factor(RegData$ErMann, exclude = "")
		 grupperingsVar <- c(katVariable, 'ErMann', 'AlderGr') #c('grVar', 'OpAar', 'ErMann', 'AlderGr')
		 AndelAKGr <- aggregate(RegData$Variabel,  by=RegData[ ,grupperingsVar], drop=FALSE, 
						FUN = function(x) AndelStGr = sum(x)/length(x)) #Variabel er en 0/1-variabel.
					#	aggregate(Variabel ~ ErMann+AlderGr+OpAar+grVar, data=RegData, drop=FALSE, #Skal ha: 2*3*4*AntGr=504
					#	   FUN = function(x) AndelStGr = sum(x)/length(x)) #Variabel er en 0/1-variabel.
		 
		  #Alternativt:
		 #Nvar <- tapply(RegData$Variabel, RegData[ ,grupperingsVar], sum, na.rm=T) #Variabel er en 0/1-variabel.
		 #Ngr <- table(RegData[ ,grupperingsVar])
		 #if(N > 0) {Ngr <- table(RegData[ ,grupperingsVar])}	else {Ngr <- 0}
		 AndelOgVekt <- cbind(AndelAKGr, Vekt = PopAldKjGr$Vekt)
		 AndelVekt <- cbind(AndelOgVekt, AndelVektGr = AndelOgVekt$Variabel*AndelOgVekt$Vekt)
		 AndelerGrStand <- aggregate(AndelVekt$AndelVektGr, by=AndelVekt[ ,katVariable], FUN = function(x) 100*sum(x))
					#aggregate(AndelVektGr ~ OpAar+grVar, data=AndelVekt, FUN = function(x) 100*sum(x))
		 AndelerGr <- matrix(AndelerGrStand$AndelVektGr, nrow=3, ncol=length(levels(RegData$grVar)), 
		                     dimnames=list((AarMax-2):AarMax, levels(RegData$grVar)))
		 return(StandAndeler=AndelerGr)
		 }
	