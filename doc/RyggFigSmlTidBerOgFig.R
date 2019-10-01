#' #Må komme inn 
#' 
#' #' Figur som viser utvikling over tid for flere ulike grupper i et linjediagram
#' #'
#' #' @param AggVerdier dataramme (liste?) med rader/kolonner for hver resultatgruppe
#' #' @param N Total N
#' #' @param KImaalGrenser Målnivåer for kvalitetsindikatorer
#' #' @param tittel 
#' #' @param varTxt Hva viser antallet over linjene? (Må fjernes når vi får >3? linjer). 
#' #' Bruk: Tall ved punktene angir varTxt
#' #' @param tidtxt Angivelse av måned, år, ..
#' #' @param tidtxt2 Mulighet for tilleggsinfo om tidspunktene
#' #' @param xAkseTxt Evt. benevning for tidsskala
#' #' @param yAkseTxt Benevning for aggregert verdi (andel, gjennomsnitt, ...)
#' #' @param utvalgTxt Hvilke utvalg som er gjort for datamaterialet
#' #' @param fargepalett tilgjengelige farger
#' #' @param medSml 
#' #' @param hovedgrTxt 
#' #' @param smltxt 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #RyggFigSmlTid <- function(){
      if (hentData == 1) {		
            RegData <- RyggRegDataSQL()
      }
      
# Preprosessering av data. I samledokument gjøre dette i samledokumentet. Off01-data er preprosessert.
if (preprosess==1){
      RegData <- RyggPreprosess(RegData=RegData)	#, reshID=reshID)
}


#------- Tilrettelegge variable
varTxt <- ''
RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, hovedkat = hovedkat,
                                   ktr=ktr, figurtype = 'andelTid')
RegData <- RyggVarSpes$RegData
sortAvtagende <- RyggVarSpes$sortAvtagende
varTxt <- RyggVarSpes$varTxt
KImaalGrenser <- RyggVarSpes$KImaalGrenser
tittel <- RyggVarSpes$tittel

#------- Gjøre utvalg
smltxt <- ''
medSml <- 0

      if (reshID==0) {enhetsUtvalg <- 0}
      RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil, 
                                  minald=minald, maxald=maxald, erMann=erMann, aar=aar, 
                                  hovedkat = hovedkat, opKat=opKat, tidlOp=tidlOp, 
                                  enhetsUtvalg=enhetsUtvalg) #, grType=grType
      smltxt <- RyggUtvalg$smltxt
      medSml <- RyggUtvalg$medSml 
      utvalgTxt <- RyggUtvalg$utvalgTxt
      ind <- RyggUtvalg$ind
RegData <- RyggUtvalg$RegData

#------------------------Klargjøre tidsenhet--------------
RegData$Mnd <- RegData$InnDato$mon +1
RegData$Kvartal <- ceiling(RegData$Mnd/3)
RegData$Halvaar <- ceiling(RegData$Mnd/6)
RegData$Aar <- 1900 + RegData$InnDato$year #strptime(RegData$Innleggelsestidspunkt, format="%Y")$year

#Brukes til sortering
RegData$TidsEnhet <- switch(tidsenhet,
                            Aar = RegData$OpAar-min(RegData$OpAar)+1,
                            Mnd = RegData$Mnd-min(RegData$Mnd[RegData$OpAar==min(RegData$OpAar)])+1
                            +(RegData$OpAar-min(RegData$OpAar))*12,
                            Kvartal = RegData$Kvartal-min(RegData$Kvartal[RegData$OpAar==min(RegData$OpAar)])+1+
                                  (RegData$OpAar-min(RegData$OpAar))*4,
                            Halvaar = RegData$Halvaar-min(RegData$Halvaar[RegData$OpAar==min(RegData$OpAar)])+1+
                                  (RegData$OpAar-min(RegData$OpAar))*2
)

tidtxt <- switch(tidsenhet,
                 Mnd = paste(substr(RegData$OpAar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                             sprintf('%02.0f', RegData$Mnd[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='.'),
                 Kvartal = paste(substr(RegData$OpAar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                 sprintf('%01.0f', RegData$Kvartal[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                 Halvaar = paste(substr(RegData$OpAar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)], 3,4),
                                 sprintf('%01.0f', RegData$Halvaar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]), sep='-'),
                 Aar = as.character(RegData$OpAar[match(1:max(RegData$TidsEnhet), RegData$TidsEnhet)]))

RegData$TidsEnhet <- factor(RegData$TidsEnhet, levels=1:max(RegData$TidsEnhet)) #evt. levels=tidtxt


#--------------- Gjøre beregninger ------------------------------

AggVerdier <- list(Hoved = 0, Rest =0)
N <- list(Hoved = length(ind$Hoved), Rest =length(ind$Rest))

grupperingsVar <- 'HovedInngrep'

tapply(RegData[, 'Variabel'], RegData[ ,'TidsEnhet'], length) #Tot. ant. per år


NAarHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'], length) #Tot. ant. per år
NAarHendHoved <- tapply(RegData[ind$Hoved, 'Variabel'], RegData[ind$Hoved ,'TidsEnhet'],sum, na.rm=T) #Ant. hendelser per år
AggVerdier$Hoved <- NAarHendHoved/NAarHoved*100

NAarRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest], length)	
NAarHendRest <- tapply(RegData$Variabel[ind$Rest], RegData$TidsEnhet[ind$Rest],sum, na.rm=T)
AggVerdier$Rest <- NAarHendRest/NAarRest*100
Ngr <- list(Hoved = NAarHendHoved, Rest = NAarHendRest)

grtxt2 <- paste0('(', sprintf('%.1f',AggVerdier$Hoved), '%)')
yAkseTxt <- 'Andel (%)'
vektor <- c('Aar','Halvaar','Kvartal','Mnd')
xAkseTxt <- paste0(c('Innleggelsesår', 'Innleggelsesår', 'Innleggelseskvartal', 'Innleggelsesmåned')
                   [which(tidsenhet==vektor)])

hovedgrTxt <- RyggUtvalg$hovedgrTxt

FigDataParam <- list(AggVerdier=AggVerdier, N=N, 
                     Ngr=Ngr,	
                     KImaalGrenser <- KImaalGrenser,
                     #soyletxt=soyletxt,
                     grtxt2=grtxt2, 
                     varTxt=varTxt,
                     tidtxt=tidtxt, #RyggVarSpes$grtxt,
                     tittel=tittel, 
                     retn='V', 
                     xAkseTxt=xAkseTxt,
                     yAkseTxt=yAkseTxt,
                     utvalgTxt=RyggUtvalg$utvalgTxt, 
                     fargepalett=RyggUtvalg$fargepalett, 
                     medSml=medSml,
                     hovedgrTxt=hovedgrTxt,
                     smltxt=RyggUtvalg$smltxt)

      #Hvis bare figur:      
      # function(AggVerdier=AggVerdier, N=NA, Ngr=NA, KImaalGrenser = NA, 
      #                     tittel='', varTxt='', tidtxt=NA, tidtxt2='', 
      #                     xAkseTxt='', yAkseTxt='Andel (%)', 
      #                     utvalgTxt='', fargepalett=, 
      #                     medSml=medSml, hovedgrTxt=hovedgrTxt, smltxt='')
# Må ha info om hvilke grupper som vises
      # reshID=0, 
# outfile=''
# enhetsUtvalg=0)
{

      #Plottspesifikke parametre:
FigTypUt <- rapFigurer::figtype(outfile, fargepalett=RyggUtvalg$fargepalett)
farger <- FigTypUt$farger
fargeHoved <- farger[3]
fargeRest <- farger[1]
NutvTxt <- length(utvalgTxt)
hmarg <- 0.04+0.01*NutvTxt
par('fig' = c(0,1,0,1-hmarg)) 
cexleg <- 1	#St?rrelse p? legendtekst
# ylabtext <- "Andel (%)"
xskala <- 1:length(tidtxt)
xmax <- max(xskala)


ymax <- min(119, 1.25*max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T))
plot(xskala, AggVerdier$Hoved,  font.main=1,  type='o', pch="'", col='white', #type='o', 
     xlim= c(0.9,xmax+0.1), xaxt='n', frame.plot = FALSE,  #xaxp=c(min(tidtxt), max(tidtxt),length(tidtxt)-1)
     cex=2, xlab='Operasjonsår', ylab="Andel (%)", ylim=c(0,ymax), yaxs = 'i') 	

#Legge på linjer i plottet. 
grid(nx = NA, ny = NULL, col = farger[4], lty = "solid")

axis(side=1, at = xskala, labels = tidtxt)

title(tittel, line=1, font.main=1)


lines(xskala, AggVerdier$Hoved, col=fargeHoved, lwd=3)
points(xskala, AggVerdier$Hoved, pch="'", cex=2, col=fargeHoved)
text(xskala, AggVerdier$Hoved, pos=3, Ngr$Hoved, cex=0.9, col=fargeHoved)
if (medSml==1) {
      lines(xskala, AggVerdier$Rest, col=fargeRest, lwd=3)
      points(xskala, AggVerdier$Rest, pch="'", cex=2, col=fargeRest)
}
#KImål
#if (valgtVar=='SympVarighUtstr') {
if (!is.na(KImaalGrenser[1])) {
      lines(xskala, rep(KImaalGrenser[2],length(xskala)), col= '#4fc63f', lwd=3) #col='#FF7260'
      text(max(xskala), KImaalGrenser[2], pos=4, 'Mål', cex=0.9, col = '#4fc63f')
}

Ttxt <- paste0('(Tall ved punktene angir antall ', varTxt, ')') 
if (medSml == 1) { 
      text(xskala, AggVerdier$Rest, pos=3, Ngr$Rest, cex=0.9, col=fargeRest)
      legend('topleft', border=NA, c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'),
                                     paste0(smltxt, ' (N=', N$Rest, ')'), Ttxt), bty='n', ncol=1, cex=cexleg, 
             col=c(fargeHoved, fargeRest, NA), lwd=3)		
} else {
      legend('top', c(paste0(hovedgrTxt, ' (N=', N$Hoved, ')'), Ttxt), 
             col=c(fargeHoved, NA), lwd=3, bty='n')
}

#Tekst som angir hvilket utvalg som er gjort
mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=fargeRest, line=c(3+0.8*((NutvTxt-1):0)))

par('fig'=c(0, 1, 0, 1)) 
if ( outfile != '') {dev.off()}
#}