#' Punktplott med konfidensintervall. Viser gjennomsnittlig endring av valgt variabel vs. 
#' variabelens verdi før operasjonen.
#'
#' Gjennomsnitt per intervall med konfidensintervall for valgt variabel.
#' I bakgrunn vises konfidensintervall for resten av landet.
#' Også beregne uteliggere og ta disse med i utdata? I så fall bør de kunne spores tilbake til PID
#' 
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item EQ5DEndrPre: Skala fra -0.594 tl 1, jo høyere jo bedre.
#'     \item OswEndrPre: Oswestry (ODI-Oswestry Disability Index) Skala fra 0 til 100, hvor lavest er friskest.
#'     \item SmBeinEndrPre: Smerter i beina. Skalaen går fra 0 til 10, dvs. at differansen ligger mellom -10 og 10.
#'     \item SmRyggEndrPre: Smerter i ryggen Skalaen går fra 0 til 10, dvs. at differansen ligger mellom -10 og 10.
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
#'    Velges ingen av disse, vil alle data vises.
#' Argumentet \emph{enhetsUtvalg} har følgende valgmuligheter:
#'    \itemize{
#'     \item 0: Hele landet
#'     \item 1: Egen enhet mot resten av landet (Standard)
#'     \item 2: Egen enhet
#'     \item 3: Egen enhet mot egen sykehustype
#'     \item 4: Egen sykehustype
#'     \item 5: Egen sykehustype mot resten av landet
#'     \item 6: Egen enhet mot egen region
#'     \item 7: Egen region
#'	   \item 8: Egen region mot resten
#'    	}							
#' @inheritParams RyggFigAndeler 
#'				
#' @return Søylediagram (fordeling) av valgt variabel. De enkelte verdiene kan også sendes med.
#'
#' @export
#'
RyggFigEndrPreUT <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='2999-12-31', hentData=0, preprosess=1,
                     minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', tittel=1, outfile='', reshID, enhetsUtvalg=1)
      #aar, ktr=1, )
{
      
      if (hentData == 1) {		
            RegData <- RyggRegDataSQL()       #(datoFra, datoTil)
      }
      
      # Hvis RegData ikke har blitt preprosessert. (I samledokument gjøre dette i samledokumentet)
      if (preprosess == 1){
            RegData <- RyggPreprosess(RegData=RegData)
      }


      retn <- 1
      KIekstrem <- c(-10,10)
      gr <- 0:11	
      GrNavn <- 0:10
      AntGr <- 11
      if (valgtVar=='EQ5DEndrPre') {	#-(RegData$Post3mnd - RegData$Pre)
            t1 <- 'EQ5D '
            RegData$Pre <- RegData$EQ5DPre	#Forbedring= høyere EQ5D
            RegData$Post3mnd <- RegData$EQ5D3mnd
            RegData$Post12mnd <- RegData$EQ5D12mnd
            KIekstrem <- c(-1.6, 1.6)
            gr <- c(round(seq(-0.6,0.8,0.2),1),1.6)	#round(seq(-0.6,1.6,0.3),1)}
            RegData$Gr <- cut(RegData$Pre, gr, right=F)
            GrNavn <- levels(RegData$Gr)
            AntGr <- length(GrNavn)
            GrNavn[AntGr] <- '0.8+'
            retn <- 2
      }
      if (valgtVar=='OswEndrPre') {
            t1 <- 'Oswestry '
            RegData$Pre <- RegData$OswTotPre	#Forbedring=lavere Oswestry
            RegData$Post3mnd <- RegData$OswTot3mnd
            RegData$Post12mnd <- RegData$OswTot12mnd
            KIekstrem <- c(-100, 100)
            gr <- c(seq(0,90,10), 101)
            RegData$Gr <- cut(RegData$Pre, gr, right=F)
            GrNavn <- levels(RegData$Gr)
            AntGr <- length(GrNavn)
            GrNavn[AntGr] <- '[90,100]'
            retn <- 2}
      if (valgtVar=='SmBeinEndrPre') {
            t1 <- 'beinsmerter '
            RegData$Pre <- RegData$SmBePre	#Forbedring = lavere smerte
            RegData$Post3mnd <- RegData$SmBe3mnd
            RegData$Post12mnd <- RegData$SmBe12mnd
            RegData$Gr <- cut(RegData$Pre, gr, right=F)
      }
      if (valgtVar=='SmRyggEndrPre') {
            t1 <- 'ryggsmerter '
            RegData$Pre <- RegData$SmRyPre
            RegData$Post3mnd <- RegData$SmRy3mnd
            RegData$Post12mnd <- RegData$SmRy12mnd
            RegData$Gr <- cut(RegData$Pre, gr, right=F)
      }
      if (ktr==1) {RegData$Endr <- (RegData$Post3mnd - RegData$Pre)
      ktrtxt <- '3 mnd etter'}
      if (ktr==2) {RegData$Endr <- (RegData$Post12mnd - RegData$Pre)
      ktrtxt <- '12 mnd etter'}
      if (valgtVar %in% c('OswEndrPre', 'SmBeinEndrPre', 'SmRyggEndrPre') ) {
            RegData$Endr <- -RegData$Endr}
      TittelVar <- paste('Forbedring av ', t1, ktrtxt, ' operasjon', sep='')
      ytxt <- paste('Gjennomsnittlig endring av ', t1 ,sep='')
      
      #Tar ut de med manglende registrering
      indMed <- intersect(which(RegData$Endr != 'NA'), which(RegData$Endr != c('NaN')))
      if (dim(RegData)[1]>0) {RegData <- RegData[indMed, ]}
      
      ind_sh <-which(as.numeric(RegData$AvdReshID)==reshID) 
      ind_resten <- which(as.numeric(RegData$AvdReshID)!=reshID)
      if(egenavd==0) { ind_resten <- 1:dim(RegData)[1] } 
      
      TittelUt <-  c(TittelVar, shustxt)
      if (tittel==0) {Tittel<-''} else {Tittel <- TittelUt} 
      
      
      
      if (dim(RegData)[1]<2) {
            #-----------Figur---------------------------------------
            figtype(outfile)
            tekst <- 'Ett eller ingen svar for dette utvalget'  
            plot.new()
            title(main=Tittel)
            text(0.5, 0.5, tekst,cex=1.5)	#, family="sans")
            if ( outfile != '') {dev.off()}
      } else {
            
            
            #Resten av landet
            GjsnLand <- tapply(RegData[ind_resten,'Endr'], RegData[ind_resten, 'Gr'], mean)	#ind_resten
            SDLand <- tapply(RegData[ind_resten,'Endr'], RegData[ind_resten, 'Gr'], sd)
            NLand <- tapply(RegData[ind_resten,'Endr'], RegData[ind_resten, 'Gr'], length)
            KonfLand <- rbind(GjsnLand - 2*SDLand/sqrt(NLand), GjsnLand + 2*SDLand/sqrt(NLand))
            KonfLand <- replace(KonfLand, which(KonfLand < KIekstrem[1]), KIekstrem[1])
            KonfLand <- replace(KonfLand, which(KonfLand > KIekstrem[2]), KIekstrem[2])
            
            #Egen avdeling
            if (egenavd==1) {
                  Gjsn <- tapply(RegData[ind_sh ,'Endr'], RegData[ind_sh, 'Gr'], mean)
                  SD <- tapply(RegData[ind_sh ,'Endr'], RegData[ind_sh, 'Gr'], sd)
                  N <- tapply(RegData[ind_sh ,'Endr'], RegData[ind_sh, 'Gr'], length)
                  Konf <- rbind(Gjsn - 2*SD/sqrt(N), Gjsn + 2*SD/sqrt(N))
                  Konf <- replace(Konf, which(Konf < KIekstrem[1]), KIekstrem[1])
                  Konf <- replace(Konf, which(Konf > KIekstrem[2]), KIekstrem[2])
            } else {
                  Gjsn <- GjsnLand
                  SD <- SDLand
                  N <- NLand
                  Konf <- KonfLand
            }
            
            #xmin <- 0	#AntGr-0.5
            #xmax <- AntGr
            ymin <- min(KonfLand, Konf, na.rm=TRUE) #-0.5
            ymax <- 1.08*max(KonfLand, Konf, na.rm=TRUE) #+0.5
            h <- 0.02*(ymax-ymin)
            b <- AntGr/35
            #farger <- hsv(7/12, c(1,0.55,0.35,0.1), v=c(0.62,0.75,0.9,0.95))
            farger <- c("#084594", "#2171B5", "#6BAED6", "#C6DBEF")	#Blåoff
            bgfarge <- farger[4]	#hsv(210/360, 0.1, .95)
            plotfarge <- farger[1]
            
            #-----------Figur---------------------------------------
            figtype(outfile)
            plot(1:AntGr, Gjsn, ylim=c(ymin, ymax), type='n', #xlim=c(xmin, xmax), 
                 cex=0.8, font.main=1, cex.main=1, cex.lab=0.9, cex.axis=0.9,
                 ylab=c(ytxt,'med 95% konfidensintervall'), 
                 xlab=paste('Prescore, ', t1, sep=''), main=Tittel, xaxt='n', 
                 sub='(Tall i boksene angir antall pasienter som har svart)', cex.sub=0.9)	#, axes=F)
            axis(side=1, at = 1:AntGr, labels=GrNavn, cex.axis=0.75, las=retn)
            #Hele landet
            med <- which(NLand>1)
            if (egenavd==1) {
                  polygon( c(med, med[length(med):1]), c(KonfLand[1,med], KonfLand[2,med[length(med):1]]), 
                           col=bgfarge, border=NA)
                  legend('top', bty='n', fill=bgfarge, border=bgfarge, cex=0.9,
                         paste('angir 95% konfidensintervall for landet forøvrig, N=', sum(NLand), sep=''))
            }
            
            rect(1:AntGr-b, Gjsn-h, med+b, Gjsn+h, border = plotfarge, lwd=1)	
            text(1:AntGr, Gjsn, N, col=plotfarge, cex=0.9) 	
            
            #Konfidensintervall:
            ind <- which(Konf[1, ] > Gjsn-h)
            options(warn=-1)
            arrows(x0=med, y0=Gjsn-h, x1=med, length=0.08, code=2, angle=90, 
                   y1=replace(Konf[1, ], ind, Gjsn[ind]-h), col=plotfarge, lwd=1)
            arrows(x0=med, y0=Gjsn+h, x1=med, y1=replace(Konf[2, ], ind, Gjsn[ind]+h), 
                   length=0.08, code=2, angle=90, col=plotfarge, lwd=1)
            if ( outfile != '') {dev.off()}
            
            ResData <- round(rbind(Gjsn, GjsnLand, Konf, KonfLand), 1)
            rownames(ResData) <- c('Gjsn', 'GjsnLand', 'KIned', 'KIopp', 'KIRLned', 'KIRLopp')
            UtData <- list(paste(toString(TittelUt),'.', sep=''), ResData )
            names(UtData) <- c('Tittel', 'Data')
            return(invisible(UtData))
            
      }	#end if statement for 0 observations
}	#end function
