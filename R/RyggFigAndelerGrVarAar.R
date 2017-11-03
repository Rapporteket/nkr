#' Søylediagram med andeler for hver grupperingsenhet (sykehus, RHF, ...)
#'
#' Funksjon som genererer en figur med andeler av en variabel for en valgt grupperingsvariabel,
#' f.eks. sykehus.
#' Funksjonen er delvis skrevet for å kunne brukes til andre grupperingsvariable enn sykehus
#'
#' Andel som mottar sykepenger er definert som svaralternativene: 'Sykemeldt',
#'        'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet' 
#' Argumentet \emph{valgtVar} har følgende valgmuligheter:
#'    \itemize{
#'     \item Alder: Pasienter over 75år    
#'     \item Antibiotika: Er det gitt antibiotikaprofylakse?    
#'     \item ArbstatusPre: Mottar sykepenger, før operasjon?    
#'     \item Arbstatus: Mottar sykepenger, 3 mnd etter operasjon?    [ENDRET fra Arbstatus3mnd, Arbstatus12mnd]
#'     \item ASA: ASA-grad > II    
#'     \item BMI: Pasienter med fedme (BMI>30)    
#'     \item ErstatningPre: Søkt/planlegger å søke erstatning?    
#'     \item Fornoyd: Fornøyde pasienter [ENDRET fra Fornoyd3mnd, Fornoyd12mnd  ]
#'     \item Kp3Mnd: Pasientrapporterte komplikasjoner    
#'     \item Misfornoyd:  Andel med Misfornøyd/litt misfornøyd [ENDRET fra Misfor3mnd, Misfor12mnd]
#'     \item Nytte: Klart bedre    [ENDRET fra Nytte3mnd, Nytte12mnd]
#'	 \item OswEndr30pst: Mer enn 30% forbedring i Oswestry-skår, 3 mnd. [ENDRET fra Osw30_3mnd, Osw30_12mnd]
#'     \item PeropKomp: Komplikasjon ved operasjon
#'     \item PeropKompDura: Komplikasjon ved operasjon: Durarift
#'     \item Roker: Røyker du?    
#'     \item Saardren: Sårdren    
#'     \item SmStiPre: Bruker smertestillende før operasjonen    
#'     \item SymptVarighRyggHof: Varighet av rygg-/hoftesmerter > 1år    
#'     \item SympVarighUtstr: Varighet av utstrålende smerter > 1 år  
#'     \item UforetrygdPre: Søkt eller planlegger å søke uføretrygd før operasjon?    
#'     \item Utd: Andel høyskole-/universitetsutdannede    
#'     \item Verre Mye verre/verre enn noen gang, 3 mnd. [ENDRET fra Verre3mnd, Verre12mnd]
#'		\item ..
#'	 	\item BeinsmLavPre: Pasienter med preop. beinsmerte < 2.5 og ikke parese.
#'		\item BeinsmEndrLav: Forbedring av beinsmerter under 1.5 poeng
#'     \item DegSponSSSten: Pasienter med Degenerativ spondylolistese og sentral spinal stenose 
#'	 \item OswEndrLav: Mer enn 20 poeng forbedring i Oswestry-skår, 3 mnd/12mnd.
#' \item OswEndr20: 
#' \item Osw48: Oswestry-skår fortsatt over 48
#' \item KpInf3Mnd: Sårinfeksjoner
#' \item Morsmal: Fremmedspråklige (ikke norsk som morsmål)
#'		}
#'
#' @inheritParams RyggFigAndeler
#' @param grVar Tekstvariabel som angir hva skal resultatene grupperes på. 
#'                ShNavn-sykehus/avdeling
#'                Fylke- Pasienten bor i det akutelle fylket
#'                BoHF - Pasienten bor i boområdene til det angitte HF.
#'                BoRHF - Pasienten bor i boområdene til det angitte RHF.
#' @param valgtVar Variabelen det skal vises resultat for. Se \strong{Details} for oversikt.
#' @param tidlAar 0:Viser resultatat for hele perioden samlet, 1: Viser resultat for siste år og forrige
#' @param AKjust Alders-og kjønnsjustering når grVar er boområder. Basert på 3 aldersgrupper gruppert ut fra alderskvartilene.
#'          0:ikke juster, 1:juster for alder og kjønn
#'
#' @return Figur med...
#'
#' @export

RyggFigAndelerGrVarAar <- function(RegData, valgtVar, datoFra='2007-01-01', datoTil='3000-12-31', 
                                   minald=0, maxald=130, erMann='', hovedkat=99, tidlOp='', opKat=99, 
								   hentData=0, preprosess=1,enhetsUtvalg=0, grVar='ShNavn', tittel=1, 
								   ktr=0, reshID=0, aar=0, Ngrense=10, tidlAar=0, AKjust=0, outfile='') {
      
      if (hentData == 1) {		
            RegData <- RyggRegDataSQL()
      }
      
      # Preprosessere data
      if (preprosess){
            RegData <- RyggPreprosess(RegData=RegData)
      }
      #------- Tilrettelegge variable
      RyggVarSpes <- RyggVarTilrettelegg(RegData=RegData, valgtVar=valgtVar, ktr=ktr, figurtype = 'andelGrVar')
      RegData <- RyggVarSpes$RegData
      sortAvtagende <- RyggVarSpes$sortAvtagende
      varTxt <- RyggVarSpes$varTxt
      KImaal <- RyggVarSpes$KImaal
      
      
      
      if ((AKjust==1) & !(grVar %in% c('BoHF', 'BoRHF'))) { AKjust=0}
      
      
     #Gjør utvalg
      if (tidlAar != 0 ) { #tidlAar - år det skal sammenliknes med
            #AarMax <- max(RegData$OpAar)	#Siste år
            AarTxt <- as.character(aar)
            #RegData <- RegData[which(RegData$OpAar %in% c((AarMax-2):AarMax)), ]
            RegData[,grVar] <- as.character(RegData[,grVar])
            RegData[,grVar] <- factor(RegData[,grVar])
		#aar <- as.numeric(c(tidlAar, aar))
      }
      
     if (reshID==0) {enhetsUtvalg <- 0}
     RyggUtvalg <- RyggUtvalgEnh(RegData=RegData, reshID=reshID, datoFra=datoFra, datoTil=datoTil, 
                                 minald=minald, maxald=maxald, erMann=erMann, aar=as.numeric(c(tidlAar, aar)),
                                 hovedkat=hovedkat, opKat=opKat, tidlOp=tidlOp,enhetsUtvalg=enhetsUtvalg)
     smltxt <- RyggUtvalg$smltxt
     medSml <- RyggUtvalg$medSml 
     hovedgrTxt <- RyggUtvalg$hovedgrTxt
     utvalgTxt <- RyggUtvalg$utvalgTxt
     ind <- RyggUtvalg$ind
     RegData <- RyggUtvalg$RegData
     
     
      #SJEKK:
      RegData <- RegData[which(!is.na(RegData[ ,grVar])), ]
      names(RegData)[which(names(RegData) == grVar)] <- 'grVar'
      #grVar kan være sykehus, boområde osv.  
      #Hvis siste år for få reg - ta også bort resultater fra foregående år.
      N <- dim(RegData)[1] #table(RegData$OpAar)      #Antall per år
      
      #----------------------------------------------------------------------------------------------
      #KODEN MÅ KOMPRIMERES!!!!!!!!!:
      if (tidlAar != 0) { #Resultater for hvert av de siste 3 år.
            katVariable <- c('OpAar', 'grVar')
            Nvar <- tapply(RegData$Variabel, RegData[ ,katVariable], sum, na.rm=T) #Variabel er en 0/1-variabel.
            if(N > 0) {Ngr <- table(RegData[ ,katVariable])}	else {Ngr <- 0}
            
            #Sjekk for AK-justering
            if (AKjust == 1) { #Alders-og kjønnsjustering
                  Nvar <- tapply(RegData$Variabel, RegData[ ,c('OpAar', 'grVar')], sum, na.rm=T) #Variabel er en 0/1-variabel.
                  AndelerGr <- StandAlderKjonn(RegData=RegData, stdPop='Register', antAldgr=3, 
                                               katVariable=katVariable)  
                  #Hvis Norge som egen søyle:
                  #StandGrVar <- StandAlderKjonn(RegData=RegData, stdPop='Register', antAldgr=3, katVariable=katVariable)
                  #StandNorge <- StandAlderKjonn(RegData=RegData, stdPop='Register', antAldgr=3, katVariable='OpAar')
                  #AndelerGr <- cbind(StandGrVar, StandNorge)
                  
            }  else {	
                  AndelerGr <- round(100*Nvar/Ngr,2)
            }
		indGrUt <- 0
		GrNavn <- names(Ngr[AarTxt, ])
            Ngrtxt <- Ngr[AarTxt, ]	#paste0('N=', as.character(Ngr[AarTxt, ]))
            #Ikke sjekket at tidl. år <Ngrense tas ut.
      if (sum(which(Ngr[AarTxt, ] < Ngrense))>0) {
            #Må ta bort punkt/søyler for de som har for få registreringer for det aktuelle året.
           indGrUt <- which(Ngr < Ngrense) #, arr.ind = TRUE ) #Alle som har for få. Indeks er kolonnevis
           indGrUt2 <- which(Ngr[2,] < Ngrense)#as.numeric() #"Hoved"år
           indGrUt1 <- as.numeric(which(Ngr[1,] < Ngrense)) #Første år union(indGrUt1, indGrUt2)
           #NB: ?Heller ta ut Alle med hovedår<Ngrense + sml.år med <Ngrense?
           AndelGrUt <- c(sum(AndelerGr[1,indGrUt1]*Ngr[1,indGrUt1], na.rm = T)/sum(Ngr[1,indGrUt1]),
                        sum(AndelerGr[2,indGrUt2]*Ngr[2,indGrUt2], na.rm = T)/sum(Ngr[2,indGrUt2]))
           
           AndelerGr <- cbind(AndelGrUt, AndelerGr[,-indGrUt2]) 
           GrNavn <- c(paste0(length(indGrUt2), ' avd. med N<',Ngrense, '*'), GrNavn[-indGrUt2])
           Ngrtxt <- c(sum(Ngr[2,indGrUt2]), Ngrtxt[-indGrUt2])  
     }
           #AndelerGr[indGrUt] <- NA	#dummy0	#Alle andeler med for lav N
            sortInd <- order(as.numeric(AndelerGr[AarTxt,]), decreasing=sortAvtagende)
            AndelerSisteSort <- AndelerGr[AarTxt,sortInd] #Unødvendig?
            
            #AntGrNgr <- length(which(Ngr[AarTxt, ] >= Ngrense))	#"Gyldige" grupper
            #Ngrtxt[which(Ngr[AarTxt, ] < Ngrense)] <- paste0('<', Ngrense) #paste0('N<', Ngrense)	
            AndelerGrSort <- AndelerGr[ ,sortInd]
            GrNavnSort <- paste0(GrNavn[sortInd],' (',Ngrtxt[sortInd], ')')  #paste0(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd])
            #GrNavnSort <- paste0(colnames(AndelerGrSort),' (',Ngrtxt[sortInd], ')')  #paste0(names(Ngr)[sortInd], ', ',Ngrtxt[sortInd])
            andeltxt <- paste0(sprintf('%.1f',AndelerGrSort[AarTxt,]), '%') 	
            #if (length(indGrUt)>0) {andeltxt[(AntGrNgr+1):(length(GrNavnSort))] <- '' } #paste0('N<', Ngrense, ' siste år')} #''
            #--------------------------------------------------------------
      } else {	#Hvis vi skal ha resultater for perioden totalt
            
            N <- dim(RegData)[1]
            Nvar <- tapply(RegData$Variabel, RegData[ ,'grVar'], sum, na.rm=T)
            if(N > 0) {Ngr <- table(RegData[ ,'grVar'])}	else {Ngr <- 0}
            AntGrNgr <- length(which(Ngr >= Ngrense))	#length(which(Midt>0))
            AndelerGr <- round(100*Nvar/Ngr,2)
            
            indGrUt <- as.numeric(which(Ngr < Ngrense))
            if (length(indGrUt)==0) { indGrUt <- 0}
            AndelerGr[indGrUt] <- NA #dummy0
            sortInd <- order(as.numeric(AndelerGr), decreasing=TRUE)
            Ngrtxt <- Ngr #paste('N=', as.character(Ngr), sep='')	#
            #Ngrtxt[indGrUt] <- paste0('N<', Ngrense)	
            
            AndelerGrSort <- AndelerGr[sortInd]
            AndelerSisteSort <- AndelerGrSort
            AndelHele <- round(100*sum(RegData$Variabel)/N, 2)
            GrNavnSort <-  paste0(names(AndelerSisteSort), ' (', Ngrtxt[sortInd],')')  
            
            andeltxt <- paste0(sprintf('%.1f',AndelerSisteSort), '%') 	#round(as.numeric(AndelerSiste),1)
            if (length(indGrUt)>0) {andeltxt[(AntGrNgr+1):(AntGrNgr+length(indGrUt))] <- ''}
      }
      
      if (tittel==0) {Tittel<-''} else {Tittel <- RyggVarSpes$tittel}
      
      #-----------Figur---------------------------------------
      # Lager ikke figur hvis ALLE N er mindre enn grensa eller hvis ugyldig parameterkombinasjon.
      if 	( max(Ngr) < Ngrense) { 
            FigTypUt <- rapbase::figtype(outfile)
            farger <- FigTypUt$farger
            plot.new()
            if (dim(RegData)[1]>0) {
                  tekst <- paste0('Færre enn ', Ngrense, ' registreringer ved hvert av sykehusene')
            } else {tekst <- 'Ingen registrerte data for dette utvalget'}
            title(main=Tittel)
            text(0.5, 0.6, tekst, cex=1.2)
            legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
            if ( outfile != '') {dev.off()}
            
      } else {
            
            #--------------------------FIGUR---------------------------------------------------
            #Innparametre: ...
            #----------- Figurparametre ------------------------------
            cexShNavn <- 1 #0.85
            hoyde <- ifelse(grVar=='BoHF', 3*600, 3*800)
            FigTypUt <- rapbase::figtype(outfile, height=3*800, fargepalett=RyggUtvalg$fargepalett)
            farger <- FigTypUt$farger
            soyleFarger <- farger[4] #rep(farger[3], AntGrNgr)
            prikkFarge <- farger[3]
            #Hvis Norge egen søyle: soyleFarger[which(names(AndelerSisteSort)=='Norge')] <- farger[4]
            fargerMaalNiva <- c('#ddffcc', '#ffffcc', '#fff0e6') #Grønn, gul, rød
            #gronn <- '#CCFF99'
            #gul <- '#FFFF99'
            #rod <- '#ffb3b3'
            #Tilpasse marger for å kunne skrive utvalgsteksten
            NutvTxt <- length(utvalgTxt)
            vmarg <- max(0, strwidth(GrNavnSort, units='figure', cex=cexShNavn)*0.85)
            #NB: strwidth oppfører seg ulikt avh. av device...
            par('fig'=c(vmarg, ifelse(tidlAar==1,1,1), 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med
            
            xmax <- min(max(AndelerGrSort, na.rm = T),100)*1.15
            xaksetxt <- ifelse(AKjust==1, 'Andel (%), justert for alder og kjønn', 'Andel operasjoner (%)')
		
		maalGrenser <- c(2,4,xmax)
		pos <- barplot(as.numeric(AndelerSisteSort), horiz=T, border=NA, col=soyleFarger, #add=TRUE , #plot=T,
                           xlim=c(0,xmax), ylim=c(0.05, 1.32)*length(GrNavnSort), font.main=1, #xlab=xaksetxt, 
		               las=1, cex.names=cexShNavn*0.9)
		rect(xleft=c(0, maalGrenser[1:2]), ybottom=0, xright=maalGrenser, ytop=max(pos)+0.4, #pos[AntGrNgr+1], 
		     col = fargerMaalNiva, border = NA) #add = TRUE,
		
            ybunn <- 0.1
            ytopp <- max(pos)+ 0.4 #pos[2]-pos[1] #pos[AntGrNgr]+ 0.4	#
            if (tidlAar != 0) {
                  #indMed <- 1:AntGrNgr
                  AartxtTidl <- as.character(tidlAar)
                  Naar <- rowSums(Ngr, na.rm=T)
                  ResAar <- 100*rowSums(Nvar, na.rm=T)/Naar
                  lines(x=rep(ResAar[2], 2), y=c(ybunn, ytopp), col=farger[1], lwd=2)
                  barplot(as.numeric(AndelerSisteSort), horiz=T, border=NA, col=soyleFarger, add=T, #plot=T,
                          xlim=c(0,xmax), ylim=c(0.05, 1.27)*length(GrNavnSort), font.main=1, #xlab=xaksetxt, 
                          las=1, cex.names=cexShNavn*0.9)
                  #points(y=pos[indMed], x=AndelerGrSort[Aar1txt, indMed], cex=1.7, pch='|')    #col=farger[2],
                  points(y=pos+0.1, x=AndelerGrSort[AartxtTidl, ], cex=1, pch='|', col=prikkFarge) #y=pos[indMed]+0.1, x=AndelerGrSort[AartxtTidl, indMed]
                  legend('top', inset=c(0.1,0), xjust=1, cex=0.85, bty='o', bg='white', box.col='white',
                         lwd=c(NA,NA,2), pch=c(124,15,NA), pt.cex=c(1, 1.9, 1), 
						 col=c(prikkFarge,soyleFarger,farger[1]),
                         legend=c(#paste0(Aar1txt, ' (', sprintf('%.1f', ResAar[1]), '%, ', 'N=', Naar[1],')'),
                                  paste0(AartxtTidl, ' (', sprintf('%.1f', ResAar[1]), '%, ', 'N=', Naar[1],')'),
                                  paste0(aar, ' (', sprintf('%.1f', ResAar[2]), '%, ', 'N=', Naar[2],')'),
                                  paste0('Hele landet, ',AarTxt)) 
                  )
                  #mtext(line=3, at=0.5, 'Måloppnåelse: ', side=1, las=1, cex=0.8, adj=1) # 
                  legend(x=0, y=-3.5, pch=c(NA,15,15,15), col=c(NA, fargerMaalNiva), ncol=4, xpd=TRUE, border=NA,
                         box.col='white',cex=0.8, pt.cex=1.5, 
                         legend=c('Måloppnåelse:', 'Meget god', 'God', 'Mindre god')) #paste(c('Meget god', 'God', 'Mindre god'), 'måloppnåelse')
                  mtext(xaksetxt, side=1, las=1, cex=cexShNavn, adj=0.5, line=2)
                  overPos <- max(pos)+0.4*log(max(pos))
                  mtext(at=overPos, paste0('(N, ', aar, ')'), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	
            } else {
                  legend('topright', xjust=1, cex=1, lwd=2, col=farger[2],
                         legend=paste0(smltxt, ' (', sprintf('%.1f',AndelHele), '%), ', 'N=', N),
                         bty='o', bg='white', box.col='white')
                  mtext(at=max(pos)+0.5*log(max(pos)), paste0('(N)' ), side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	
                  lines(x=rep(AndelHele, 2), y=c(ybunn, ytopp), col=farger[1], lwd=2)
            }
            mtext(at=pos+max(pos)*0.0045, GrNavnSort, side=2, las=1, cex=cexShNavn, adj=1, line=0.25)	#Legge på navn som eget steg
            title(Tittel, line=1, font.main=1, cex.main=1.3)
            
            text(x=xmax*0.01, y=pos+0.1, andeltxt, #x=AndelerGrSort+xmax*0.01
                 las=1, cex=0.8, adj=0, col=farger[1])	#Andeler, hvert sykehus
            
            #Tekst som angir hvilket utvalg som er gjort
            mtext(utvalgTxt, side=3, las=1, cex=1, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))
            
            
            par('fig'=c(0, 1, 0, 1))
            if ( outfile != '') {dev.off()}
            #----------------------------------------------------------------------------------
      }
}
