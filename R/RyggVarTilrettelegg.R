#' Funksjon for å tilrettelegge variable for beregning. 
#'
#' Denne funksjonen gjør utvalg og tilrettelegger variable (gitt ved valgtVar) til videre bruk. 
#' Videre bruk kan eksempelvis være beregning av AggVerdier eller gjennomsnitt. 
#' Funksjonen gjør også filtreringer som å fjerne ugyldige verdier for den valgte variabelen, samt ta høyde for avhengigheter med
#' andre variable. Det er også her man angir aksetekster og titler for den valgte variabelen. 
#' Her kan mye hentes til analysebok
#'
#' @inheritParams RyggFigAndeler
#' @inheritParams RyggUtvalgEnh
#' @param figurtype Hvilken figurtype det skal tilrettelegges variable for: 
#'                'andeler', 'andelGrVar', 'andelTid', 'gjsnGrVar', 'gjsnTid'
#'				
#' @return Definisjon av valgt variabel.
#'
#' @export
#'

RyggVarTilrettelegg  <- function(RegData, valgtVar, grVar='', ktr=0, figurtype='andeler'){

      
      "%i%" <- intersect
      
      #----------- Figurparametre ------------------------------
      cexgr <- 1	#Kan endres for enkeltvariable
      retn <- 'V'		#Vertikal som standard. 'H' angis evt. for enkeltvariable
      flerevar <- 0
      grtxt <- ''		#Spesifiseres for hver enkelt variabel
      grtxt2 <- ''	#Spesifiseres evt. for hver enkelt variabel
      grNavn <- ''
      varTxt <- ''
      xAkseTxt <- ''	#Benevning
      yAkseTxt <- ''
      pktTxt <- '' #(evt. søyletekst)
      txtEtiketter  <- ''	#legend
      verdier <- ''	#AggVerdier, gjennomsnitt, ...
      verdiTxt <- '' 	#pstTxt, ...
      strIfig <- ''		#cex
      sortAvtagende <- TRUE  #Sortering av resultater
      KImaal <- NA
      tittel <- 'Mangler tittel' 
      variable <- 'Ingen'
      #deltittel <- ''
      RegData$Variabel <- 0
      #Kan her definere opp alle aktuelle grupperingsvariable og deres tekst, eller 
      #sende inn grupperingsvariabel og så gjøre beregninger. (Ulempe: Ekstra avhengigheter)
      #Sentralt spm: Hvor skal det avgjøres hvilken figurtype som vises???
      
      
      #--------------- Definere variable ------------------------------
      #Variabeltyper: Numeriske, kategoriske, indikator
      # For hver valgtVar:
      # Definer og gjør utvalg for variabelen
      # tittel, xAkseTxt, sortAvtagende (standard: TRUE)
      
      tittel <- '' #I AndelerGrVar og GjsnGrVar genereres tittel i beregningsfunksjonen
      #if (valgtVar %in% c('OswEndr20', 'OswEndr30pst' )) {
      #ktr kan ha verdiene 0, 1 eller 2
      ktrtxt <- c(', 3 mnd etter', ', 12 mnd. etter')[ktr]
      #	}
      
      
      #------------------------------------- 
      
      if (valgtVar=='alder') {	#Fordeling, GjsnGrVar, GjsnTid
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel <- RegData$Alder  	#GjsnTid, GjsnGrVar
            xAkseTxt <- 'alder (år)'	
            tittel <- 'Alder ved innleggelse'
            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
                  tittel <- 'alder ved innleggelse'}
            if (grVar == '') {	#Fordelingsfigur
                  gr <- c(seq(0, 100, 10),150)		
                  RegData$VariabelGr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)	
                  grtxt <- c('0-9','10-19','20-29','30-39','40-49','50-59','60-69','70-79','80-89','90-99','100+')
                  xAkseTxt <- 'Aldersgrupper (år)'}
            sortAvtagende <- FALSE
      }
      
      if (valgtVar=='alder70') {	#AndelTid, AndelerGrVar
            RegData <- RegData[which(RegData$Alder>=0), ]    #Tar bort alder<0
            RegData$Variabel[which(RegData$Alder>=70)] <- 1 
            varTxt <- 'over 70 år'
            tittel <- 'Pasienter over 70 år'
      }

      if (valgtVar == 'Antibiotika') { #AndelGrVar
            RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
            RegData$Variabel <- RegData[ ,valgtVar]
            tittel <- 'Fått antibiotika'
      }
      if (valgtVar == 'Arbstatus') { #AndelGrVar
            # Andel i kategori 6 tom 9, mottar sykepenger Av 1-9, (ikke bare de som sykemeldt fra før)
            #  #grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
            #		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
            RegData$Arbstatus <- switch(as.character(ktr), 
                                        '1'= RegData$Arbstatus3mnd,
                                        '2'= RegData$Arbstatus12mnd)
            RegData <- RegData[which(RegData$Arbstatus %in% 1:10), ]
            RegData$Variabel[which(RegData[ ,valgtVar] %in% 6:10)] <- 1
            tittel <- paste0('Mottar sykepenger' ,ktrtxt)
      }
      if (valgtVar == 'ArbstatusPre') { #AndelGrVar
            # Andel i kategori 6 tom 9, mottar sykepenger Av 1-9, (ikke bare de som sykemeldt fra før)
            #  #grtxt <- c('I arbeid','Hjemmeværende', 'Studie/skole', 'Pensjonist', 'Arbeidsledig', 'Sykemeldt',
            #		'Aktiv sykemeldt', 'Delvis sykemeldt', 'Attføring/rehab.', 'Uføretrygdet', 'Ukjent')
            RegData <- RegData[which(RegData[ ,valgtVar] %in% 1:10), ]
            tittel <- 'Mottar sykepenger, preoperativt?'
            RegData$Variabel[which(RegData[ ,valgtVar] %in% 6:10)] <- 1
      }
      if (valgtVar == 'ASA') { #AndelGrVar
            RegData <- RegData[which(RegData[,valgtVar] %in% 1:5), ]
            RegData$Variabel[which(RegData[ ,valgtVar] > 2)] <- 1
            tittel <- 'ASA-grad > II'
      }
      if (valgtVar == 'BeinsmLavPre') { #AndelGrVar
            #Lav beinsmerte og ingen parese. (Først og fremst prolaps)
            RegData$Variabel[which(is.na(RegData$OpIndParese) & (RegData$SmBePre < 2.5))] <- 1
            tittel <- 'Beinsmerte <= 2 og ingen parese'
            sortAvtagende <- F
            #tittel <- 'Beinsmerte \u2264 og ingen parese'
            #intToUtf8(2264)
      }
      if (valgtVar == 'BeinsmEndrLav') { #AndelGrVar
            #Mislykkede operasjoner
            RegData$BeinsmEndr <- switch(as.character(ktr), 
                                         '1'= (RegData$SmBePre - RegData$SmBe3mnd),
                                         '2'= (RegData$SmBePre - RegData$SmBe12mnd))
            RegData <- RegData[which(RegData$BeinsmEndr >= -10), ]	#Fjerne tomme og ugyldige
            #          RegData$Variabel[which(RegData$BeinsmEndr <1.5)] <- 1
            RegData$Variabel[which(is.na(RegData$OpIndParese) & (RegData$BeinsmEndr < 1.5))] <- 1
            tittel <- paste0('Forbedring av beinsmerte-skår < 1.5 poeng', ktrtxt)
      }
      if (valgtVar == 'BMI') { #AndelGrVar
            #BMI > 30
            RegData <- RegData[which(RegData[,valgtVar] >10), ]
            RegData$Variabel[which(RegData[ ,valgtVar] > 30)] <- 1
            tittel <- 'Pasienter med fedme (BMI>30)'
      }
      if (valgtVar == 'degSponFusj') { #AndelGrVar
            #hovedkat=9 #Degen. spondylolistese
            RegData <- RyggUtvalgEnh(RegData, hovedkat=9)$RegData
            RegData$Variabel[which(RegData$HovedInngrep ==5)] <- 1
            varTxt <- 'tilfeller'
            tittel <- 'Degen. spondylolistese operert med fusjonskirurgi'
            sortAvtagende <- F
      }
      if (valgtVar == 'degSponSSSten') { #AndelGrVar
            #(Først og fremst fusjonskirurgi)
            RegData$Variabel[which((RegData$RfSentr==1) & (RegData$RfSpondtypeDegen == 1))] <- 1
            tittel <- 'Degenerativ spondylolistese og sentral spinal stenose'
            sortAvtagende <- FALSE
      }
      if (valgtVar == 'ErstatningPre') { #AndelGrVar
            #Pasientskjema. Andel med ErstatningPre 1 el 3
            #Kode 1:4,9: 'Ja', 'Nei', 'Planlegger', 'Innvilget', 'Ukjent'
            RegData <- RegData[which(RegData$ErstatningPre %in% 1:4), ]
            RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
            tittel <- 'Pasienten har søkt/planlegger å søke erstatning'
      }
      if (valgtVar=='EQ5DEndr') {#gjsnGrVar
            RegData$Variabel <- switch(as.character(ktr), 
                                       '1'= (RegData$EQ5D3mnd - RegData$EQ5DPre),
                                       '2'= (RegData$EQ5D12mnd - RegData$EQ5DPre))
            RegData <- RegData[which(!is.na(RegData$Variabel)),]
            tittel <- paste0('forbedring av EQ5D', ktrtxt)#gjsnGrVar
            
      }
      
      if (valgtVar =='Fornoyd') { #AndelGrVar	#%in% c('Fornoyd3mnd','Fornoyd12mnd')) {
            #3/12mndSkjema. Andel med helt Fornøyd (1)
            #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
            RegData$Fornoyd <- switch(as.character(ktr), 
                                      '1'= RegData$Fornoyd3mnd,
                                      '2'= RegData$Fornoyd12mnd)
            RegData <- RegData[which(RegData$Fornoyd %in% 1:5), ]
            RegData$Variabel[which(RegData$Fornoyd %in% 1:2)] <- 1 #
            #RegData$Variabel[which(RegData$Fornoyd ==1)] <- 1 #%in% 1:2
            tittel <- paste0('Helt fornøyde pasienter', ktrtxt)
      }
      if (valgtVar == 'Kp3Mnd') { #AndelGrVar
            #Komplikasjoner 0:nei, 1:ja
            RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
            RegData$Variabel <- RegData[ ,valgtVar]
            tittel <- 'Pasientrapporterte komplikasjoner'
      }
      if (valgtVar == 'KpInf3Mnd') { #AndelGrVar
            #Komplikasjoner 0:nei, 1:ja
            RegData <- RegData[which(RegData[,valgtVar] %in% 0:1), ]
            RegData$Variabel <- RegData[ ,valgtVar]
            varTxt <- 'tilfeller'
            tittel <- 'Sårinfeksjon, pasientrapportert'
            sortAvtagende <- FALSE
      }
      if (valgtVar == 'Misfornoyd') { #AndelGrVar	#%in% c('Misfor3mnd','Misfor12mnd')) { #AndelGrVar
            #3/12mndSkjema. Andel med Misfornøyd/litt misfornøyd (1,2)
            #Kode 1:5,9: 'Fornøyd', 'Litt fornøyd', 'Verken eller', 'Litt misfornøyd', 'Misfornøyd', 'Ukjent')
            RegData$Misfornoyd <- switch(as.character(ktr), 
                                         '1'= RegData$FornoydBeh3mnd,
                                         '2'= RegData$FornoydBeh12mnd)
            RegData <- RegData[which(RegData$Misfornoyd %in% 1:5), ]
            RegData$Variabel[which(RegData$Misfornoyd %in% 4:5)] <- 1
            tittel <- paste0('Misfornøyde pasienter, 3 mnd.' ,ktrtxt)
      }
      if (valgtVar == 'Morsmal') { #AndelGrVar
            #           Kode 1:3:'Norsk', 'Samisk', 'Annet'
            RegData <- RegData[which(RegData$Morsmal %in% 1:3), ]
            RegData$Variabel[which(RegData$Morsmal %in% 2:3)] <- 1 
            tittel <- 'Fremmedspråklige (ikke norsk som morsmål)'
            sortAvtagende <- F
      }
      
      if (valgtVar == 'Nytte') { #AndelGrVar
            #Andel med helt bra/mye bedre (1:2)
            #Kode 1:7: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
            #				'Verre enn noen gang', 'Ukjent')
            RegData$Nytte <- switch(as.character(ktr), 
                                    '1'=RegData$Nytte3mnd,
                                    '2'=RegData$Nytte12mnd)
            RegData <- RegData[which(RegData$Nytte %in% 1:7), ]
            RegData$Variabel[RegData$Nytte %in% 1:2] <- 1
            tittel <- paste0('Helt bra eller mye bedre' , ktrtxt)
      }
      if (valgtVar=='Liggedogn') {#gjsnGrVar, andeler
            #For opphold registrert som dagkirurgi uten at liggedogn er reg., settes liggedogn=0
            dagind <- which( (is.na(RegData$Liggedogn) | is.nan(RegData$Liggedogn))  & RegData$Dagkirurgi==1)
            RegData$Liggedogn[dagind]<-0
            RegData <- RegData[which(RegData$Liggedogn>=0),]
            RegData$Variabel <- RegData$Liggedogn #gjsnGrVar
            gr <- c(0:7,100)	
            RegData$VariabelGr <- cut(RegData$Liggedogn, breaks=gr, include.lowest=TRUE, right=FALSE)
            grtxt <- c(0:6, '7+')
            xAkseTxt <- 'Antall liggedøgn' #(subtxt
            tittel <- 'liggetid' #gjsnGrVar
            sortAvtagende <- 'F'
      }
#      if (valgtVar == 'liggetid') { #Andeler #GjsnGrVar
#            #Liggetid bare >0
#            RegData$Variabel  <- as.numeric(RegData$liggetid)
#            RegData <- RegData[which(RegData$Variabel>0), ] 
#            tittel <- 'Liggetid'
#            if (figurtype %in% c('gjsnGrVar', 'gjsnTid')) {
#                  tittel <- 'liggetid'}
#            gr <- c(0, 1, 2, 3, 4, 5, 6, 7, 14, 1000)
#            RegData$VariabelGr <- cut(RegData$liggetid, breaks=gr, include.lowest=TRUE, right=FALSE)	
#            grtxt <- c('(0-1)','[1-2)','[2-3)','[3-4)','[4-5)','[5-6)','[6-7)','[7-14)','14+')
#            xAkseTxt <- 'Liggetid (døgn)'
#      }
      
      if (valgtVar=='OswEndr') {#gjsnGrVar
            RegData$Variabel <- switch(as.character(ktr), 
                                      '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                      '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
            RegData <- RegData[which(!is.na(RegData$Variabel)),]
            tittel <- paste0('forbedring av Oswestry', ktrtxt)#gjsnGrVar

      }
      
      if (valgtVar == 'OswEndrLav') { #AndelGrVar
            #Mislykkede operasjoner
            RegData$OswEndr <- switch(as.character(ktr), 
                                      '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                      '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
            RegData <- RegData[which(RegData$OswEndr >= -100), ]
            RegData$Variabel[which(RegData$OswEndr <13)] <- 1
            tittel <- paste0('Forbedring av Oswestry-skår < 13 poeng', ktrtxt)
            sortAvtagende <- F
      }
      if (valgtVar == 'OswEndr20') { #AndelGrVar, andelTid
            #Mislykkede operasjoner
            RegData$OswEndr <- switch(as.character(ktr), 
                                      '1'= (RegData$OswTotPre - RegData$OswTot3mnd),
                                      '2'= (RegData$OswTotPre - RegData$OswTot12mnd))
            RegData <- RegData[which(RegData$OswEndr >= -100), ]
            RegData$Variabel[which(RegData$OswEndr >20)] <- 1
            varTxt <- 'med >20 poeng forbedring'
            tittel <- paste0('Forbedring av Oswestry-skår > 20 poeng', ktrtxt)
      }
      
      if (valgtVar == 'OswEndr30pst') { #AndelGrVar, andelTid
            #Andel med klinisk signifikant forbedring i Oswestry-skår. 
            #Forbedring = nedgang
            RegData$OswPst <- switch(as.character(ktr),
                                     '1' = (RegData$OswTotPre - RegData$OswTot3mnd)/RegData$OswTotPre*100,
                                     '2' = (RegData$OswTotPre - RegData$OswTot12mnd)/RegData$OswTotPre*100)
            RegData <- RegData[which(RegData$OswPst>=-1000), ]
            RegData$Variabel[which(RegData$OswPst >=30)] <- 1
            varTxt <- 'med \u2265 30 % forbedring'
            tittel <- paste0('Minst 30% forbedring av Oswestry-skår', ktrtxt)
      }
      if (valgtVar == 'Osw22') { #AndelGrVar
            #Andel med Oswestry-skår under 22 etter op. 
            RegData$OswPost <- switch(as.character(ktr),
                                      '1' = RegData$OswTot3mnd,
                                      '2' = RegData$OswTot12mnd)
            RegData <- RegData[which(RegData$OswPost>=0), ]
            RegData$Variabel[which(RegData$OswPost <22)] <- 1
            tittel <- paste0('Oswestry-skår < 22 poeng', ktrtxt)
      }
      if (valgtVar == 'Osw48') { #AndelGrVar
            #Andel med Oswestry-skår fortsatt over 48. 
            RegData$OswPost <- switch(as.character(ktr),
                                      '1' = RegData$OswTot3mnd,
                                      '2' = RegData$OswTot12mnd)
            RegData <- RegData[which(RegData$OswPost>=0), ]
            RegData$Variabel[which(RegData$OswPost >48)] <- 1
            tittel <- paste0('Oswestry-skår > 48 poeng', ktrtxt)
            sortAvtagende <- F
      }
      
      if (valgtVar=='PeropKomp') { #AndelGrVar
            #Komplikasjoner ved operasjon
            #Kode 1:Ja,  tomme:Nei 
            RegData$Variabel[which(RegData$PeropKomp == 1)] <- 1
            tittel <- 'Komplikasjoner ved operasjon'
      }
      if (valgtVar=='PeropKompDura') { #AndelGrVar
            #Durarift ved operasjon
            #Kode 1:Ja,  tomme:Nei 
            RegData$Variabel[which(RegData$PeropKompDura == 1)] <- 1
            tittel <- 'Komplikasjon ved operasjon: Durarift'
            sortAvtagende <- FALSE
      }
      if (valgtVar=='Roker') { #AndelGrVar
            #PasientSkjema. Andel med Roker=1
            #Kode 0,1,tom: Nei, Ja Ukjent
            RegData <- RegData[which(RegData$Roker %in% 0:1), ]
            RegData$Variabel <- RegData$Roker
            tittel <- 'Røykere'
      }
      
      if (valgtVar == 'Saardren') { #AndelGrVar
            #LegeSkjema. Andel med Saardren=1
            #Kode 0,1,tom: Nei, Ja Ukjent
            RegData <- RegData[which(RegData$Saardren %in% 0:1), ]
            RegData$Variabel <- RegData$Saardren
            tittel <- 'Andel som får sårdren'
      }
      if (valgtVar=='SmBeinEndr') {#gjsnGrVar
            RegData$Variabel <- switch(as.character(ktr), 
                                       '1'= (RegData$SmBePre - RegData$SmBe3mnd),
                                       '2'= (RegData$SmBePre - RegData$SmBe12mnd))
            RegData <- RegData[which(!is.na(RegData$Variabel)),]
            tittel <- paste0('forbedring av beinsmerter', ktrtxt)#gjsnGrVar
            
      }
      if (valgtVar=='SmRyggEndr') {#gjsnGrVar
            RegData$Variabel <- switch(as.character(ktr), 
                                       '1'= (RegData$SmRyPre - RegData$SmRy3mnd),
                                       '2'= (RegData$SmRyPre - RegData$SmRy12mnd))
            RegData <- RegData[which(!is.na(RegData$Variabel)),]
            tittel <- paste0('forbedring av ryggsmerter', ktrtxt)#gjsnGrVar
            
      }
            if (valgtVar == 'SmStiPre') { #AndelGrVar
            #PasientSkjema. Andel med SmStiPre=1
            #Kode 0,1,tom: Nei, Ja Ukjent
            RegData <- RegData[which(RegData$SmStiPre %in% 0:1), ]
            RegData$Variabel <- RegData$SmStiPre
            tittel <- 'Bruker smertestillende, før operasjon'
            sortAvtagende <- F
      }
      
      if (valgtVar == 'SymptVarighRyggHof') { #AndelGrVar
            #PasientSkjema. Andel med SymptVarighRyggHof 4 el 5
            #Kode 1:5,tom: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
            RegData <- RegData[which(RegData$SymptVarighRyggHof %in% 1:5), ]
            RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
            varTxt <- 'med varighet minst 1 år'
            tittel <- 'Varighet av rygg-/hoftesmerter minst ett år'
            sortAvtagende <- F
      }
      
      if (valgtVar == 'SympVarighUtstr') { #AndelGrVar
            #PasientSkjema. Andel med SympVarighUtstr 4 el 5
            #Kode 1:5,tom: 'Ingen', '<3 mnd', '3-12 mnd', '1-2 år', '>2 år', 'Ukjent'
            RegData <- RegData[which(RegData$SympVarighUtstr %in% 1:5), ]
            RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
            varTxt <- 'med varighet minst 1år'
            tittel <- 'Varighet av utstrålende smerter minst ett år'
            sortAvtagende <- F
      }
      if (valgtVar == 'tidlOp3'){ #AndelTid, AndelGrVar
            RegData$Variabel[RegData$TidlOprAntall>2] <- 1
            varTxt <- 'med >2 tidl. operasjoner'
            tittel <- 'Flere enn to tidligere operasjoner'
            sortAvtagende <- F
            }
      if (valgtVar == 'UforetrygdPre') { #AndelGrVar, AndelTid
            #PasientSkjema. Andel med UforetrygdPre 1 og 3
            #Kode 1:4,tom: 'Ja', 'Nei', 'Planlegger søknad', 'Innvilget', 'Ukjent')
            RegData <- RegData[which(RegData$UforetrygdPre %in% 1:4), ]
            RegData$Variabel[which(RegData[ ,valgtVar] %in% c(1,3))] <- 1
            varTxt <- 'søkt/planlagt å søke'
            tittel <- 'Har søkt eller planlegger å søke uføretrygd'
            sortAvtagende <- F
      }
      if (valgtVar == 'Utd') { #AndelGrVar
            #PasientSkjema. Andel med Utdanning 4 el 5
            #Kode 1:5,9: 'Grunnskole++, 7-10år','Real-, yrkes- el vg skole', 'Allmennfaglig vg skole',
            #Høyskole/universitet, <4 år', 'Høyskole/universitet, 4år+', 'Ukjent'
            RegData <- RegData[which(RegData$Utd %in% 1:5), ]
            RegData$Variabel[which(RegData[ ,valgtVar] %in% 4:5)] <- 1
            varTxt <- 'med høyere utdanning'
            tittel <- 'Andel høyskole-/universitetsutdannede'
      }
      if (valgtVar == 'Verre') { #AndelGrVar		#%in% c('Verre3mnd','Verre12mnd')) {
            #3/12mndSkjema. Andel med helt mye verre og noen sinne (6:7)
            #Kode 1:7,9: ''Helt bra', 'Mye bedre', 'Litt bedre', 'Uendret', 'Litt verre', 'Mye verre',
            #				'Verre enn noen gang', 'Ukjent')
            RegData$Nytte <- switch(as.character(ktr), 
                                    '1'=RegData$Nytte3mnd,
                                    '2'=RegData$Nytte12mnd)
            RegData <- RegData[which(RegData$Nytte %in% 1:7), ]
            RegData$Variabel[RegData$Nytte %in% 6:7] <- 1
            tittel <- paste0('Mye verre/verre enn noen gang' , ktrtxt)
      }
      
      
      if (valgtVar == 'nyreBeh' ) {   # Andeler, andelerGrVar
            tittel <- 'Andel av opphold med registrert nyreerstattende behandling'
            RegData <- RegData[which(RegData$InnDato>=as.POSIXlt('2015-01-01')), ] 
            if (figurtype == 'andelGrVar') {
                  RegData <- RegData[RegData$KidneyReplacingTreatment %in% 1:2,]
                  RegData$Variabel[which(RegData$KidneyReplacingTreatment ==1)] <- 1
            }
            if (figurtype == 'andeler') {
                  RegData <- RegData[which(RegData$KidneyReplacingTreatment ==1), ]		#Bare de som har fått behandling
                  RegData$VariabelGr <- 9
                  RegData$VariabelGr[which(RegData$Kontinuerlig == TRUE)] <- 1	
                  RegData$VariabelGr[which(RegData$Intermitterende == TRUE)] <- 2
                  RegData$VariabelGr[(RegData$Kontinuerlig & RegData$Intermitterende) == TRUE] <- 3 #Overskriver tidl 1 eller 2
                  RegData$VariabelGr <- factor(RegData$VariabelGr, levels=c(1:3,9))	
            }
            grtxt <- c('Kontinuerlig \n(hemo-/dia-filtrasjon)', 'Intermitterende \n(hemodialyse)', 'Begge', 'Ukjent')
            retn <- 'H'
            #xAkseTxt <- 'Andel (%)'
      }
      
      
      if (valgtVar == 'trakeostomi') { #andelGrVar 
            #-1: Velg verdi, 1 = Nei, 2 = Ja – perkutan teknikk på intensiv/oppv., 3 = Ja – åpen teknikk (operativ)
            
            RegData <- RegData[which((RegData$Trakeostomi %in% 1:3) 
                                     & (RegData$InnDato >= as.POSIXlt('2016-01-01'))), ] #Innført ila 2015
            retn <- 'H'
            tittel <- 'Trakeostomi utført'
            RegData$Variabel[which(RegData$Trakeostomi %in% 2:3)] <- 1
            cexgr <- 0.9
      } 
      
      #---------------KATEGORISKE
      
      if (valgtVar=='InnMaate') { #andeler
            tittel <- 'Fordeling av Innkomstmåte'   
            gr <- c(0,6,8)
            RegData <- RegData[ which((RegData$InnMaate %in% gr)), ]             
            RegData$VariabelGr <- factor(RegData$InnMaate, levels=gr)
            grtxt <- c('Elektivt','Akutt med.', 'Akutt kir.') #InnMaate - 0-El, 6-Ak.m, 8-Ak.k, standard: alle (alt unntatt 0,6,8)
            xAkseTxt <- 'Innkomstmåte'
      }
      if (valgtVar == 'PrimaryReasonAdmitted') { #Andeler 
            #                       1:Respiratorisk svikt, 2:Sirk./kardiovaskulær svikt, 3:Gastroenterologisk svikt, 
            #                       4:Nevrologisk svikt, 5:Sepsis, 6:Skade/traume, 7:Metabolsk/intoksikasjon, 8:Hematologisk svikt, 
            #                       9:Nyresvikt, 10:Postoperativt, 11:Annet
            gr <- 1:11
            RegData <- RegData[which((RegData$PrimaryReasonAdmitted %in% gr) 
                                     & (RegData$InnDato >= as.POSIXlt('2016-01-01'))), ] #Innført ila 2015
            retn <- 'H'
            tittel <- 'Primærårsak til intensivoppholdet'
            RegData$VariabelGr <- factor(RegData$PrimaryReasonAdmitted, levels=gr)
            grtxt <- c('Respiratorisk svikt', 'Sirk./kardiovaskulær svikt', 'Gastroenterologisk svikt', 
                       'Nevrologisk svikt', 'Sepsis', 'Skade/traume', 'Metabolsk/intoksikasjon', 'Hematologisk svikt', 
                       'Nyresvikt', 'Postoperativt', 'Annet')
            cexgr <- 0.9
      } 
      #-------------- SAMMENSATTE variable
      #For flerevar=1 må vi omdefinere variablene slik at alle gyldige registreringer 
      #(dvs. alle registreringer som skal telles med) er 0 eller 1. De som har oppfylt spørsmålet
      # er 1, mens ugyldige registreringer er NA. Det betyr at hvis vi skal ta bort registreringer
      # som i kategorier av typen "Ukjent" kodes disse som NA, mens hvis de skal være med kodes de
      # som 0.
      #Vi sender tilbake alle variable som indikatorvariable, dvs. med 0,1,NA
      #(Alternativt kan vi gjøre beregninga her og sende tilbake teller og nevner for den sammensatte variabelen)
      
      if (valgtVar == 'inklKrit' ) {   # Andeler
            tittel <- 'Inklusjonskriterier, Rygg'
            #RegData <- RegData[which(RegData$InnDato>=as.POSIXlt('2016-01-01')), ] 
            sortAvtagende <- T
            retn <- 'H'
            flerevar <- 1
            variable <- c('MoreThan24Hours',  'MechanicalRespirator', 'DeadPatientDuring24Hours',  
                          'MovedPatientToAnotherIntensivDuring24Hours', 'VasoactiveInfusion' )
            #retn <- 'H'
            grtxt <- c('Liggetid over 24t', 'Mekanisk \nrespirasjonsstøtte', 'Død innen 24t',  'Overflyttet innen 24t', 
                       'Kontinuerlig \nvasoaktiv infusjon')
            ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
            ind1 <- which(RegData[ ,variable] == 1, arr.ind=T) #Ja i alle variable
            RegData[ ,variable] <- NA
            RegData[ ,variable][ind01] <- 0
            RegData[ ,variable][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
            #Beregne direkte:
            #apply(RegData[,variable], MARGIN=2, FUN=function(x) sum(x %in% 0:1))
      }
      if (valgtVar == 'spesTiltak' ) {   # Andeler
            #SpecialMeasures
            tittel <- 'Spesielle tiltak/intervensjoner'
            RegData <- RegData[which(RegData$InnDato>=as.POSIXlt('2016-01-01')), ] 
            sortAvtagende <- T
            retn <- 'H'
            flerevar <- 1
            variable <- c('TerapetiskHypotermi', 'EcmoEcla', 'Iabp', 'Impella', 'Icp', 'Oscillator', 'No', 
                          'Leverdialyse', 'Hyperbar', 'Eeg')
            #retn <- 'H'
            grtxt <- c('Terapetisk hypotermi', 'ECMO/ECLA', 'IABP Aortaballongpumpe', 'Impella/VV-assist', 
                       'ICP, intrakranielt trykk', 'Oscillator', 'NO-behandling', 
                       'Leverdialyse', 'Hyperbar oksygenbeh.', 'Kontinuerlig EEG')
            #ind01 <- which(RegData[ ,variable] != -1, arr.ind = T) #Alle ja/nei
            ind1 <- which(RegData[ ,variable] == TRUE, arr.ind=T) #Ja i alle variable
            RegData[ ,variable] <- 0
            RegData[ ,variable][ind1] <- 1
            xAkseTxt <- 'Andel opphold (%)'
      }

      
      UtData <- list(RegData=RegData, grtxt=grtxt, cexgr=cexgr, varTxt=varTxt, xAkseTxt=xAkseTxt, KImaal=KImaal, retn=retn,
                     tittel=tittel, flerevar=flerevar, variable=variable, sortAvtagende=sortAvtagende)
      #RegData inneholder nå variablene 'Variabel' og 'VariabelGr'
      return(invisible(UtData)) 
      
}
