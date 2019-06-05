library(shiny)
library(magrittr)
library(rapRegTemplate)

server <- function(input, output, session) {

  # Last inn data
   #regData <- getFakeRegData()
   NKRdata <- read.table('A:/Rygg/RyggAarsrapp2018.csv', sep=';', header=T, encoding = 'UTF-8')
   RegData <- NKRdata
   #----------Hente data og evt. parametre som er statistke i appen----------
   context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
   if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
         registryName <- "nakke"
         dbType <- "mysql"
         query <- paste0('SELECT  ...')
         RegData <- rapbase::LoadRegData(registryName, qLivs, dbType)
   } #hente data på server
   
   reshID <- 601161 #999999	#601161 #100133	#111065 #105783	#103618	#102949	#   #Må sendes med til funksjon
   
   
  # Gjenbrukbar funksjon for å bearbeide Rmd til html
  htmlRenderRmd <- function(srcFile, params = list()) {
    # set param needed for report meta processing
    # params <- list(tableFormat="html")
    system.file(srcFile, package="rapRegTemplate") %>%
      knitr::knit() %>%
      markdown::markdownToHTML(.,
                               options = c('fragment_only',
                                           'base64_images',
                                           'highlight_code')) %>%
      shiny::HTML()
  }


  # Veiledning
  output$veiledning <- renderUI({
    htmlRenderRmd("veiledning.Rmd")
  })


  #-----------Fordelinger---------------------
  output$fordelinger <- renderPlot({
        RyggFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                        reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                        datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                        minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                        erMann=as.numeric(input$erMann))
  }, height=700, width=600)
  
  
  observe({
        UtDataFord <-  RyggFigAndeler(RegData=RegData, preprosess = 0, valgtVar=input$valgtVar,
                                       reshID=reshID, enhetsUtvalg=as.numeric(input$enhetsUtvalg),
                                       datoFra=input$datovalg[1], datoTil=input$datovalg[2],
                                       minald=as.numeric(input$alder[1]), maxald=as.numeric(input$alder[2]),
                                       erMann=as.numeric(input$erMann))
        
        #Følgende kan være likt for fordelingsfigurer i alle registre:
        tabFord <- lagTabavFig(UtDataFraFig = UtDataFord) #lagTabavFigAndeler
        output$tittelFord <- renderUI({
              tagList(
                    h3(UtDataFord$tittel),
                    h5(HTML(paste0(UtDataFord$utvalgTxt, '<br />')))
              )}) #, align='center'
        #output$fordelingTab <- renderTable(tabFord, rownames = T)
        
        #tittelKolGr <- c(UtDataFord$hovedgrTxt, UtDataFord$smltxt)
        kolGruppering <- c(1,3,3)
        names(kolGruppering) <- c(' ', UtDataFord$hovedgrTxt, UtDataFord$smltxt)
        output$fordelingTab <- function() { #gr1=UtDataFord$hovedgrTxt, gr2=UtDataFord$smltxt renderTable(
              antKol <- ncol(tabFord)
              kableExtra::kable(tabFord, format = 'html'
                                , full_width=F
                                , digits = c(0,0,1,0,0,1)[1:antKol]
              ) %>%
                    add_header_above(kolGruppering[1:(2+UtDataFord$medSml)]) %>%
                    #add_header_above(c(" "=1, tittelKolGr[1] = 3, 'Resten' = 3)[1:(antKol/3+1)]) %>%
                    column_spec(column = 1, width='5em') %>% #width_min = '3em', width_max = '10em') %>%
                    column_spec(column = 2:(ncol(tabFord)+1), width = '7em') %>%
                    row_spec(0, bold = T)
        }
        
        output$lastNed_tabFord <- downloadHandler(
              filename = function(){paste0(input$valgtVar, '_fordeling.csv')},
              content = function(file, filename){write.csv2(tabFord, file, row.names = T, na = '')
              })
        
  }) #observe, fordelinger
  
  ## Tabell
  output$distTable <- renderTable({
    makeHist(df = regData, var = input$var, bins = input$bins, makeTable = TRUE)
  })


  # Samlerapport
  ## vis
  output$samlerapport <- renderUI({
    htmlRenderRmd(srcFile = "samlerapport.Rmd",
                  params = list(var = input$varS, bins = input$binsS))
  })

  ## last ned
  output$downloadSamlerapport <- downloadHandler(
    filename = function() {
      "rapRegTemplateSamlerapport.html"
    },
    content = function(file) {
      srcFile <- normalizePath(system.file("samlerapport.Rmd",
                                           package = "rapRegTemplate"))
      tmpFile <- "tmpSamlerapport.Rmd"
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      file.copy(srcFile, tmpFile, overwrite = TRUE)
      out <- rmarkdown::render(tmpFile,
                               output_format =  rmarkdown::html_document(),
                               params = list(var = input$varS,
                                             bins = input$binsS),
                               output_dir = tempdir())
      file.rename(out, file)
    }
  )


  # Abonnement
  ## rekative verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    options = list(dom = 't')
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    userName <- rapbase::getUserName(session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", userName))
    } else {
      tagList(
        p(paste0("Aktive abonnement som sendes per epost til ", userName, ":")),
        DT::dataTableOutput("activeSubscriptions")
      )
    }
  })

  ## nye abonnement
  observeEvent (input$subscribe, {
    package <- "rapRegTemplate"
    owner <- getUserName(session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(
      interval = input$subscriptionFreq
    )
    email <- "test@test.no" # need new function i rapbase
    if (input$subscriptionRep == "Samlerapport1") {
      synopsis <- "Automatisk samlerapport1"
      fun <- "samlerapport1Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("Alder", 1)

    }
    if (input$subscriptionRep == "Samlerapport2") {
      synopsis <- "Automatisk samlerapport2"
      fun <- "samlerapport2Fun"
      paramNames <- c("p1", "p2")
      paramValues <- c("BMI", 2)
    }
    rapbase::createAutoReport(synopsis = synopsis, package = package,
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, runDayOfYear = runDayOfYear)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })

  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(session)
  })
}
