library(shiny)
library(rapbase)
library(markdown)

addResourcePath('rap', system.file('www', package='rapbase'))
regTitle = "Degenerativ Rygg"

#Definere innhold i felles rullegardinmenyer:
kjonn <- c("Begge"=2, "Menn"=1, "Kvinner"=0)
enhetsUtvalg <- c("Egen mot resten av landet"=1,
                  "Hele landet"=0,
                  "Egen enhet"=2)
tidsenhetValg <- rev(c('År'= 'Aar', 'Halvår' = 'Halvaar',
                       'Kvartal'='Kvartal', 'Måned'='Mnd'))



ui <- tagList(
  navbarPage(
    title = div(img(src="rap/logo.svg", alt="Rapporteket", height="26px"),
                regTitle),
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",

    #------------- Fordelingsfigurer--------------------
    
    tabPanel(p("Fordelinger", title='Her finner du resultater for: Alder, antibiotika, arbeidsstatus, BMI, erstatning, fornøydhet, komorbiditet,
               komplikasjoner, liggetid, morsmål, nytteverdi, operasjonskategori, operasjonsindikasjon, radiologi,
               snus, smertestillende, symptomvaribhet, tidl.operert, uføretrygdet, utdanning'),
             h2("Fordeling av valgt variabel", align='center'),
             sidebarPanel(width = 3,
                          selectInput(inputId = "valgtVar", label="Velg variabel",
                                      choices = c('Alder' = 'Alder', 'Antall nivå operert' = 'AntallNivaaOpr',
                                                  'Antibiotika' = 'Antibiotika', 'Arbeidstaus før operasjon' = 'ArbeidstausPreOp','Arbeidstaus 3 mnd. etter' = 'Arbeidstaus3mnd',
                                                  'Arbeidstaus 12 mnd. etter' = 'Arbeidstaus12mnd', 'ASA-grad' = 'ASAgrad',
                                                  'Utdanning' = 'Utdanning') #c('Alder'='Alder', "Ant. nivå operert" = 'AntallNivaaOpr')
                          ),
                          dateRangeInput(inputId = 'datovalg', start = "2018-01-01", end = Sys.Date(),
                                         label = "Tidsperiode", separator="t.o.m.", language="nb"),
                          selectInput(inputId = "erMann", label="Kjønn",
                                      choices = kjonn
                          ),
                          sliderInput(inputId="alder", label = "Alder", min = 0,
                                      max = 110, value = c(0, 110)
                          ),
                          selectInput(inputId = 'enhetsUtvalg', label='Egen enhet og/eller landet',
                                      choices = enhetsUtvalg
                          )
                          #sliderInput(inputId="aar", label = "Årstall", min = 2012,  #min(RegData$Aar),
                          #           max = as.numeric(format(Sys.Date(), '%Y')), value = )
             ),
             mainPanel(
                   h5("Hvilken variabel man ønsker å se resultater for, velges fra rullegardinmenyen
                      til venstre. Der kan man også gjøre ulike filtreringer."),
                   tabsetPanel(
                         tabPanel('Figur',
                                  helpText('Høyreklikk på figuren for å laste den ned'),
                                  br(),
                                  plotOutput("fordelinger")),
                         tabPanel('Tabell',
                                  uiOutput("tittelFord"),
                                  br(),
                                  tableOutput('fordelingTab'),
                                  br(),
                                  downloadButton(outputId = 'lastNed_tabFord', label='Last ned tabell')
                         )
                   ))#main
             ),
    
    
    
    
#--------------------------- Samlerapporter---------------------    
        tabPanel("Samlerapport"
        ,
        tabPanel("Fordeling av mpg",
          sidebarLayout(
            sidebarPanel(width = 3,
              selectInput(inputId = "varS",
                          label = "Variabel:",
                          c("mpg", "disp", "hp", "drat", "wt", "qsec")),
              sliderInput(inputId = "binsS",
                          label = "Antall grupper:",
                          min = 1,
                          max = 10,
                          value = 5),
              downloadButton("downloadSamlerapport", "Last ned!")
            ),
            mainPanel(
              uiOutput("samlerapport")
            )
          )
        )
      ),
    tabPanel("Abonnement"
      # ,
      # sidebarLayout(
      #   sidebarPanel(width = 3,
      #     selectInput("subscriptionRep", "Rapport:", c("Samlerapport1", "Samlerapport2")),
      #     selectInput("subscriptionFreq", "Frekvens:",
      #                 list(Årlig="year", Kvartalsvis="quarter", Månedlig="month", Ukentlig="week", Daglig="DSTday"),
      #                 selected = "month"),
      #     actionButton("subscribe", "Bestill!")
      #   ),
      #   mainPanel(
      #     uiOutput("subscriptionContent")
      #   )
      # )
    ),
    tabPanel("Veiledning",
             mainPanel(width = 12,
                       #htmlOutput("veiledning", inline = TRUE)
                       includeMarkdown('veiledning.md')
             )
    ),
    
    # Use this to place a logo to the right in the nav-bar
    tags$script(HTML("var header = $('.navbar> .container-fluid');
                       header.append('<div class=\"navbar-brand\" style=\"float:right\"><img src=\"rap/logo.svg\", alt=\"Rapporteket\", height=\"26px\"></div>');
                       console.log(header)"))

  ) # navbarPage
) # tagList
