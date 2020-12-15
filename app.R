source("pakker.R")
source("data.R")

options(scipen = 10000)
options(encoding = "UTF-8")


#### UI #####

ui <- dashboardPage(dashboardHeader(title = "Hovedtittel", titleWidth = 250),
                    
                    dashboardSidebar(width = 250,
                                     
                                     sidebarMenu(
                                       
                                       br(),
                                       br(),
                                       
                                       menuItem("Hovedmeny1", tabName = "hovedmeny1"),
                                       
                                       br(),
                                       
                                       menuItem("Hovedmeny2", tabName = "hovedmeny2",
                                                menuSubItem("Hovedundermeny2-1", tabName = "hovedundermeny2-1"),
                                                menuSubItem("Hovedundermeny2-2", tabName = "hovedundermeny2-2")
                                       )
                                       
                                     ),
                                     
                                     br(),
                                     br(),
                                     br(),
                                     br(),
                                     HTML('<left><img src="ssb-logo.png", width = "200"></left>')
                                     
                    ),
                    
                    dashboardBody(
                      
                      tags$head(
                        
                        tags$style(
                          HTML(".content-wrapper {overflow: auto;}",
                               
                               ".small-box.bg-green { background-color: #274247 !important; color: #e3f1e6 !important; }",
                               
                               "
                                 .multicol { 
                                   height: 900px;
                                   -webkit-column-count: 2; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 2;    /* Firefox */ 
                                   column-count: 2; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "
                          
                        )
                      ),
                      
                      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
                      
                      fluidRow(valueBoxOutput("infobox1", width = 4),
                               valueBoxOutput("infobox2", width = 4),
                               valueBoxOutput("infobox3", width = 4)
                      ),
                      
                      tabItems(
                        
                        tabItem(tabName = "hovedmeny1",
                                br(),
                                h1("Hovedside1-tittel"),
                                br(),
                                
                                fluidRow(
                                  
                                  column(width = 12, wellPanel(
                                    
                                    h2("Hovedside1-undertittel1"),
                                    br(),
                                  
                                    prettyCheckbox("rollmean_dag", 
                                                   label = "Hovedside1 Checkboxvalg"),
                                    
                                    # Grafoutput1
                                    
                                  )),
                                  
                                  br(),
                                  
                                  column(width = 12, wellPanel(
                                    
                                    h2("Hovedside1-undertittel2"),
                                    
                                    # Grafoutput2

                                    
                                  )),
                                  
                                  br(),
                                  
                                  column(width = 12,
                                         helpText(a("Fotnoter, eller annen tekst med link.",
                                                    href = "https://www.ssb.no/",
                                                    target = "blank")))
                                  
                                )),
                        
                        tabItem(tabName = "hovedundermeny2-1",
                                br(),
                                h1("Hovedunderside2-1-tittel"),
                                br(),
                                
                                fluidRow(
                                  column(width = 12,
                                         br(),
                                         h2("Hovedside2-undertittel1"),
                                         helpText("Hjelpetekst under Hovedside2-undertittel1"),
                                         br(),
                                         br())),
                                
                                fluidRow(
                                  column(width = 12,
                                         wellPanel(
                                           selectInput("varchoice1", "Velg variabel:", 
                                                       choices = c("valg1", 
                                                                   "valg2",
                                                                   "valg3",
                                                                   "valg4",
                                                                   "valg5"),
                                                       selected = "valg1")))),
                                         # Grafoutput3
                                br(),
                                
                                fluidRow(
                                  column(width = 12,
                                         
                                         helpText(a("Fotnoter, eller annen tekst med link.",
                                                    href = "https://www.ssb.no/",
                                                    target = "blank"))))
                                
                        ),
                        
                        tabItem(tabName = "hovedundermeny2-2",
                                br(),
                                h1("Hovedunderside2-2-tittel"),
                                br(),
                                
                                fluidRow(
                                  column(width = 12,
                                         br(),
                                         h2("Hovedunderside2-2-undertittel1."),
                                         helpText("Velg filter under."),
                                         br(),
                                         br())),
                                
                                fluidRow(
                                  column(width = 6,
                                         wellPanel(
                                           selectInput("varchoice2", "Velg variabel for X-akse:", 
                                                       choices = c("valg1", 
                                                                   "valg2",
                                                                   "valg3",
                                                                   "valg4",
                                                                   "valg5"),
                                                       selected = "valg1"))),
                                  
                                  column(width = 6,
                                         wellPanel(
                                           selectInput("varchoice3", "Velg variabel for Y-akse:", 
                                                       choices = c("valg1", 
                                                                   "valg2",
                                                                   "valg3",
                                                                   "valg4",
                                                                   "valg5"),
                                                       selected = "valg1")))),
                                
                                fluidRow(
                                  
                                  column(width = 1),
                                  
                                  column(width = 11,
                                         
                                         #Grafoutput 4
                                         )),
                                
                                br(),
                                
                                fluidRow(
                                  
                                  column(width = 3,
                                         wellPanel(
                                           
                                           list(h3("Filtrer ut grupper:"),
                                                tags$div(align = 'left',
                                                         class = 'multicol',
                                                         prettyCheckboxGroup("gruppevalg", "",
                                                              choices = c('gruppe1' , 'gruppe2', 'gruppe3'),
                                                              selected = c('gruppe1' , 'gruppe2', 'gruppe3')
                                                              ))))),
                                  
                                  column(width = 2,
                                         wellPanel(
                                           
                                           actionButton("checkall", label = "Velg alle grupper"),
                                           br(),
                                           br(),
                                           actionButton("uncheck", label = "Fjern alle markeringer"),
                                           br()
                                  
                                ))),
                                
                                br(),
                                
                                fluidRow(
                                  
                                  column(width = 10,
                                         
                                         helpText(a("Fotnoter, eller annen tekst med link.",
                                                    href = "https://www.ssb.no/",
                                                    target = "blank"))))
                                
                        )
                        
                      ),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
                      )
                    )
                    
)


server <- function(input, output, session){
  
  
  # For iconer sjekk ut hva som er tilgjengelig i Font Awesome og Glyphicons
  # https://shiny.rstudio.com/reference/shiny/0.14/icon.html
  # https://fontawesome.com/icons?from=io
  
  ### INFOBOKSER-OUTPUT ###
  output$infobox1 <- renderValueBox({
    valueBox(value = 1000,
             subtitle = "Infobox1 undertekst ",
             icon = icon("star"), color = "green")
  })
  
  output$infobox2 <- renderValueBox({
    valueBox(value = 2000,
             subtitle = "Infobox2 undertekst ",
             icon = icon("dice-six"), color = "green")
  })
  
  output$infobox3 <- renderValueBox({
    valueBox(value = 3000,
             subtitle = "Infobox3 undertekst ",
             icon = icon("user"), color = "green")
  })
  
  
  
  
  ### Graphoutputs ###
  # Feks ggplot eller plotly
  
  
  
  
  ### Inputreactions ###
  
  
  

  
  
  ### Observe-callbacks ###
  observe({
    if (input$uncheck > 0) {
      updatePrettyCheckboxGroup(session = session,
                                "gruppevalg",
                                choices = c('gruppe1' , 'gruppe2', 'gruppe3'),
                                selected = NULL)
    }
    
    })
  
  observe({
    if (input$checkall > 0) {
      updatePrettyCheckboxGroup(session = session,
                                "gruppevalg",
                                choices = c('gruppe1' , 'gruppe2', 'gruppe3'),
                                selected = c('gruppe1' , 'gruppe2', 'gruppe3'))
    }
    
  })
    

}

shiny::shinyApp(ui = ui, server = server)


