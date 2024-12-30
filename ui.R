library(shiny)
library(shinythemes)
library(DT)
library(stringi)
#install_github("dreamRs/shinyWidgets")
library(shinyWidgets)
library(networkD3)

shinyUI(fluidPage(
    theme = shinytheme("cerulean"),
    titlePanel(div("Verlegungsketten und Kontakte in Krankenhäusern",
                   img(height = 43, width = 20, src = "white.png",class="pull-right"),
                   img(height = 43, width = 50, src = "UKM.png",class="pull-right")),
               windowTitle="TRACE"
    ),
    sidebarPanel(
      shinyjs::useShinyjs(),
      tabsetPanel(id="config",
                  tabPanel("Input",
                           br(),
                           column(4,offset = 10,actionButton('do_clear',"RESET",class = "btn-warning")),
                           br(),
                           h4("Input"),
                           radioButtons("ownData",label="",choices=c("Eigene Daten hochladen","Demo Daten laden"),
                                        selected = "Eigene Daten hochladen",inline = T),
                           br(),
                           uiOutput("inputFileUI"),
                           uiOutput("sepInputUI"),
                           br(),
                           uiOutput("inputFileUIb"),
                           uiOutput("sepInputUIb"),
                           br(),
                           actionButton('do_in',"Daten einlesen",class = "btn-primary"),
                           hr(),
                           h4(textOutput("columnUI0")),
                           tags$head(
                             tags$style(type="text/css", 
                                        "#inline label.control-label, #inline .selectize-control.single { 
         display: table-cell; 
         text-align: left; 
         vertical-align: middle; 
      } 
      #inline label.control-label {
        padding-right: 20px;
      }
      #inline .form-group { 
        display: table-row;
      }
      #inline .selectize-control.single div.item {
        padding-right: 150px;
      }")
                           ),
                           
                           tags$div(id = "inline", uiOutput("columnUI1")),
                           tags$div(id = "inline", uiOutput("columnUI2")),
                           tags$div(id = "inline", uiOutput("columnUI3")),
                           tags$div(id = "inline", uiOutput("columnUI4")),
                           tags$div(id = "inline", uiOutput("columnUI5")),
                           h6(textOutput("columnUI6")),
                           br(),
                           h4(textOutput("columnUI0b")),
                           tags$div(id = "inline", uiOutput("columnUI1b")),
                           tags$div(id = "inline", uiOutput("columnUI2b")),
                           tags$div(id = "inline", uiOutput("columnUI3b")),
                           tags$div(id = "inline", uiOutput("columnUI4b")),
                           tags$div(id = "inline", uiOutput("columnUI5b")),
                           h6(textOutput("columnUI6b")),
                           br(),
                           uiOutput("do_in2UI")
                           
                  ),
            tabPanel("Kontakte & Verlegungen",
                     radioGroupButtons("FA2_oder_Station",label = "",
                                       choices = c("Fachabteilungen","Stationen"),selected = "Fachabteilungen",
                                       status = "primary",justified = T,
                                       checkIcon = list(yes = icon("ok",
                                                                   lib = "glyphicon"))),
                     
                     br(),
                     uiOutput("FA2_UI1"),
                     uiOutput("ST2_UI1"),
                     
                     uiOutput("FA2_UI2a"),
                     uiOutput("FA2_UI2b"),
                     uiOutput("FA2_UI2c"),
                     uiOutput("ST2_UI2a"),
                     uiOutput("ST2_UI2b"),
                     uiOutput("ST2_UI2c"),
                     
                     h4(textOutput("FA_intro")),
                     h4(textOutput("ST_intro")),
                     
                     br(),
                     h3(textOutput("FA_add2a")),
                     uiOutput("FA2_UI2"),
                     uiOutput("FA2_UI3"),
                     uiOutput("FA2_UI3b"),
                     uiOutput("ST2_UI2"),
                     uiOutput("ST2_UI3"),
                     uiOutput("ST2_UI3b"),
                     br(),
                     
                     h3(textOutput("FA_add3a")),
                     fluidRow(
                       column(7,
                              uiOutput("FA_add7a")),
                       column(5,
                              uiOutput("FA_add8a"))
                     ),
                     br(),
                     uiOutput("FA2_UI4"),
                     uiOutput("FA2_UI4b"),
                     uiOutput("ST2_UI4"),
                     uiOutput("ST2_UI4b"),
                     br(),
                     uiOutput("FAST2_UI5")
            ),
            tabPanel("Zusammenfassung Verlegungen",
                     radioGroupButtons("FA3_oder_Station",label = "",
                                       choices = c("Fachabteilungen","Stationen"),selected = "Fachabteilungen",
                                       status = "primary",justified = T,
                                       checkIcon = list(yes = icon("ok",
                                                                   lib = "glyphicon"))),
                     br(),
                     
                     h4(textOutput("FA3_intro")),
                     h4(textOutput("ST3_intro")),
                     
                     uiOutput("FA3_add1a"),
                     
                     uiOutput("FA3_UI2"),
                     uiOutput("ST3_UI2"),
                     
                     uiOutput("FA3_UI2a"),
                     uiOutput("FA3_UI2b"),
                     uiOutput("FA3_UI2c"),
                     uiOutput("ST3_UI2a"),
                     uiOutput("ST3_UI2b"),
                     uiOutput("ST3_UI2c"),
                     
                     br(),
                     h3(textOutput("FA3_add2a")),
                     uiOutput("FA3_UI4"),
                     uiOutput("FA3_UI5"),
                     
                     uiOutput("ST3_UI4"),
                     uiOutput("ST3_UI5"),
                     br(),
                     uiOutput("FAST3_UI5"),
                     br(),
                     uiOutput("FAST3_UI6")
            )
        )
    ),
    mainPanel(
      shinyjs::useShinyjs(),
      tabsetPanel(id="main",
                  tabPanel("Log",
                           h3("Log"),
                           div(id = "text")
                  ),
                  tabPanel("Ergebnisse",
                    h4(htmlOutput("text_analyse1")),
                    h5(htmlOutput("text_analyse2")),
                    #plotOutput("plotBasic",width=2,height=2),
                    
                    forceNetworkOutput("force",width = 1500,height = 1500)
                  )
      )
      )
))



