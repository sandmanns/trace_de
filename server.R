library(shiny)
library(shinyjs)
library(openxlsx)
library(timetools)
library(stringr)
library(igraph)
library(networkD3)
library(RColorBrewer)
library(dplyr)
library(htmlwidgets)
library(readxl)


function(input, output, session) {
  output$FA_UI2<-renderUI({NULL})
  output$FA_UI2a<-renderUI({NULL})
  output$FA_UI2b<-renderUI({NULL})
  output$FA_UI2c<-renderUI({NULL})
  output$FA_UI3<-renderUI({NULL})
  output$FA_UI3b<-renderUI({NULL})
  output$FA_UI4<-renderUI({NULL})
  output$FA_UI4b<-renderUI({NULL})
  output$FA_UI5<-renderUI({NULL})
  output$FA_UI5b<-renderUI({NULL})
  
  output$ST_UI2<-renderUI({NULL})
  output$ST_UI2a<-renderUI({NULL})
  output$ST_UI2b<-renderUI({NULL})
  output$ST_UI2c<-renderUI({NULL})
  output$ST_UI3<-renderUI({NULL})
  output$ST_UI3b<-renderUI({NULL})
  output$ST_UI4<-renderUI({NULL})
  output$ST_UI4b<-renderUI({NULL})
  output$ST_UI5<-renderUI({NULL})
  output$ST_UI5b<-renderUI({NULL})
  
  observe({
    if(input$FA2_oder_Station=="Fachabteilungen"){
      output$FA_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
      output$ST_intro<-renderText({NULL})
      
      output$FA_add2a<-renderText({NULL})
      output$FA_add3a<-renderText({NULL})
      output$FA_add7a<-renderUI({NULL})
      output$FA_add8a<-renderUI({NULL})
      
    }
    if(input$FA2_oder_Station=="Stationen"){
      output$ST_intro<-renderText({"Kein Input File mit Stationsinformationen hochgeladen."})
      output$FA_intro<-renderText({NULL})
      
      output$FA_add2a<-renderText({NULL})
      output$FA_add3a<-renderText({NULL})
      output$FA_add7a<-renderUI({NULL})
      output$FA_add8a<-renderUI({NULL})
    }
  })
  
  observe({
    if(input$FA3_oder_Station=="Fachabteilungen"){
      output$FA3_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
      output$ST3_intro<-renderText({NULL})
      
      output$FA3_add1a<-renderUI({NULL})
      output$FA3_add2a<-renderText({NULL})
      
    }
    if(input$FA3_oder_Station=="Stationen"){
      output$ST3_intro<-renderText({"Kein Input File mit Stationsinformationen hochgeladen."})
      output$FA3_intro<-renderText({NULL})
      
      output$FA3_add1a<-renderUI({NULL})
      output$FA3_add2a<-renderText({NULL})
    }
  })
  
  output$inputFileUI<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      fileInput('inputFile',label = "Datei hochladen (Fachabteilungen)")
    }else{
      NULL
    }
  })
  output$inputFileUIb<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      fileInput('inputFileb',label = "Datei hochladen (Stationen)")
    }else{
      NULL
    }
  })
  
  output$sepInputUI<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      radioButtons('sepInput',label = "Trennzeichen",
                   choices = c("Komma","Semikolon","Tab"),selected = character(0),
                   inline = T)
    }else{
      NULL
    }
  })
  output$sepInputUIb<-renderUI({
    if(input$ownData=="Eigene Daten hochladen"){
      radioButtons('sepInputb',label = "Trennzeichen",
                   choices = c("Komma","Semikolon","Tab"),selected = character(0),
                   inline = T)
    }else{
      NULL
    }
  })
  
  
  observeEvent(input$do_clear,{
    session$reload()
    
  })
  
  
  observeEvent(input$do_in,{
    if(input$ownData=="Demo Daten laden"){
      shinyjs::html("text", paste0("<br>Demo Files werden eingelesen.<br><br>"), add = FALSE)
      input1a<-read.table("www/Fachabteilung_V2.txt",sep="\t",header=T)
      input1b<-read.table("www/Station_V2.txt",sep="\t",header=T)
      input_temp<-T
      input_tempb<-T
      shinyjs::html("text", paste0("Input Files erfolgreich eingelesen.","<br>"), add = TRUE)  
    }else{
      shinyjs::html("text", paste0("<br>Input Files werden eingelesen.<br><br>"), add = FALSE)
      input_temp<-input$inputFile
      input_tempb<-input$inputFileb
      if(is.null(input_temp)&&is.null(input_tempb)){
        shinyjs::html("text", paste0("ERROR: Keine Input Files definiert.","<br>"), add = TRUE) 
        return()
      }
      if(!is.null(input_temp)&&is.null(input$sepInput)){
        shinyjs::html("text", paste0("ERROR: Kein Trennzeichen für Fachabteilungs-Datei definiert.","<br>"), add = TRUE) 
        return()
      }
      if(!is.null(input_tempb)&&is.null(input$sepInputb)){
        shinyjs::html("text", paste0("ERROR: Kein Trennzeichen für Stations-Datei definiert.","<br>"), add = TRUE) 
        return()
      }
      
      if(!is.null(input_temp)){
        if(input$sepInput=="Komma"){
          input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
        }
        if(input$sepInput=="Semikolon"){
          input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
        }
        if(input$sepInput=="Tab"){
          input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
        }        
      }
      if(!is.null(input_tempb)){
        if(input$sepInputb=="Komma"){
          input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
        }
        if(input$sepInputb=="Semikolon"){
          input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
        }
        if(input$sepInputb=="Tab"){
          input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
        }        
      }
      if(!is.null(input_temp)&&!is.null(input_tempb)){
        shinyjs::html("text", paste0("Input Files erfolgreich eingelesen.","<br>"), add = TRUE)        
      }else{
        shinyjs::html("text", paste0("Input File erfolgreich eingelesen.","<br>"), add = TRUE)
      }
    }
  
      ##Fachabteilungen
    if(!is.null(input_temp)){
      output$columnUI0<-renderText({"Fachabteilungen: Wählen Sie die Spalte, die Informationen enthält zu..."})
      
      output$columnUI1<-renderUI({
        selectInput('column1',label = HTML("...Fallnummer:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1a))
      })
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI2<-renderUI({
          selectInput('column2',label = HTML("...Fachabteilung:&nbsp&nbsp"),
                      choices = names(input1a),selected = "Fachabteilung")
        })
      }else{
        output$columnUI2<-renderUI({
          selectInput('column2',label = HTML("...Fachabteilung:&nbsp&nbsp"),
                      choices = names(input1a))
        })
      }
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI3<-renderUI({
          selectInput('column3',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                      choices = names(input1a),selected="Aufnahme")
        })
      }else{
        output$columnUI3<-renderUI({
          selectInput('column3',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                      choices = names(input1a))
        })
      }
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI4<-renderUI({
          selectInput('column4',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                      choices = names(input1a),selected="Entlassung")
        })
      }else{
        output$columnUI4<-renderUI({
          selectInput('column4',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1a))
        })
      }
      
      if(input$ownData=="Demo Daten laden"){
        output$columnUI5<-renderUI({
          selectInput('column5',label = HTML("...Hauptdiagnose:&nbsp"),
                      choices = names(input1a),selected="Hauptdiagnose")
        })
      }else{
        output$columnUI5<-renderUI({
          selectInput('column5',label = HTML("...Hauptdiagnose:&nbsp"),choices = names(input1a))
        })
      }
      
      output$columnUI6<-renderText({"Hinweise:\n Die Zeit der Aufnahme und Entlassung ist zu kodieren als JJJJ-MM-TT Std:Min:Sek.\n
      Die Hauptdiagnose ist über ICD-Codes zu definieren. Es wird die Angabe 
    eines konkreten Codes erwartet, nicht nur die eines Kapitels."}) 
    }
    
      ##Stationen
      if(!is.null(input_tempb)){
          output$columnUI0b<-renderText({"Stationen: Wählen Sie die Spalte, die Informationen enthält zu..."})
          
          output$columnUI1b<-renderUI({
              selectInput('column1b',label = HTML("...Fallnummer:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1b))
          })
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI2b<-renderUI({
                  selectInput('column2b',label = HTML("...Station:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b),selected = "Station")
              })
          }else{
              output$columnUI2b<-renderUI({
                  selectInput('column2b',label = HTML("...Station:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b))
              })
          }
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI3b<-renderUI({
                  selectInput('column3b',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b),selected="Aufnahme")
              })
          }else{
              output$columnUI3b<-renderUI({
                  selectInput('column3b',label = HTML("...Aufnahme:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b))
              })
          }
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI4b<-renderUI({
                  selectInput('column4b',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),
                              choices = names(input1b),selected="Entlassung")
              })
          }else{
              output$columnUI4b<-renderUI({
                  selectInput('column4b',label = HTML("...Entlassung:&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp&nbsp"),choices = names(input1b))
              })
          }
          
          if(input$ownData=="Demo Daten laden"){
              output$columnUI5b<-renderUI({
                  selectInput('column5b',label = HTML("...Hauptdiagnose:&nbsp"),
                              choices = names(input1b),selected="Hauptdiagnose")
              })
          }else{
              output$columnUI5b<-renderUI({
                  selectInput('column5b',label = HTML("...Hauptdiagnose:&nbsp"),choices = names(input1b))
              })
          }
          
          output$columnUI6b<-renderText({"Hinweise:\n Die Zeit der Aufnahme und Entlassung ist zu kodieren als JJJJ-MM-TT Std:Min:Sek.\n
      Die Hauptdiagnose ist über ICD-Codes zu definieren. Es wird die Angabe 
    eines konkreten Codes erwartet, nicht nur die eines Kapitels."}) 
      }

  
  output$do_in2UI<-renderUI({
    actionButton('do_in2',"Input konfigurieren",class = "btn-primary")
  })

    
  
  observeEvent(input$do_in2,{
    shinyjs::html("text", paste0("<br>Input Files werden konfiguriert.<br><br>"), add = FALSE)
    
      if(input$ownData=="Demo Daten laden"){
          input1a<-read.table("www/Fachabteilung_V2.txt",sep="\t",header=T)
          input1b<-read.table("www/Station_V2.txt",sep="\t",header=T)
          input_temp<-NULL
          input_tempb<-NULL
      }else{
          input_temp<-input$inputFile
          input_tempb<-input$inputFileb
          input1a<-NULL
          input1b<-NULL
          
          if(!is.null(input_temp)){
              if(input$sepInput=="Komma"){
                  input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
              }
              if(input$sepInput=="Semikolon"){
                  input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
              }
              if(input$sepInput=="Tab"){
                  input1a<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
              }        
          }
          if(!is.null(input_tempb)){
              if(input$sepInputb=="Komma"){
                  input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=",",stringsAsFactors = F)      
              }
              if(input$sepInputb=="Semikolon"){
                  input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep=";",stringsAsFactors = F)      
              }
              if(input$sepInputb=="Tab"){
                  input1b<-read.table(input_temp$datapath,header=T,quote = "",comment.char = "",sep="\t",stringsAsFactors = F)    
              }        
          }
      } 
      
    icd_ref<-data.frame(Kapitel=c(1,1,2,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,19,20,20,20,20,21,22),
                        Buchstabe=c("A","B","C","D","D","E","F","G","H","H","I","J","K","L","M","N","O","P","Q","R","S","T","V","W","X","Y","Z","U"),
                        Start=c(0,0,0,0,50,0,0,0,0,60,0,0,0,0,0,0,0,0,0,0,0,0,1,49,19,9,0,0),
                        Ende=c(99,99,97,48,90,90,99,99,59,95,99,99,93,99,99,99,99,96,99,99,99,98,99,94,84,84,99,99))
    
      if(!is.null(input1a)){
        spalten<-c(input$column1,input$column2,input$column3,input$column4,input$column5)
        spalten_table<-table(spalten)
        if(sum(as.numeric(spalten_table)>1)>0){
          shinyjs::html("text", paste0("ERROR: Spalten ",names(spalten_table)[which(as.numeric(spalten_table)>1)],
                                       " mehr als 1x für Input Fachabteilungen gewählt.","<br>"), add = TRUE) 
          return()
        }
        
        input_neu<-data.frame(Patientennummer=input1a[,names(input1a)==input$column1],
                              Fallnummer=input1a[,names(input1a)==input$column1],
                              Abteilung=input1a[,names(input1a)==input$column2],
                              Aufnahme=input1a[,names(input1a)==input$column3],
                              Entlassung=input1a[,names(input1a)==input$column4],
                              ICDfull=input1a[,names(input1a)==input$column5])
        input1a<-input_neu
        
        helper<-data.frame(Buchstabe=substring(input1a$ICDfull,1,1),
                           Zahl=as.numeric(substring(input1a$ICDfull,2,3)),
                           ICDfull=input1a$ICDfull)
        result <- helper %>%
          inner_join(icd_ref, by = "Buchstabe",relationship="many-to-many") %>%
          filter(Zahl >= Start & Zahl <= Ende) %>%
          select(ICDfull, Kapitel) 
        input1a$ICDKapitel<-result$Kapitel
        
        input1a$Aufnahme<-as.POSIXct(input1a$Aufnahme,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
        input1a$Entlassung<-as.POSIXct(input1a$Entlassung,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      }

    if(!is.null(input1b)){
      spalten<-c(input$column1b,input$column2b,input$column3b,input$column4b,input$column5b)
      spalten_table<-table(spalten)
      if(sum(as.numeric(spalten_table)>1)>0){
        shinyjs::html("text", paste0("ERROR: Spalten ",names(spalten_table)[which(as.numeric(spalten_table)>1)],
                                     " mehr als 1x für Input Stationen gewählt.","<br>"), add = TRUE) 
        return()
      } 
      
      input_neu<-data.frame(Patientennummer=input1b[,names(input1b)==input$column1b],
                            Fallnummer=input1b[,names(input1b)==input$column1b],
                            Abteilung=input1b[,names(input1b)==input$column2b],
                            Aufnahme=input1b[,names(input1b)==input$column3b],
                            Entlassung=input1b[,names(input1b)==input$column4b],
                            ICDfull=input1b[,names(input1b)==input$column5b])
      input1b<-input_neu
      
      helper<-data.frame(Buchstabe=substring(input1b$ICDfull,1,1),
                         Zahl=as.numeric(substring(input1b$ICDfull,2,3)),
                         ICDfull=input1b$ICDfull)
      result <- helper %>%
        inner_join(icd_ref, by = "Buchstabe",relationship="many-to-many") %>%
        filter(Zahl >= Start & Zahl <= Ende) %>%
        select(ICDfull, Kapitel) 
      input1b$ICDKapitel<-result$Kapitel
      
      input1b$Aufnahme<-as.POSIXct(input1b$Aufnahme,format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      input1b$Entlassung<-as.POSIXct(input1b$Entlassung,format="%Y-%m-%d %H:%M:%OS",tz="UTC") 
    }

    shinyjs::html("text", paste0("<br>Input Files erfolgreich konfiguriert.<br><br>"), add = TRUE)
    

    
    
    ##################################################
    ##2. Reiter: Superspreader
    output$FA2_UI1<-renderUI({NULL})
    output$FA2_UI2<-renderUI({NULL})
    output$FA2_UI2a<-renderUI({NULL})
    output$FA2_UI2b<-renderUI({NULL})
    output$FA2_UI2c<-renderUI({NULL})
    output$FA2_UI3<-renderUI({NULL})
    output$FA2_UI3b<-renderUI({NULL})
    output$FA2_UI4<-renderUI({NULL})
    output$FA2_UI4b<-renderUI({NULL})
    
    output$ST2_UI1<-renderUI({NULL})
    output$ST2_UI2<-renderUI({NULL})
    output$ST2_UI2a<-renderUI({NULL})
    output$ST2_UI2b<-renderUI({NULL})
    output$ST2_UI2c<-renderUI({NULL})
    output$ST2_UI3<-renderUI({NULL})
    output$ST2_UI3b<-renderUI({NULL})
    output$ST2_UI4<-renderUI({NULL})
    output$ST2_UI4b<-renderUI({NULL})
    
    
    observe({
      if(input$FA2_oder_Station=="Fachabteilungen"&&!is.null(input1a)){
        output$FA2_UI1<-renderUI({NULL})
        output$FA2_UI2<-renderUI({NULL})
        output$FA2_UI2a<-renderUI({NULL})
        output$FA2_UI2b<-renderUI({NULL})
        output$FA2_UI2c<-renderUI({NULL})
        output$FA2_UI3<-renderUI({NULL})
        output$FA2_UI3b<-renderUI({NULL})
        output$FA2_UI4<-renderUI({NULL})
        output$FA2_UI4b<-renderUI({NULL})
        output$FA_intro<-renderText({NULL})
        
        output$ST2_UI1<-renderUI({NULL})
        output$ST2_UI2<-renderUI({NULL})
        output$ST2_UI2a<-renderUI({NULL})
        output$ST2_UI2b<-renderUI({NULL})
        output$ST2_UI2c<-renderUI({NULL})
        output$ST2_UI3<-renderUI({NULL})
        output$ST2_UI3b<-renderUI({NULL})
        output$ST2_UI4<-renderUI({NULL})
        output$ST2_UI4b<-renderUI({NULL})
        output$ST_intro<-renderText({NULL})
        
        output$FA_add2a<-renderText({"Auswahl Beobachtungszeitraum"})
        output$FA_add3a<-renderText({"Kontaktanalyse"})

        output$FA_add7a<-renderUI({pickerInput("Min_Kontakt_Tag2",label = HTML("Minimale Kontaktzeit:&nbsp;&nbsp;&nbsp;"),
                                               choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),selected = "0 Tage",
                                               options = list(title="Tage",size=5),inline = T,width = "fit")})
        output$FA_add8a<-renderUI({pickerInput("Min_Kontakt_Stunde2",label = "",
                                               choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),selected = "1 Stunde",
                                               options = list(title="Stunden",size=5),inline = T,width = "fit")})

        #input1<-readxl::read_excel("www/Verlegungshistorien_2023_2024-03-04_01.xlsx",sheet=2)
        input1<-input1a
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer),]
        fa<-sort(unique(input1$Abteilung))
        
        output$FA2_UI1<-renderUI({
          pickerInput('FA2_Fokus',label = "Analyse der Fachabteilungen",choices = fa, 
                      options = list(`actions-box` = TRUE,
                                     `deselect-all-text` = "Auswahl aufheben",
                                     `select-all-text` = "Alle auswählen",
                                     `none-selected-text` = "Nichts ausgewählt"),multiple=TRUE)
        })
        output$ST2_UI1<-renderUI({NULL})
        
        observe({
          if(!is.null(input$FA2_Fokus)){
            #     if(length(input$FA2_Fokus)>1){
            #      output$FA2_UI4b<-renderUI({h5(paste0("Werden >10,000 Kontakte detektiert, wird die Darstellung von Kontakten innnerhalb 
            #der jeweiligen Fachabteilungen aus Gründen der Übersichtlichkeit automatisch deaktiviert. 
            # Dies betrifft jedoch nur die Darstellung, nicht die Analyse im Hintergrund."))})              
            #    }else{
            output$FA2_UI4b<-renderUI({NULL})
            #   }
            
            fa2_fokus<-as.character(input$FA2_Fokus)
            input1_fa<-input1[input1$Abteilung%in%fa2_fokus,]
            
            output$FA2_UI2a<-renderUI({
              prettyRadioButtons(inputId = "switch0",label = "Hauptdiagnose nach ICD-Code filtern?",
                                 choices = c("nein","ICD-Kapitel","exakter ICD-Code"),selected = "nein",
                                 inline=T,outline = T,bigger = T,
                                 status = "primary",
                                 icon=icon("check")
              )
            })
            
            observe({
              if(!is.null(input$switch0)){
                if(input$switch0=="nein"){
                  output$FA2_UI2b<-renderUI({NULL})
                  output$FA2_UI2c<-renderUI({NULL})
                  
                  output$FA2_UI2<-renderUI({dateInput("FA2_Start","Beginn des Beobachtungszeitraums",
                                                      value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                      weekstart = 1,language = "de",format="yyyy-mm-dd")})
                  output$ST2_UI2<-renderUI({NULL})
                  
                  if(length(fa2_fokus)==1){
                    output$FA2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Fachabteilung können Zeiträume zwischen ",
                                                         as.Date(min(input1_fa$Aufnahme)),
                                                         " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                         "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                         as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                         " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                  }else{
                    output$FA2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählten Fachabteilungen können Zeiträume zwischen ",
                                                         as.Date(min(input1_fa$Aufnahme)),
                                                         " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                         "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                         as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                         " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                  }
                  output$ST2_UI3<-renderUI({NULL})
                  
                  observe({
                    if(!is.null(input$FA2_Start)){
                      if((input$FA2_Start+30)<=as.Date(max(input1_fa$Entlassung))){
                        output$FA2_UI3<-renderUI({dateInput("FA2_Ende","Ende des Beobachtungszeitraums",
                                                            value=input$FA2_Start+30,
                                                            min = input$FA2_Start,
                                                            max = as.Date(max(input1_fa$Entlassung)),
                                                            weekstart = 1,language = "de",format="yyyy-mm-dd")})                            
                      }else{
                        output$FA2_UI3<-renderUI({dateInput("FA2_Ende","Ende des Beobachtungszeitraums",
                                                            value=as.Date(max(input1_fa$Entlassung)),
                                                            min = input$FA2_Start,
                                                            max = as.Date(max(input1_fa$Entlassung)),
                                                            weekstart = 1,language = "de",format="yyyy-mm-dd")})
                      }
                      
                      output$ST2_UI3b<-renderUI({NULL})
                    }
                  })
                  
                }else{
                  if(input$switch0=="ICD-Kapitel"){
                    output$FA2_UI2b<-renderUI({
                      pickerInput('FA2_ICDKapitel',
                                  choices = c(1:22), 
                                  choicesOpt = list(
                                    content = c(HTML('<b>I</b>: A00-B99'),
                                                HTML('<b>II</b>: C00-D48'),
                                                HTML('<b>III</b>: D50-D90'),
                                                HTML('<b>IV</b>: E00-E90'),
                                                HTML('<b>V</b>: F00-F99'),
                                                HTML('<b>VI</b>: G00-G99'),
                                                HTML('<b>VII</b>: H00-H59'),
                                                HTML('<b>VIII</b>: H60-H95'),
                                                HTML('<b>IX</b>: I00-I99'),
                                                HTML('<b>X</b>: J00-J99'),
                                                HTML('<b>XI</b>: K00-K93'),
                                                HTML('<b>XII</b>: L00-L99'),
                                                HTML('<b>XIII</b>: M00-M99'),
                                                HTML('<b>XIV</b>: N00-N99'),
                                                HTML('<b>XV</b>: O00-O99'),
                                                HTML('<b>XVI</b>: P00-P96'),
                                                HTML('<b>XVII</b>: Q00-Q99'),
                                                HTML('<b>XVIII</b>: R00-R99'),
                                                HTML('<b>XIX</b>: S00-T98'),
                                                HTML('<b>XX</b>: V01-Y84'),
                                                HTML('<b>XXI</b>: Z00-Z99'),
                                                HTML('<b>XXII</b>: U00-U99'))
                                  ),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Auswahl aufheben",
                                                 `select-all-text` = "Alle auswählen",
                                                 `none-selected-text` = "Nichts ausgewählt",
                                                 sanitize=FALSE),multiple=TRUE)
                    })
                    output$FA2_UI2c<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$FA2_ICDKapitel)){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$FA2_ICDKapitel,]
                        
                        if(nrow(input1_fa)>0){
                          output$FA2_UI2<-renderUI({dateInput("FA2_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$ST2_UI2<-renderUI({NULL})
                          output$FA2_UI3<-renderUI({NULL})
                          output$ST2_UI3<-renderUI({NULL})
                          
                          if(length(fa2_fokus)==1){
                            output$FA2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Fachabteilung und den ICD-Filter können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                          }else{
                            output$FA2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählten Fachabteilungen und den ICD-Filter können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})   
                          }
                          output$ST2_UI3<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$FA2_Start)){
                              if((input$FA2_Start+30)<=as.Date(max(input1_fa$Entlassung))){
                                output$FA2_UI3<-renderUI({dateInput("FA2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=input$FA2_Start+30,
                                                                    min = input$FA2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})                            
                              }else{
                                output$FA2_UI3<-renderUI({dateInput("FA2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=as.Date(max(input1_fa$Entlassung)),
                                                                    min = input$FA2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})
                              }
                              
                              output$ST2_UI3b<-renderUI({NULL})
                            }
                          })
                        }else{
                          if(length(fa2_fokus)==1){
                            output$FA2_UI3b<-renderUI({h5(paste0("Für die ausgewählte Fachabteilung gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Fachabteilung und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$FA2_UI2<-renderUI({NULL})
                            output$FA2_UI3<-renderUI({NULL})
                          }else{
                            output$FA2_UI3b<-renderUI({h5(paste0("Für die ausgewählten Fachabteilungen gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie andere Fachabteilungen und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$FA2_UI2<-renderUI({NULL})
                            output$FA2_UI3<-renderUI({NULL})
                          }
                          output$ST2_UI3<-renderUI({NULL})
                        }
                        
                        
                      }else{
                        output$FA2_UI2<-renderUI({NULL})
                        output$FA2_UI3<-renderUI({NULL})
                        output$FA2_UI3b<-renderUI({NULL})
                      }
                    })
                  }else{
                    output$FA2_UI2c<-renderUI({
                      textInput("FA2_ICDExakt",value = NULL,placeholder = "z.B. A00 oder A00.0",label = NULL)
                    })
                    output$FA2_UI2b<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$FA2_ICDExakt)&&input$FA2_ICDExakt!=""&&nchar(input$FA2_ICDExakt)>=3){
                        user_icd<-gsub(".","\\.",input$FA2_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                        
                        if(nrow(input1_fa)>0){
                          output$FA2_UI2<-renderUI({dateInput("FA2_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$ST2_UI2<-renderUI({NULL})
                          output$FA2_UI3<-renderUI({NULL})
                          output$ST2_UI3<-renderUI({NULL})
                          
                          if(length(fa2_fokus)==1){
                            output$FA2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Fachabteilung und den ICD-Filter können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                          }else{
                            output$FA2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählten Fachabteilungen und den ICD-Filter können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})   
                          }
                          output$ST2_UI3<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$FA2_Start)){
                              if((input$FA2_Start+30)<=as.Date(max(input1_fa$Entlassung))){
                                output$FA2_UI3<-renderUI({dateInput("FA2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=input$FA2_Start+30,
                                                                    min = input$FA2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})                            
                              }else{
                                output$FA2_UI3<-renderUI({dateInput("FA2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=as.Date(max(input1_fa$Entlassung)),
                                                                    min = input$FA2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})
                              }
                              
                              output$ST2_UI3b<-renderUI({NULL})
                            }
                          })
                        }else{
                          if(length(fa2_fokus)==1){
                            output$FA2_UI3b<-renderUI({h5(paste0("Für die ausgewählte Fachabteilung gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Fachabteilung und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$FA2_UI2<-renderUI({NULL})
                            output$FA2_UI3<-renderUI({NULL})
                          }else{
                            output$FA2_UI3b<-renderUI({h5(paste0("Für die ausgewählten Fachabteilungen gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie andere Fachabteilungen und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$FA2_UI2<-renderUI({NULL})
                            output$FA2_UI3<-renderUI({NULL})
                          }
                          output$ST2_UI3<-renderUI({NULL})
                          output$ST2_UI3b<-renderUI({NULL})
                          output$ST2_UI2<-renderUI({NULL})
                        }
                      }else{
                        output$FA2_UI2<-renderUI({NULL})
                        output$FA2_UI3<-renderUI({NULL})
                        output$FA2_UI3b<-renderUI({NULL})
                      }
                    })
                  }
                }
                
                
              }
              
              
              
            })
          }else{
            output$FA2_UI2a<-renderUI({NULL})
            output$FA2_UI2b<-renderUI({NULL})
            output$FA2_UI2c<-renderUI({NULL})
            output$FA2_UI3<-renderUI({NULL})
            output$FA2_UI3b<-renderUI({NULL})
            output$FA2_UI4b<-renderUI({NULL})
            
            output$ST2_UI2a<-renderUI({NULL})
            output$ST2_UI2b<-renderUI({NULL})
            output$ST2_UI2c<-renderUI({NULL})
            output$ST2_UI3<-renderUI({NULL})
            output$ST2_UI3b<-renderUI({NULL})
            output$ST2_UI4b<-renderUI({NULL})
          }     
        })
        
        output$FA2_UI4<-renderUI({
          #materialSwitch("FA2_komplex","Darstellung von Kontakten innerhalb der Fachabteilungen?",
          #               value = FALSE,status = "primary")
          NULL
        })
        
        output$ST2_UI4<-renderUI({NULL})
        output$ST2_UI4b<-renderUI({NULL})
        
      }
      if(input$FA2_oder_Station=="Fachabteilungen"&&is.null(input1a)){
        output$FA2_UI1<-renderUI({NULL})
        output$FA2_UI2<-renderUI({NULL})
        output$FA2_UI2a<-renderUI({NULL})
        output$FA2_UI2b<-renderUI({NULL})
        output$FA2_UI2c<-renderUI({NULL})
        output$FA2_UI3<-renderUI({NULL})
        output$FA2_UI3b<-renderUI({NULL})
        output$FA2_UI4<-renderUI({NULL})
        output$FA2_UI4b<-renderUI({NULL})
        output$FA_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
        
        output$ST2_UI1<-renderUI({NULL})
        output$ST2_UI2<-renderUI({NULL})
        output$ST2_UI2a<-renderUI({NULL})
        output$ST2_UI2b<-renderUI({NULL})
        output$ST2_UI2c<-renderUI({NULL})
        output$ST2_UI3<-renderUI({NULL})
        output$ST2_UI3b<-renderUI({NULL})
        output$ST2_UI4<-renderUI({NULL})
        output$ST2_UI4b<-renderUI({NULL})
        output$ST_intro<-renderText({NULL})
        
        output$FA_add2a<-renderText({NULL})
        output$FA_add3a<-renderText({NULL})
        output$FA_add7a<-renderUI({NULL})
        output$FA_add8a<-renderUI({NULL})
      }
    })
    
    observe({
      if(input$FA2_oder_Station=="Stationen"&&!is.null(input1b)){
        output$FA2_UI1<-renderUI({NULL})
        output$FA2_UI2<-renderUI({NULL})
        output$FA2_UI2a<-renderUI({NULL})
        output$FA2_UI2b<-renderUI({NULL})
        output$FA2_UI2c<-renderUI({NULL})
        output$FA2_UI3<-renderUI({NULL})
        output$FA2_UI3b<-renderUI({NULL})
        output$FA2_UI4<-renderUI({NULL})
        output$FA2_UI4b<-renderUI({NULL})
        output$FA_intro<-renderText({NULL})
        
        output$ST2_UI1<-renderUI({NULL})
        output$ST2_UI2<-renderUI({NULL})
        output$ST2_UI2a<-renderUI({NULL})
        output$ST2_UI2b<-renderUI({NULL})
        output$ST2_UI2c<-renderUI({NULL})
        output$ST2_UI3<-renderUI({NULL})
        output$ST2_UI3b<-renderUI({NULL})
        output$ST2_UI4<-renderUI({NULL})
        output$ST2_UI4b<-renderUI({NULL})
        output$ST_intro<-renderText({NULL})
        
        output$FA_add2a<-renderText({"Auswahl Beobachtungszeitraum"})
        output$FA_add3a<-renderText({"Kontaktanalyse"})
        
        output$FA_add7a<-renderUI({pickerInput("Min_Kontakt_Tag2",label = HTML("Minimale Kontaktzeit:&nbsp;&nbsp;&nbsp;"),
                                               choices = c("0 Tage","1 Tag",paste0(c(2:30)," Tage")),selected = "0 Tage",
                                               options = list(title="Tage",size=5),inline = T,width = "fit")})
        output$FA_add8a<-renderUI({pickerInput("Min_Kontakt_Stunde2",label = "",
                                               choices = c("0 Stunden","1 Stunde",paste0(c(2:23)," Stunden")),selected = "1 Stunde",
                                               options = list(title="Stunden",size=5),inline = T,width = "fit")})
        
        #input1<-readxl::read_excel("www/Verlegungshistorien_2023_2024-03-04_01.xlsx",sheet=2)
        input1<-input1b
        names(input1)[3]<-"Abteilung"
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer),]
        fa<-sort(unique(input1$Abteilung))
        
        output$ST2_UI1<-renderUI({
          pickerInput('ST2_Fokus',label = "Analyse der Stationen",choices = fa, 
                      options = list(`actions-box` = TRUE,
                                     `deselect-all-text` = "Auswahl aufheben",
                                     `select-all-text` = "Alle auswählen",
                                     `none-selected-text` = "Nichts ausgewählt"),multiple=TRUE)
        })
        output$FA2_UI1<-renderUI({NULL})
        
        observe({
          if(!is.null(input$ST2_Fokus)){
            #     if(length(input$ST2_Fokus)>1){
            #        output$ST2_UI4b<-renderUI({h5(paste0("Werden >10,000 Kontakte detektiert, wird die Darstellung von Kontakten innnerhalb 
            #  der jeweiligen Stationen aus Gründen der Übersichtlichkeit automatisch deaktiviert. 
            #   Dies betrifft jedoch nur die Darstellung, nicht die Analyse im Hintergrund."))})              
            #      }else{
            output$ST2_UI4b<-renderUI({NULL})
            #     }
            
            fa2_fokus<-as.character(input$ST2_Fokus)
            input1_fa<-input1[input1$Abteilung%in%fa2_fokus,]
            
            output$ST2_UI2a<-renderUI({
              prettyRadioButtons(inputId = "switch0c",label = "Hauptdiagnose nach ICD-Code filtern?",
                                 choices = c("nein","ICD-Kapitel","exakter ICD-Code"),selected = "nein",
                                 inline=T,outline = T,bigger = T,
                                 status = "primary",
                                 icon=icon("check")
              )
            })
            
            observe({
              if(!is.null(input$switch0c)){
                if(input$switch0c=="nein"){
                  output$ST2_UI2b<-renderUI({NULL})
                  output$ST2_UI2c<-renderUI({NULL})
                  
                  output$ST2_UI2<-renderUI({dateInput("ST2_Start","Beginn des Beobachtungszeitraums",
                                                      value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                      weekstart = 1,language = "de",format="yyyy-mm-dd")})
                  output$FA2_UI2<-renderUI({NULL})
                  
                  if(length(fa2_fokus)==1){
                    output$ST2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Station können Zeiträume zwischen ",
                                                         as.Date(min(input1_fa$Aufnahme)),
                                                         " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                         "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                         as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                         " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                  }else{
                    output$ST2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählten Stationen können Zeiträume zwischen ",
                                                         as.Date(min(input1_fa$Aufnahme)),
                                                         " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                         "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                         as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                         " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                  }
                  output$FA2_UI3b<-renderUI({NULL})
                  
                  observe({
                    if(!is.null(input$ST2_Start)){
                      if((input$ST2_Start+30)<=as.Date(max(input1_fa$Entlassung))){
                        output$ST2_UI3<-renderUI({dateInput("ST2_Ende","Ende des Beobachtungszeitraums",
                                                            value=input$ST2_Start+30,
                                                            min = input$ST2_Start,
                                                            max = as.Date(max(input1_fa$Entlassung)),
                                                            weekstart = 1,language = "de",format="yyyy-mm-dd")})                            
                      }else{
                        output$ST2_UI3<-renderUI({dateInput("ST2_Ende","Ende des Beobachtungszeitraums",
                                                            value=as.Date(max(input1_fa$Entlassung)),
                                                            min = input$ST2_Start,
                                                            max = as.Date(max(input1_fa$Entlassung)),
                                                            weekstart = 1,language = "de",format="yyyy-mm-dd")})
                      }
                      output$FA2_UI3<-renderUI({NULL})
                    }
                  })
                }else{
                  if(input$switch0c=="ICD-Kapitel"){
                    output$ST2_UI2b<-renderUI({
                      pickerInput('ST2_ICDKapitel',
                                  choices = c(1:22), 
                                  choicesOpt = list(
                                    content = c(HTML('<b>I</b>: A00-B99'),
                                                HTML('<b>II</b>: C00-D48'),
                                                HTML('<b>III</b>: D50-D90'),
                                                HTML('<b>IV</b>: E00-E90'),
                                                HTML('<b>V</b>: F00-F99'),
                                                HTML('<b>VI</b>: G00-G99'),
                                                HTML('<b>VII</b>: H00-H59'),
                                                HTML('<b>VIII</b>: H60-H95'),
                                                HTML('<b>IX</b>: I00-I99'),
                                                HTML('<b>X</b>: J00-J99'),
                                                HTML('<b>XI</b>: K00-K93'),
                                                HTML('<b>XII</b>: L00-L99'),
                                                HTML('<b>XIII</b>: M00-M99'),
                                                HTML('<b>XIV</b>: N00-N99'),
                                                HTML('<b>XV</b>: O00-O99'),
                                                HTML('<b>XVI</b>: P00-P96'),
                                                HTML('<b>XVII</b>: Q00-Q99'),
                                                HTML('<b>XVIII</b>: R00-R99'),
                                                HTML('<b>XIX</b>: S00-T98'),
                                                HTML('<b>XX</b>: V01-Y84'),
                                                HTML('<b>XXI</b>: Z00-Z99'),
                                                HTML('<b>XXII</b>: U00-U99'))
                                  ),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Auswahl aufheben",
                                                 `select-all-text` = "Alle auswählen",
                                                 `none-selected-text` = "Nichts ausgewählt",
                                                 sanitize=FALSE),multiple=TRUE)
                    })
                    output$ST2_UI2c<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$ST2_ICDKapitel)){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$ST2_ICDKapitel,]
                        if(nrow(input1_fa)>0){
                          output$ST2_UI2<-renderUI({dateInput("ST2_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$FA2_UI2<-renderUI({NULL})
                          output$FA2_UI3<-renderUI({NULL})
                          output$ST2_UI3<-renderUI({NULL})
                          
                          if(length(fa2_fokus)==1){
                            output$ST2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Station können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                          }else{
                            output$ST2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählten Stationen können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                          }
                          output$FA2_UI3<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$ST2_Start)){
                              if((input$ST2_Start+30)<=as.Date(max(input1_fa$Entlassung))){
                                output$ST2_UI3<-renderUI({dateInput("ST2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=input$ST2_Start+30,
                                                                    min = input$ST2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})                            
                              }else{
                                output$ST2_UI3<-renderUI({dateInput("ST2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=as.Date(max(input1_fa$Entlassung)),
                                                                    min = input$ST2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})
                              }
                              output$FA2_UI3<-renderUI({NULL})
                            }
                          })
                        }else{
                          if(length(fa2_fokus)==1){
                            output$ST2_UI3b<-renderUI({h5(paste0("Für die ausgewählte Station gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Station und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$ST2_UI2<-renderUI({NULL})
                            output$ST2_UI3<-renderUI({NULL})
                          }else{
                            output$ST2_UI3b<-renderUI({h5(paste0("Für die ausgewählten Stationen gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie andere Stationen und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$ST2_UI2<-renderUI({NULL})
                            output$ST2_UI3<-renderUI({NULL})
                          }
                          output$ST2_UI3<-renderUI({NULL})
                        }
                      }else{
                        output$ST2_UI2<-renderUI({NULL})
                        output$ST2_UI3<-renderUI({NULL})
                        output$ST2_UI3b<-renderUI({NULL})
                      }
                    })
                  }else{
                    output$ST2_UI2c<-renderUI({
                      textInput("ST2_ICDExakt",value = NULL,placeholder = "z.B. A00 oder A00.0",label = NULL)
                    })
                    output$ST2_UI2b<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$ST2_ICDExakt)&&input$ST2_ICDExakt!=""&&nchar(input$ST2_ICDExakt)>=3){
                        user_icd<-gsub(".","\\.",input$ST2_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                        
                        if(nrow(input1_fa)>0){
                          output$ST2_UI2<-renderUI({dateInput("ST2_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$FA2_UI2<-renderUI({NULL})
                          
                          if(length(fa2_fokus)==1){
                            output$ST2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählte Station können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                          }else{
                            output$ST2_UI3b<-renderUI({h5(paste0("Bedingt durch die ausgewählten Stationen können Zeiträume zwischen ",
                                                                 as.Date(min(input1_fa$Aufnahme)),
                                                                 " und ",as.Date(max(input1_fa$Entlassung))," gewählt werden.\n",
                                                                 "Standard ist ein Zeitraum von 30 Tagen. Dieser kann jedoch beliebig verändert werden (maximal ",
                                                                 as.numeric(as.Date(max(input1_fa$Entlassung))-as.Date(min(input1_fa$Aufnahme))),
                                                                 " Tage). Beachten Sie aber, dass die Ergebnisse bei einem sehr großen Zeitraum 
                                                                unübersichtlich werden können."))})
                          }
                          output$FA2_UI3b<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$ST2_Start)){
                              if((input$ST2_Start+30)<=as.Date(max(input1_fa$Entlassung))){
                                output$ST2_UI3<-renderUI({dateInput("ST2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=input$ST2_Start+30,
                                                                    min = input$ST2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})                            
                              }else{
                                output$ST2_UI3<-renderUI({dateInput("ST2_Ende","Ende des Beobachtungszeitraums",
                                                                    value=as.Date(max(input1_fa$Entlassung)),
                                                                    min = input$ST2_Start,
                                                                    max = as.Date(max(input1_fa$Entlassung)),
                                                                    weekstart = 1,language = "de",format="yyyy-mm-dd")})
                              }
                              output$FA2_UI3<-renderUI({NULL})
                            }
                          })
                        }else{
                          if(length(fa2_fokus)==1){
                            output$ST2_UI3b<-renderUI({h5(paste0("Für die ausgewählte Station gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie eine andere Station und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$ST2_UI2<-renderUI({NULL})
                            output$ST2_UI3<-renderUI({NULL})
                          }else{
                            output$ST2_UI3b<-renderUI({h5(paste0("Für die ausgewählten Stationen gibt es mit dem von Ihnen definierten ICD-Filter 
                                                                 keine Fälle. Wählen Sie andere Stationen und/oder ändern Sie Ihren ICD-Filter, um 
                                                                 einen Beobachtungszeitraum wählen zu können."))})
                            output$ST2_UI2<-renderUI({NULL})
                            output$ST2_UI3<-renderUI({NULL})
                          }
                          output$FA2_UI3<-renderUI({NULL})
                          output$FA2_UI3b<-renderUI({NULL})
                          output$FA2_UI2<-renderUI({NULL})
                        }
                      }else{
                        output$ST2_UI2<-renderUI({NULL})
                        output$ST2_UI3<-renderUI({NULL})
                        output$ST2_UI3b<-renderUI({NULL})
                      }
                    })
                    
                  }
                }    
              }
            })
          }else{
            output$FA2_UI2a<-renderUI({NULL})
            output$FA2_UI2b<-renderUI({NULL})
            output$FA2_UI2c<-renderUI({NULL})
            output$FA2_UI3<-renderUI({NULL})
            output$FA2_UI3b<-renderUI({NULL})
            output$FA2_UI4b<-renderUI({NULL})
            
            output$ST2_UI2a<-renderUI({NULL})
            output$ST2_UI2b<-renderUI({NULL})
            output$ST2_UI2c<-renderUI({NULL})
            output$ST2_UI3<-renderUI({NULL})
            output$ST2_UI3b<-renderUI({NULL})
            output$ST2_UI4b<-renderUI({NULL})
          }
          
          
          
          
        })
        output$ST2_UI4<-renderUI({
          #materialSwitch("ST2_komplex","Darstellung von Kontakten innerhalb der Stationen?",
          #                  value = FALSE,status = "primary")
          NULL
        })
        
        output$FA2_UI4<-renderUI({NULL})
        output$FA2_UI4b<-renderUI({NULL})
      }
      if(input$FA2_oder_Station=="Stationen"&&is.null(input1b)){
        output$FA2_UI1<-renderUI({NULL})
        output$FA2_UI2<-renderUI({NULL})
        output$FA2_UI2a<-renderUI({NULL})
        output$FA2_UI2b<-renderUI({NULL})
        output$FA2_UI2c<-renderUI({NULL})
        output$FA2_UI3<-renderUI({NULL})
        output$FA2_UI3b<-renderUI({NULL})
        output$FA2_UI4<-renderUI({NULL})
        output$FA2_UI4b<-renderUI({NULL})
        output$FA_intro<-renderText({NULL})
        
        output$ST2_UI1<-renderUI({NULL})
        output$ST2_UI2<-renderUI({NULL})
        output$ST2_UI2a<-renderUI({NULL})
        output$ST2_UI2b<-renderUI({NULL})
        output$ST2_UI2c<-renderUI({NULL})
        output$ST2_UI3<-renderUI({NULL})
        output$ST2_UI3b<-renderUI({NULL})
        output$ST2_UI4<-renderUI({NULL})
        output$ST2_UI4b<-renderUI({NULL})
        output$ST_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
        
        output$FA_add2a<-renderText({NULL})
        output$FA_add3a<-renderText({NULL})
        output$FA_add7a<-renderUI({NULL})
        output$FA_add8a<-renderUI({NULL})
      }
    })
    
    output$FAST2_UI5<-renderUI({actionBttn("do_superspreader",label = "Starte Analyse",style = "gradient",color = "primary",disabled=T)})
    
    observe({
      if((!is.null(input1a)&&!is.null(input$FA2_Fokus)&&!is.null(input$switch0)&&
          (input$switch0=="nein"|(input$switch0=="ICD-Kapitel"&&!is.null(input$FA2_ICDKapitel))|(!is.null(input$FA2_ICDExakt)&&input$FA2_ICDExakt!=""&&nchar(input$FA2_ICDExakt)>=3)))||
         (!is.null(input1b)&&!is.null(input$ST2_Fokus)&&!is.null(input$switch0c)&&
          (input$switch0c=="nein"|(input$switch0c=="ICD-Kapitel"&&!is.null(input$ST2_ICDKapitel))|(!is.null(input$ST2_ICDExakt)&&input$ST2_ICDExakt!=""&&nchar(input$ST2_ICDExakt)>=3)))){
        if(input$FA2_oder_Station=="Fachabteilungen"&&!is.null(input1a)){
          shinyjs::enable("do_superspreader")
        }
        if(input$FA2_oder_Station=="Fachabteilungen"&&is.null(input1a)){
          shinyjs::disable("do_superspreader")
        }
        if(input$FA2_oder_Station=="Stationen"&&!is.null(input1b)){
          shinyjs::enable("do_superspreader")
        }
        if(input$FA2_oder_Station=="Stationen"&&is.null(input1b)){
          shinyjs::disable("do_superspreader")
        }
      }else{
        shinyjs::disable("do_superspreader")
      }
    })
    
    
    
    observeEvent(input$do_superspreader,{
      updateTabsetPanel(session,"main",
                        selected="Ergebnisse")
      if(input$FA2_oder_Station=="Fachabteilungen"){
        input1<-input1a
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer,as.numeric(input1$Aufnahme)),]
        fa<-sort(unique(input1$Abteilung))
        
        fa2_fokus<-as.character(input$FA2_Fokus)
        input1_fa<-input1[input1$Abteilung%in%fa2_fokus,]
        FA2_ICD<-23
        
        if(input$switch0=="ICD-Kapitel"){
          input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$FA2_ICDKapitel,]
          FA2_ICD<-input$FA2_ICDKapitel
        }
        if(input$switch0=="exakter ICD-Code"){
          user_icd<-gsub(".","\\.",input$FA2_ICDExakt,fixed=T)
          input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
          FA2_ICD<-input$FA2_ICDExakt
        }
      }
      
      if(input$FA2_oder_Station=="Stationen"){
        input1<-input1b
        names(input1)[3]<-"Abteilung"
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer),]
        fa<-sort(unique(input1$Abteilung))
        
        fa2_fokus<-as.character(input$ST2_Fokus)
        input1_fa<-input1[input1$Abteilung%in%fa2_fokus,]
        FA2_ICD<-23
        
        if(input$switch0c=="ICD-Kapitel"){
          input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$ST2_ICDKapitel,]
          FA2_ICD<-input$ST2_ICDKapitel
        }
        if(input$switch0c=="exakter ICD-Code"){
          user_icd<-gsub(".","\\.",input$ST2_ICDExakt,fixed=T)
          input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
          FA2_ICD<-input$ST2_ICDExakt
        }
      }
      
      if(input$FA2_oder_Station=="Fachabteilungen"){
        FA_Start<-input$FA2_Start
        FA_Ende<-input$FA2_Ende
        #innerhalb<-input$FA2_komplex
        innerhalb<-ifelse(length(fa2_fokus)>1,F,T)
      }else{
        FA_Start<-input$ST2_Start
        FA_Ende<-input$ST2_Ende
        #innerhalb<-input$ST2_komplex
        innerhalb<-ifelse(length(fa2_fokus)>1,F,T)
      }
      
      if(sum(as.numeric(FA_Start)==as.numeric(as.Date(input1_fa$Aufnahme,format="yyyy-mm-dd")))>=1&&
         sum(as.numeric(FA_Start)>as.numeric(as.Date(input1_fa$Aufnahme,format="yyyy-mm-dd")))==0){
        FA_Start<-min(input1_fa$Aufnahme[which(as.numeric(FA_Start)==as.numeric(as.Date(input1_fa$Aufnahme,format="yyyy-mm-dd")))])
      }else{
        FA_Start<-as.POSIXct(paste0(FA_Start," 00:00:01"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      }
      FA_Ende<-as.POSIXct(paste0(FA_Ende," 23:59:59"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      
      if(input$Min_Kontakt_Tag2==""){
        min_kontakt_tag<-0
      }else{
        min_kontakt_tag<-as.numeric(strsplit(input$Min_Kontakt_Tag2," ")[[1]][1])
      }
      if(input$Min_Kontakt_Stunde2==""){
        min_kontakt_stunde<-0
      }else{
        min_kontakt_stunde<-as.numeric(strsplit(input$Min_Kontakt_Stunde2," ")[[1]][1])
      }
      kontaktzeit<-min_kontakt_tag+min_kontakt_stunde/24
      
      ##Filter Zeitraum
      if(kontaktzeit==0){
        output$text_analyse1<-renderUI({"Bitte wählen Sie eine minimale Kontaktzeit."})
        output$text_analyse2<-renderUI({NULL})
        #output$plotBasic<-renderPlot({NULL})
        output$force<-renderForceNetwork({NULL})
        return()
      }
      if((FA_Ende-FA_Start)<kontaktzeit){
        output$text_analyse1<-renderUI(HTML("Der gewählte Beobachtungszeitraum ist kürzer als die minimale Kontaktzeit.<br>
        Bitte wählen Sie einen längeren Beobachtungszeitraum oder eine kürzere minimale Kontaktzeit."))
        output$text_analyse2<-renderUI({NULL})
        #output$plotBasic<-renderPlot({NULL})
        output$force<-renderForceNetwork({NULL})
        return()
      }
      
      ##auf Zeitraum filtern
      input1_filtered<-input1_fa[as.numeric(input1_fa$Aufnahme)<=as.numeric(FA_Ende)
                                 &as.numeric(input1_fa$Entlassung)>=as.numeric(FA_Start),]
      input1_filtered$Aufnahme<-as.POSIXct(apply(data.frame(V1=input1_filtered$Aufnahme,V2=FA_Start),1,max),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      input1_filtered$Entlassung<-as.POSIXct(apply(data.frame(V1=input1_filtered$Entlassung,V2=FA_Ende),1,min),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      
      #for(i in 1:nrow(input1_filtered)){
      #    input1_filtered$Aufnahme[i]<-max(input1_filtered$Aufnahme[i],FA_Start)
      #    input1_filtered$Entlassung[i]<-min(input1_filtered$Entlassung[i],FA_Ende)
      #}
      input1_filtered$Dauer<-(as.numeric(input1_filtered$Entlassung)-as.numeric(input1_filtered$Aufnahme))/(60*60*24)
      input1_filtered2<-input1_filtered#[input1_filtered$Dauer>=kontaktzeit,]
      fa<-sort(unique(input1_filtered2$Abteilung))
      
      
      input1_ohnefilter<-input1[as.numeric(input1$Aufnahme)<=as.numeric(FA_Ende)
                                &as.numeric(input1$Entlassung)>=as.numeric(FA_Start),]
      input1_ohnefilter$Aufnahme<-as.POSIXct(apply(data.frame(V1=input1_ohnefilter$Aufnahme,V2=FA_Start),1,max),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      input1_ohnefilter$Entlassung<-as.POSIXct(apply(data.frame(V1=input1_ohnefilter$Entlassung,V2=FA_Ende),1,min),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      input1_ohnefilter$Dauer<-(as.numeric(input1_ohnefilter$Entlassung)-as.numeric(input1_ohnefilter$Aufnahme))/(60*60*24)
      input1_ohnefilter2<-input1_ohnefilter#[input1_ohnefilter$Dauer>=kontaktzeit,]
      
      
      output_all<-data.frame(i=NA,j=NA,Patientennummer_i=input1_filtered$Patientennummer,Fallnummer_i=input1_filtered$Fallnummer,
                             Patientennummer_j=input1_filtered$Patientennummer,Fallnummer_j=input1_filtered$Fallnummer,
                             Abteilung=input1_filtered$Abteilung,Start_i=input1_filtered$Aufnahme,Ende_i=input1_filtered$Entlassung,
                             Start_j=input1_filtered$Aufnahme,Ende_j=input1_filtered$Entlassung,Dauer=input1_filtered$Dauer)
      
      progress <- shiny::Progress$new()
      progress$set(message = "Bestimme Kontakte", value = 0)
      for(abt in 1:length(fa)){
        progress$inc(1/length(fa),detail=fa[abt])
        temp<-input1_filtered2[input1_filtered2$Abteilung==fa[abt],]
        if(nrow(temp)>1){
          temp_intervalle<-TimeIntervalDataFrame(start=temp$Aufnahme,end=temp$Entlassung)
          temp_overlapping<-overlapping(temp_intervalle,idx = T)
          
          if(nrow(temp_overlapping)>0){
            temp_overlapping$Patientennummer_i<-temp$Patientennummer[temp_overlapping$i]
            temp_overlapping$Fallnummer_i<-temp$Fallnummer[temp_overlapping$i]
            temp_overlapping$Patientennummer_j<-temp$Patientennummer[temp_overlapping$j]
            temp_overlapping$Fallnummer_j<-temp$Fallnummer[temp_overlapping$j]
            
            temp_overlapping$Abteilung<-fa[abt]
            
            temp_overlapping$Start_i<-temp$Aufnahme[temp_overlapping$i]
            temp_overlapping$Ende_i<-temp$Entlassung[temp_overlapping$i]
            temp_overlapping$Start_j<-temp$Aufnahme[temp_overlapping$j]
            temp_overlapping$Ende_j<-temp$Entlassung[temp_overlapping$j]
            
            helper<-cbind(as.numeric(temp$Aufnahme[temp_overlapping$i]),as.numeric(temp$Aufnahme[temp_overlapping$j]))
            helper_start<-apply(helper,1,max)
            helper<-cbind(as.numeric(temp$Entlassung[temp_overlapping$i]),as.numeric(temp$Entlassung[temp_overlapping$j]))
            helper_ende<-apply(helper,1,min)
            temp_overlapping$Dauer<-(helper_ende-helper_start)/(60*60*24)
            
            temp_overlapping2<-temp_overlapping[temp_overlapping$Dauer>=kontaktzeit,]
            
            if(nrow(temp_overlapping2)>0){
              output_all<-rbind(output_all,temp_overlapping2)
            }
          }
        }
      }
      progress$close()
      
      output_all<-output_all[!is.na(output_all$Patientennummer_i),]
      output_all_test<-unique(output_all[,c(3:7)])
      
      counts<-table(c(output_all_test$Fallnummer_i,output_all_test$Fallnummer_j))
      counts2<-table(c(paste0(output_all_test$Abteilung,"\n",output_all_test$Fallnummer_i),paste0(output_all_test$Abteilung,"\n",output_all_test$Fallnummer_j)))
      counts<-counts-2
      counts2<-counts2-2
      
      ##Schwellwert Mindest-Anzahl von Kontakten
      min_kontakt<-0#input$Min_Kontakte
      fall_in<-names(counts)[counts>=min_kontakt]
      
      if(length(fall_in)>0){
        nodes1<-data.frame(NodeID=NA,Group=NA,NodeSize=NA,Count_einzeln=NA,Count_ges=NA,Dauer=NA,Dauer_ges=NA,Weitere=NA,ICDfull=NA)
        progress <- shiny::Progress$new()
        progress$set(message = "Prozessiere Patienten-Informationen", value = 0)
        for(i in 1:length(fall_in)){
          progress$inc(1/length(fall_in),detail=paste0(i,"/",length(fall_in)))
          #temp<-input1_filtered2[input1_filtered2$Fallnummer==fall_in[i],]
          temp<-input1_ohnefilter2[input1_ohnefilter2$Fallnummer==fall_in[i],]
          fa_temp<-unique(temp$Abteilung)
          if(nrow(temp)>1){
            for(j in fa_temp){
              if(length(as.numeric(counts2[names(counts2)==paste0(j,"\n",fall_in[i])]))>0){
                temp_in<-data.frame(NodeID=unique(temp$Fallnummer[temp$Abteilung==j]),
                                    Group=j,NodeSize=NA,
                                    Count_einzeln=as.numeric(counts2[names(counts2)==paste0(j,"\n",fall_in[i])]),
                                    Count_ges=as.numeric(counts[names(counts)==fall_in[i]]),
                                    Dauer=sum(temp$Dauer[temp$Abteilung==j]),
                                    Dauer_ges=sum(temp$Dauer),
                                    Weitere=ifelse(length(fa_temp)==1,"-",paste0(fa_temp[fa_temp!=j],collapse = ", ")),
                                    ICDfull=unique(temp$ICDfull[temp$Abteilung==j]))
                nodes1<-rbind(nodes1,temp_in)
              }
            }
          }else{
            temp_in<-data.frame(NodeID=temp$Fallnummer,Group=temp$Abteilung,NodeSize=NA,
                                Count_einzeln=as.numeric(counts[names(counts)==fall_in[i]]),
                                Count_ges=as.numeric(counts[names(counts)==fall_in[i]]),
                                Dauer=temp$Dauer,
                                Dauer_ges=temp$Dauer,
                                Weitere="-",
                                ICDfull=temp$ICDfull)
            nodes1<-rbind(nodes1,temp_in)
          }
          
        }
        progress$close()
        nodes1<-nodes1[!is.na(nodes1$NodeID),]
        nodes1$NodeID2<-paste0(nodes1$Group,"\n",nodes1$NodeID)
        
        ##1. jeder mit sich selbst
        links1<-data.frame(Source=nodes1$NodeID2,Target=nodes1$NodeID2,Farbe="gray85")
        
        ##2. selbe wenn auf verschiedenen Abteilungen -> schwarz
        count_intern<-table(nodes1$NodeID)
        count_intern<-count_intern[count_intern>1]
        progress <- shiny::Progress$new()
        progress$set(message = ifelse(input$FA2_oder_Station=="Fachabteilungen",
                                      "Prozessiere Kontakte zwischen Fachabteilungen",
                                      "Prozessiere Kontakte zwischen Stationen"), value = 0)
        for(i in names(count_intern)){
          progress$inc(1/length(names(count_intern)))
          temp<-nodes1[nodes1$NodeID==i,]
          if(nrow(temp)==2){
            links1<-rbind(links1,data.frame(Source=temp$NodeID2[1],Target=temp$NodeID2[2],Farbe="black"))
          }else{
            for(j in 1:(nrow(temp)-1)){
              for(k in (j+1):nrow(temp)){
                links1<-rbind(links1,data.frame(Source=temp$NodeID2[j],Target=temp$NodeID2[k],Farbe="black"))
              }
            }
          }
        }
        progress$close()
        
        ##3. echte Kontakte unter Superspreadern -> grau
        fa<-sort(unique(nodes1$Group))
        
        if(innerhalb==T){
          progress <- shiny::Progress$new()
          progress$set(message = ifelse(input$FA2_oder_Station=="Fachabteilungen",
                                        "Prozessiere Kontakte innerhalb Fachabteilungen",
                                        "Prozessiere Kontakte innerhalb Stationen"), value = 0)
          for(abt in 1:length(fa)){
            progress$inc(1/length(fa),detail=fa[abt])
            temp<-input1_filtered2[input1_filtered2$Abteilung==fa[abt],]
            ids_in<-nodes1$NodeID[nodes1$Group==fa[abt]]
            #                temp<-temp[match(ids_in,temp$Fallnummer),]
            if(nrow(temp)>1){
              temp_intervalle<-TimeIntervalDataFrame(start=temp$Aufnahme,end=temp$Entlassung)
              temp_overlapping<-overlapping(temp_intervalle,idx = T)
              
              if(nrow(temp_overlapping)>0){
                temp_overlapping$Patientennummer_i<-temp$Patientennummer[temp_overlapping$i]
                temp_overlapping$Fallnummer_i<-temp$Fallnummer[temp_overlapping$i]
                temp_overlapping$Patientennummer_j<-temp$Patientennummer[temp_overlapping$j]
                temp_overlapping$Fallnummer_j<-temp$Fallnummer[temp_overlapping$j]
                
                temp_overlapping$Abteilung<-fa[abt]
                
                temp_overlapping$Start_i<-temp$Aufnahme[temp_overlapping$i]
                temp_overlapping$Ende_i<-temp$Entlassung[temp_overlapping$i]
                temp_overlapping$Start_j<-temp$Aufnahme[temp_overlapping$j]
                temp_overlapping$Ende_j<-temp$Entlassung[temp_overlapping$j]
                
                helper<-cbind(as.numeric(temp$Aufnahme[temp_overlapping$i]),as.numeric(temp$Aufnahme[temp_overlapping$j]))
                helper_start<-apply(helper,1,max)
                helper<-cbind(as.numeric(temp$Entlassung[temp_overlapping$i]),as.numeric(temp$Entlassung[temp_overlapping$j]))
                helper_ende<-apply(helper,1,min)
                temp_overlapping$Dauer<-(helper_ende-helper_start)/(60*60*24)
                
                temp_overlapping2<-temp_overlapping[temp_overlapping$Dauer>=kontaktzeit,]
                
                if(nrow(temp_overlapping2)>0){
                  links1<-rbind(links1,data.frame(Source=paste0(temp_overlapping2$Abteilung,"\n",temp_overlapping2$Fallnummer_i),
                                                  Target=paste0(temp_overlapping2$Abteilung,"\n",temp_overlapping2$Fallnummer_j),
                                                  Farbe="gray85"))
                }
              }
            }
          }
          progress$close()
        }
        links1<-unique(links1)
        
        
        nodes1$ID<-seq(0,(nrow(nodes1)-1))
        links1$Source1<-nodes1$ID[match(links1$Source,nodes1$NodeID2)]
        links1$Target1<-nodes1$ID[match(links1$Target,nodes1$NodeID2)]
        
        nodes1$NodeSize<-nodes1$Count_einzeln/20
        nodes1$NodeSizeb<-2+3*nodes1$NodeSize/max(nodes1$NodeSize)
        nodes1$NodeSizeb[is.na(nodes1$NodeSizeb)]<-2
        
        ##Kontakte pro Dauer
        nodes1$NodeSize2<-nodes1$Count_einzeln/nodes1$Dauer
        nodes1$NodeSize2b<-2+3*nodes1$NodeSize2/max(nodes1$NodeSize2)
        nodes1$NodeSize2b[is.na(nodes1$NodeSize2b)]<-2
        ##normalisiert auf 0-1, dann *3, aber mindestgröße 2
        
        links1$Value<-ifelse(links1$Farbe == "gray85", 0.1,1)
        
        if(sum(links1$Value==0.5)>1){
          help_dist<-links1[links1$Value==0.5,]
          help_dist$Source<-str_split_fixed(help_dist$Source,pattern = "\n",Inf)[,1]
          help_dist$Target<-str_split_fixed(help_dist$Target,pattern = "\n",Inf)[,1]
          help_dist$Count<-0
          for(ij in 1:nrow(help_dist)){
            help_dist$Count[ij]<-sum(nrow(help_dist[help_dist$Source==help_dist$Source[ij]&help_dist$Target==help_dist$Target[ij],]),
                                     nrow(help_dist[help_dist$Target==help_dist$Source[ij]&help_dist$Source==help_dist$Target[ij],]))
            help_dist$Value_neu[ij]<--1*log(nrow(help_dist[help_dist$Source==help_dist$Source[ij]&help_dist$Target==help_dist$Target[ij],])/
                                              nrow(help_dist[help_dist$Target==help_dist$Target[ij],]))
          }
          #help_dist$Value_neu<-max(help_dist$Count)*0.01/help_dist$Count
          #help_dist$Value_neu2<--1*log(help_dist$Count/max(help_dist$Count))
          #help_dist$Value_neu2<-help_dist$Value_neu2/max(help_dist$Value_neu2)
          help_dist$Value_neu2<-1+help_dist$Value_neu
          help_dist$Value_neu<-help_dist$Value_neu/max(help_dist$Value_neu)
          for(ij in 1:nrow(help_dist)){
            links1$Value[links1$Source1==help_dist$Source1[ij]&links1$Target1==help_dist$Target1[ij]]<-help_dist$Value_neu[ij]
          }              
        }
        #write.table(help_dist,"Distanz_Beispiel.txt",sep="\t",row.names=F,quote=F)
        
        ##NodeSize korreliert mit Dauer wie lange jemand auf Station war
        ##Value bisher nur schwarz - grau, schwarz dicker damit klarer zu erkennen
        
        #if(input$FA_oder_Station=="Fachabteilungen"){
        #script <-'alert("Fall: " + (d.NodeID) + "\\n \\n Kontakte insgesamt: " + (d.Count_ges) + "\\n Aufenthaltsdauer insgesamt: " + (d.Dauer_ges) + " Tage\\n \\n Kontakte auf Fachabteilung " + (d.Group) + ": " + (d.Count_einzeln) + "\\n Aufenthaltsdauer auf Fachabteilung " + (d.Group) + ": " + (d.Dauer) + " Tage \\n \\n Weitere besuchte Fachabteilung: " + (d.Weitere));'
        script <-'alert(d.Test)'
        #}else{
        #script <-'alert("Fall: " + (d.NodeID) + "\\n \\n Kontakte insgesamt: " + (d.Count_ges) + "\\n Aufenthaltsdauer insgesamt: " + (d.Dauer_ges) + " Tage\\n \\n Kontakte auf Station " + (d.Group) + ": " + (d.Count_einzeln) + "\\n Aufenthaltsdauer auf Station " + (d.Group) + ": " + (d.Dauer) + " Tage \\n \\n Weitere besuchte Stationen: " + (d.Weitere));'
        #}
        #message(nrow(links1))
        links1_backup<-links1
        nodes1_backup<-nodes1
        
        if((innerhalb==F)){#|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1))>10000)&length(fa)!=1){
          progress <- shiny::Progress$new()
          progress$set(message = ifelse(input$FA2_oder_Station=="Fachabteilungen",
                                        "Erstelle Fachabteilungs-Knoten",
                                        "Erstelle Stations-Knoten"), value = 0)
          
          links1<-links1[links1$Farbe!="gray85"|links1$Source1==links1$Target1,]
          ##Superknoten hinzufügen
          max_node<-max(nodes1$ID)+1
          max_link<-nrow(links1)+1
          for(abt in fa){
            progress$inc(1/length(fa),detail=abt)
            nodes1<-rbind(nodes1,NA)
            nodes1$ID[(max_node+1)]<-max_node
            nodes1$NodeID2[(max_node+1)]<-abt
            nodes1$Group[(max_node+1)]<-abt
            nodes1$NodeSizeb[(max_node+1)]<-sum(nodes1$Group==abt,na.rm = T)-1
            nodes1$NodeID[(max_node+1)]<-abt
            ##Message transformieren wg superknoten?
            nodes1$NodeSize[(max_node+1)]<-sum(nodes1$Group==abt,na.rm = T)-1##Fälle auf FA
            #nodes1$Count_einzeln[(max_node+1)]<-paste0(sum(grep(abt,links1$Source)%in%grep(abt,links1$Target,invert = T)),
            #                                           " (",round(100*sum(grep(abt,links1$Source)%in%grep(abt,links1$Target,invert = T))/nodes1$NodeSize[(max_node+1)],2),"%)")##mit Verlegung von FA wo anders hin
            #nodes1$Count_ges[(max_node+1)]<-paste0(sum(grep(abt,links1$Target)%in%grep(abt,links1$Source,invert = T)),
            #                                       " (",round(100*sum(grep(abt,links1$Target)%in%grep(abt,links1$Source,invert = T))/nodes1$NodeSize[(max_node+1)],2),"%)")##mit Verlegung von wo anders nach FA
            
            nodes1$Dauer[(max_node+1)]<-mean(nodes1$Dauer[nodes1$Group==abt],na.rm=T)##mittlere Aufenthaltsdauer auf FA
            nodes1$Dauer_ges[(max_node+1)]<-mean(nodes1$Dauer_ges[nodes1$Group==abt],na.rm=T)##mittlere Aufenthaltsdauer insg
            
            
            andere<-nodes1[!is.na(nodes1$Weitere)&nodes1$Weitere!="-"&nodes1$Group==abt,]
            if(nrow(andere)>0){
              andere_alle<-input1_ohnefilter2[input1_ohnefilter2$Fallnummer%in%andere$NodeID,]
              out_andere<-data.frame(Von=unique(andere_alle$Abteilung)[unique(andere_alle$Abteilung)!=abt],Count_von=0,
                                     Nach=unique(andere_alle$Abteilung)[unique(andere_alle$Abteilung)!=abt],Count_nach=0)
              for(k in 1:nrow(out_andere)){
                relevant<-intersect((which(andere_alle$Abteilung==abt)+1),which(andere_alle$Abteilung==out_andere$Von[k]))
                out_andere$Count_von[k]<-sum(andere_alle$Fallnummer[relevant-1]==andere_alle$Fallnummer[relevant])
                
                relevant<-intersect((which(andere_alle$Abteilung==abt)-1),which(andere_alle$Abteilung==out_andere$Von[k]))
                out_andere$Count_nach[k]<-sum(andere_alle$Fallnummer[relevant+1]==andere_alle$Fallnummer[relevant])
              }
              nodes1$Count_einzeln[(max_node+1)]<-paste0(sum(out_andere$Count_von),
                                                         " (",round(100*sum(out_andere$Count_von)/nodes1$NodeSize[(max_node+1)],2),"%)")##mit Verlegung von FA wo anders hin
              nodes1$Count_ges[(max_node+1)]<-paste0(sum(out_andere$Count_nach),
                                                     " (",round(100*sum(out_andere$Count_nach)/nodes1$NodeSize[(max_node+1)],2),"%)")##mit Verlegung von wo anders nach FA
              
              #help_fas<-sort(table(nodes1$Group[links1$Target1[intersect(grep(abt,links1$Source),grep(abt,links1$Target,invert = T))]+1]),decreasing = T)[1:5]
              out_andere<-out_andere[order(out_andere$Count_von,decreasing=T),]
              help_fas<-out_andere$Count_von[1:min(nrow(out_andere),5)]
              names(help_fas)<-out_andere$Von[1:min(nrow(out_andere),5)]
              help_fas<-help_fas[help_fas!=0]
              nodes1$NodeSize2[(max_node+1)]<-paste0("\t",names(help_fas),": ",help_fas,collapse = "\n\t\t\t     ")##von hier nach...Top5
              #help_fas<-sort(table(nodes1$Group[links1$Source1[intersect(grep(abt,links1$Target),grep(abt,links1$Source,invert = T))]+1]),decreasing = T)[1:5]
              out_andere<-out_andere[order(out_andere$Count_nach,decreasing=T),]
              help_fas<-out_andere$Count_nach[1:min(nrow(out_andere),5)]
              names(help_fas)<-out_andere$Nach[1:min(nrow(out_andere),5)]
              help_fas<-help_fas[help_fas!=0]
              nodes1$NodeSize2b[(max_node+1)]<-paste0("\t",names(help_fas),": ",help_fas,collapse = "\n\t\t\t     ")##hierher von...Top5
            }else{
              nodes1$Count_einzeln[(max_node+1)]<-"0 (0%)"
              nodes1$Count_ges[(max_node+1)]<-"0 (0%)"
              nodes1$NodeSize2[(max_node+1)]<-"-"
              nodes1$NodeSize2b[(max_node+1)]<-"-"
            }
            
            
            links1<-rbind(links1,NA)
            links1$Source[max_link]<-abt
            links1$Target[max_link]<-abt
            links1$Source1[max_link]<-max_node
            links1$Target1[max_link]<-max_node
            links1$Farbe[max_link]<-"gray85"
            links1$Value[max_link]<-0.05
            max_link<-max_link+1
            
            neu<-nodes1[nodes1$Group==abt&!is.na(nodes1$Weitere),]
            for(z in 1:nrow(neu)){
              links1<-rbind(links1,NA)
              links1$Source[max_link]<-abt
              links1$Target[max_link]<-neu$NodeID2[z]
              links1$Source1[max_link]<-max_node
              links1$Target1[max_link]<-neu$ID[z]
              links1$Farbe[max_link]<-"gray85"
              links1$Value[max_link]<-0.1
              max_link<-max_link+1
            }
            max_node<-max_node+1
          }
          nodes1$NodeSizeb[nodes1$NodeID2==nodes1$Group]<-15+20*nodes1$NodeSizeb[nodes1$NodeID2==nodes1$Group]/max(nodes1$NodeSizeb[nodes1$NodeID2==nodes1$Group])
          nodes1$NodeSize2<-gsub("\t: "," -",nodes1$NodeSize2)
          nodes1$NodeSize2b<-gsub("\t: "," -",nodes1$NodeSize2b)
          progress$close()
        }else{
          progress <- shiny::Progress$new()
          progress$set(message = ifelse(input$FA2_oder_Station=="Fachabteilungen",
                                        "Analysiere andere Fachabteilungen",
                                        "Analysiere andere Stationen"), value = 0)
          abt<-fa
          andere<-nodes1[!is.na(nodes1$Weitere)&nodes1$Weitere!="-"&nodes1$Group==abt,]
          if(nrow(andere)>0){
            andere_alle<-input1_ohnefilter2[input1_ohnefilter2$Fallnummer%in%andere$NodeID,]
            out_andere<-data.frame(Von=unique(andere_alle$Abteilung)[unique(andere_alle$Abteilung)!=abt],Count_von=0,
                                   Nach=unique(andere_alle$Abteilung)[unique(andere_alle$Abteilung)!=abt],Count_nach=0)
            for(k in 1:nrow(out_andere)){
              progress$inc(1/nrow(out_andere),detail=out_andere$Von[k])
              relevant<-intersect((which(andere_alle$Abteilung==abt)+1),which(andere_alle$Abteilung==out_andere$Von[k]))
              out_andere$Count_von[k]<-sum(andere_alle$Fallnummer[relevant-1]==andere_alle$Fallnummer[relevant])
              
              relevant<-intersect((which(andere_alle$Abteilung==abt)-1),which(andere_alle$Abteilung==out_andere$Von[k]))
              out_andere$Count_nach[k]<-sum(andere_alle$Fallnummer[relevant+1]==andere_alle$Fallnummer[relevant])
            }
            von_summe<-paste0(sum(out_andere$Count_von),
                              " (",round(100*sum(out_andere$Count_von)/nrow(nodes1),2),"%)")##mit Verlegung von FA wo anders hin
            nach_summe<-paste0(sum(out_andere$Count_nach),
                               " (",round(100*sum(out_andere$Count_nach)/nrow(nodes1),2),"%)")##mit Verlegung von wo anders nach FA
            
            out_andere<-out_andere[order(out_andere$Count_von,decreasing=T),]
            help_fas<-out_andere$Count_von[1:min(nrow(out_andere),5)]
            names(help_fas)<-out_andere$Von[1:min(nrow(out_andere),5)]
            help_fas<-help_fas[help_fas!=0]
            von5<-paste0("\t",names(help_fas),": ",help_fas,collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;")##von hier nach...Top5
            out_andere<-out_andere[order(out_andere$Count_nach,decreasing=T),]
            help_fas<-out_andere$Count_nach[1:min(nrow(out_andere),5)]
            names(help_fas)<-out_andere$Nach[1:min(nrow(out_andere),5)]
            help_fas<-help_fas[help_fas!=0]
            nach5<-paste0("\t",names(help_fas),": ",help_fas,collapse = "<br>&nbsp;&nbsp;&nbsp;&nbsp;")##hierher von...Top5
          }else{
            von_summe<-"0 (0%)"
            nach_summe<-"0 (0%)"
            von5<-"-"
            nach5<-"-"
          }
          progress$close()
        }
        
        color_scale <- colorRampPalette(brewer.pal(9, "Set1"))(length(fa))
        
        fn<-forceNetwork(Links = links1,Nodes = nodes1,NodeID="NodeID2",Group = "Group",
                         Source="Source1",Target="Target1",zoom=T,opacityNoHover = 0.2,opacity = 1,Nodesize = "NodeSizeb",
                         radiusCalculation = JS("1.05*d.nodesize"),Value="Value",charge=-15,linkWidth = 0.5,
                         linkDistance = JS("function(d){return d.value*500}"),
                         linkColour = ifelse(links1$Farbe == "gray85", "#c2c5ca","black"),
                         clickAction = script,
                         colourScale = JS(paste0('d3.scaleOrdinal().range(["', paste(color_scale, collapse = '", "'), '"])')))
        
        fn$x$nodes$NodeID<-nodes1$NodeID
        fn$x$nodes$ICDfull<-nodes1$ICDfull
        fn$x$nodes$NodeSize<-nodes1$NodeSize
        fn$x$nodes$Count_einzeln<-nodes1$Count_einzeln
        fn$x$nodes$Count_ges<-nodes1$Count_ges
        fn$x$nodes$Dauer_ges<-as.character(round(nodes1$Dauer_ges,2))
        
        fn$x$nodes$Group<-nodes1$Group
        fn$x$nodes$Count_einzeln<-nodes1$Count_einzeln
        fn$x$nodes$Dauer<-as.character(round(nodes1$Dauer,2))
        fn$x$nodes$NodeSize2<-nodes1$NodeSize2
        fn$x$nodes$NodeSize2b<-nodes1$NodeSize2b
        
        fn$x$nodes$Weitere<-nodes1$Weitere
        
        if(input$FA2_oder_Station=="Fachabteilungen"){
          if(length(fa)==1){
            #fn$x$nodes$Test<-paste0("Fall: ",fn$x$nodes$NodeID,"\nHauptdiagnose: ",
            #                        fn$x$nodes$ICDfull,"\n\n \nKontakte auf Fachabteilung ",
            #                        fn$x$nodes$Group,": ",fn$x$nodes$Count_einzeln,
            #                        "\nAufenthaltsdauer auf Fachabteilung ",fn$x$nodes$Group,": ",
            #                        fn$x$nodes$Dauer," Tage") 
            
            fn$x$nodes$Test<-paste0("Fall: ",fn$x$nodes$NodeID,"\nHauptdiagnose: ",
                                    fn$x$nodes$ICDfull,
                                    "\n\nAufenthaltsdauer insgesamt: ",
                                    fn$x$nodes$Dauer_ges," Tage\n \nKontakte auf Fachabteilung ",
                                    fn$x$nodes$Group,": ",fn$x$nodes$Count_einzeln,
                                    "\nAufenthaltsdauer auf Fachabteilung ",fn$x$nodes$Group,": ",
                                    fn$x$nodes$Dauer," Tage \n \nWeitere besuchte Fachabteilungen: ",fn$x$nodes$Weitere) 
          }else{
            fn$x$nodes$Test<-paste0("Fall: ",fn$x$nodes$NodeID,"\nHauptdiagnose: ",
                                    fn$x$nodes$ICDfull,"\n \nKontakte insgesamt: ",
                                    fn$x$nodes$Count_ges,"\nAufenthaltsdauer insgesamt: ",
                                    fn$x$nodes$Dauer_ges," Tage\n \nKontakte auf Fachabteilung ",
                                    fn$x$nodes$Group,": ",fn$x$nodes$Count_einzeln,
                                    "\nAufenthaltsdauer auf Fachabteilung ",fn$x$nodes$Group,": ",
                                    fn$x$nodes$Dauer," Tage \n \nWeitere besuchte Fachabteilungen: ",fn$x$nodes$Weitere) 
          }
          
          fn$x$nodes$Test[fn$x$nodes$name==fn$x$nodes$group]<-paste0("Fachabteilung: ",
                                                                     fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\n\nFälle insgesamt: ",
                                                                     fn$x$nodes$NodeSize[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\nFälle mit Verlegung von ",fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],": ",
                                                                     fn$x$nodes$Count_einzeln[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\nTop-5 nach: ",fn$x$nodes$NodeSize2[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\n\nFälle mit Verlegung nach ",fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],": ",
                                                                     fn$x$nodes$Count_ges[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\nTop-5 von: ",fn$x$nodes$NodeSize2b[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\n\nMittlere Aufenthaltsdauerer gesamt: ",fn$x$nodes$Dauer_ges[fn$x$nodes$name==fn$x$nodes$group]," Tage",
                                                                     "\nMittlere Aufenthaltsdauer auf ",fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],": ",
                                                                     fn$x$nodes$Dauer[fn$x$nodes$name==fn$x$nodes$group]," Tage"
          )
        }else{
          if(length(fa)==1){
            fn$x$nodes$Test<-paste0("Fall: ",fn$x$nodes$NodeID,"\nHauptdiagnose: ",
                                    fn$x$nodes$ICDfull,"\n\n \nKontakte auf Station ",
                                    fn$x$nodes$Group,": ",fn$x$nodes$Count_einzeln,
                                    "\nAufenthaltsdauer auf Station ",fn$x$nodes$Group,": ",
                                    fn$x$nodes$Dauer," Tage \n \nWeitere besuchte Stationen: ",fn$x$nodes$Weitere) 
          }else{
            fn$x$nodes$Test<-paste0("Fall: ",fn$x$nodes$NodeID,"\nHauptdiagnose: ",
                                    fn$x$nodes$ICDfull,"\n \nKontakte insgesamt: ",
                                    fn$x$nodes$Count_ges,"\nAufenthaltsdauer insgesamt: ",
                                    fn$x$nodes$Dauer_ges," Tage\n \nKontakte auf Station ",
                                    fn$x$nodes$Group,": ",fn$x$nodes$Count_einzeln,
                                    "\nAufenthaltsdauer auf Station ",fn$x$nodes$Group,": ",
                                    fn$x$nodes$Dauer," Tage \n \nWeitere besuchte Stationen: ",fn$x$nodes$Weitere)
          }
          
          fn$x$nodes$Test[fn$x$nodes$name==fn$x$nodes$group]<-paste0("Station: ",
                                                                     fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\n\nFälle insgesamt: ",
                                                                     fn$x$nodes$NodeSize[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\nFälle mit Verlegung von ",fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],": ",
                                                                     fn$x$nodes$Count_einzeln[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\nTop-5 nach: ",fn$x$nodes$NodeSize2[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\n\nFälle mit Verlegung nach ",fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],": ",
                                                                     fn$x$nodes$Count_ges[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\nTop-5 von: ",fn$x$nodes$NodeSize2b[fn$x$nodes$name==fn$x$nodes$group],
                                                                     "\n\nMittlere Aufenthaltsdauer gesamt: ",fn$x$nodes$Dauer_ges[fn$x$nodes$name==fn$x$nodes$group]," Tage",
                                                                     "\nMittlere Aufenthaltsdauer auf ",fn$x$nodes$NodeID[fn$x$nodes$name==fn$x$nodes$group],": ",
                                                                     fn$x$nodes$Dauer[fn$x$nodes$name==fn$x$nodes$group]," Tage"
          )
        }
        
        
        
        customJS <- '
  function() {
    simulation = this;
    simulation.stop();
    for (var i = 0; i < 300; ++i) simulation.tick();
    simulation.nodes().forEach( function(d,i) {
      d.cx = d.x;
      d.cy = d.y;
    });
    simulation.restart();
  }
'
        
        
        
        nodes1$FarbeAussen[nodes1$Weitere!="-"]<-"#141414"
        fn$x$nodes$FarbeAussen<-nodes1$FarbeAussen
        fn$x$nodes$borderWidth<-1.5
        
        output$force <- renderForceNetwork({
          message("done")
          if((innerhalb==F)){#|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1){
            #htmlwidgets::onRender(fn, customJS)
            fn <- onRender(fn, '
                function(el, x) {
                    // Convert R data frame to JavaScript array of objects
                var nodeColors = HTMLWidgets.dataframeToD3(x.nodes);
                var nodeData = HTMLWidgets.dataframeToD3(x.nodes);
                d3.selectAll(".node").select("circle")
                .style("stroke", function(d, i) {
                    return nodeColors[i].FarbeAussen;  // Use the AB column for node stroke color
                })
                .style("stroke-width", function(d, i) {
                    return nodeData[i].borderWidth + "px";  // Use the borderWidth column for node stroke width
                 });
                 
                     simulation = this;
    simulation.stop();
    for (var i = 0; i < 300; ++i) simulation.tick();
    simulation.nodes().forEach( function(d,i) {
      d.cx = d.x;
      d.cy = d.y;
    });
    simulation.restart();
              }
            ')
          }else{
            fn <- onRender(fn, '
                function(el, x) {
                    // Convert R data frame to JavaScript array of objects
                var nodeColors = HTMLWidgets.dataframeToD3(x.nodes);
                var nodeData = HTMLWidgets.dataframeToD3(x.nodes);
                d3.selectAll(".node").select("circle")
                .style("stroke", function(d, i) {
                    return nodeColors[i].FarbeAussen;  // Use the AB column for node stroke color
                })
                .style("stroke-width", function(d, i) {
                    return nodeData[i].borderWidth + "px";  // Use the borderWidth column for node stroke width
                 });
              }
            ')
          }
          fn
        })
        
        
        output$text_analyse1<-renderUI({NULL})
        output$text_analyse2<-renderUI({NULL})
        #output$plotBasic<-renderPlot({NULL})
        
        links1_backup<-links1_backup[links1_backup$Source1!=links1_backup$Target1,]
        
        icd_help_out<-c("I: A00-B99",
                        "II: C00-D48",
                        "III: D50-D90",
                        "IV: E00-E90",
                        "V: F00-F99",
                        "VI: G00-G99",
                        "VII: H00-H59",
                        "VIII: H60-H95",
                        "IX: I00-I99",
                        "X: J00-J99",
                        "XI: K00-K93",
                        "XII: L00-L99",
                        "XIII: M00-M99",
                        "XIV: N00-N99",
                        "XV: O00-O99",
                        "XVI: P00-P96",
                        "XVII: Q00-Q99",
                        "XVIII: R00-R99",
                        "XIX: S00-T98",
                        "XX: V01-Y84",
                        "XXI: Z00-Z99",
                        "XXII: U00-U99",
                        "nein")
        
        if(input$FA2_oder_Station=="Fachabteilungen"){
          if(min_kontakt_tag!=1&min_kontakt_stunde!=1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilungen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Fachabteilungen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Fachabteilungen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Fachabteilungen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
          if(min_kontakt_tag==1&min_kontakt_stunde!=1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilungen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Fachabteilungen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Fachabteilungen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),  
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Fachabteilungen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
          if(min_kontakt_tag!=1&min_kontakt_stunde==1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilungen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Fachabteilungen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Fachabteilungen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Fachabteilungen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
          if(min_kontakt_tag==1&min_kontakt_stunde==1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilungen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Fachabteilungen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Fachabteilungen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Fachabteilungen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Fachabteilung ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
        }else{
          if(min_kontakt_tag!=1&min_kontakt_stunde!=1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Stationen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Stationen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Stationen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Stationen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Station ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
          if(min_kontakt_tag==1&min_kontakt_stunde!=1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Stationen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Stationen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Stationen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Stationen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Station ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunden",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
          if(min_kontakt_tag!=1&min_kontakt_stunde==1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Stationen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Stationen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Stationen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Stationen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Station ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tage und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
          if(min_kontakt_tag==1&min_kontakt_stunde==1){
            if(length(fa2_fokus)!=1){
              output$text_analyse2<-renderUI(HTML(paste0("Stationen ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>Stationen in Analyse enthalten: ",paste0(fa,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br>Kontakte innerhalb der Stationen ",
                                                         #ifelse(((innerhalb==F|(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa))>10000)&length(fa)!=1),"(nicht dargestellt): ","(dargestellt): "),
                                                         #(nrow(links1_backup[links1_backup$Farbe=="gray85",])-nrow(nodes1)-length(fa)),
                                                         sum(as.numeric(nodes1_backup$Count_einzeln))/2,
                                                         "<br>Verlegungen zwischen den Stationen: ",nrow(links1_backup[links1_backup$Farbe!="gray85",]),
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }else{
              output$text_analyse2<-renderUI(HTML(paste0("Station ausgewählt: ",paste0(fa2_fokus,collapse = ", "),
                                                         "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA2_ICD)],collapse=", "),
                                                         "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                         "<br>Minimale Kontaktzeit: ",min_kontakt_tag," Tag und ",min_kontakt_stunde," Stunde",
                                                         #"<br>Minimale Kontakt-Anzahl: ",min_kontakt,
                                                         "<br><br>Fälle: ",nrow(nodes1_backup),
                                                         "<br><br>Kontakte innerhalb der Fachabteilung: ",nrow(links1_backup[links1_backup$Farbe=="gray85",]),
                                                         "<br><br>Fälle mit Verlegung von ",fa2_fokus,": ",von_summe,
                                                         "<br>Top-5 nach: ",von5,
                                                         "<br><br>Fälle mit Verlegung nach ",fa2_fokus,": ",nach_summe,
                                                         "<br>Top-5 von: ",nach5,
                                                         "<br><br>Analyse erfolgreich durchgeführt")))
            }
          }
        }
        
        
        
      }else{
        output$text_analyse1<-renderUI(HTML("Für die gewählte Konfiguartion (minimale Kontaktzeit ",round(kontaktzeit,2)," Tage) gibt es keine Fälle.<br>
        Bitte wählen Sie eine kürzere Minimale Kontaktzeit."))
        output$text_analyse2<-renderUI({NULL})
        output$force<-renderForceNetwork({NULL})
        #output$plotBasic<-renderPlot({NULL})
        return()
      }
      
      
      
      
      
      
    })
    
    
    
    #############################
    ##3. Reiter: Zusammenfassung
    output$FA3_UI1<-renderUI({NULL})
    output$FA3_UI2<-renderUI({NULL})
    output$FA3_UI2a<-renderUI({NULL})
    output$FA3_UI2b<-renderUI({NULL})
    output$FA3_UI2c<-renderUI({NULL})
    output$FA3_UI4<-renderUI({NULL})
    output$FA3_UI5<-renderUI({NULL})
    
    output$ST3_UI1<-renderUI({NULL})
    output$ST3_UI2<-renderUI({NULL})
    output$ST3_UI2a<-renderUI({NULL})
    output$ST3_UI2b<-renderUI({NULL})
    output$ST3_UI2c<-renderUI({NULL})
    output$ST3_UI4<-renderUI({NULL})
    output$ST3_UI5<-renderUI({NULL})

    
    observe({
      if(input$FA3_oder_Station=="Fachabteilungen"&&!is.null(input1a)){
        output$FA_erweitert_UI2<-renderUI({NULL})
        
        output$FA3_UI1<-renderUI({NULL})
        output$FA3_UI2<-renderUI({NULL})
        output$FA3_UI2a<-renderUI({NULL})
        output$FA3_UI2b<-renderUI({NULL})
        output$FA3_UI2c<-renderUI({NULL})
        output$FA3_UI4<-renderUI({NULL})
        output$FA3_UI5<-renderUI({NULL})
        
        output$ST3_UI1<-renderUI({NULL})
        output$ST3_UI2<-renderUI({NULL})
        output$ST3_UI2a<-renderUI({NULL})
        output$ST3_UI2b<-renderUI({NULL})
        output$ST3_UI2c<-renderUI({NULL})
        output$ST3_UI4<-renderUI({NULL})
        output$ST3_UI5<-renderUI({NULL})
        
        output$FA3_intro<-renderText({NULL})
        output$ST3_intro<-renderText({NULL})
        output$FA3_add1a<-renderUI({prettyRadioButtons(inputId = "switch1",label = "Verlegungen",
                                                      choices = c("von","nach"),selected = "von",
                                                      inline=T,outline = T,bigger = T,
                                                      status = "primary",
                                                      icon=icon("check"))})
        output$FA3_add2a<-renderText({"Auswahl Beobachtungszeitraum"})
        
        
        #input1<-readxl::read_excel("www/Verlegungshistorien_2023_2024-03-04_01.xlsx",sheet=2)
        input1<-input1a
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer),]
        fa<-sort(unique(input1$Abteilung))
        
        output$FA3_UI2<-renderUI({
          pickerInput('FA3_Fokus',label = "Fachabteilung",choices = fa,selected = fa[1],options = list(`live-search` = TRUE))
        })
        output$ST3_UI2<-renderUI({NULL})
        
        observe({
          if(!is.null(input$FA3_Fokus)){
            input1_fa<-input1[input1$Abteilung==input$FA3_Fokus,]
            
            output$FA3_UI2a<-renderUI({
              prettyRadioButtons(inputId = "switch1b",label = "Hauptdiagnose nach ICD-Code filtern?",
                                 choices = c("nein","ICD-Kapitel","exakter ICD-Code"),selected = "nein",
                                 inline=T,outline = T,bigger = T,
                                 status = "primary",
                                 icon=icon("check")
              )
            })
            
            observe({
              if(!is.null(input$switch1b)){
                
                if(input$switch1b=="nein"){
                  output$FA3_UI2b<-renderUI({NULL})
                  output$FA3_UI2c<-renderUI({NULL})
                  
                  output$FA3_UI4<-renderUI({dateInput("FA3_Start","Beginn des Beobachtungszeitraums",
                                                      value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                      weekstart = 1,language = "de",format="yyyy-mm-dd")})
                  output$ST3_UI4<-renderUI({NULL})
                  
                  observe({
                    if(!is.null(input$FA3_Start)){
                      output$FA3_UI5<-renderUI({dateInput("FA3_Ende","Ende des Beobachtungszeitraums",
                                                          value=min(input$FA3_Start+365,as.Date(max(input1_fa$Entlassung))),
                                                          min = input$FA3_Start,
                                                          max = as.Date(max(input1_fa$Entlassung)),
                                                          weekstart = 1,language = "de",format="yyyy-mm-dd")})
                      output$ST3_UI5<-renderUI({NULL})
                    }
                  })
                }else{
                  if(input$switch1b=="ICD-Kapitel"){
                    output$FA3_UI2b<-renderUI({
                      pickerInput('FA3_ICDKapitel',
                                  choices = c(1:22), 
                                  choicesOpt = list(
                                    content = c(HTML('<b>I</b>: A00-B99'),
                                                HTML('<b>II</b>: C00-D48'),
                                                HTML('<b>III</b>: D50-D90'),
                                                HTML('<b>IV</b>: E00-E90'),
                                                HTML('<b>V</b>: F00-F99'),
                                                HTML('<b>VI</b>: G00-G99'),
                                                HTML('<b>VII</b>: H00-H59'),
                                                HTML('<b>VIII</b>: H60-H95'),
                                                HTML('<b>IX</b>: I00-I99'),
                                                HTML('<b>X</b>: J00-J99'),
                                                HTML('<b>XI</b>: K00-K93'),
                                                HTML('<b>XII</b>: L00-L99'),
                                                HTML('<b>XIII</b>: M00-M99'),
                                                HTML('<b>XIV</b>: N00-N99'),
                                                HTML('<b>XV</b>: O00-O99'),
                                                HTML('<b>XVI</b>: P00-P96'),
                                                HTML('<b>XVII</b>: Q00-Q99'),
                                                HTML('<b>XVIII</b>: R00-R99'),
                                                HTML('<b>XIX</b>: S00-T98'),
                                                HTML('<b>XX</b>: V01-Y84'),
                                                HTML('<b>XXI</b>: Z00-Z99'),
                                                HTML('<b>XXII</b>: U00-U99'))
                                  ),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Auswahl aufheben",
                                                 `select-all-text` = "Alle auswählen",
                                                 `none-selected-text` = "Nichts ausgewählt",
                                                 sanitize=FALSE),multiple=TRUE)
                    })
                    output$FA3_UI2c<-renderUI({NULL})
                    output$ST3_UI2b<-renderUI({NULL})
                    output$ST3_UI2c<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$FA3_ICDKapitel)){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$FA3_ICDKapitel,]
                        
                        if(nrow(input1_fa)>0){
                          output$FA3_UI4<-renderUI({dateInput("FA3_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$ST3_UI4<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$FA3_Start)){
                              output$FA3_UI5<-renderUI({dateInput("FA3_Ende","Ende des Beobachtungszeitraums",
                                                                  value=min(input$FA3_Start+365,as.Date(max(input1_fa$Entlassung))),
                                                                  min = input$FA3_Start,
                                                                  max = as.Date(max(input1_fa$Entlassung)),
                                                                  weekstart = 1,language = "de",format="yyyy-mm-dd")})
                              output$ST3_UI5<-renderUI({NULL})
                            }
                          })
                        }
                      }
                    })
                  }else{
                    output$FA3_UI2c<-renderUI({
                      textInput("FA3_ICDExakt",value = NULL,placeholder = "z.B. A00 oder A00.0",label = NULL)
                    })
                    output$FA3_UI2b<-renderUI({NULL})
                    output$ST3_UI2b<-renderUI({NULL})
                    output$ST3_UI2c<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$FA3_ICDExakt)&&input$FA3_ICDExakt!=""&&nchar(input$FA3_ICDExakt)>=3){
                        user_icd<-gsub(".","\\.",input$FA3_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                        
                        if(nrow(input1_fa)>0){
                          output$FA3_UI4<-renderUI({dateInput("FA3_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$ST3_UI4<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$FA3_Start)){
                              output$FA3_UI5<-renderUI({dateInput("FA3_Ende","Ende des Beobachtungszeitraums",
                                                                  value=min(input$FA3_Start+365,as.Date(max(input1_fa$Entlassung))),
                                                                  min = input$FA3_Start,
                                                                  max = as.Date(max(input1_fa$Entlassung)),
                                                                  weekstart = 1,language = "de",format="yyyy-mm-dd")})
                              output$ST3_UI5<-renderUI({NULL})
                            }
                          })
                        }
                      }
                    })
                  }
                }
              }
            })
          }else{
            output$FA3_UI2a<-renderUI({NULL})
            output$FA3_UI2b<-renderUI({NULL})
            output$FA3_UI2c<-renderUI({NULL})
            output$FA3_UI4<-renderUI({NULL})
            output$FA3_UI5<-renderUI({NULL})
            
            output$ST3_UI2a<-renderUI({NULL})
            output$ST3_UI2b<-renderUI({NULL})
            output$ST3_UI2c<-renderUI({NULL})
            output$ST3_UI4<-renderUI({NULL})
            output$ST3_UI5<-renderUI({NULL})
          }
        })
      }
      if(input$FA3_oder_Station=="Fachabteilungen"&&is.null(input1a)){
        output$FA_erweitert_UI2<-renderUI({NULL})
        
        output$FA3_UI1<-renderUI({NULL})
        output$FA3_UI2<-renderUI({NULL})
        output$FA3_UI2a<-renderUI({NULL})
        output$FA3_UI2b<-renderUI({NULL})
        output$FA3_UI2c<-renderUI({NULL})
        output$FA3_UI4<-renderUI({NULL})
        output$FA3_UI5<-renderUI({NULL})
        
        output$ST3_UI1<-renderUI({NULL})
        output$ST3_UI2<-renderUI({NULL})
        output$ST3_UI2a<-renderUI({NULL})
        output$ST3_UI2b<-renderUI({NULL})
        output$ST3_UI2c<-renderUI({NULL})
        output$ST3_UI4<-renderUI({NULL})
        output$ST3_UI5<-renderUI({NULL})
        
        output$FA3_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
        output$ST3_intro<-renderText({NULL})
        output$FA3_add1a<-renderUI({NULL})
        output$FA3_add2a<-renderText({NULL})
      }
    })
    
    
    #####################pro Stationen
    observe({
      if(input$FA3_oder_Station=="Stationen"&&!is.null(input1b)){
        output$FA_erweitert_UI2<-renderUI({NULL})
        
        output$FA3_UI1<-renderUI({NULL})
        output$FA3_UI2<-renderUI({NULL})
        output$FA3_UI2a<-renderUI({NULL})
        output$FA3_UI2b<-renderUI({NULL})
        output$FA3_UI2c<-renderUI({NULL})
        output$FA3_UI4<-renderUI({NULL})
        output$FA3_UI5<-renderUI({NULL})
        
        output$ST3_UI1<-renderUI({NULL})
        output$ST3_UI2<-renderUI({NULL})
        output$ST3_UI2a<-renderUI({NULL})
        output$ST3_UI2b<-renderUI({NULL})
        output$ST3_UI2c<-renderUI({NULL})
        output$ST3_UI4<-renderUI({NULL})
        output$ST3_UI5<-renderUI({NULL})
        
        output$FA3_intro<-renderText({NULL})
        output$ST3_intro<-renderText({NULL})
        output$FA3_add1a<-renderUI({prettyRadioButtons(inputId = "switch1",label = "Verlegungen",
                                                       choices = c("von","nach"),selected = "von",
                                                       inline=T,outline = T,bigger = T,
                                                       status = "primary",
                                                       icon=icon("check"))})
        output$FA3_add2a<-renderText({"Auswahl Beobachtungszeitraum"})
        
        #input1<-readxl::read_excel("www/Verlegungshistorien_2023_2024-03-04_01.xlsx",sheet=4)
        input1<-input1b
        names(input1)[3]<-"Abteilung"
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer),]
        fa<-sort(unique(input1$Abteilung))
        
        output$ST3_UI2<-renderUI({
          pickerInput('ST3_Fokus',label = "Station",choices = fa,selected=fa[1],options = list(`live-search` = TRUE))
        })
        output$FA3_UI2<-renderUI({NULL})
        
        observe({
          if(!is.null(input$ST3_Fokus)){
            input1_fa<-input1[input1$Abteilung==input$ST3_Fokus,]
            
            output$ST3_UI2a<-renderUI({
              prettyRadioButtons(inputId = "switch1c",label = "Hauptdiagnose nach ICD-Code filtern?",
                                 choices = c("nein","ICD-Kapitel","exakter ICD-Code"),selected = "nein",
                                 inline=T,outline = T,bigger = T,
                                 status = "primary",
                                 icon=icon("check")
              )
            })
            
            observe({
              if(!is.null(input$switch1c)){
                if(input$switch1c=="nein"){
                  output$ST3_UI2b<-renderUI({NULL})
                  output$ST3_UI2c<-renderUI({NULL})
                  output$FA3_UI2b<-renderUI({NULL})
                  output$FA3_UI2c<-renderUI({NULL})
                  
                  output$ST3_UI4<-renderUI({dateInput("ST3_Start","Beginn des Beobachtungszeitraums",
                                                      value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                      max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                      weekstart = 1,language = "de",format="yyyy-mm-dd")})
                  output$FA3_UI4<-renderUI({NULL})
                  
                  observe({
                    if(!is.null(input$ST3_Start)){
                      output$ST3_UI5<-renderUI({dateInput("ST3_Ende","Ende des Beobachtungszeitraums",
                                                          value=min(as.Date(max(input1_fa$Entlassung)),input$ST3_Start+365),
                                                          min = input$ST3_Start,
                                                          max = as.Date(max(input1_fa$Entlassung)),
                                                          weekstart = 1,language = "de",format="yyyy-mm-dd")})                                   
                      
                      output$FA3_UI5<-renderUI({NULL})
                    }
                  })
                }else{
                  if(input$switch1c=="ICD-Kapitel"){
                    output$ST3_UI2b<-renderUI({
                      pickerInput('ST3_ICDKapitel',
                                  choices = c(1:22), 
                                  choicesOpt = list(
                                    content = c(HTML('<b>I</b>: A00-B99'),
                                                HTML('<b>II</b>: C00-D48'),
                                                HTML('<b>III</b>: D50-D90'),
                                                HTML('<b>IV</b>: E00-E90'),
                                                HTML('<b>V</b>: F00-F99'),
                                                HTML('<b>VI</b>: G00-G99'),
                                                HTML('<b>VII</b>: H00-H59'),
                                                HTML('<b>VIII</b>: H60-H95'),
                                                HTML('<b>IX</b>: I00-I99'),
                                                HTML('<b>X</b>: J00-J99'),
                                                HTML('<b>XI</b>: K00-K93'),
                                                HTML('<b>XII</b>: L00-L99'),
                                                HTML('<b>XIII</b>: M00-M99'),
                                                HTML('<b>XIV</b>: N00-N99'),
                                                HTML('<b>XV</b>: O00-O99'),
                                                HTML('<b>XVI</b>: P00-P96'),
                                                HTML('<b>XVII</b>: Q00-Q99'),
                                                HTML('<b>XVIII</b>: R00-R99'),
                                                HTML('<b>XIX</b>: S00-T98'),
                                                HTML('<b>XX</b>: V01-Y84'),
                                                HTML('<b>XXI</b>: Z00-Z99'),
                                                HTML('<b>XXII</b>: U00-U99'))
                                  ),
                                  options = list(`actions-box` = TRUE,
                                                 `deselect-all-text` = "Auswahl aufheben",
                                                 `select-all-text` = "Alle auswählen",
                                                 `none-selected-text` = "Nichts ausgewählt",
                                                 sanitize=FALSE),multiple=TRUE)
                    })
                    output$ST3_UI2c<-renderUI({NULL})
                    output$FA3_UI2b<-renderUI({NULL})
                    output$FA3_UI2c<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$ST2_ICDKapitel)){
                        input1_fa<-input1_fa[input1_fa$ICDKapitel%in%input$ST2_ICDKapitel,]
                        
                        if(nrow(input1_fa)>0){
                          output$ST3_UI4<-renderUI({dateInput("ST3_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$FA3_UI4<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$ST3_Start)){
                              output$ST3_UI5<-renderUI({dateInput("ST3_Ende","Ende des Beobachtungszeitraums",
                                                                  value=min(as.Date(max(input1_fa$Entlassung)),input$ST3_Start+365),
                                                                  min = input$ST3_Start,
                                                                  max = as.Date(max(input1_fa$Entlassung)),
                                                                  weekstart = 1,language = "de",format="yyyy-mm-dd")})                                   
                              
                              output$FA3_UI5<-renderUI({NULL})
                            }
                          })
                        }
                      }
                    })
                  }else{
                    output$ST3_UI2c<-renderUI({
                      textInput("ST3_ICDExakt",value = NULL,placeholder = "z.B. A00 oder A00.0",label = NULL)
                    })
                    output$ST3_UI2b<-renderUI({NULL})
                    output$FA3_UI2b<-renderUI({NULL})
                    output$FA3_UI2c<-renderUI({NULL})
                    
                    observe({
                      if(!is.null(input$ST3_ICDExakt)&&input$ST3_ICDExakt!=""&&nchar(input$ST3_ICDExakt)>=3){
                        user_icd<-gsub(".","\\.",input$ST3_ICDExakt,fixed=T)
                        input1_fa<-input1_fa[grep(paste0("^",user_icd),input1_fa$ICDfull),]
                        
                        if(nrow(input1_fa)>0){
                          output$ST3_UI4<-renderUI({dateInput("ST3_Start","Beginn des Beobachtungszeitraums",
                                                              value=as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              min = as.Date(min(input1_fa$Aufnahme),format="yyyy-mm-dd"),
                                                              max = as.Date(max(input1_fa$Entlassung),format="yyyy-mm-dd"),
                                                              weekstart = 1,language = "de",format="yyyy-mm-dd")})
                          output$FA3_UI4<-renderUI({NULL})
                          
                          observe({
                            if(!is.null(input$ST3_Start)){
                              output$ST3_UI5<-renderUI({dateInput("ST3_Ende","Ende des Beobachtungszeitraums",
                                                                  value=min(as.Date(max(input1_fa$Entlassung)),input$ST3_Start+365),
                                                                  min = input$ST3_Start,
                                                                  max = as.Date(max(input1_fa$Entlassung)),
                                                                  weekstart = 1,language = "de",format="yyyy-mm-dd")})                                   
                              
                              output$FA3_UI5<-renderUI({NULL})
                            }
                          })
                        }
                      }
                    })
                    
                  }
                }
              }
            })
          }else{
            output$FA3_UI2a<-renderUI({NULL})
            output$FA3_UI2b<-renderUI({NULL})
            output$FA3_UI2c<-renderUI({NULL})
            output$FA3_UI4<-renderUI({NULL})
            output$FA3_UI5<-renderUI({NULL})
            
            output$ST3_UI2a<-renderUI({NULL})
            output$ST3_UI2b<-renderUI({NULL})
            output$ST3_UI2c<-renderUI({NULL})
            output$ST3_UI4<-renderUI({NULL})
            output$ST3_UI5<-renderUI({NULL})
          }
        })
      }
      if(input$FA3_oder_Station=="Stationen"&&is.null(input1b)){
        output$FA_erweitert_UI2<-renderUI({NULL})
        
        output$FA3_UI1<-renderUI({NULL})
        output$FA3_UI2<-renderUI({NULL})
        output$FA3_UI2a<-renderUI({NULL})
        output$FA3_UI2b<-renderUI({NULL})
        output$FA3_UI2c<-renderUI({NULL})
        output$FA3_UI4<-renderUI({NULL})
        output$FA3_UI5<-renderUI({NULL})
        
        output$ST3_UI1<-renderUI({NULL})
        output$ST3_UI2<-renderUI({NULL})
        output$ST3_UI2a<-renderUI({NULL})
        output$ST3_UI2b<-renderUI({NULL})
        output$ST3_UI2c<-renderUI({NULL})
        output$ST3_UI4<-renderUI({NULL})
        output$ST3_UI5<-renderUI({NULL})
        
        output$ST3_intro<-renderText({"Kein Input File mit Fachabteilungsinformationen hochgeladen."})
        output$FA3_intro<-renderText({NULL})
        output$FA3_add1a<-renderUI({NULL})
        output$FA3_add2a<-renderText({NULL})
      }
      
    })
    
    output$FAST3_UI5<-renderUI({actionBttn("do_zusammenfassung",label = "Starte Analyse",style = "gradient",color = "primary",disabled=T)})
    
    observe({
      if((!is.null(input$FA3_Fokus)&&!is.null(input$switch1b)&&(input$switch1b=="nein"|(input$switch1b=="ICD-Kapitel"&&!is.null(input$FA3_ICDKapitel))|(!is.null(input$FA3_ICDExakt)&&input$FA3_ICDExakt!=""&&nchar(input$FA3_ICDExakt)>=3)))||
         (!is.null(input$ST3_Fokus)&&!is.null(input$switch1c)&&(input$switch1c=="nein"|(input$switch1c=="ICD-Kapitel"&&!is.null(input$ST3_ICDKapitel))|(!is.null(input$ST3_ICDExakt)&&input$ST3_ICDExakt!=""&&nchar(input$ST3_ICDExakt)>=3)))){
        if(input$FA2_oder_Station=="Fachabteilungen"&&!is.null(input1a)){
          shinyjs::enable("do_zusammenfassung")
        }
        if(input$FA2_oder_Station=="Fachabteilungen"&&is.null(input1a)){
          shinyjs::disable("do_zusammenfassung")
        }
        if(input$FA2_oder_Station=="Stationen"&&!is.null(input1b)){
          shinyjs::enable("do_zusammenfassung")
        }
        if(input$FA2_oder_Station=="Stationen"&&is.null(input1b)){
          shinyjs::disable("do_zusammenfassung")
        }
      }else{
        shinyjs::disable("do_zusammenfassung")
      }
    })
    
    observeEvent(input$do_zusammenfassung,{
      updateTabsetPanel(session,"main",
                        selected="Ergebnisse")
      if(input$FA3_oder_Station=="Fachabteilungen"){
        input1<-input1a
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer),]
        fa<-sort(unique(input1$Abteilung))
        abteilung<-input$FA3_Fokus
        #input1_fa<-input1[input1$Abteilung==input$FA3_Fokus,]
        FA3_ICD<-23
        
        if(input$switch1b=="ICD-Kapitel"){
          input1<-input1[input1$ICDKapitel%in%input$FA3_ICDKapitel,]
          FA3_ICD<-input$FA3_ICDKapitel
        }
        if(input$switch1b=="exakter ICD-Code"){
          user_icd<-gsub(".","\\.",input$FA3_ICDExakt,fixed=T)
          input1<-input1[grep(paste0("^",user_icd),input1$ICDfull),]
          FA3_ICD<-input$FA3_ICDExakt
        }
      }
      
      if(input$FA3_oder_Station=="Stationen"){
        input1<-input1b
        names(input1)[3]<-"Abteilung"
        input1<-input1[input1$Abteilung!="Patient_abwesend",]
        input1<-input1[order(input1$Fallnummer),]
        fa<-sort(unique(input1$Abteilung))
        abteilung<-input$ST3_Fokus
        #input1_fa<-input1[input1$Abteilung==input$ST3_Fokus,]
        FA3_ICD<-23
        
        if(input$switch1c=="ICD-Kapitel"){
          input1<-input1[input1$ICDKapitel%in%input$ST3_ICDKapitel,]
          FA3_ICD<-input$ST3_ICDKapitel
        }
        if(input$switch1c=="exakter ICD-Code"){
          user_icd<-gsub(".","\\.",input$ST3_ICDExakt,fixed=T)
          input1<-input1[grep(paste0("^",user_icd),input1$ICDfull),] 
          FA3_ICD<-input$ST3_ICDExakt
        }
      }
      
      FA_Fokus<-input$FA3_Fokus
      ST_Fokus<-input$ST3_Fokus
      
      if(input$FA3_oder_Station=="Fachabteilungen"){
        FA_Start<-input$FA3_Start
        FA_Ende<-input$FA3_Ende
      }else{
        FA_Start<-input$ST3_Start
        FA_Ende<-input$ST3_Ende
      }
      
      FA_Start<-as.POSIXct(paste0(FA_Start," 00:00:01"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      FA_Ende<-as.POSIXct(paste0(FA_Ende," 23:59:59"),format="%Y-%m-%d %H:%M:%OS",tz="UTC")
      
      input1_fa_filtered<-input1[as.numeric(input1$Aufnahme)<=as.numeric(FA_Ende)
                                 &as.numeric(input1$Entlassung)>=as.numeric(FA_Start),]
      
      if(input$FA3_oder_Station=="Fachabteilungen"){
        input1_fa_filtered<-input1_fa_filtered[input1_fa_filtered$Fallnummer%in%input1_fa_filtered$Fallnummer[input1_fa_filtered$Abteilung==FA_Fokus],]
      }else{
        input1_fa_filtered<-input1_fa_filtered[input1_fa_filtered$Fallnummer%in%input1_fa_filtered$Fallnummer[input1_fa_filtered$Abteilung==ST_Fokus],]
      }
      
      if(nrow(input1_fa_filtered)>1&&length(unique(input1_fa_filtered$Abteilung))>1){
        input1_fa_filtered$Aufnahme<-as.POSIXct(format(input1_fa_filtered$Aufnahme),tz="CET")
        input1_fa_filtered$Entlassung<-as.POSIXct(format(input1_fa_filtered$Entlassung),tz="CET")
        
        
        input1_fa_filtered$Dauer<-(as.numeric(input1_fa_filtered$Entlassung)-as.numeric(input1_fa_filtered$Aufnahme))/(60*60*24)
        
        daten_1<-data.frame(FA=unique(input1_fa_filtered$Abteilung),Faelle=NA,MittlereDauer=NA)
        
        for(i in 1:nrow(daten_1)){
          temp<-input1_fa_filtered[input1_fa_filtered$Abteilung==daten_1$FA[i],]
          daten_1$Faelle[i]<-length(unique(temp$Fallnummer))
          daten_1$MittlereDauer[i]<-sum(temp$Dauer)/daten_1$Faelle[i]
        }
        
        FA<-daten_1$FA
        
        output_matrix1<-matrix(rep(0,(nrow(daten_1)*nrow(daten_1))),nrow=nrow(daten_1))
        rownames(output_matrix1)<-colnames(output_matrix1)<-FA
        
        progress <- shiny::Progress$new()
        progress$set(message = ifelse(input$FA3_oder_Station=="Fachabteilungen","Analysiere Fachabteilung","Analysiere Station"), value = 0)
        
        for(k in 1:(length(rownames(output_matrix1))-1)){
          #message(k)
          progress$inc(1/(length(rownames(output_matrix1))-1),detail=FA[k])
          for(m in (k+1):length(rownames(output_matrix1))){
            input_fa1<-input1_fa_filtered[input1_fa_filtered$Abteilung==FA[k],]
            input_fa2<-input1_fa_filtered[input1_fa_filtered$Abteilung==FA[m],]
            
            id1<-unique(input_fa1$Fallnummer)
            id2<-unique(input_fa2$Fallnummer)
            
            test1<-input_fa1[input_fa1$Fallnummer%in%id2,]        
            test2<-input_fa2[input_fa2$Fallnummer%in%id1,]
            
            ids<-unique(test1$Fallnummer)
            for(n in ids){
              datum1<-test1$Aufnahme[test1$Fallnummer==n]
              datum2<-test2$Aufnahme[test2$Fallnummer==n]
              datum1b<-test1$Entlassung[test1$Fallnummer==n]
              datum2b<-test2$Entlassung[test2$Fallnummer==n]
              if(length(datum1)==1&&length(datum2)==1){
                if(datum1<datum2&&datum1b==datum2){
                  output_matrix1[k,m]<-output_matrix1[k,m]+1
                }
                if(datum1>datum2&&datum2b==datum1){
                  output_matrix1[m,k]<-output_matrix1[m,k]+1
                }
              }else{
                helper<-data.frame(Datum=c(datum1,datum2),
                                   Entlassung=c(datum1b,datum2b),
                                   FA=c(rep("K",length(datum1)),rep("M",length(datum2))))
                helper<-helper[order(helper$Datum),]
                
                for(o in 1:(nrow(helper)-1)){
                  if(helper$Entlassung[o]==helper$Datum[o+1]&helper$FA[o]=="K"&helper$FA[o+1]=="M"){
                    output_matrix1[k,m]<-output_matrix1[k,m]+1
                  }
                  if(helper$Entlassung[o]==helper$Datum[o+1]&helper$FA[o]=="M"&helper$FA[o+1]=="K"){
                    output_matrix1[m,k]<-output_matrix1[m,k]+1
                  }
                }
              }
            }
          }
        }
        progress$close()
        
        for(k in 1:nrow(output_matrix1)){
          output_matrix1[k,k]<-daten_1$Faelle[k]
        }
        
        if(input$switch1=="von"){
          #von
          if(input$FA3_oder_Station=="Fachabteilungen"){
            output_matrix1<-output_matrix1[output_matrix1[rownames(output_matrix1)==FA_Fokus,]>0,output_matrix1[rownames(output_matrix1)==FA_Fokus,]>0]
          }else{
            output_matrix1<-output_matrix1[output_matrix1[rownames(output_matrix1)==ST_Fokus,]>0,output_matrix1[rownames(output_matrix1)==ST_Fokus,]>0]
          }
        }else{
          if(input$FA3_oder_Station=="Fachabteilungen"){
            output_matrix1<-output_matrix1[output_matrix1[,colnames(output_matrix1)==FA_Fokus]>0,output_matrix1[,colnames(output_matrix1)==FA_Fokus]>0]
          }else{
            output_matrix1<-output_matrix1[output_matrix1[,colnames(output_matrix1)==ST_Fokus]>0,output_matrix1[,colnames(output_matrix1)==ST_Fokus]>0]
          }
        }
        
        output_matrix2<-output_matrix1
        for(k in 1:nrow(output_matrix2)){
          output_matrix2[k,]<-output_matrix2[k,]/output_matrix2[k,k]
        }
        
        output_matrix3<-output_matrix2
        output_matrix3[output_matrix3>0]<-1
        
        net2.bp<-graph_from_adjacency_matrix(output_matrix3,mode = "directed",diag = F)
        
        links1<-as.data.frame(ends(net2.bp,es=E(net2.bp)))
        links1$Source<-links1$V1
        links1$Target<-links1$V2
        nodes1<-data.frame(NodeID=rownames(output_matrix3),ID=seq(0,(nrow(output_matrix3)-1)))
        links1$Source1<-nodes1$ID[match(links1$Source,nodes1$NodeID)]
        links1$Target1<-nodes1$ID[match(links1$Target,nodes1$NodeID)]
        nodes1$NodeSize<-diag(output_matrix1)
        nodes1$NodeSizeb<-1+4*nodes1$NodeSize/max(nodes1$NodeSize)
        
        temp3<-ends(net2.bp,es=E(net2.bp))
        edge_size<-c()
        for(k in 1:length(temp3[,1])){
          zeile<-which(colnames(output_matrix1)==temp3[k,1])
          spalte<-which(colnames(output_matrix1)==temp3[k,2])
          edge_size<-c(edge_size,output_matrix1[zeile,spalte])
        }
        links1$Value<-edge_size
        #links1$Valueb<-5*links1$Value/max(links1$Value)
        links1$Valueb<-NA
        links1$Valueb2<-NA
        links1$Source_Total<-NA
        links1$Source_Anteil<-NA
        links1$Target_Anteil<-NA
        links1$Target_Anteil<-NA
        for(i in 1:nrow(links1)){
          #links1$Valueb[i]<-links1$Value[i]/output_matrix1[rownames(output_matrix1)==links1$Target[i],colnames(output_matrix1)==links1$Target[i]]
          links1$Source_Total[i]<-output_matrix1[rownames(output_matrix1)==links1$Source[i],colnames(output_matrix1)==links1$Source[i]]
          links1$Source_Anteil[i]<-sum(output_matrix1[rownames(output_matrix1)==links1$Source[i],])-links1$Source_Total[i]
          links1$Target_Total[i]<-output_matrix1[rownames(output_matrix1)==links1$Target[i],colnames(output_matrix1)==links1$Target[i]]
          links1$Target_Anteil[i]<-sum(output_matrix1[,colnames(output_matrix1)==links1$Target[i]])-links1$Target_Total[i]
          ifelse(input$switch1=="von",links1$Valueb[i]<-links1$Value[i]/links1$Source_Anteil[i],links1$Valueb[i]<-links1$Value[i]/links1$Target_Anteil[i])
          ifelse(input$switch1=="von",links1$Valueb2[i]<-links1$Value[i]/links1$Source_Total[i],links1$Valueb2[i]<-links1$Value[i]/links1$Target_Total[i])
          #z.b. auf FA_ANAES sind 2479 verschiedene Fälle, aber manche doppelt -> 2793 Fall-Ereignisse
        }
        links1$Valuec<-(1-log(links1$Valueb))
        links1$Valuec2<-(1-log(links1$Valueb2))
        links1$Valued<-0.05*links1$Valuec
        
        
        #write.table(links1,"Test_links.txt",row.names=F,sep="\t",quote=F)
        #write.table(nodes1,"Test_nodes.txt",row.names=F,sep="\t",quote=F)
        #links1_neu<-read.table("./Test_links.txt",header=T,sep="\t")
        #nodes1<-read.table("./Test_nodes.txt",header=T,sep="\t")
        
        if(input$switch1=="von"){
          ##von
          if(input$FA3_oder_Station=="Fachabteilungen"){
            links1<-links1[links1$Source==input$FA3_Fokus,]
          }else{
            links1<-links1[links1$Source==input$ST3_Fokus,]
          }
        }else{
          ##nach
          if(input$FA3_oder_Station=="Fachabteilungen"){
            links1<-links1[links1$Target==input$FA3_Fokus,]
          }else{
            links1<-links1[links1$Target==input$ST3_Fokus,]
          }
        }
        
        for(i in 1:nrow(nodes1)){
          if(input$switch1=="von"){
            if(nodes1$NodeID[i]!=abteilung){
              nodes1$NodeSize2[i]<-links1$Value[nodes1$NodeID[i]==links1$Target&abteilung==links1$Source]
              nodes1$Distanz[i]<-round(links1$Valuec[nodes1$NodeID[i]==links1$Target&abteilung==links1$Source],2)
              nodes1$Distanz2[i]<-round(links1$Valuec2[nodes1$NodeID[i]==links1$Target&abteilung==links1$Source],2)
            }else{
              nodes1$NodeSize2[i]<-unique(links1$Source_Anteil[abteilung==links1$Source])
              nodes1$Distanz[i]<-"-"
              nodes1$Distanz2[i]<-"-"
            }
          }else{
            if(nodes1$NodeID[i]!=abteilung){
              nodes1$NodeSize2[i]<-links1$Value[nodes1$NodeID[i]==links1$Source&abteilung==links1$Target]   
              nodes1$Distanz[i]<-round(links1$Valuec[nodes1$NodeID[i]==links1$Source&abteilung==links1$Target],2)
              nodes1$Distanz2[i]<-round(links1$Valuec2[nodes1$NodeID[i]==links1$Source&abteilung==links1$Target],2)
            }else{
              nodes1$NodeSize2[i]<-unique(links1$Target_Anteil[abteilung==links1$Target])
              nodes1$Distanz[i]<-"-"
              nodes1$Distanz2[i]<-"-"
            }
          }
        }
        nodes1$NodeSizeb<-1+4*nodes1$NodeSize2/max(nodes1$NodeSize2)
        
        
        if(input$switch1=="von"){
          script <-'alert((d.Referenz) + "\\n\\n Fälle insgesamt: " + (d.Referenz_Count) + "\\n Fälle mit Verlegung: " + (d.Referenz_Anteil) + "\\n \\n" + "Von " + (d.Referenz) + " nach " + (d.NodeID) + ": " + (d.Nodes_Value) + "/" + (d.Referenz_Anteil) + "\\n\\nEffektive Distanz:\\n     " + (d.Distanz) + " (basierend auf verlegten Fällen)" + "\\n     " + (d.Distanz2) + " (basierend auf allen Fällen)" );'
        }else{
          script <-'alert((d.Referenz) + "\\n\\n Fälle insgesamt: " + (d.Referenz_Count) + "\\n Fälle mit Verlegung: " + (d.Referenz_Anteil) + "\\n \\n" + "Von " + (d.NodeID) + " nach " + (d.Referenz) + ": " + (d.Nodes_Value) + "/" + (d.Referenz_Anteil) + "\\n\\nEffektive Distanz:\\n     " + (d.Distanz) + " (basierend auf verlegten Fällen)" + "\\n     " + (d.Distanz2) + " (basierend auf allen Fällen)" );'
        }
        
        color_scale <- colorRampPalette(brewer.pal(9, "Set1"))(nrow(nodes1))
        
        fn<-forceNetwork(Links = links1,Nodes = nodes1,NodeID="NodeID",Group = 1,
                         Source="Source1",Target="Target1",zoom=T,opacityNoHover = 1,opacity = 1,Nodesize = "NodeSizeb",
                         radiusCalculation = JS("2.5*d.nodesize"),Value="Valued",charge=-15,
                         linkColour = "#d5d8dc",linkDistance = JS("function(d){return 400*d.value}"),
                         linkWidth = JS("function(d){return 0.1/d.value}"),arrows = T,clickAction = script,
                         colourScale = JS(paste0('d3.scaleOrdinal().range(["', paste(color_scale, collapse = '", "'), '"])')))
        
        
        fn$x$nodes$NodeID<-nodes1$NodeID
        if(input$switch1=="von"){
          fn$x$nodes$Referenz<-unique(links1$Source)
          fn$x$nodes$Referenz_Count<-unique(links1$Source_Total)
          fn$x$nodes$Referenz_Anteil<-unique(links1$Source_Anteil)
          fn$x$nodes$Nodes_Value<-nodes1$NodeSize2
          fn$x$nodes$Distanz<-nodes1$Distanz
          fn$x$nodes$Distanz2<-nodes1$Distanz2
        }else{
          fn$x$nodes$Referenz<-unique(links1$Target)
          fn$x$nodes$Referenz_Count<-unique(links1$Target_Total)
          fn$x$nodes$Referenz_Anteil<-unique(links1$Target_Anteil)
          fn$x$nodes$Nodes_Value<-nodes1$NodeSize2
          fn$x$nodes$Distanz<-nodes1$Distanz
          fn$x$nodes$Distanz2<-nodes1$Distanz2
        }
        
        fn$x$nodes$Nodes_Value[fn$x$nodes$name==unique(fn$x$nodes$Referenz)]<-"-"
        
        
        
        
        output$force <- renderForceNetwork({
          fn
        })
        
        icd_help_out<-c("I: A00-B99",
                        "II: C00-D48",
                        "III: D50-D90",
                        "IV: E00-E90",
                        "V: F00-F99",
                        "VI: G00-G99",
                        "VII: H00-H59",
                        "VIII: H60-H95",
                        "IX: I00-I99",
                        "X: J00-J99",
                        "XI: K00-K93",
                        "XII: L00-L99",
                        "XIII: M00-M99",
                        "XIV: N00-N99",
                        "XV: O00-O99",
                        "XVI: P00-P96",
                        "XVII: Q00-Q99",
                        "XVIII: R00-R99",
                        "XIX: S00-T98",
                        "XX: V01-Y84",
                        "XXI: Z00-Z99",
                        "XXII: U00-U99",
                        "nein")
        
        
        if(input$FA3_oder_Station=="Fachabteilungen"){
          if(input$switch1=="von"){
            output$text_analyse2<-renderUI(HTML(paste0("Verlegungen",
                                                       "<br><br>Von: ",FA_Fokus,
                                                       "<br>Nach: ",paste0(links1$Target,collapse = ", "),
                                                       "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA3_ICD)],collapse=", "),
                                                       "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                       "<br><br>Analyse erfolgreich durchgeführt")))
          }else{
            output$text_analyse2<-renderUI(HTML(paste0("Verlegungen",
                                                       "<br><br>Von: ",paste0(links1$Source,collapse = ", "),
                                                       "<br>Nach: ",FA_Fokus,
                                                       "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA3_ICD)],collapse=", "),
                                                       "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                       "<br><br>Analyse erfolgreich durchgeführt")))
          }
        }else{
          if(input$switch1=="von"){
            output$text_analyse2<-renderUI(HTML(paste0("Verlegungen",
                                                       "<br><br>Von: ",ST_Fokus,
                                                       "<br>Nach: ",paste0(links1$Target,collapse = ", "),
                                                       "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA3_ICD)],collapse=", "),
                                                       "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                       "<br><br>Analyse erfolgreich durchgeführt")))
          }else{
            output$text_analyse2<-renderUI(HTML(paste0("Verlegungen",
                                                       "<br><br>Von: ",paste0(links1$Source,collapse = ", "),
                                                       "<br>Nach: ",ST_Fokus,
                                                       "<br>ICD-Filter: ",paste0(icd_help_out[as.numeric(FA3_ICD)],collapse=", "),
                                                       "<br><br>Beobachtungszeitraum: ",as.Date(FA_Start)," bis ",as.Date(FA_Ende),
                                                       "<br><br>Analyse erfolgreich durchgeführt")))
          }
        }
        output$text_analyse1<-renderUI({NULL})
      }else{
        output$text_analyse1<-renderUI(HTML("Für die gewählte Beobachtungszeit gibt es keine Fälle.<br>
        Bitte wählen Sie eine längere Beobachtungszeit oder einen anderen ICD-Filter."))
        output$text_analyse2<-renderUI({NULL})
        output$force<-renderForceNetwork({NULL})
        return()
      }
      
      rv2<-reactiveValues(nodes1=NULL,links1=NULL,fa=NULL,FA_Start=NULL,FA_Ende=NULL)
      rv2$nodes1<-nodes1
      rv2$links1<-links1
      rv2$fa<-ifelse(input$FA3_oder_Station=="Fachabteilungen",FA_Fokus,ST_Fokus)
      rv2$FA_Start<-FA_Start
      rv2$FA_Ende<-FA_Ende
      
      
      output$FAST3_UI6<-renderUI({downloadButton("do_zusammenfassung_export","xlsx-Export")})
      
      output$do_zusammenfassung_export <- downloadHandler(
        filename = function() {
          paste0("Zusammenfassung_Verlegungen_",ifelse(input$switch1=="von","von_","nach_"),
                 rv2$fa,"_",as.Date(rv2$FA_Start),"_bis_",as.Date(rv2$FA_Ende),".xlsx")
        },
        content = function(file){
          nodes1<-rv2$nodes1
          links1<-rv2$links1
          
          wb<-createWorkbook()
          fa<-nodes1$NodeID[nodes1$Distanz=="-"]
          addWorksheet(wb,sheetName = fa)
          
          export_data<-nodes1[nodes1$Distanz!="-",!colnames(nodes1)%in%c("ID","NodeSize","NodeSizeb")]
          export_data$rel<-paste0(format(round(100*export_data$NodeSize2/nodes1$NodeSize2[nodes1$NodeID==fa],2),nsmall = 2),"%")
          export_data<-export_data[,c(1,2,5,3,4)]
          names(export_data)<-c("V1","V2","V3","V4","V5")
          export_data<-export_data[order(export_data$V4),]
          export_data$V4<-format(as.numeric(export_data$V4),nsmall=2)
          export_data$V5<-format(as.numeric(export_data$V5),nsmall=2)
          
          add_data<-data.frame(V1=c("","Zusammenfassung Verlegungsketten und Kontakte in Krankenhäusern",
                                    ifelse(input$switch1=="von","Von: ","Nach: "),
                                    "ICD-Filter: ",
                                    "Zeitraum: ",
                                    "Fälle insgesamt: ",
                                    "Fälle mit Verlegung: ",
                                    "",
                                    ifelse(input$switch1=="von",
                                           paste0("Verlegt von ",fa," nach "),
                                           paste0("Verlegt nach ",fa," von ")),
                                    ""),
                               V2=c("","",
                                    fa,
                                    paste0(icd_help_out[as.numeric(FA3_ICD)],collapse=", "),
                                    paste0(as.Date(FA_Start)," bis ",as.Date(FA_Ende)),
                                    nodes1$NodeSize[nodes1$NodeID==fa],
                                    nodes1$NodeSize2[nodes1$NodeID==fa],
                                    "",
                                    paste0("#Patienten"),
                                    "absolut"),
                               V3=c(rep("",9),
                                    "relativ"),
                               V4=c(rep("",8),
                                    "Effektive Distanz",
                                    "basierend auf verlegten Fällen"),
                               V5=c(rep("",8),
                                    "",
                                    "basierend auf allen Fällen"))
          
          
          export_data<-rbind(add_data,export_data)
          
          
          insertImage(wb,sheet=fa,file = "www/UKM.png",
                      height = 0.516, width = 0.6,
                      startRow = 1,startCol = 1)
          
          openxlsx::writeData(wb,sheet=fa,export_data,colNames = F,rowNames = F)
          all_white<-createStyle(fgFill="white")
          bold<-createStyle(textDecoration = "bold")
          size<-createStyle(fontSize = 10)
          size_big<-createStyle(fontSize = 14)
          linie<-createStyle(border = "bottom",borderColour = "grey85")
          linie2<-createStyle(border = "bottom",borderStyle = "medium")
          linie3<-createStyle(border = "bottom",borderStyle = "medium",borderColour = "grey85")
          right<-createStyle(halign = "right")
          center<-createStyle(halign = "center")
          hintergrund_grau<-createStyle(fgFill="grey90")
          ueberschrift<-createStyle(fontColour = "white",fgFill = rgb(42/255,77/255,125/255),
                                    fontSize = 14,textDecoration = "bold",valign = "center")
          topalign <- createStyle(valign="top")
          
          addStyle(wb,sheet=fa,rows=c(1:3600),cols=1:27,
                   gridExpand = T,style=all_white)
          
          addStyle(wb,sheet=fa,rows=2,cols=1:5,style = ueberschrift,stack=T)
          addStyle(wb,sheet=fa,rows=c(3:7),cols=1,style = bold,stack=T)
          addStyle(wb,sheet=fa,rows=c(7),cols=1:5,style = linie,
                   gridExpand = T, stack=T)
          
          addStyle(wb,sheet=fa,rows=c(9:10),cols=c(1:5),style = hintergrund_grau,stack=T,gridExpand = T)
          addStyle(wb,sheet=fa,rows=c(9:10),cols=c(1:5),style = bold,stack=T,gridExpand = T)
          addStyle(wb,sheet=fa,rows=c(10),cols=c(1:5),style = linie2,stack=T,gridExpand = T)
          addStyle(wb,sheet=fa,rows=c((nrow(export_data))),cols=c(1:5),style = linie,stack=T,gridExpand = T)
          addStyle(wb,sheet=fa,rows=c(10:(nrow(export_data))),cols=c(2,3,4,5),style = right,stack=T,gridExpand = T)
          addStyle(wb,sheet=fa,rows=c(1),cols=c(5),style = right,stack=T,gridExpand = T)
          addStyle(wb,sheet=fa,rows=c(9),cols=c(2,3),style = center,stack=T,gridExpand = T)
          addStyle(wb,sheet=fa,rows=c(9),cols=c(4,5),style = center,stack=T,gridExpand = T)
          
          mergeCells(wb, fa, rows=2,cols=c(1,2,3))
          mergeCells(wb, fa, rows=9,cols=c(2,3))
          mergeCells(wb, fa, rows=9,cols=c(4,5))
          mergeCells(wb, fa, rows=5,cols=c(2,3))
          
          setRowHeights(wb,sheet=fa,rows = c(2,8),heights = 30)
          setRowHeights(wb,sheet=fa,rows = 1,heights = 43)
          
          setColWidths(wb,sheet=fa,cols = c(1:5),widths = "auto")
          setColWidths(wb,sheet=fa,cols=c(1),widths = 28)
          setColWidths(wb,sheet=fa,cols=c(2,3),widths = 15)
          saveWorkbook(wb,file,overwrite=T)
        }
      )
      
    })
    
    observeEvent(input$switch1,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$FA3_oder_Station,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$FA3_Fokus,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$switch1b,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$FA3_Start,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$FA3_Ende,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$ST3_Fokus,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$switch1c,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$ST3_Start,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    observeEvent(input$ST3_Ende,{
      output$FAST3_UI6<-renderUI({NULL})
    })
    
    
    

    
    })
  })
    
    session$onSessionEnded(function(){
      stopApp()
    })
    
    
}












