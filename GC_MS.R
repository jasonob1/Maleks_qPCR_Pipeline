list.of.packages <- c(
  "tidyverse",
  "read_xl",
  "shiny",
  "bslib",
  "data.table"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages) 

library(tidyverse)
library(readxl)
library(shiny)
library(bslib)
library(data.table)

#### UI ####
ui <- fluidPage(
  
  titlePanel("QPCR App"),
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  tabsetPanel(
    id = "switch",
 
    
    tabPanel("Import Data",
             
             fluidRow(
               column(width = 12,
                      sidebarPanel(
                        style = "height: 350px",
                        width = 16, 
                        
                        actionButton("instrButton", "Instructions", style = "background-color: black; color: white;"),
                        br(),
                        br(),
                        
                        fileInput("metaFile", strong("Upload metadata.xlsx File:"),
                                  accept = c(".xlsx")),
                        
                        fileInput("dataFiles", strong("Upload .txt File(s):"),
                                  accept = c(".txt"),
                                  multiple = TRUE),
                      fluidRow(
                        column(width = 12, align = "right",
                        actionButton("page_12", "Proceed to Analysis Options")
                      )
                    )
                   ),
                      
               )
             ),
             
             mainPanel(
               DT::dataTableOutput("fullTable")
             )
    ),
    
    tabPanel("Analysis Options",
             sidebarLayout(
               sidebarPanel(
                 column(12,
                        selectInput("sfactors", label = strong("Select Factors"), 
                                    choices = NULL, multiple = TRUE)
                 ),
                 
                 
                 column(12,
                        selectInput("sHK", label = strong("Select House Keeping Genes"), 
                                    choices = NULL, multiple = TRUE),
                        actionButton("reset", "Reset")
                 ),
                 
                 fluidRow(
                   column(6,
                          selectInput("sGC", label = strong("Genomic Contamination"), 
                                      choices = NULL, multiple = TRUE)),
                   
                     column(6,
                          selectInput("sPP", label = strong("PCR Positive"), 
                                        choices = NULL, multiple = TRUE)),
                        ),
                   
                   
                  fluidRow(
                    column(6,
                        selectInput("sRTC", label = strong("Reverse Transcriptase Control"), 
                                          choices = NULL, multiple = TRUE)),
                    
                    column(6,
                        selectInput("sNTC", label = strong("No Template Control"), 
                                            choices = NULL, multiple = TRUE)),
                         ),
                 
                 fluidRow(
                   column(6, 
                          numericInput("lowCT", label = strong("Low CT Cutoff"), value = 1, min=1, max=15)),
                   
                   column(6, 
                          numericInput("highCT", label = strong("High CT Cutoff"), value = 25, min=25, max=40)),
                 ),
                 
                 
                 fluidRow(
                 column (6, actionButton("page_21", "Return to Import Data"), align = "left"),
                 column (6, actionButton("page_23", "Proceed to QC Report"), align = "right"),
                 )
                 
                 ),
               
               mainPanel(),
             ), 
    ),
    
    tabPanel("QC Report",
             
             h3("QC Summary"),
             mainPanel(tableOutput("RTable")),
             
             h3("QC Details"),
             
             h5("Failed PPC Samples"),
             mainPanel(tableOutput("failedPcrTable")),
             
             h5("Failed RTC Samples"),
             mainPanel(tableOutput("failedRtcTable")),
             
             h5("Failed NTC Samples"),
             mainPanel(tableOutput("failedNtcTable")),
             
             h5("Failed GCC Samples"),
             mainPanel(tableOutput("failedGcTable")),
             
             actionButton("page_32", "Return to Analysis Options")
    )
  )
)

#### SERVER ####
server <- function(input, output, session) {
  
  observeEvent(input$instrButton, {
    showModal(modalDialog(
      title = "Instructions",
      p("Input 1: Upload a metadata.xlsx file with 3 columns (SampleID, Type, Control)"),
      p("Input 2: Upload .txt files that match the name in metadata's SampleID column. Each .txt file has to contain 'Well Name' and 'Ct (dRn)' columns"),
      easyClose = TRUE,
      footer = tagList(
        actionButton("instrCloseButton", "Close", style = "background-color: black; color: white;")),
      size = "l",
    ))
    observeEvent(input$instrCloseButton, {
      removeModal()
    })
  })
  
 
  switch_page <- function(tab_name) {
    updateTabsetPanel(inputId = "switch", selected = tab_name)
  }
  
  observeEvent(input$page_12, {switch_page("Analysis Options")})
  observeEvent(input$page_21, {switch_page("Import Data")})
  observeEvent(input$page_23, {switch_page("QC Report")})
  observeEvent(input$page_32, {switch_page("Analysis Options")})
  
  
  # LOAD DATA ----
  # Load metaData
  metaData <- reactive({
    req(input$metaFile)
    read_excel(input$metaFile$datapath)
  })
  
  #load geneData
  geneData <- reactive({
    req(input$dataFiles)
    rawList<-list()
    for(i in 1:nrow(input$dataFiles)) {
      lname<-gsub(".txt", "", input$dataFiles$name[i])
      rawList[[lname]] <- read.table(file = input$dataFiles$datapath[i], header = TRUE, sep="\t", stringsAsFactors = FALSE, check.names=FALSE, na.strings = "No Ct")
      rawList[[lname]] <- rawList[[lname]][c("Well Name","Ct (dRn)")]
      colnames(rawList[[i]])[2] <-lname
    }
    combData<- reduce(rawList, left_join, by = 'Well Name')
  })
  
  # Join metaData and geneData into one table
  fullTable<-reactive({
    
    # Move gene names into row names so that they become column names when we transform table
    tempGeneData <- geneData()
    rownames(tempGeneData)<-tempGeneData$'Well Name'
    tempGeneData <- tempGeneData %>%
      dplyr::select(-'Well Name') %>%
      t() 
    
    # Move "sample IDs" (which are currently the new row names) into a column called "SampleID"
    tempGeneData <- data.frame(SampleID=rownames(tempGeneData), tempGeneData)
    
    # join with metaData
    fullData <- inner_join(metaData(), tempGeneData, by="SampleID")
  })
  
  
  # OBSERVE EVENTS ----
  observeEvent(metaData(), {
    choices <- colnames(metaData())
    updateSelectInput(inputId = "sfactors", choices = choices) 
  })
  
  observeEvent(geneData(), {
    choices <- geneData()$'Well Name'
    updateSelectInput(inputId = "sHK", choices = choices) 
  })
  
  observeEvent(input$reset, {
    # Reset the selected options by setting choices to an empty set
    updateSelectInput(inputId = "sHK", selected = character(0))
  })
  
  
  input_ids <- c("sGC", "sPP", "sRTC", "sNTC")
  update_select_input <- function(input_id) {
    observe({
      choices <- c("None", geneData()$'Well Name')
      updateSelectInput(session, input_id, choices = choices)
    })
  }
  
  # Apply the function for each input ID
  lapply(input_ids, update_select_input)
  
  
  
  # RENDERED OBJECTS
  output$fullTable<-DT::renderDataTable({
    fullTable()
  })
  
  
  fPCR <- reactiveVal()
  fRTC <- reactiveVal()
  fNTC <- reactiveVal()
  fGC <- reactiveVal()
  
  
  #Page 3 Tables
  RTable <- reactive({
    highCT <- input$highCT
    pcrTable <- fullTable() %>% select(all_of(input$sPP))
    
    #checks all values of pcrTable pass
    pcrPositiveFull <- pcrTable[input$sPP] < highCT & !(pcrTable[input$sPP] == "" | is.na(pcrTable[input$sPP])) 
    
    # identify which rows have failed values
    pcrPositiveFailedRows <- apply(pcrPositiveFull,1, FUN=function(x){all(x)})
    
    # create summary table with only failed samples
    pcrPositiveSamples <- fullTable() %>% select(SampleID, all_of(input$sPP))
    pcrPositiveFailedDetails <- pcrPositiveSamples[!pcrPositiveFailedRows,]
    
    #Updates fPCR values
    fPCR(pcrPositiveFailedDetails)
    
    pcrResult <- if (all(pcrPositiveFull)) "PASS" else "FAIL" 
    
    
    rtcTable <- fullTable() %>% select(all_of(input$sRTC))
    rtcPositiveFull <- rtcTable[input$sRTC] < highCT & !(rtcTable[input$sRTC] == "" | is.na(rtcTable[input$sRTC]))
    rtcPositiveFailedRows <- apply(rtcPositiveFull,1, FUN=function(x){all(x)})
    rtcPositiveSamples <- fullTable() %>% select(SampleID, all_of(input$sRTC))
    rtcPositiveFailedDetails <- rtcPositiveSamples[!rtcPositiveFailedRows,]
    fRTC(rtcPositiveFailedDetails)
    rtcResult <- if (all(rtcPositiveFull)) "PASS" else "FAIL" 
    
    
    ntcTable <- fullTable() %>% select(all_of(input$sNTC))
    ntcPositiveFull <- ntcTable[input$sNTC] > highCT & !(ntcTable[input$sNTC] == "" | is.na(ntcTable[input$sNTC]))
    ntcPositiveFailedRows <- apply(ntcPositiveFull,1, FUN=function(x){all(x)})
    ntcPositiveSamples <- fullTable() %>% select(SampleID, all_of(input$sNTC))
    ntcPositiveFailedDetails <- ntcPositiveSamples[!ntcPositiveFailedRows,]
    fNTC(ntcPositiveFailedDetails)
    ntcResult <- if (all(ntcPositiveFull)) "PASS" else "FAIL"
    

    gcTable <- fullTable() %>% select(all_of(input$sGC))
    gcPositiveFull <- gcTable[input$sGC] > highCT & !(gcTable[input$sGC] == "" | is.na(gcTable[input$sGC]))
    gcPositiveFailedRows <- apply(gcPositiveFull,1, FUN=function(x){all(x)})
    gcPositiveSamples <- fullTable() %>% select(SampleID, all_of(input$sGC))
    gcPositiveFailedDetails <- gcPositiveSamples[!gcPositiveFailedRows,]
    fGC(gcPositiveFailedDetails)
    gcResult <- if (all(gcPositiveFull)) "PASS" else "FAIL"
    
    
    tibble(
      "Control Type" = c("PCR Positive Control", 
                         "Reverse Transcription Control", 
                         "No Template Control", 
                         "Genomic Contamination Control"),
      
      "Purpose" = c("To test if your PCR reactions worked",
                    "To test if your RT reactions worked", 
                    "Checks for RNA Contamination",
                    "Checks for DNA Contamination"),
      
      "Pass Criteria" = c("Ct < High Ct Cutoff",
                          "Ct < High Ct Cutoff",
                          "Ct > High Ct Cutoff or No Ct",
                          "Ct > High Ct Cutoff or No Ct"),
      
      "Result" = c(pcrResult, rtcResult, ntcResult, gcResult) 
    )
  })

  
  output$RTable <- renderTable({
    RTable()
  })
  
  output$failedPcrTable <- renderTable({
    fPCR()
  })
  
  output$failedRtcTable <- renderTable({
    fRTC()
  })
  
  output$failedNtcTable <- renderTable({
    fNTC()
  })
  
  output$failedGcTable <- renderTable({
    fGC()
  })
  
}

#### Run the app ####
shinyApp(ui = ui, server = server)

