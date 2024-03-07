library(tidyverse)
library(readxl)
library(shiny)
library(bslib)
library(data.table)


ui <- fluidPage(
  
  titlePanel("QPCR App"),
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  
  tabPanel("Import Data",
           
           fluidRow(
             column(width = 12,
                    sidebarPanel(
                      style = "height: 350px",
                      width = 16, 
                      
                      fileInput("metaFile", strong("Upload metadata.xlsx File:"),
                                accept = c(".xlsx")),
                      
                      fileInput("dataFiles", strong("Upload .txt File(s):"),
                                accept = c(".txt"),
                                multiple = TRUE),
                    ),
                    
             )
           ),
           
           mainPanel(
             DT::dataTableOutput("fullTable")
           )
  ),
  
  tabPanel("Analysis Options",
           fluidRow(
             column(12,
                    selectInput("sPP", label = strong("PCR Positive"), 
                                choices = NULL, multiple = TRUE)),
             
             column(12,
                    selectInput("sRTC", label = strong("Reverse Transcriptase Control"), 
                                choices = NULL, multiple = TRUE)),
             
             column(12, numericInput("highCT", label = strong("High CT Cutoff"), value = 25, min=25, max=40))
             
           ) 
  ),
  
  tabPanel("QC Report",
           
           h3("QC Summary"),
           mainPanel(tableOutput("RTable")),
           
           h3("QC Details"),
           
           h5("Failed PPC Samples"),
           mainPanel(tableOutput("FPPCTable")),
           
  )
)


server <- function(input, output, session) {
  
  
  metaData <- reactive({
    req(input$metaFile)
    read_excel(input$metaFile$datapath)
  })
  
  
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
  
  
  fullTable<-reactive({
    
    
    tempGeneData <- geneData()
    rownames(tempGeneData)<-tempGeneData$'Well Name'
    tempGeneData <- tempGeneData %>%
      dplyr::select(-'Well Name') %>%
      t() 
    
    
    tempGeneData <- data.frame(SampleID=rownames(tempGeneData), tempGeneData)
    
    fullData <- inner_join(metaData(), tempGeneData, by="SampleID")
  })
  
  
  observeEvent(geneData(), {
    choices <- c("None", geneData()$'Well Name')
    updateSelectInput(inputId = "sPP", choices = choices) 
  })
  
  observeEvent(geneData(), {
    choices <- c("None", geneData()$'Well Name')
    updateSelectInput(inputId = "sRTC", choices = choices) 
  })
  
  
  output$fullTable<-DT::renderDataTable({
    fullTable()
  })
  
  
  RTable <- reactive({
    highCT <- input$highCT
    
    
    pcr_table = fullTable() |> select(all_of(input$sPP))
    pcrPositive <- all(pcr_table[input$sPP] < highCT)
    #pcrEmpty <- any(selected_table[input$sPP] =="" | is.na(selected_table[input$sPP]))
    pcrResult <- if (pcrPositive) "PASS" else "FAIL" 
    # NEED TO MAKE SOMETHING FOR THE EMPTY CELLS (EMPTY > highCT) (why doesn't 2 lines above work?)
    
    rtc_table = fullTable() |> select(all_of(input$sRTC))
    rtcPositive <- all(rtc_table[input$sRTC] < highCT)
    rtcResult <- if (rtcPositive) "PASS" else "FAIL" 
    
    
    print(pcr_table)
    print(pcr_table[input$sPP] < highCT)
    
    
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
      
      "Result" = c(pcrResult, rtcResult, T, F) 
    )
  })
  
  
  output$RTable <- renderTable({
    RTable()
  })
  
  
  detailTable <- reactive({
    tibble(
      "Sample ID" = c("NA"),
      "Gene Name" = c("NA"),
      "Ct" = c("NA"))
    
  })
  
  
  output$FPPCTable <- renderTable({
    detailTable()
  })
  
}


shinyApp(ui = ui, server = server)
