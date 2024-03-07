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
             mainPanel(tableOutput("FPPCTable")),
             
             h5("Failed RTC Samples"),
             mainPanel(tableOutput("RTCTable")),
             
             h5("Failed NTC Samples"),
             mainPanel(tableOutput("NTCTable")),
             
             h5("Failed GCC Samples"),
             mainPanel(tableOutput("GCCTable")),
             
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
  
  
  
  
  #Page 3 Tables
  # RTable with custom data
  RTable <- reactive({
    highCT <- input$highCT
    
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
      
      "Result" = c(fullTable() |> select(all_of(input$sPP)),
                   "NA"
                  ) 
                  #make QC Detail tables first, and just have the Result display everything (don't need to have it test things here)
    )
  })

  
  
  # Render empty table
  output$RTable <- renderTable({
    RTable()
  })
  
  
  detailTable <- reactive({
    tibble(
      "Sample ID" = c("NA"),
      "Gene Name" = c("NA"),
      "Ct" = c("NA"))
    #fullTable %>%
      #select(SampleID, all_of(output$sPP)) %>% #choose what column u want, keep SampleID hardcoded but the other column needs to be universal 
      #filter() #keeps what rows u want (code needs to keep rows that are higher than CT cutoff)
    
  })
  
  # FPPCTable with custom data + reactivity
  #FPPCTable <- reactive({
    #tibble(
    #"Sample ID" = c("NA"),
    #"Well ID" = c("NA"),
    #"Ct" = c("NA")
    #)
  #})
  
  output$FPPCTable <- renderTable({
    detailTable()
  })
  
  output$RTCTable <- renderTable({
    detailTable()
  })
  
  output$NTCTable <- renderTable({
    detailTable()
  })
  
  output$GCCTable <- renderTable({
    detailTable()
  })
  
}

#### Run the app ####
shinyApp(ui = ui, server = server)

