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


ui <- fluidPage(
  
  titlePanel("QPCR App"),
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  tabsetPanel(
    tabPanel("Page 1", h2 ("Import Data"),
             
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
                      )
               )
             ),
             
             mainPanel(
               DT::dataTableOutput("mergedTable")
             )
    ),
    
    tabPanel("Page 2", h2("Quality Control"),
             fluidRow(
               column(6,
                      selectInput("sfactors", label = strong("Select Factors"), 
                                  choices = NULL
                      ),
                      
                      actionButton("addFactor", "Add Another Factor"),
                      br(),
                      br(),
                      
                      # WHAT IS THIS FOR? DOESN'T MAKE A DIFFERENCE IF YOU KEEP/REMOVE
                      uiOutput("dynamicButtons")
               ),
               
               column(6,
                      selectInput("sHK", label = strong("Select House Keeping Genes"), 
                                  choices = NULL, selected = NULL #SELECTED NULL DOESN'T WORK, DOESNT GIVE BLANK 
                      ),
                      
                      actionButton("addHK", "Add Another HK Gene"),
                      br(),
                      br(),
               ),
               
               column(6,
                      selectInput("sQC", label = strong("Select QC Genes"), 
                                  choices = NULL 
                      ),
                      
                      actionButton("addQC", "Add Another QC Gene"),
                      br(),
                      br(),
               ),
               
               column(6,
                      selectInput("sQCT", label = strong("Select QC Types"), 
                                  choices = list("Genomic Contamination", 
                                                 "PCR Positive", 
                                                 "Reverse Transcriptase Control",
                                                 "No Template Control")
                      ),
                      
                      actionButton("addQCT", "Add Another QC Type"),
                      br(),
                      br(),
               ),
               
               column(6, 
                      numericInput("highCT", label = strong("Filter Genes With High CT"), value = 25, min=25, max=40)),
               
               column(6, 
                      numericInput("lowCT", label = strong("Filter Genes With Low CT"), value = 1, min=1, max=15)),
             ),
             
             mainPanel(
               textOutput("selected"),
               DT::dataTableOutput("QCTable")
             )
    ),
    
    tabPanel("Page 3", h2("Normalize Data")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
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
  
  # Output ----
  
  meta_input <- reactive({
    req(input$metaFile)
    read_excel(input$metaFile$datapath)
  })
  
  #### To update "sfactors" pulldown menu ----
  # load metadata file into a "reactive" object 
  metadata <- reactive({
    meta_input()
  })
  # update the "sfactors" select input after metadata has been loaded  
  observeEvent(metadata(), {
    choices <- colnames(metadata())
    updateSelectInput(inputId = "sfactors", choices = choices) 
  })
  
  #### To update "sHK" pulldown menu ----
  txt_inputs <- reactive({
    req(input$dataFiles)
    rawList <- lapply(input$dataFiles$datapath, read.table, sep = "\t", header = TRUE, check.names = FALSE)  
    combData<- reduce(rawList, left_join, by = 'Well Name')
  })
  
  dataF <- reactive({
    txt_inputs()
  })
  
  observeEvent(dataF(), {
    # Assuming that all files have the same structure, take the column names from the first file
    choices <- dataF()$'Well Name'
    updateSelectInput(inputId = "sHK", choices = choices)
  })
  
  
  #### To update "sQC" pulldown menu ----
  txt_sQC <- reactive({
    req(input$dataFiles)
    rawList <- lapply(input$dataFiles$datapath, read.table, sep = "\t", header = TRUE, check.names = FALSE)  
    combData<- reduce(rawList, left_join, by = 'Well Name')
  })
  
  dataFsQC <- reactive({
    txt_sQC()
  })
  
  observeEvent(dataFsQC(), {
    # Assuming that all files have the same structure, take the column names from the first file
    choices <- dataFsQC()$'Well Name'
    updateSelectInput(inputId = "sQC", choices = choices)
  })
  
  # Merge and render metadata and Raw qPCR data ----
  output$mergedTable<-DT::renderDataTable({
    meta_input()
    metaPath<-input$metaFile$datapath
    metadata<-read_excel(metaPath)
    
    # WHEN I REMOVE THIS, IT DOESN'T PROVIDE A TABLE ('rawList not found')
    rawList<-list()
    for(i in 1:nrow(input$dataFiles)) {
      lname<-gsub(".txt", "", input$dataFiles$name[i])
      rawList[[lname]] <- read.table(file = input$dataFiles$datapath[i], header = TRUE, sep="\t", stringsAsFactors = FALSE, check.names=FALSE, na.strings = "No Ct")
      rawList[[lname]] <- rawList[[lname]][c("Well Name","Ct (dRn)")]
      colnames(rawList[[i]])[2] <-lname
    }
    
    # WHEN I REMOVE THIS, IT DOESN'T PROVIDE A TABLE ('combData not found')
    combData<- reduce(rawList, left_join, by = 'Well Name')
    
    # WHEN I REMOVE THIS, IT DOESN'T PROVIDE A TABLE (NO VALUES PROVIDED)
    rownames(combData)<-combData$'Well Name'
    combData <- combData %>%
      dplyr::select(-'Well Name') %>%
      t()
    
    # combine with metadata
    # but first, put sample names back into a column
    combData <- data.frame(SampleID=rownames(combData), combData)
    
    # join with metadata
    fullData <- inner_join(metadata, combData, by="SampleID")
  })
  
  # Normalization plot (NEEDS TO BE FIXED)
  output$QCTable<-DT::renderDataTable({
    dataFsQC()
  })
  
  output$selected <- renderText({
    paste("Selected:", input$sfactors, ",", input$sHK, ",", input$sQC, ",", input$sQCT, ",", input$highCT, ",", input$lowCT)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)
