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
                                  # would this be the right code: choices = list(colnames(output$metaPath))
                                  choices = NULL
                      ),
                      
                      # Initial button
                      actionButton("addFactor", "Add Another Factor"),
                      br(),
                      br(),
                      
                     
               ),
               
               column(6,
                      selectInput("sHK", label = strong("Select House Keeping Genes"), 
                                  #Put all Column names (see if you can get code to know the difference between QC genes, factors, and the *rest of the genes*)
                                  
                                  # QCgenes <- c("Genomic Contamination", "PCR Positive", "No Template Control", "Reverse Transcriptase Control")
                                  # Genes <- #Can use raw Data "Well Name" column (have to save raw data file as an output) 
                                  # Factors <- c("SampleID", "Site") <- #Can pull factors from metadata (sheet 1)
                                  #choices = list(colnames(FullData))
                                  choices = NULL #make it all the columns in gene column name (from reactive object u have to create)
                      ),
                      
                      # Initial button
                      actionButton("addHK", "Add Another HK Gene"),
                      br(),
                      br(),
               ),
               
               column(6, 
                      numericInput("highCT", label = strong("Filter Genes With High CT"),  value = 1)),
               
               column(6, 
                      numericInput("lowCT", label = strong("Filter Genes With Low CT"),  value = 1)),
               
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
  
  #observeEvent(input$addFactor ?
    
  
  
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
  
  # Merge and render metadata and Raw qPCR data ----
  output$mergedTable<-DT::renderDataTable({
    meta_input()
    metaPath<-input$metaFile$datapath
    metadata<-read_excel(metaPath)
  
  #### To update "sHK" pulldown menu ----
  txt_inputs <- reactive({
    req(input$dataFiles)
    #readLines causes the app to shut down
  })
  
  dataF <- reactive({
    txt_inputs()
  })
  
  observeEvent(dataF(), {
    txtF <- colnames(dataF())
    updateSelectInput(inputId = "sHK", choices = txtF)
  })
    
    #From line 154-173 needs to be its own object (make reactivity work somehow) (metadata separate object, gene table separate object, then combine)
    rawList<-list()
    for(i in 1:nrow(input$dataFiles)) {
      lname<-gsub(".txt", "", input$dataFiles$name[i])
      rawList[[lname]] <- read.table(file = input$dataFiles$datapath[i], header = TRUE, sep="\t", stringsAsFactors = FALSE, check.names=FALSE, na.strings = "No Ct")
      rawList[[lname]] <- rawList[[lname]][c("Well Name","Ct (dRn)")]
      colnames(rawList[[i]])[2] <-lname
    }
    
    # combine files into a single table
    combData<- reduce(rawList, left_join, by = 'Well Name')
    
    # transpose ....but first move column 1 (well name) into row names
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
  
}


# Run the app ----
shinyApp(ui = ui, server = server)
