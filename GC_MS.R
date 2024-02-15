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
 
    
    tabPanel("page_1", h2("Import Data"),
             
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
                      ),
                      actionButton("page_12", "Next")
               )
             ),
             
             mainPanel(
               DT::dataTableOutput("fullTable")
             )
    ),
    
    tabPanel("page_2", h2("Analysis Options"),
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
                          selectInput("sQCG", label = strong("Select QC Genes"), 
                                      choices = NULL, multiple = TRUE),
                   ),
                   
                   column(6,
                          selectInput("sQCT", label = strong("Select QC Types"), 
                                      choices = list("Genomic Contamination", 
                                                     "PCR Positive", 
                                                     "Reverse Transcriptase Control",
                                                     "No Template Control"),
                                      multiple = TRUE),
                   ),
                 ),
                 
                 fluidRow(
                   column(6, 
                          numericInput("lowCT", label = strong("Low CT Cutoff"), value = 1, min=1, max=15)),
                   
                   column(6, 
                          numericInput("highCT", label = strong("High CT Cutoff"), value = 25, min=25, max=40)),
                 ),
                 
                 actionButton("page_21", "Return to Import Data"),
                 actionButton("page_23", "Proceed to QC Report")
               ),
               
               mainPanel(
                 DT::dataTableOutput("QCTable")
               ),
               
               # Unneeded as that's the sidebarLayout default (mainPanel is on the right)
               position = c("left", "right")
             ), 
    ),
    
    tabPanel("page_3", h2("QC Report"),
             actionButton("page_32", "Prev")
    )
  )
)

#### SERVER ####
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
  
  switch_page <- function(i) {
    updateTabsetPanel(inputId = "switch", selected = paste0("page_", i))
  }
  
  observeEvent(input$page_12, switch_page(2))
  observeEvent(input$page_21, switch_page(1))
  observeEvent(input$page_23, switch_page(3))
  observeEvent(input$page_32, switch_page(2))
  
  
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
  
  
  observeEvent(geneData(), {
    choices <- geneData()$'Well Name'
    updateSelectInput(inputId = "sQCG", choices = choices)
  })
  
  
  # RENDERED OBJECTS
  output$fullTable<-DT::renderDataTable({
    fullTable()
  })
  
  output$QCTable<-DT::renderDataTable({
    geneData()
  })
}

#### Run the app ####
shinyApp(ui = ui, server = server)

