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
               DT::dataTableOutput("fullTable")
             )
    ),
    
    tabPanel("Page 2", h2("Quality Control"),
             sidebarLayout(
               #EACH "SELECTION" IS ON ITS OWN LINE EXCEPT sQCG and sQCT, SIDE BY SIDE AND LINKED!
               sidebarPanel(
                 column(12,
                        selectInput("sfactors", label = strong("Select Factors"), 
                                    choices = NULL),
                        
                        actionButton("addFactor", "Add Another Factor"),
                        br(),
                        br(),
                        
                 ),
                 
                 column(12,
                        selectInput("sHK", label = strong("Select House Keeping Genes"), 
                                    choices = NULL),
                        
                        actionButton("addHK", "Add Another HK Gene"),
                        br(),
                        br(),
                 ),
                 fluidRow(
                   column(6,
                          selectInput("sQCG", label = strong("Select QC Genes"), 
                                      choices = NULL),
                          
                          actionButton("addQC", "Add Another QC Gene"), # LINK to sQCT
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
                   ),
                 ),
                 
                 fluidRow(
                   column(6, 
                          numericInput("lowCT", label = strong("Filter Genes With Low CT"), value = 1, min=1, max=15)),
                   
                   column(6, 
                          numericInput("highCT", label = strong("Filter Genes With High CT"), value = 25, min=25, max=40)),
                 ),
                 
                 textOutput("selected")
               ),
               
               mainPanel(
                 DT::dataTableOutput("QCTable")
               ),
               
               # Unneeded as that's the sidebarLayout default (mainPanel is on the right)
               position = c("left", "right")
             ), 
    ),
    
    tabPanel("Page 3", h2("Normalize Data")
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
 rv <- reactiveValues(
   numQCG = 1,
   numQCT = 1
 )
  
  observeEvent(input$addQC, {
    # Increment the number of QC Genes
    rv$numQCG <- rv$numQCG + 1
    
    # Create new input IDs for the additional QC Genes and Types
    newQCG_id <- paste0("sQCG_", rv$numQCG)
    newQCT_id <- paste0("sQCT_", rv$numQCT)
    
    # Create additional QC Genes and Types dropdowns
    insertUI(
      selector = "#addQC",
      where = "afterEnd",
      ui = fluidRow(
        column(6,
               selectInput(newQCG_id, label = strong("Select QC Genes"), 
                           choices = NULL),
               br(),
               br()
        ),
        column(6,
               selectInput(newQCT_id, label = strong("Select QC Types"), 
                           choices = list("Genomic Contamination", 
                                          "PCR Positive", 
                                          "Reverse Transcriptase Control",
                                          "No Template Control")
               )
        )
      )
    )
    
    observeEvent(metaData(), {
      choices <- colnames(metaData())
      updateSelectInput(inputId = "sfactors", choices = choices) 
    })
    
    observeEvent(geneData(), {
      choices <- geneData()$'Well Name'
      updateSelectInput(inputId = "sHK", choices = choices)
    })
    
    observeEvent(geneData(), {
      choices <- geneData()$'Well Name'
      updateSelectInput(inputId = "sQCG", choices = choices)
    })
    
    
     # Update choices for the new QC Genes dropdown
    observeEvent(geneData(), {
      choices <- geneData()$'Well Name'
      updateSelectInput(inputId = newQCG_id, choices = choices)
    }, ignoreInit = TRUE)
    
    # Update choices for the new QC Types dropdown
    observeEvent(geneData(), {
      updateSelectInput(inputId = newQCT_id, choices = geneData()$'Well Name')
    }, ignoreInit = TRUE)
  })
  
  
  # RENDERED OBJECTS
  
  output$fullTable<-DT::renderDataTable({
    fullTable()
  })
  
  # Normalization plot (NEEDS TO BE FIXED)
  output$QCTable<-DT::renderDataTable({
    geneData()
  })
  
  output$selected <- renderText({
    paste(
      "Selected:",
      input$sfactors, ",",
      input$sHK, ",",
      if (rv$numQCG > 1) paste0(input$sQCG, "_", rv$numQCG) else input$sQCG, ",",
      if (rv$numQCT > 1) paste0(input$sQCT, "_", rv$numQCT) else input$sQCT, ",",
      input$lowCT, ",",
      input$highCT
    )
  })
  
}

#### Run the app ####
shinyApp(ui = ui, server = server)

