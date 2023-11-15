#Previously qPCR_UI
# check for installed libraries and install any missing ones
list.of.packages <- c(
  "tidyverse",
  "readxl",
  "ecotox",
  "caret",
  "sqldf",
  "tcplfit2",
  "drc",
  "ggplot2",
  "data.table",
  "shiny",
  "shinyscreenshot",
  "refund.shiny",
  "bslib"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load libraries
library(tidyverse)
library(readxl)
library(ecotox)
library(caret)
library(sqldf)
library(tcplfit2)
library(drc)
library(ggplot2)
library(data.table)
library(shiny)
library(shinyscreenshot)
library(refund.shiny)
library(bslib)


#Load the metadata and data files into R like we did for the qPCR_script file 


# Define UI ----
ui <- fluidPage(
  titlePanel("QPCR App"),
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    width = 16, 
  
    # Instructions
    actionButton("label", "Instructions", style = "background-color: black; color: white;"),
    br(),
    br(),
    
    # Input: Select metadata.xlsx file ----
    fileInput("metadata", strong("Upload metadata.xlsx File:"),
              accept = c(".xlsx")),
    
    fileInput("data", strong("Upload .txt File(s):"),
              accept = c(".txt"),
              multiple = TRUE),
  
    actionButton("createtable", "Generate Table"),
    
    absolutePanel(
      bottom = 20, right = 20,
      actionButton("goToQC", "Proceed to Quality Control", style = "background-color: white; color: black;"))
  )
 )

# Define server logic ----
server <- function(input, output) {
  
  observeEvent(input$label, {
    showModal(modalDialog(
      title = "Instructions",
      p("Input 1: Upload a metadata.xlsx file with 3 columns (SampleID, Type, Control)"),
      p("Input 2: Upload .txt files that match the name in metadata's SampleID column. Each .txt file has to contain 'Well Name' and 'Ct (dRn)' columns"),
      easyClose = TRUE,
      footer = tagList(
        actionButton("closeButton", "Close", style = "background-color: black; color: white;")),
      size = "l",
    ))
    observeEvent(input$closeButton, {
      removeModal()
    })
  })
}
  
# Output ----
  output <- NULL
  output$tableO <- NULL
  output$tableO <- renderTable({
    req(input$metadata)
    req(input$data)
    inFile <- input$metadata
   
    #Find out how to store the files being uploaded. They're just being selected at this point
        #Need to show up in the Environment tab ->
    
  })
 

  
# Run the app ----
shinyApp(ui = ui, server = server)
  
  #### OVERALL APP FUNCTIONALITY #####
  # LOAD DATA
  # QUALITY CONTROL CHECK DATA
  # NORMALIZE DATA
  #   - house keeping gene normalization
  #   - TMM normalization
  # Differentially Expressed Gene analysis
  # Principal Component Analysis (PCA)
  # Heat Maps