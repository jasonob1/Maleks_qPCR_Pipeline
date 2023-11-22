#### check for installed libraries and install any missing ones ####
list.of.packages <- c(
  "tidyverse",
  "read_xl",
  "shiny",
  "bslib"
)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load libraries
library(tidyverse)
library(readxl)
library(shiny)
library(bslib)


# Define UI ----
ui <- fluidPage(
  titlePanel("QPCR App"),
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  # fluidRow contains 12 columns, in this case 1 column taking 12 spaces ----
  fluidRow(
    column(width = 12,
           sidebarPanel(
             style = "height: 350px",
             width = 16, 
             
             # Instructions
             actionButton("instrButton", "Instructions", style = "background-color: black; color: white;"),
             br(),
             br(),
             
             # Input: Select metadata.xlsx file ----
             fileInput("metaFile", strong("Upload metadata.xlsx File:"),
                       accept = c(".xlsx")),
             
             fileInput("dataFiles", strong("Upload .txt File(s):"),
                       accept = c(".txt"),
                       multiple = TRUE),
             
             absolutePanel(
               bottom = 10, right = 20,
               actionButton("goToQC", "Proceed to Quality Control", style = "background-color: white; color: black;"))
           )
    )
  ),
  
  # mainPanel is outside fluidRow so it takes the full width below the sidebar ----
  mainPanel(
    tableOutput("mergedTable")
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
  output$mergedTable<-renderTable({
    req(input$metaFile)
    req(input$dataFiles)
    metaPath<-input$metaFile$datapath
    metadata<-read_excel(metaPath)
    
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




#### OVERALL APP FUNCTIONALITY #####
# LOAD DATA
# QUALITY CONTROL CHECK DATA
# NORMALIZE DATA
#   - house keeping gene normalization
#   - TMM normalization
# Differentially Expressed Gene analysis
# Principal Component Analysis (PCA)
# Heat Maps