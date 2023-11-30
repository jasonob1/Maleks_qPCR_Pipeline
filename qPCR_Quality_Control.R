library(tidyverse)
library(readxl)
library(shiny)
library(bslib)

# Define UI ----
ui <- fluidPage(
  titlePanel("Quality Control"),
  theme = bs_theme(version = 4, bootswatch = "journal"), br(),
  
  # Sidebar panel for inputs ----
  fluidRow(
    
    column(6,
           selectInput("sfactors", 
                       label = strong("Select Factors"), 
                       # would this be the right code: choices = list(colnames(output$metaPath))
                       choices = list("Site", "Type")
                       ),
          
            # Initial button
           actionButton("addButton", "Add Button"),
           
           # Rendered UI for dynamic buttons
           uiOutput("dynamicButtons")
    ),
  
  
    
    column(6,
           selectInput("sHK", 
                       label = strong("Select House Keeping Genes"), 
                       #Put all Column names (see if you can get code to know the difference between QC genes, factors, and the *rest of the genes*)
                       
  # QCgenes <- c("Genomic Contamination", "PCR Positive", "No Template Control", "Reverse Transcriptase Control")
  # Genes <- #Can use raw Data "Well Name" column (have to save raw data file as an output) 
  # Factors <- c("SampleID", "Site") <- #Can pull factors from metadata (sheet 1)
  
                       #choices = list(colnames(FullData))
                       choices = list("RPL4", "EEF1A1")
           )),
  
    column(6, 
           numericInput("highCT", label = strong("Filter Genes With High CT"),  value = 1)),
  
    column(6, 
           numericInput("lowCT", label = strong("Filter Genes With Low CT"),  value = 1)),
    
    ),
  )
   

# Define server logic ----
server <- function(input, output) {
  
  # Track the number of button clicks
  clickCount <- reactiveVal(1)
  
  # Render additional buttons based on click event
  output$dynamicButtons <- renderUI({
    numButtons <- clickCount()
    
    # Create a list of buttons
    buttons <- lapply(1:numButtons, function(i) {
      actionButton(paste0("dynamicButton", i), paste("Button", i))
    })
    
    # Return the list of buttons
    tagList(buttons)
  })
  
  # Increment click count when the initial button is clicked
  observeEvent(input$addButton, {
    clickCount(clickCount() + 1)
  })
  
  }



# Run the app ----
shinyApp(ui = ui, server = server)
#test
