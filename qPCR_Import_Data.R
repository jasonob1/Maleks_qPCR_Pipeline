#Previously qPCR_UI
source("Cell_Viability_Libraries_and_Functions.R")

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