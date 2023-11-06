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
    # Input: Select metadata.xlsx file ----
    fileInput("metadata", "Upload metadata.xlsx File:",
              accept = c(".xlsx")),
    
    fileInput("data", "Upload .txt File(s):",
              accept = c(".txt"),
              multiple = TRUE),
  
    tabsetPanel(type = "tabs",
                tabPanel("Table",  tableOutput("tableO"))
                )
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
  output$tableO <- renderTable({
    req(input$metadata)
    req(input$data)
    inFile <- input$metadata
   
     if (inFile$datapath == "metadata") {
      source("qPCR_script.R")
    } else {
      "Please upload metadata.xlsx and at least one .txt file."
    }
  })
  
 
# Run the app ----
shinyApp(ui = ui, server = server)
