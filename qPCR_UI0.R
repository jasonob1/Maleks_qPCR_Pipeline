source("Cell_Viability_Libraries_and_Functions.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("QPCR App"),
  theme = bs_theme(version = 4, bootswatch = "journal"),
  
  # Sidebar panel for inputs ----
  tabsetPanel(
    
    tabPanel("Page 1", h2 ("Import Data"),
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
          ),
    
    tabPanel("Page 2", h2("Quality Control"),
             fluidRow(
               
               column(2,
                      selectInput("sfactors", 
                                  label = strong("Select Factors"), 
                                  choices = list("Site", "Type"))),
               
               column(4,
                      selectInput("sHK", 
                                  label = strong("Select House Keeping Genes"), 
                                  choices = list("RPL4", "EEF1A1"))),
               
               column(3, 
                      numericInput("highCT", 
                                   label = strong("Filter Genes With High CT"),  
                                   value = 1)),
               
               column(3, 
                      numericInput("lowCT", 
                                   label = strong("Filter Genes With Low CT"),  
                                   value = 1)),
               
      )
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
output$tableO <- NULL
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

#Make a button that says "Generate Table" and "Proceed to Quality Control"

# Run the app ----
shinyApp(ui = ui, server = server)

# runApp() is the filepath from your working directory to the app’s directory

