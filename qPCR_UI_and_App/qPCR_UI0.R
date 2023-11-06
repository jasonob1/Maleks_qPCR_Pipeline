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
    fileInput("file1", "Upload metadata.xlsx File:",
              accept = c(".xlsx")),
    
    fileInput("data", "Upload .txt File(s):",
              accept = c(".txt"),
              multiple = TRUE),
    
    tabsetPanel(type = "tabs",
                tabPanel("Table",  tableOutput("myTable"))
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
  
  plotInput <- reactive({
    if(input$returnpdf){
      pdf("plot.pdf", width=as.numeric(input$w), height=as.numeric(input$h))
      req(input$file1)
      inFile <- input$file1
      if (input$chemVal != "ALL") {
        genPlot(inFile$datapath,input$chemVal,input$fixedLimit)
      } else {
        chems <- getChems(inFile$datapath)
        genMultPlots(inFile$datapath,chems,input$fixedLimit)
      }
      dev.off()
    }
  })
  
  output$chooseChem <- renderUI({
    req(input$file1)
    inFile <- input$file1
    chems <- getChems(inFile$datapath)
    #### add "ALL" option ####
    chems <- append(chems,'ALL')
    selectInput("chemVal", "Chemical:", chems)
    
  })
  
  output$myTable <- renderTable({
    req(input$file1)
    inFile <- input$file1
    if (input$chemVal != "ALL") {
      out <- genPlot(inFile$datapath,input$chemVal,input$fixedLimit)
      out[[2]]
    } else {
      chems <- getChems(inFile$datapath)
      out <- genMultPlots(inFile$datapath,chems,input$fixedLimit)
      out[[2]]
    }
  })
  
  output$myRawData <- renderTable({
    req(input$file1)
    inFile <- input$file1
    prepData(inFile$datapath)
  })
  
  output$myPlot <- renderPlot({
    
    req(input$file1)
    inFile <- input$file1
    
    if (input$chemVal != "ALL") {
      genPlot(inFile$datapath,input$chemVal,input$fixedLimit)
      plotInput()
    } else {
      chems <- getChems(inFile$datapath)
      genMultPlots(inFile$datapath,chems,input$fixedLimit)
      plotInput()
    }
  })
  
  
  output$exportpdf <- downloadHandler(
    filename <- "myplot.pdf",
    content <- function(file) {
      file.copy("plot.pdf", file)
    }
  )
  
}

# Run the app ----
shinyApp(ui = ui, server = server)

