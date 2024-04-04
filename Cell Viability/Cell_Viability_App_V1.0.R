source("Cell_Viability_Libraries_and_Functions.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Cell Viability Curve App"),
  theme = bs_theme(version = 4, bootswatch = "lux"),
  
  # Sidebar panel for inputs ----
  sidebarPanel(
    width = 12,
    # Instructions
    actionButton("guide", "Instructions"),
    # Input: Select an .xlsx file ----
    fileInput('file1', 'Upload .xlsx File:',
              accept = c(".xlsx")),
    # Input: Choose Chemical ----
    uiOutput("chooseChem"),
    #textInput("chemVal", "Chemical Name", "FR3"),
    # Input: Fixed Limit ----
    checkboxInput("fixedLimit", "Fixed Upper/Lower Response Limits to 100% and 0%", TRUE),
    # Output: Export plot as .pdf ----
    checkboxInput('returnpdf', 'Export Plot as PDF', FALSE),
    conditionalPanel(
      condition = "input.returnpdf == true",
      strong("PDF size (inches):"),
      sliderInput(inputId="w", label = "width:", min=3, max=20, value=8, width=100, ticks=F),
      sliderInput(inputId="h", label = "height:", min=3, max=20, value=6, width=100, ticks=F),
      br(),
      downloadLink('exportpdf')),
    # Output: Export plot as .png ----
    screenshotButton(label = 'Export Table as PNG', id = "myTable", filename = paste('table',format(Sys.Date(), "%Y-%m-%d")))
  ),
  mainPanel(
    width = 12,
    # Output: Tabset w/ plot, table, and raw data ----
    tabsetPanel(type = "tabs",
                tabPanel("Plot", plotOutput("myPlot")),
                tabPanel("Table", tableOutput("myTable")),
                tabPanel("Raw Data", tableOutput("myRawData"))
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  observeEvent(input$guide, {
    showModal(modalDialog(
      title = "Instructions",
      p("Input 1: Upload an .xlsx file with 3 columns (experiment, dose_uM, lum) by pressing the 'Browse' button"),
      p("Input 2: Choose a chemical name in the dropdown menu"),
      p("Optional: Uncheck Fixed Upper/Lower Response Limits box to remove plot limits"),
      p("Output 1: Export table as PNG (Press the button)"),
      p("Output 2: Export plot as PDF (Check the box -> Resize plot -> Click 'Download' link)"),
      easyClose = TRUE,
      footer = NULL
    ))
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

