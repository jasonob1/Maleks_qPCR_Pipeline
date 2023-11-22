library(shiny)

ui <- fluidPage(
  actionButton("QCG", "QC Gene"),
  actionButton("QCT", "QC Type"), 
  hr(),
  plotOutput("plot")
)

server <- function(input, output){
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$QCG, {
    v$data <- QCG(100)
  })
  
  observeEvent(input$QCT, {
    v$data <- QCT(100)
  })  
  
  output$plot <- renderPlot({
    if (is.null(v$data)) return()
    hist(v$data)
  })
}

shinyApp(ui, server)