library(shiny)

ui <- fluidPage(
  
  # First sidebarPanel is test
  sidebarPanel(
    actionButton("instrButton", "Instructions", style = "background-color: black; color: white;")
  ),
  
    sidebarPanel(
      # Initial button
      actionButton("addButton", "Add Button"),
      
      # Rendered UI for dynamic buttons
      uiOutput("dynamicButtons"),
  )
)

server <- function(input, output, session) {
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

shinyApp(ui=ui, server=server)

