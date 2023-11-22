library(shiny)
library(shinyjs)

NUM_PAGES <- 5

ui <- fluidPage(
  useShinyjs(),
  hidden(
    lapply(seq(NUM_PAGES), function(i) {
      if(i==1){
      div(
        class = "page",
        id = paste0("step", i),
        "PAGEA", i
      )} else { 
        div(
          class = "page",
          id = paste0("step", i),
          "Step", i)
      }
    })
  ),
  br(),
  actionButton("prevBtn", "< Previous"),
  actionButton("nextBtn", "Next >")
)

server <- function(input, output, session) {
  rv <- reactiveValues(page = 1)
  
  observe({
    toggleState(id = "prevBtn", condition = rv$page > 1)
    toggleState(id = "nextBtn", condition = rv$page < NUM_PAGES)
    hide(selector = ".page")
    show(paste0("step", rv$page))
  })
  
  navPage <- function(direction) {
    rv$page <- rv$page + direction
  }
  
  observeEvent(input$prevBtn, navPage(-1))
  observeEvent(input$nextBtn, navPage(1))
}

shinyApp(ui, server)


# Implement next and previous buttons 
# Have navigation tabs (page 1 and 2 of the other script, the UI0)
# Link the next/previous step to the upcoming page



# See if when you run the app, it saves in the Rstudio environment -> 
# And if there's a way to have the app is running and update the changes real time


