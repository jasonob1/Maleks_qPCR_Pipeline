library(shiny)


pageButtonUi <- function(id) {
  actionButton(NS(id, "page_change"),
               label="Change the Page")
}


pageButtonServer <- function(id, parentSession) {
  moduleServer(id, function(input, output, session) {
    
    observeEvent(input$page_change, {
      updateNavbarPage(session=parentSession,
                       inputId="pages",
                       selected="second_page")
    })
  })
}


ui <- navbarPage(
  title="test",
  id="pages",
  tabPanel(title="first page",
           sidebarLayout(
             sidebarPanel(
               pageButtonUi("page")
             ),
             mainPanel(
             )
           )
  ),
  tabPanel(title="second_page", "second_page")
)


server <- function(input, output, session) {
  pageButtonServer("page", parentSession = session)
}


shinyApp(ui, server)
#test