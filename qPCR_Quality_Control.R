source("Cell_Viability_Libraries_and_Functions.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Quality Control"),
  theme = bs_theme(version = 4, bootswatch = "journal"), br(),
  
  # Sidebar panel for inputs ----
  fluidRow(
    
    column(2,
           selectInput("sfactors", 
                       label = strong("Select Factors"), 
                       choices = list("Site", "Type")
                       )),
  
    column(4,
           selectInput("sHK", 
                       label = strong("Select House Keeping Genes"), 
                       choices = list("RPL4", "EEF1A1")
           )),
  
    column(3, 
           numericInput("highCT", label = strong("Filter Genes With High CT"),  value = 1)),
  
    column(3, 
           numericInput("lowCT", label = strong("Filter Genes With Low CT"),  value = 1)),
    
    )
  )
   

# Define server logic ----
server <- function(input, output) {
  
  }



# Run the app ----
shinyApp(ui = ui, server = server)

# runApp() is the filepath from your working directory to the app’s directory









