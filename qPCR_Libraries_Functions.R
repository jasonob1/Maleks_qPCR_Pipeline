#### Libraries ####

package_list <- c(
  "tidyverse",
  "readxl",
  "ggplot2",
  "shiny")

#Check for installed libraries + install missing ones
new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

#Load libraries
library(tidyverse) 
library(readxl)
library(ggplot2)
library(shiny)

####Prep Data for Plotting ----
prepData <- function(xl_file) {

# Import Data ####
  rawData <- read_excel(xl_file)
}
  
#### Get Data, Filter, Plot ####
rawData <- prepData(xl_file)





#### OVERALL APP FUNCTIONALITY #####

# LOAD DATA
# QUALITY CONTROL CHECK DATA
# NORMALIZE DATA
#   - house keeping gene normalization
#   - TMM normalization
# Differentially Expressed Gene analysis
# Principal Component Analysis (PCA)
# Heat Maps



####Put inside server once adjusted and understood ----
output$tableO <- renderTable({
  req(input$metadata)
  inFile <- input$metadata
  if (input$tableI = TRUE) {
    out <- genPlot(inFile$datapath)
    out[[2]]
  }
})








