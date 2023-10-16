#### Initialize Graphical User Interface (GUI) using Shiny?? ####



#### Libraries ####
library(tidyverse)
library(readxl)


#### 1st step: Import Data #####

# locate files
setwd("Practice Data/2023-10-12 Practice/")
allFiles<-list.files()
metaDataFileName <- "metadata.xlsx"
dataFiles <- allFiles[!allFiles %in% metaDataFileName]


# load files
metadata <- read_excel(metaDataFileName)

rawData <- list()

for(i in dataFiles){
  rawData[[i]] <- read.table(file = i, header = TRUE, sep="\t", stringsAsFactors = FALSE, check.names=FALSE)
}

# combine files into a single table

# A) combine all "rawData files"
        # tidyverse "join" using "Well Name" column
        # transpose?

# B) "map" them to the metadata
        # join using "SampleID" column (with some trickery)


# BONUS CHALLENGE: Load files using Shiny GUI

