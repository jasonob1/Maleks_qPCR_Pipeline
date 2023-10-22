####Libraries####
library(tidyverse)
library(readxl)

####Task A: Combine All "rawData Files"####
    #locate files
setwd("C:\\Users\\maloo\\OneDrive\\Desktop\\GitHub\\Maleks_qPCR_Pipeline\\Practice Data\\2023-10-12 Practice")


#How would I set it so you can access it from your side? ^






allfiles <- list.files()
metaDataFileName <- "metadata.xlsx"
datafiles <- allfiles[!allfiles %in% metaDataFileName]


#Does the %in% mean inside? Google: used to identify an element, used for matching values




    #load files 
metadata <- read_excel(metaDataFileName)
rawData <- list()
for(i in dataFiles){
  rawData[[i]] <- read.table(file = i, header = TRUE, sep="\t", stringsAsFactors = FALSE, check.names=FALSE)
}

  #Combine all "rawData Files"
selected_columns <- map(rawData, ~ .x[["Well Name"]])
combined_data2 <- bind_cols(selected_columns)