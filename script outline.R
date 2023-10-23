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
  rawData[[i]] <- read.table(file = i, header = TRUE, sep="\t", stringsAsFactors = FALSE, check.names=FALSE, na.strings = "No Ct")
  rawData[[i]] <- rawData[[i]][c("Well Name","Ct (dRn)")]
  colnames(rawData[[i]])[2] <-gsub(".txt", "", i)
}

# combine files into a single table
combData<- reduce(rawData, left_join, by = 'Well Name')

# transpose ....but first move column 1 (well name) into row names
rownames(combData)<-combData$'Well Name'
combData <- combData %>%
  select(-"Well Name") %>%
  t()
  
# combine with metadata
# put sample names back into a column
combData <- data.frame(SampleID=rownames(combData), combData)

# left_join with metadata
fullData <- left_join(metadata, combData, by="SampleID")


#combData["Well Name"]
#combData$`Well Name`





#combinedData <- left_join(rawData[[1]][c("Well Name","Ct (dRn)")], rawData[[2]][c("Well Name","Ct (dRn)")], by = c("Well Name"))




# A) combine all "rawData files"
        # tidyverse "join" using "Well Name" column
        # transpose?

# B) "map" them to the metadata
        # join using "SampleID" column (with some trickery)


# BONUS CHALLENGE: Load files using Shiny GUI

