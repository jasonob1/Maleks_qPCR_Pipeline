#### Libraries ####
library(tidyverse)
library(readxl)



# locate files
setwd("Practice Data\\Laura's Data")
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

# join with metadata
fullData <- inner_join(metadata, combData, by="SampleID")
