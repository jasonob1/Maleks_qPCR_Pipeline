library(tidyverse)
library(readxl)
library(EnvStats) # for geoMean function
library(ggpubr)
library(edgeR)
library(ggfortify)
library(ggthemes)
library(factoextra)

source("qPCR_pipeline_functions.R")

#### CURATE DATA ####
curData <- fullTable()    # make a copy to manipulate

# identify total number of remaining samples
nSamples<- curData %>% nrow() #MS: COUNTING ROWS


#### CURATE FACTORS ####

# identify "experimental factors" (to differentiate from gene columns)
expFactors <- sfactors #MS: IDENTIFYING FACTORS (ALREADY DID THIS BUT DID NOT HARDWIRE)


#Purpose??
# covert to factors and specify any orders
curData <- curData %>% 
  mutate(site = factor(site)) #MS: CONVERTS THE SELECTED EXPERIMENTAL FACTORS INTO FACTORS
#?

#DIDN'T TOUCH
#  ANOVA factors (to use for statistical analysis)
aovFactor <- c("site") #MS: MIGHT NOT USE THIS, WE'LL SEE (IF WE DON'T, MAKE SURE TO CHANGE A LOT IN THE SCRIPT SO IT WORKS)
#DIDN'T TOUCH


#### CURATE GENES ####

# Identify Housekeeping Genes
hkGenes <- sHK #MS: IT'S HARDCODED, CHANGE SO IT GOES BASED OFF WHAT'S SELECTED 

# identy and remove QC genes
qcGenes<-c("GGDC", "RTC", "PPC") # ? MS: How does it look like? Am I supposed to write it the way it appears in drop down menu, menu heading, or saved in the code
curData<-cleanQC(curData, qcGenes) #MS: ORGANIZES THE TABLE IN A CERTAIN WAY

# Identifies all of the "Gene" columns "ctCols" function # RUN EVERYTIME
rawCtCols<-names(curData)[!names(curData)%in%c("sample", "experiment", "type", "flagNoCt", "flagHigh", expFactors)] #MS: IDENTIFIES EVERY COLUMN THAT ARE GENES (NOT SAMPLE_ID, NOT SITE), LIKE sHK
#MS:I think we did this on line 177-187 in GC_MS

# reorder genes alphabetically (easier to look through final results)
gOrder<-ctCols(curData) %>%
  order() %>% #MS: MAKES THE GENE LIST ALPHABETICAL ORDER
  ctCols(curData)[.] #MS: THAT'S A CUSTOM FUNCTION (ctCols), YOU'LL FIND IT IN THE OTHER SCRIPT, IT JUST TAKES THE LIST OF GENES THAT WE DID NOT USE (rawCtCols so line 63)
curData <- curData %>%
  select_at(c("sample", "experiment", expFactors, gOrder)) #MS: I don't have "sample" and "experiment" in GC_MS, what exactly are we looking at


#### FILTER GENES ####

# identify genes with ct > "highCt" or NA (No Ct) in > y% of samples
#highCt<-36 MS: I have the "reactive" highCT 
highCT_proportion<-0.50 
highGenes<- curData %>%
  dplyr::select(ctCols(.)) %>%
  #apply(MARGIN=2, FUN=function(x) (sum(x>highCt|is.na(x))/nSamples)>highCt_proportion) %>%
  apply(MARGIN=2, FUN=function(x) (sum(x>highCT|is.na(x))/nSamples)>highCT_proportion) %>% #MS: changed Ct --> CT
  which() %>%
  names()
highGenes

# remove high Ct genes
curData <- curData %>% dplyr::select(-all_of(highGenes))

# identify genes with ct < "lowCt" or NA (No Ct) in >y% of samples
#lowCt<-14 MS: I have the "reactive" lowCT
lowCT_proportion<-0.50
lowGenes<- curData %>%
  dplyr::select(ctCols(.)) %>%
  #apply(MARGIN=2, FUN=function(x) (sum(x<lowCt|is.na(x))/nSamples)>lowCt_proportion ) %>%
  apply(MARGIN=2, FUN=function(x) (sum(x<lowCT|is.na(x))/nSamples)>lowCT_proportion ) %>%
  which() %>%
  names()
lowGenes

# remove low genes
curData <- curData %>% dplyr::select(-all_of(lowGenes))