curData <- fullTable() 
qcGenes <-c(input$sGC, input$sPP, input$sRTC, input$sNTC) 
curData <-cleanQC(curData, qcGenes)

# Identifies all of the "Gene" columns "ctCols" function # RUN EVERYTIME
rawCtCols<-names(curData)[!names(curData)%in%c("SampleID", "Site")] #MS: IDENTIFIES EVERY COLUMN THAT ARE GENES (NOT SAMPLE_ID, NOT SITE), LIKE sHK

# reorder genes alphabetically (easier to look through final results)
gOrder<-ctCols(curData) %>%
  order() %>% #MS: MAKES THE GENE LIST ALPHABETICAL ORDER
  ctCols(curData)[.] #MS: THAT'S A CUSTOM FUNCTION (ctCols), YOU'LL FIND IT IN THE OTHER SCRIPT, IT JUST TAKES THE LIST OF GENES THAT WE DID NOT USE (rawCtCols so line 63)
curData <- curData %>%
  select_at(c("SampleID", "Site", gOrder)) #MS: I don't have "sample" and "experiment" in GC_MS, what exactly are we looking at


#### FILTER GENES ####

# identify genes with ct > "highCt" or NA (No Ct) in > y% of samples
highCt <- input$highCT
highCT_proportion <-0.50 #MS: MAKE DEFAULT .5 BUT MAKE IT REACTIVE (SIMILAR TO LOW/HIGHCT)
highGenes <- curData %>%
  dplyr::select(ctCols(.)) %>% #MS: REMOVE ALL NON-GENE COLUMNS (METADATA ENTIRELY) + QC GENES (PPC, GC, RTC, NTC)

  apply(MARGIN=2, FUN=function(x) (sum(x>highCT|is.na(x))/nSamples)>highCT_proportion) %>% 
  which() %>%
  names()
highGenes

# remove high Ct genes
curData <- curData %>% dplyr::select(-all_of(highGenes))