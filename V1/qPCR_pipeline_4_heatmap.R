library(gplots)
library(factoextra)
library(ggfortify)

# Heatmap ---------------------------------------
#   required input:
#     1) tibble of log 2 transformed data
#     2) Character vector of differentially expressed genes to include


# DATA SETUP  -----------------------------------

reps_or_mean<-"reps"  # set as "reps" or "mean"


### ALL REPS

if(reps_or_mean=="reps"){

  heatData <- curData %>%
    dplyr::filter(experiment=="BPA") %>%
    unnest(log2Data)
  deGenes  <- deGenes
  
  plotData <- heatData %>%
    #filter(type != "control") %>%
    ungroup() %>%
    dplyr::select_at(c("sample",deGenes))
  sNames <- plotData$sample  
  plotData<- plotData %>%
    dplyr::select(-sample) %>%
    as.matrix
  rownames(plotData) <- sNames
  plotData<-t(plotData) 

}

### Group average
if(reps_or_mean=="mean"){
  
  heatData <- aovData %>%
    unnest(log2Sum)
  deGenes  <- deGenes
  plotData <- heatData %>%
    #filter(type != "control") %>%
    ungroup() %>%
    dplyr::select_at(c(aovFactor,deGenes))
  sNames <- plotData %>%
    select_at(aovFactor) %>%
    unlist(use.names = FALSE) %>%
    as.character
  plotData<- plotData %>%
    dplyr::select(-aovFactor) %>%
    as.matrix
  rownames(plotData) <- sNames
  plotData<-t(plotData) 
}


# use PCA data
plotData<-pcaData
sNames <- plotData$sample  
plotData<- plotData %>%
  dplyr::select(-sample, -chemical) %>%
  as.matrix
rownames(plotData) <- sNames
plotData<-t(plotData) 



# remove outliers
#outliers<-c("WE6","GR1","GR2")
#plotData<-plotData[,!colnames(plotData)%in%outliers]



# HEATMAP OPTIONS -----------------------------

# Create color palette
heatCol <- colorRampPalette(c("darkgoldenrod1", "gray95", "blue"))(n = 99)
#heatCol <- colorRampPalette(c("gray90", hsv(0.25,1,0.3)))(n = 99)

#  defines the color breaks manually for a "skewed" color transition
col_breaks = c(seq(-3,0,length=50),      # for col1
               seq(0.01,3,length=40),    # for col2
               seq(3.1, 10,length=10))  # for col3

# distance function using euclidian distance on standardized data
dist_method<-"euclidian"
dist_standardized<-TRUE

distfun<-function(x){
  get_dist(x, method=dist_method, stand=dist_standardized)
}


# PLOT HEATMAP ------------------------------------

heatmap.2(plotData,
          distfun = distfun,
          scale="none",
          col=heatCol,
          #breaks=col_breaks,
          #symbreaks=TRUE,
          #symkey = TRUE,
          trace="none",
          key=TRUE,
          keysize=1,
          key.title=NA,
          key.xlab="log2 Fold Change",
          density.info="none",
          dendrogram="column",
          margins=c(10,6)
)

heat_settings<-list(
  reps_or_mean=reps_or_mean,
  distance_method=dist_method,
  distance_standardized=dist_standardized
)


#write.table(heat_settings, paste0(filePrefix, "10_heat_settings", fileSuffix, ".txt"), sep="\t", row.names = FALSE)





