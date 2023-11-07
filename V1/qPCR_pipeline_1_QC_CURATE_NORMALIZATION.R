#### LOAD LIBRARIES AND FUNCTION SOURCE CODE ####
library(tidyverse)
library(readxl)
library(EnvStats) # for geoMean function
library(ggpubr)
library(edgeR)
library(ggfortify)
library(ggthemes)
library(factoextra)

source("qPCR_pipeline_functions.R")


#### IMPORT DATA FROM TAB DELIMITED TEXT FILE ####
dataFile <- "PracticeData(Lauras).txt"
rawData <- read.table(dataFile, header = TRUE, stringsAsFactors = FALSE, sep="\t", na.strings="No Ct", strip.white=TRUE) %>% as_tibble()

# NOTE: In addition to the "sample" and "site" columns, this table has TWO additional and mandatory columns called "experiment" and "type"


#### CURATE DATA ####
curData <- rawData    # make a copy to manipulate

# remove any unwanted samples  ### NOT NEEDED IN V1.0 of qPCR App (commented out)
# curData <- curData %>%
#  filter(Year==1999)

# remove any outlier samples  ### NOT NEEDED IN V1.0 of qPCR App (commented out)
# curData <- curData %>%
#   filter(!sample%in%("SGL_99_29")) 

# remove any unwanted columns  ### NOT NEEDED IN V1.0 of qPCR App (commented out)
# curData <- curData %>%
#   select(-c("Notes"))

# identify total number of remaining samples
nSamples<- curData %>% nrow()


#### CURATE FACTORS ####

# identify "experimental factors" (to differentiate from gene columns)
expFactors <- c("site","type")

# covert to factors and specify any orders
curData <- curData %>%
  mutate(site = factor(site))

#  ANOVA factors (to use for statistical analysis)
aovFactor <- c("site")


#### CURATE GENES ####

# Identify Housekeeping Genes
hkGenes <- c("RPL4", "EEF1A1")

# identy and remove QC genes
qcGenes<-c("GGDC", "RTC", "PPC")
curData<-cleanQC(curData, qcGenes)

# Identifies all of the "Gene" columns "ctCols" function # RUN EVERYTIME
rawCtCols<-names(curData)[!names(curData)%in%c("sample", "experiment", "type", "flagNoCt", "flagHigh", expFactors)]

# reorder genes alphabetically (easier to look through final results)
gOrder<-ctCols(curData) %>%
  order() %>%
  ctCols(curData)[.]
curData <- curData %>%
  select_at(c("sample", "experiment", expFactors, gOrder))


#### FILTER GENES ####

# identify genes with ct > "highCt" or NA (No Ct) in > y% of samples
highCt<-36
highCt_proportion<-0.50
highGenes<- curData %>%
  dplyr::select(ctCols(.)) %>%
  apply(MARGIN=2, FUN=function(x) (sum(x>highCt|is.na(x))/nSamples)>highCt_proportion) %>%
  which() %>%
  names()
highGenes

# remove high Ct genes
curData <- curData %>% dplyr::select(-all_of(highGenes))

# identify genes with ct < "lowCt" or NA (No Ct) in >y% of samples
lowCt<-14
lowCt_proportion<-0.50
lowGenes<- curData %>%
  dplyr::select(ctCols(.)) %>%
  apply(MARGIN=2, FUN=function(x) (sum(x<lowCt|is.na(x))/nSamples)>lowCt_proportion ) %>%
  which() %>%
  names()
lowGenes

# remove low genes
curData <- curData %>% dplyr::select(-all_of(lowGenes))


#### WHAT TO DO WITH REMAINING CT>35 (or any other value) and No Cts ####

# flag any remaining ct >35 (or any other value) and NoCts to keep track (adds "flagHigh" and "flagNoCt" columns to the data)
curData <- curData %>%
  mutate(flagHigh=flagHigh(curData, ct=35))

curData <- curData %>%
  mutate(flagNoCt=flagNoCt(curData))


# Replace with 35 (replaceCt), remove, group average (groupAvg or groupAvg_rand), experiment average (setAvg or setAvg_rand), or ignore (ignore)
# "_rand" options select a random number from a distrubtion with the same mean and SD as the selected group
highCt_method<-"ignore"
noCtGene_method<-"setAvg" 

set.seed(1)
curData<-cleanHighCt(curData, method=highCt_method)
curData<-cleanNoCt(curData, method=noCtGene_method)


#### NEST DATA BY EXPERIMENT ####
# NOTE: This nesting step is required for all following steps
curData <-curData %>%
  group_by(experiment) %>%
  nest()

# Count number of reps per group per gene
curData<-curData %>%
  mutate(repCount=map(data, repCount, dataGroups=aovFactor))

# convert raw Cts to Expression (2^-ct)
curData <- curData %>%
  mutate(expRaw = map(data, ct2Exp))


#### PRE-NORMALIZATION PCA ####

# PCA Data
pcaData<-curData %>%
  unnest(data) %>%
  ungroup() 

# PCA Model
PCA <- pcaData%>% 
  select(all_of(ctCols(.))) %>%
  prcomp(scale. = TRUE) # do PCA on scaled data

# Plot of Variance Captured
fviz_screeplot(PCA, ncp=19, geom=c("bar"), addlabels = TRUE)

# Plot colour options
load.col<-rgb(100,100,100,alpha=125, maxColorValue = 255)

# Plot
autoplot(PCA,
         data=as.data.frame(pcaData),
         col="site",
         #shape="Time",
         x=1, y=2,
         size=4,
         scale=0,
         label.colour="black",
         label=TRUE,
         loadings.label=TRUE,
         loadings.label.repel=TRUE,
         loadings.label.hjust=1,
         loadings.colour=load.col,
         loadings.label.colour=load.col
) +
  #scale_shape_manual(values=c(16, 17)) +  # 1 is empty circle, 16 is full circle, 17 is full triangle
  scale_color_brewer(palette = "Dark2")


#### CHECK HOUSE KEEPING GENES ####
curData <- curData %>%
  mutate(hkCheck = map(data, hkTest, hkNames=hkGenes, aovFactor=aovFactor, with_interaction=TRUE))
curData$hkCheck

# plot hkGenes
y_lims<-curData %>%
  unnest(data) %>%
  ungroup %>%
  select(all_of(hkGenes)) %>%
  range()
y_lims<-y_lims + c(-0.5,0.5)

plots<-lapply(hkGenes, function(GENE){
  if(length(aovFactor)<2){
    curData %>%
      unnest(data) %>%
      ggplot(aes(x=.data[[aovFactor]], y=.data[[GENE]])) +
      ylim(c(5, 35)) +
      geom_jitter(size=3, position=position_dodge2(width=0.4)) +
      theme_bw() + 
      facet_grid(.~experiment)
  }else{
    curData %>%
      unnest(data) %>%
      ggplot(aes(x=.data[[aovFactor[2]]], y=.data[[GENE]], colour = .data[[aovFactor[1]]])) +
      ylim(y_lims) +
      geom_jitter(size=3, position=position_dodge2(width=0.4)) +
      theme_bw() + 
      facet_grid(.~experiment)
  }
})
ggarrange(plotlist = plots, nrow=length(hkGenes))  


### NORMALIZATION ###
normMethod <- "HK"   # Choose between two normalization methods "HK" (housekeeping genes) or "TMM" (Trimmed M means)

# NORMLAIZE TO HOUSE KEEPING GENES ---------------------
if(normMethod=="HK"){
  curData <- curData %>%
    mutate(expdctData = map(data, hkNorm, hkNames=hkGenes, hkKeep=FALSE, exp2=TRUE)) %>%
    mutate(dctData = map(data, hkNorm, hkNames=hkGenes, hkKeep=FALSE, exp2=FALSE))
}

# NORMLAIZE USING TRIMMED M MEANS (TMM) FROM EDGE R ---------------------
if(normMethod=="TMM"){
  curData <- curData %>%
    mutate(expdctData = map(data, tmmNorm, logratioTrim=0.5, sumTrim=0.1))
  #mutate(expdctData = map(data, tmmNorm, logratioTrim=0.3, sumTrim=0.05)) #defaults
}

# View Normalization

#plot before normalization
plotData<-curData %>%
  unnest(data) %>%
  ungroup() %>%
  select_at(ctCols(.)) %>%
  as.data.frame()

row.names(plotData)<-curData %>%
  unnest(data) %>%
  ungroup() %>%
  dplyr::select(sample)%>%
  unlist() %>%
  make.names(unique=TRUE)

plotData%>%
  t %>%
  data.frame %>%
  stack %>%
  plot(formula=values~ind, data=., cex.axis=0.5)


#plot after HK normalization
if(normMethod=="HK"){
  plotData<-curData %>%
    unnest(dctData) %>%
    ungroup() %>%
    select_at(ctCols(.)) %>%
    as.data.frame()
  
  row.names(plotData)<-curData %>%
    unnest(data) %>%
    ungroup() %>%
    dplyr::select(sample)%>%
    unlist() %>%
    make.names(unique=TRUE)
  
  plotData%>%
    t %>%
    data.frame %>%
    stack %>%
    plot(formula=values~ind, data=.)
}

#plot after TMM normalization
if(normMethod=="TMM"){
  plotData<-curData %>%
    unnest(expdctData) %>%
    ungroup() %>%
    select_at(ctCols(.)) %>%
    mutate_all(~-log(.,2)) %>%
    as.data.frame()
  
  row.names(plotData)<-curData %>%
    unnest(data) %>%
    ungroup() %>%
    dplyr::select(sample)%>%
    unlist() %>%
    make.names(unique=TRUE)
  
  plotData%>%
    t %>%
    data.frame %>%
    stack %>%
    plot(formula=values~ind, data=.)
}

#### POST NORM PCA ####

# PCA Model

# for HK Normalization
if(normMethod=="HK"){
  pcaData<-curData %>%
    unnest(dctData) %>%
    ungroup()
  PCA <- pcaData %>% 
    select(all_of(ctCols(.))) %>% # retain only gene columns
    prcomp(scale. = TRUE) # do PCA on scaled data
}
# for TMM normalization
if(normMethod=="TMM"){
  pcaData<-curData %>%
    unnest(expdctData) %>%
    ungroup()
  
  PCA <- pcaData %>% 
    select(all_of(ctCols(.))) %>% # retain only gene columns
    mutate_all(~-log(.,2)) %>%
    prcomp(scale. = TRUE) # do PCA on scaled data
}

# Plot Variance Captured
fviz_screeplot(PCA, ncp=19, geom=c("bar"), addlabels = TRUE)

# Plot colour options
load.col<-rgb(100,100,100,alpha=125, maxColorValue = 255)

# Plot
autoplot(PCA,
         data=as.data.frame(pcaData),
         col="site",
         #shape="Time",
         x=1, y=2,
         size=4,
         scale=0,
         label=TRUE,
         label.color="black",
         label.repel=TRUE,
         loadings.label=TRUE,
         loadings.label.repel=TRUE,
         loadings.label.hjust=1,
         loadings.colour=load.col,
         loadings.label.colour=load.col
) +
  #scale_shape_manual(values=c(16, 17)) +  # 1 is empty circle, 16 is full circle, 17 is full triangle
  scale_color_brewer(palette = "Dark2")


#### FOLD CHANGE ####

fcRef<-"control"

# fold change
curData <- curData %>%
  mutate(fcData = map(expdctData, dct2FC, negInv=FALSE, log2=FALSE, geoMean=FALSE, fcRef=fcRef))
#mutate(fcData = map(expRaw, dct2FC, negInv=FALSE, log2=FALSE, geoMean=FALSE))

#log2 fold change
curData <- curData %>%
  mutate(log2Data = map(expdctData, dct2FC, negInv=FALSE, log2=TRUE, geoMean=TRUE, fcRef=fcRef))
#mutate(log2Data = map(expRaw, dct2FC, negInv=FALSE, log2=TRUE, geoMean=TRUE))



##### STOP HERE FOR NOW ######



