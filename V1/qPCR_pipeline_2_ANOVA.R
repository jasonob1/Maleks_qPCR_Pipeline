#
# NOTE: This script uses curated data.  To create rquired "curData" object, Please first run:
#        qPCR_pipeline_1_QC_CURATE_NORM_FC.R

# LOAD LIBRARIES AND SOURCE FUNCTIONS
library(ggpubr)
library(car)
library(EnvStats)
source("C:\\Users\\ObrienJas\\Documents\\R\\qPCR_array_stats\\qPCR_pipeline_functions.R")

# RUN ANOVA FUNCTIONS ON DATA ------------------------------------
aovFactor<-"Dose"

aovData <- curData %>% 
  mutate(aovMods = map(log2Data, aovModFun, aovFactor=aovFactor)) %>%
  mutate(aovPvals = map(aovMods, aovPvalExtract)) %>%
  mutate(pairwisePvals = map(aovMods, extractPairwise)) %>%
  mutate(experimentFDR = map(pairwisePvals, extractAdjustPairwise))

aovData <- aovData %>%
  global_frd("pairwisePvals")


# SUMMARIZE RESULTS ------------------------------------------

aovData <- aovData %>%
  mutate(log2Sum = map(log2Data, summarize.FC, dataGroups=aovFactor, negInv=FALSE, geoMean=FALSE)) %>%
  mutate(log2SD = map(log2Data, summarize.SD, dataGroups=aovFactor, negInv=FALSE))  %>%
  mutate(flagSum = map(fcData, summarize.Flag, dataGroups=aovFactor)) %>%
  mutate(fcSum = map(fcData, summarize.FC, dataGroups=aovFactor, negInv=TRUE, geoMean=FALSE))

fcReps<-aovData %>%
  unnest(fcData) %>%
  dplyr::select(experiment, all_of(aovFactor), ctCols(.)) %>%
  arrange(experiment)


fcSummary<-aovData %>%
  unnest(fcSum) %>%
  dplyr::select(experiment, all_of(aovFactor), ctCols(.)) %>%
  arrange(experiment)


log2Summary<-aovData %>%
  unnest(log2Sum) %>%
  dplyr::select(experiment, all_of(aovFactor), ctCols(.)) %>%
  arrange(experiment)


sdSummary<-aovData %>%
  unnest(log2SD) %>%
  dplyr::select(experiment, all_of(aovFactor), ctCols(.)) %>%
  arrange(experiment)

flagSummary<-aovData %>%
  unnest(flagSum) %>%
  dplyr::select(experiment, all_of(aovFactor), flagHigh, flagNoCt) %>%
  arrange(experiment)

repSummary<-aovData %>%
  unnest(repCount) %>%
  dplyr::select(experiment, all_of(aovFactor), ctCols(.)) %>%
  arrange(experiment)

aovSummary<-aovData %>%
  unnest(aovPvals) %>%
  dplyr::select(experiment, ctCols(.)) %>%
  arrange(experiment)

pairwiseSummary <- aovData %>%
  unnest(pairwisePvals) %>%
  dplyr::select(experiment, group, ctCols(.)) %>%
  arrange(experiment)

experimentFDRSummary <-aovData %>%
  unnest(experimentFDR) %>%
  dplyr::select(experiment, group, ctCols(.)) %>%
  arrange(experiment)

globalFDRSummary <-aovData %>%
  unnest(globalFDR) %>%
  dplyr::select(experiment, group, ctCols(.)) %>%
  arrange(experiment)

filePrefix<-"Bile_"
fileSuffix<-""

if(FALSE){
  dir.create("output")
  write.table(log2Summary, paste0("output/",filePrefix, "1_log2Summary", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(sdSummary, paste0("output/",filePrefix, "2_sdSummary", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(repSummary, paste0("output/",filePrefix, "3_repSummary", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(pairwiseSummary, paste0("output/",filePrefix, "4_pairwizeSummary", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(experimentFDRSummary, paste0("output/",filePrefix, "5_experimentFDR", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(globalFDRSummary, paste0("output/",filePrefix, "6_globalFDR", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(flagSummary, paste0("output/",filePrefix, "7_flagSummary", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(settings, paste0("output/",filePrefix, "8_settings", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
  write.table(fcReps, paste0("output/",filePrefix, "9_fcReps", fileSuffix, ".txt"), sep="\t", row.names = FALSE)
}


# DE GENE LIST -------  

pCutOff<-0.05
fcCutOff<-1.5

# identify any unwanted experiments, groups etc to remove
#exp_omit<-c("ceh")
exp_omit<-c("")

#group_omit<-c("BHPF")
group_omit<-c("")

# create p-value table (omitting unwanted exp/groups)
pTable<-aovData %>%
  filter(!experiment %in% exp_omit) %>%
  unnest(globalFDR) %>%
  #unnest(pairwisePvals) %>%
  #filter(group!="control")%>%
  ungroup() %>%
  filter(!group %in% group_omit) %>%
  dplyr::select(ctCols(.)) %>%
  sapply(unlist)

log2Table <- aovData %>%
  filter(!experiment %in% exp_omit) %>%
  unnest(log2Sum) %>%
 #filter(type!="control") %>%
  ungroup() %>%
  filter_at(aovFactor, all_vars(!.%in% group_omit)) %>%
  dplyr::select(ctCols(.)) %>%
  sapply(unlist)
  
deMat<-pTable<pCutOff & !is.na(pTable) & abs(log2Table) > log(fcCutOff,2)
deGenes<-colnames(deMat)[apply(deMat,2,any)]


# all genes
if(FALSE){
  deGenes<-curData %>%
    unnest(log2Data) %>%
    ungroup() %>%
    dplyr::select(ctCols(.)) %>%
    colnames
}


de_settings<-list(
  p_value_cutoff=pCutOff,
  fold_change_cutoff=fcCutOff
)

#write.table(de_settings, paste0(filePrefix, "9_de_settings", fileSuffix, ".txt"), sep="\t", row.names = FALSE)

