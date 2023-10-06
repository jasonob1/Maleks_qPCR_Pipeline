# aovModFun: creates a linear model for ANOVA analysis for each gene
aovModFun <- function(dataSet, aovFactor){
  gNames<-ctCols(dataSet)
  mods<-lapply(gNames, FUN=function(y){
    form <- as.formula(paste0(y,"~",aovFactor))
    m <- lm(form, data=dataSet)
    m$aovFac<-aovFactor
    return(m)
  })
  names(mods)<-gNames
  return(mods)
}

# aovPvalExtract: extract the overall ANOVA pvalue for all genes
aovPvalExtract <- function(modList){
  pOut<-lapply(modList, FUN=function(m){
    fstat<-summary(m)$fstatistic
    aovP<-pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
    names(aovP)<-NULL
    return(aovP)
  })
  return(as_tibble(pOut))
}

# cleanHighCt(dataSet, method="replaceCt", ct=35, group):
#   choose between: 
#     "replaceCt": : replace all NoCts with Ct=35 (or other value)
#     "groupAvg" : replace NoCts with average of group (group must be defined)
#     "setAvg" : replace NoCts with dataset average
#     "ignore" : leave as NA to be ignore in downstream analysis
#     "

cleanHighCt<-function(dataSet, method="replaceCt", ct=35, group){
  require(tidyverse)
  if(method=="replaceCt"){
    cleanData <- dataSet %>%
      mutate_at(ctCols(.), ~ifelse(.>ct & !is.na(.),ct,.))
  }
  
  if(method=="remove"){
    cleanData <- dataSet %>%
      mutate_at(ctCols(.), ~ifelse(.>ct & !is.na(.),NA,.))
  }
  
  if(method=="ignore"){
    cleanData<-dataSet
  }
  return(cleanData)
}


# cleanNoCt(dataSet, method="replaceCt", ct=35, group):
#   choose between: 
#     "replaceCt": : replace all NoCts with Ct=35 (or other value)
#     "groupAvg" : replace NoCts with average of group (group must be defined)
#     "setAvg" : replace NoCts with dataset average
#     "ignore" : leave as NA to be ignore in downstream analysis
#     "...._rand": the "rand" option pulls random numbers from a distribution with an equal mean and SD as the indicated group (group or set)

cleanNoCt<-function(dataSet, method="replaceCt", ct=35, group){
  require(tidyverse)
  if(method=="replaceCt"){
    cleanData <- dataSet %>%
      mutate_at(ctCols(.), ~ifelse(is.na(.),ct,.))
  }
  if(method=="groupAvg"){
    avgGroup <- c("experiment", group)
    cleanData <- dataSet %>%
      group_by_at(avgGroup) %>%
      mutate_at(ctCols(.), ~ifelse(is.na(.),mean(., na.rm=TRUE),.)) %>%
      ungroup()
  }
  
  if(method=="groupAvg_rand"){
    avgGroup <- c("experiment", group)
    cleanData <- dataSet %>%
      group_by_at(avgGroup) %>%
      mutate_at(ctCols(.), ~sapply(., FUN=function(x){
        ifelse(is.na(x),rnorm(1, mean(., na.rm=TRUE),sd(., na.rm=TRUE)),x)
      })) %>%
      ungroup()
  }
  if(method=="setAvg"){
    avgGroup <- c("experiment")
    cleanData <- dataSet %>%
      group_by_at(avgGroup) %>%
      mutate_at(ctCols(.), ~ifelse(is.na(.),mean(., na.rm=TRUE),.)) %>%
      ungroup()
  }
  if(method=="setAvg_rand"){
    avgGroup <- c("experiment")
    cleanData <- dataSet %>%
      group_by_at(avgGroup) %>%
      mutate_at(ctCols(.), ~sapply(., FUN=function(x){
        ifelse(is.na(x),rnorm(1, mean(., na.rm=TRUE),sd(., na.rm=TRUE)),x)
      })) %>%
      ungroup()
  }
  
  if(method=="ignore"){
    cleanData<-dataSet
  }
  return(cleanData)
}


# cleanQC(dataSet,  qcNames): remove qc genes from data set (set to NA if no QC genes)
cleanQC<-function(dataSet, qcNames){
  require(tidyverse)
  if(all(is.na(qcNames))){
    cleanData<-dataSet
  }else{
    cleanData<-dataSet %>% dplyr::select(-all_of(qcNames))
  }
  return(cleanData)
}

# ct2Exp(dataSet): convert ct values to expression value (2^-ct)
ct2Exp <- function(dataSet){
  require(tidyverse)
  expData<-dataSet %>%
    mutate_at(ctCols(.), ~2^(-.))
  return(expData)
}

# ctCols(dataSet): Identify gene of interest columns
#   NOTE: requires a vector, "rawCtCols", of the names of the gene of interest columns
ctCols<-function(x){
  require(dplyr)
  names(x)[names(x)%in%rawCtCols]
}


# dct2FC(dctData, log2=FALSE): convert normalzied delta ct data to fold change or log2foldchange
#     negInv = returns the negative inverse for values when fc < 1
#     log2 = returns log2 fold-change
#     geoMean = fc is based on geometric mean (usually used with log transformation)
#     fcRef= is fc determined relative to the mean of the "control" group, or the mean of "all" samples
dct2FC<-function(dctData, negInv=TRUE, log2=FALSE, geoMean=FALSE, fcRef="control"){
  require(EnvStats) 
  require(tidyverse)
  
  if(fcRef=="control"){
    refSamples<-c("control")
  }else if(fcRef=="all"){
    refSamples<-c("control","test")
  }
  
  if(geoMean){
  fcData <- dctData %>%
    mutate_at(ctCols(.), ~./geoMean(.[type%in%refSamples], na.rm=TRUE))  
  }else{
    fcData <- dctData %>%
      mutate_at(ctCols(.), ~./mean(.[type%in%refSamples], na.rm=TRUE))  
  }
  if(log2==TRUE){
    fcData <- fcData %>%
      mutate_at(ctCols(.), ~log(.,2))
  }else{
    if(negInv){
      fcData <- fcData %>%
        mutate_at(ctCols(.), ~ifelse(.<1,-1/.,.))  
    }else{
      fcData <- fcData
    }
  }
}


# extractPairwise(modList): extact the pairwise comparison pvalues for each gene
extractPairwise<-function(modList){
  require(tidyverse)
  pList <- lapply(modList, FUN=function(m){
    p<-summary(m)$coefficients[,4]
    p[1]<-NA
    nameSub<-m$aovFac
    names(p)<-sub(nameSub, "", names(p))
    names(p)[1]<-"control"
    return(p)
  })
  
  pList <- pList %>%
    as.data.frame() %>%
    rownames_to_column("group") %>%
    as_tibble()
  return(pList)
}


# extractAdjustPairwise(pTib): FDR adjust the pairwise comparison pvalues for each gene
extractAdjustPairwise<-function(pTib){
  require(tidyverse)
  adjP<- pTib %>%
    dplyr::select(ctCols(.)) %>%
    as.data.frame() %>%
    as.matrix() %>%
    p.adjust(method = "fdr") %>%
    matrix(ncol=length(ctCols(pTib)))
  
  dimnames(adjP)<- pTib %>%
    dplyr::select(ctCols(.)) %>%
    as.data.frame() %>%
    as.matrix() %>%
    dimnames()
  
  adjP<-adjP %>% as_tibble()  
  
  selCol<-ctCols(pTib)
  
  adjP <-pTib %>%
    dplyr::select(-all_of(selCol)) %>%
    bind_cols(adjP)
  
  return(adjP)
}


# global_fdr(dataSet, p_value_location): apply a global fdr adjustment across a nested group of experiments

global_frd <- function(dataSet, p_value_location){
  tempData <- dataSet %>%
    dplyr::select("experiment", all_of(p_value_location)) %>%
    unnest(all_of(p_value_location)) %>%
    ungroup()
  
  selCols <- ctCols(tempData)
  tempCols <- tempData %>%
    dplyr::select(-all_of(selCols))
  
  fdr_ps<-dataSet %>%
    dplyr::select(experiment, all_of(p_value_location)) %>%
    unnest(all_of(p_value_location)) %>%
    ungroup() %>%
    dplyr::select(ctCols(.)) %>%
    stack %>%
    mutate(values=p.adjust(values, method="fdr") )%>%
    unstack %>%
    as_tibble %>%
    bind_cols(tempCols,.) %>%
    group_by(experiment) %>%
    nest(globalFDR=c(group, ctCols(.)))
  
  updatedData<-dataSet %>%
    bind_cols(fdr_ps["globalFDR"])
  
  return(updatedData)
}


# flagHigh(dataSet, ct=35): create a vector of lenght = # of samples to flag any genes with Ct > 35 (or any other value)
flagHigh<-function(dataSet, ct=35){
  require(tidyverse)
  require(dplyr)
  dataSet %>%
    select_at(ctCols(.)) %>%
    mutate_all(~.>ct & !is.na(.)) %>%
    sapply(seq_along(.),FUN=function(i, f, n){ifelse(f[[i]],n[[i]],"")}, f=., n=names(.)) %>%
    apply(1,function(x){
      paste(x[!x==""], collapse=" ")
    })
}


# flagNoCt(dataSet): create vector of lenght = # of samples to flag any genes with No Ct (must be marked as NA)
flagNoCt<-function(dataSet){
  require(tidyverse)
  dataSet %>%
    dplyr::select(ctCols(.)) %>%
    mutate_all(~is.na(.)) %>%
    sapply(seq_along(.),FUN=function(i, f, n){ifelse(f[[i]],n[[i]],"")}, f=., n=names(.)) %>%
    apply(1,function(x){
      paste(x[!x==""], collapse=" ")
    })
}


# hkTest(dataSet, hkNames, aovFactor): ANOVA test on house keeping genes
hkTest<-function(dataSet, hkNames, aovFactor, with_interaction=FALSE){
  if(with_interaction){
    term_sep="*"
  }else{
    term_sep="+"
  }
  hk_res<-lapply(hkNames, FUN=function(hk_name){
    form<-as.formula(paste0(hk_name,"~", paste0(aovFactor, collapse=term_sep)))
    m<-lm(form, data=dataSet)
    f<-summary(m)$fstatistic
    p<-pf(f[1],f[2],f[3], lower.tail = FALSE )
    attributes(p)<-NULL
    names(p)<-"p_value"
    p_var<-data.frame(p_value=summary(m)$coefficients[,4])
    hk_ps<-list()
    hk_ps[["AOV"]]<-p
    hk_ps[["variable_specific"]]<-p_var
    return(hk_ps)
  })
  names(hk_res)<-hkNames
  return(hk_res)
}


# hkNorm(dataSet, hkNames): normalize to one or more house keeping genes
hkNorm<-function(dataSet, hkNames, hkKeep=TRUE, exp2=TRUE){
  require(tidyverse)
  hkNormFactor<-dataSet %>%
    dplyr::select(all_of(hkNames)) %>%
    rowMeans()
  
  if(exp2){
    dctData<-dataSet %>%
      mutate_at(ctCols(.), ~2^-(.-hkNormFactor))
  }else{
    dctData<-dataSet %>%
      mutate_at(ctCols(.), ~(.-hkNormFactor))
  }
  
  if(hkKeep==FALSE){
    dctData <- dctData %>%
      dplyr::select(-all_of(hkNames))
  }
  return(dctData)
}


# nestFactor(dataSet, factor): factor columns of a dataset AFTER it has been grouped and nested
nestFactor <- function(dataSet, fac){
  dataSet %>%
    mutate_at(fac, ~factor(., levels=unique(.)))
}


# repCount(dataSet, dataGroups = NA): counts the number of replicates per indicated dataGroups
repCount<-function(dataSet, dataGroups=NA){
  if(any(is.na(dataGroups))){
    repData<-dataSet %>%
      summarize(across(ctCols(.), ~sum(!is.na(.))), .groups = "drop")
  }else{
    repData<-dataSet %>%
      group_by(across(all_of(dataGroups))) %>%
      summarize(across(ctCols(.), ~sum(!is.na(.))), .groups = "drop")
  }
  return(repData)
}



# sharedControl(dataSet, controlGroup): Split a control group across multiple experiments
sharedControl<-function(dataSet, controlGroup){
  expGroups <- dataSet %>%
    filter(experiment!=controlGroup) %>%
    distinct(experiment) %>%
    unlist()
  
  sharedData<-expGroups %>%
    lapply(FUN=function(x){
      dataSet %>%
        filter(experiment==controlGroup) %>%
        mutate(experiment=x)
    }) %>%
    bind_rows() %>%
    bind_rows(dataSet) %>%
    filter(experiment!=controlGroup) %>%
    arrange(experiment, type)
  return(sharedData)
}


# summarize.Flag(dataSet)
summarize.Flag <-function(dataSet, dataGroups){
  flagSum<-dataSet %>%
    group_by_at(dataGroups) %>%
    summarise_at(c("flagHigh","flagNoCt"), ~paste(unique(.[!.==""]), collapse=", "))
  return(flagSum)
}

# summarize.FC(dataSet, dataGroups, negInv=TRUE, geoMean=TRUE): summarize fold change data
summarize.FC <-function(dataSet, dataGroups, negInv=FALSE, geoMean=FALSE){
  require(EnvStats)
  dSum <- dataSet %>%
    group_by_at(dataGroups) %>%
    summarize_at(ctCols(.), ~ifelse(geoMean, geoMean(., na.rm=TRUE), mean(., na.rm=TRUE)))
  if(negInv){
    dSum<-dSum %>%
      mutate_at(ctCols(.), ~ifelse(.<1,-1/.,.))  
  }
  return(dSum)
}

# summarize.SD(dataSet, dataGroups): return SD of fold change data
summarize.SD <-function(dataSet, dataGroups, negInv=FALSE, geoSD=FALSE){
  require(EnvStats)
  dSum <- dataSet %>%
    group_by_at(dataGroups) %>%
    summarize_at(ctCols(.), ~ifelse(geoSD,
            ifelse(negInv & -1/geoMean(., na.rm=TRUE)< (-1.00001),-1/geoSD(., na.rm=TRUE),geoSD(., na.rm=TRUE)),
            ifelse(negInv & -1/mean(., na.rm=TRUE)< (-1.00001),-1/sd(., na.rm=TRUE),sd(., na.rm=TRUE))
          ))
  return(dSum)
}

# tmmNorm(dataSet, expTrans=TRUE):
#   Normalizes using the TMM Menthasashasdkudfkuh
#   qPCR Data MUST be 2^-ct transformed!!
tmmNorm <-function(dataSet, expTrans=TRUE, logratioTrim=0.3, sumTrim=0.05){
  require(edgeR)
  require(tidyverse)
  if(expTrans){
    testData<-dataSet %>%
      select_at(ctCols(.)) %>%
      mutate_all(~2^-.) %>%
      t %>%
      as.data.frame
  }else{
    testData <- dataSet %>%
      select_at(ctCols(.)) %>%
      t %>%
      as.data.frame
  }
  
  names(testData) <-dataSet$sample
  
  sizeFactor <- calcNormFactors(testData, method = "TMM", logratioTrim=logratioTrim, sumTrim=sumTrim )
  normData<-testData
  for(i in 1:ncol(normData)){
    normData[,i]<-normData[,i]/(sum(normData[,i])*sizeFactor[i])
  }
  normData <- normData %>%
    t %>%
    as_tibble
  
  newData <- dataSet %>%
    dplyr::select(-one_of(ctCols(.))) %>%
    bind_cols(normData)
  
  return(newData)
  
}





