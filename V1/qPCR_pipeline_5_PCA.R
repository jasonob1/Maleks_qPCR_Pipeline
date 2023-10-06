library(tidyverse)
library(ggfortify)
library(ggthemes)
library(factoextra)

### PCA ####

#load data
pcaData<-curData %>%
  unnest(log2Data) %>%
  select_at(c("experiment", "sample", "site", "array", "array_group", "date", deGenes)) %>%
  mutate_at("site", ~factor(.,levels=c("Reference", "Beach", "Island"))) %>%
  mutate_at("array", factor) %>%
  mutate_at("array_group", factor)


#load data from multiple arrays

# step 1: run each array and save results
tcData<-curData %>%
  unnest(log2Data) %>%
  ungroup() %>%
  select_at(c("sample", "chemical", deGenes))

acData<-curData %>%
  unnest(log2Data) %>%
  ungroup() %>%
  select_at(c("sample", "chemical",deGenes))

# step 2: combine
pcaData<-full_join(tcData, acData, by=c("sample", "chemical" ))

pcaData<-pcaData %>%
  mutate_at("chemical", ~factor(.,levels=c("DMSO", "E2", "BPA", "BPAF", "BPF", "BPSIP", "DD70", "TGSH")))



#Save a data set
#write.table(pcaData, "pcaData.txt", sep="\t", row.names = FALSE)


#load a previously saved dataset
#pcaData<-read_tsv("pcaData.txt") %>%
#  mutate_at("chemical", ~factor(.,levels=c("DMSO", "E2", "BPA", "BPAF", "BPF", "BPSIP", "DD70", "TGSH")))



# identify data cols
pcaCols<-7:27


### PCA
PCA<-prcomp(pcaData[pcaCols], scale.=FALSE)


# variance accumulation plot
fviz_screeplot(PCA, ncp=19, geom=c("bar"), addlabels = TRUE)

#or
varData<-get_eig(PCA)
varData$component<-as.character(1:nrow(varData))
varData$component<-factor(varData$component, levels=varData$component)
ggplot(varData, aes(x=component, y=cumulative.variance.percent))+
  geom_bar(stat="identity") +
  geom_text(aes(label = round(cumulative.variance.percent,1)), vjust = 1.5, colour = "white")

# Top contributors to PC1 and PC2
top5_pc1<-names(PCA$rotation[order(abs(PCA$rotation[,1]), decreasing=TRUE),1])[1:5]
top5_pc2<-names(PCA$rotation[order(abs(PCA$rotation[,2]), decreasing=TRUE),2])[1:5]
top5<-unique(c(top5_pc1, top5_pc2))

PCA2<-PCA
PCA2$rotation<-PCA2$rotation[top5,]
PCA2$center<-PCA2$center[top5]
#PCA2$scale<-PCA2$scale[top5]

load.col<-rgb(100,100,100,alpha=125, maxColorValue = 255)
#col.Pal<-c("#000000", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")#[-c(3,5)]
# colorblind palette found online

# Plot
autoplot(PCA2,
         data=as.data.frame(pcaData),
         colour="site",
         shape="array_group",
         x=1, y=2,
         size=4,
         scale=0,
         loadings.label=TRUE,
         loadings.label.repel=TRUE,
         loadings.label.hjust=1,
         loadings.colour=load.col,
         loadings.label.colour=load.col) +
  scale_shape_manual(values=c(15, 16, 17, 18, 25)) +
  scale_color_brewer(palette = "Dark2") +
  scale_y_reverse()
 # scale_color_manual(values=col.Pal)

