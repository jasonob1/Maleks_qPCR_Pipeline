# LOAD LIBRARIES AND SOURCE CODE -------------------------------------------
library(nlme)
library(emmeans)
library(ggpubr)
library(lsmeans)
source("C:\\Users\\obrienja\\Documents\\R\\qPCR_array_stats\\qPCR_pipeline_functions.R")


# PULL DATASET OF INTEREST FROM CURATED DATA -----------------------------
mData<-curData %>%
  unnest(fcData)

# VISUALIZE --------------------------------------------------------------

# Group means and SD for plotting
mSum<-mData %>%
  group_by(experiment, dose) %>%
  summarise(
    count = n(),
    mean = mean(CYP1A5, na.rm = TRUE),
    sd = sd(CYP1A5, na.rm = TRUE)
  ) 
mSum
mSum<-mSum[c(1:4,9:12),]
#write.table(mSum, "dose_response_summary_CYP1A5.txt", sep="\t", row.names = FALSE)




# plot
ggplot(mSum, aes(x=dose, y=mean, fill=experiment)) + 
  geom_bar(stat="identity", color="black", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.2, position=position_dodge(.9)) +
  labs(x="dose (nM)", y="fold change") + 
  theme_classic()


# MODEL ------------------------------------------------------------------
#   using gls to permit unequal variance
#   using emmeans for contrasts
m_eqVar<-gls(CYP1A5~experiment*dose, data=mData)
m_unEqVar<-update(m_eqVar, weights=varIdent(form=~1|dose))
AIC(m_eqVar, m_unEqVar)
m<-m_unEqVar



#######  multComp method -----------------------------------------------

library(multcomp)

#             c10  p10
#  1-p + 0  :   1    1
#  2-c + 0  :   1    0
#  3-s + 0  :   0    1
#  4-p + 0.1:   0    0
#  5-p + 1  :   0    0
#  6-p + 10 :   1    1
#  7-c + 0.1:   0    0
#  8-s + 0.1:   0    0
#  9-c + 1  :   0    0
# 10-s + 1  :   0    0
# 11-c + 10 :   1    0
# 12-s + 10 :   0    1

p0<-c0<-s0<-c(1,rep(0,11))
c0[2]<-1
s0[3]<-1

p0.1<-p1<-p10<-p0
p0.1[4]<-1
p1[5]<-1
p10[6]<-1

c0.1<-s0.1<-p0.1
c0.1[7]<-1
s0.1[8]<-1

c1<-s1<-p1
c1[9]<-1
s1[10]<-1

c10<-s10<-p10
c10[11]<-1
s10[12]<-1

stndCon<-rbind(
  p0,
  c0-p0,
  s0-p0,
  p0.1-p0,
  p1-p0,
  p10-p0,
  c0.1-(c0+p0),
  s0.1-(s0-p0),
  c1-(p1-p0),
  s1-(p1-p0),
  c10-(p10-p0),
  s10-(p10-p0)
)


testCon<-rbind(
  p0.1-p0,
  p1-p0,
  p10-p0,
  c0.1-c0,
  c1-c0,
  c10-c0,
  s0.1-s0,
  s1-s0,
  s10-s0,
  c0.1-p0.1,
  s0.1-p0.1,
  s0.1-c0.1,
  c1-p1,
  s1-p1,
  s1-c1,
  c10-p10,
  s10-p10,
  s10-c10
)

rownames(testCon)<-c(
  "c0.1-c0",
  "c1-c0",
  "c10-c0",
  "s0.1-s0",
  "s1-s0",
  "s10-s0",
  "CEH0.1-CEH0",
  "CEH1-CEH0",
  "CEH10-CEH0",
  "s0.1-c0.1",
  "CEH0.1-c0.1",
  "CEH0.1-s0.1",
  "s1-c1",
  "CEH1-c1",
  "CEH1-s1",
  "s10-c10",
  "CEH10-c10",
  "CEH10-s10"
)


con<-glht(m, linfct=testCon)
res<-summary(con, test=adjusted(type="none"))
res_adj<-summary(con, test=adjusted(type="holm"))

contrastResults<-data.frame(diff=res$test$coefficients,
                            SE=res$test$sigma,
                            p_value=res$test$pvalues,
                            adj_p_values=res_adj$test$pvalues)


#write.table(contrastResults, "contrast_results_CYP1A5.txt", sep="\t", row.names = TRUE)
