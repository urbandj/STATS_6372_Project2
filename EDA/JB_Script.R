
# diabetes MLR exploration

library(MASS)

# Load data
setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/Data")
medData <- read.csv(file="DJ_Project2_Data.csv", header=TRUE)

medData

# -17, -143  IMH_Diabetes  IMH_OtherMentalHealthSpec
reduced<-medData[,-c(17,143)]
reduced<-medData[,c(143)]
reduced

reduced2<-medData[,c(2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28
                     ,29,30,31,32,33,34,35,36,37,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60
                     ,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90
                     ,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115
                     ,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138
                     ,139,140,141,142,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160)]
testDatalm <- lm(IMH_Diabetes~., data = reduced2)
summary(testDatalm)


reduced3 <- reduced2[,c("IMH_Diabetes","ID_Education","OM_BP2_DIA","OM_AbCircumference","IMH_HighBP","IMH_Alzheimers","IMH_UTI","bw_eGFRAA","bw_eGFRnonAA","bw_glucose","bw_ALT")]

testDatalm <- lm(IMH_Diabetes~ID_Education + OM_BP2_DIA + OM_AbCircumference + IMH_HighBP 
                 + IMH_Alzheimers + IMH_UTI + bw_eGFRAA + bw_eGFRnonAA + bw_glucose + bw_ALT, data = reduced3)
summary(testDatalm)


# scatterplot matrix
pairs(reduced3)

# cdx_cog | cdx_mci (mild cognitive impairment) - APOE_1 - 4 are genetic marker for cognition
# normal vs. demented  |  normal vs. MCI
# control for sex, age, race



reduced4 <- reduced2[,c("cdx_cog")]

summary(reduced4)

reduced5 <- ifelse(medData)



# cognitive data exploration

library(dplyr)
library(RColorBrewer)
library(glmnet)
library(gplots)
library(ggplot2)
library(leaps)
library(MASS)


# Load data
setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/EDA")

medData2 <- read.csv(file="data_cut.csv", header=TRUE)

reduced <- select (medData2,-c(OM_Notes, ID_Race_Specify, cdx_dt, IMH_OtherMentalHealthSpec, cdx_mcid, Med_ID))


# recode cognitive variable
######
#levels of lgr_eda2$cdx_cog
######
#0=normal*****
#1=Subjective Memory Complaint(SMC)****
#2=MCI****
#3=Dementia****
#4=Other Cognition
#5=Abnormal Cognition****
#7=Dont Know
#8=Refuse to answer
#9=Not Appicable
reduced3 <- reduced %>% mutate(cdx_cog2=recode(cdx_cog, 
                         `0`=0,
                         `1`=1,
                         `2`=1,
                         `3`=1,
                         `4`=1,
                         `5`=0))


reduced3a <- reduced3 %>% mutate(ID_Hispanic2=recode(ID_Hispanic, 
                                               `1`=0,
                                               `2`=1,
                                               `3`=1,
                                               `4`=1,
                                               `5`=1))


# Set NAs to 0

reduced4 <- reduced3a %>%
  mutate(cdx_cog2 = if_else(is.na(cdx_cog2), 0, cdx_cog2))

reduced4a <- reduced4 %>%
  mutate(ID_Hispanic2 = if_else(is.na(ID_Hispanic2), 0, ID_Hispanic2))


# see all the column types
sapply(reduced4, class)

# replace all NA values with 0
reduced4a[is.na(reduced4a)] = 0

# MLR to identify significant variables
testDatalm <- lm(cdx_cog2~., data = reduced4)
summary(testDatalm)

testDatalm2 <- lm(cdx_cog2~cdx_depression + LM2_AB_sscore + Age, data = reduced4)
summary(testDatalm2)

# removing these two columns because they are very highly correlated with the predictor
reduced4a <- select (reduced4a,-c(cdx_mci, cdx_cog))

# test / train data set split
train <- reduced4a[1:350,]
test <- reduced4a[351:741,]

# full fit (MLR) before converting response to factor
train$cdx_cog2<-as.numeric(train$cdx_cog2) 
full.model <- lm(cdx_cog2 ~., data = train)
summary(full.model)

# stepwise selection
step(lm(cdx_cog2 ~., data = train),direction="both")

# stepwise recommended model - AIC: -825.72 Adj. R2 = 0.5785
model.stepwise <- lm(formula = cdx_cog2 ~ OM_Pulse2 + OM_BP2_SYS + OM_BMI + IMH_HighBPAge + 
                       IMH_Dementia + IMH_HeartDisease + IMH_HeartDiseaseAge + IMH_StrokeAge + 
                       IMH_AnxietyAge + IMH_OsteoporosisAge + IMH_Arthritis + IMH_ArthritisAge + 
                       IMH_SeizuresDisorderAge + IMH_TBI + TrailsAtime + TrailsAerrors + 
                       TrailsBtime + LM2_Atotal + LM2_Btotal + LM2_AB_total + LM2_AB_sscore + 
                       mmse_t_w + bw_eGFRnonAA + bw_eGFRAA + bw_Bilirubin + bw_hematocrit + 
                       bw_platelet + cdx_hypothyroid + cdx_anemia + APOE_4 + ID_Hispanic2 + 
                       bw_calcium + Age + ID_Education + ID_Race_White, data = train)
summary(model.stepwise)

# convert dependent variable to factor
train[, 'cdx_cog2'] <- as.factor(train[, 'cdx_cog2'])
test[, 'cdx_cog2'] <- as.factor(test[, 'cdx_cog2'])

# LM2_AB_sscore | LM1_AB_sscore and TrailsA_sscore | TrailsB_sscore  - trails (higher the score the worse)are cognitive test scores
# Partial fit using stepwise recommendations - AIC 193.88
model.partial <- glm(cdx_cog2 ~ OM_Pulse2 + OM_BP2_SYS + OM_BMI + IMH_HighBPAge + 
  IMH_Dementia + IMH_HeartDisease + IMH_HeartDiseaseAge + IMH_StrokeAge + 
  IMH_AnxietyAge + IMH_OsteoporosisAge + IMH_Arthritis + IMH_ArthritisAge + 
  IMH_SeizuresDisorderAge + IMH_TBI + TrailsAtime + TrailsAerrors + 
  TrailsBtime + LM2_Atotal + LM2_Btotal + LM2_AB_total + LM2_AB_sscore + 
  mmse_t_w + bw_eGFRnonAA + bw_eGFRAA + bw_Bilirubin + bw_hematocrit + 
  bw_platelet + cdx_hypothyroid + cdx_anemia + APOE_4 + ID_Hispanic2 + 
  bw_calcium + Age + ID_Education + ID_Race_White
  ,family=binomial(link='logit'),data=train)
summary(model.partial)


# manually built model using practical knowledge - AIC 255.93
# should we run LASSO against it as well?
model.manual  <- glm(cdx_cog2 ~ LM1_AB_sscore + Age + TrailsA_sscore + mmse_t_w + ID_Gender
              ,family=binomial(link='logit'),data=train)
summary(model.manual)

# using aggregate to check the various groupings and summary stats for each predictor
aggregate(cdx_cog2 ~ LM1_AB_sscore,data=train,summary)
aggregate(cdx_cog2 ~ Age,data=train,summary)
aggregate(cdx_cog2 ~ TrailsA_sscore,data=train,summary)
aggregate(cdx_cog2 ~ mmse_t_w,data=train,summary)
aggregate(cdx_cog2 ~ ID_Gender,data=train,summary)

# proportion tables
prop.table.LM1 <- prop.table(table(train$cdx_cog2, train$LM1_AB_sscore),2)
prop.table.Age <-prop.table(table(train$cdx_cog2, train$Age),2)
prop.table.TrailsA <-prop.table(table(train$cdx_cog2, train$TrailsA_sscore),2)
prop.table.Gender <-prop.table(table(train$cdx_cog2, train$ID_Gender),2)
prop.table.MMSE <-prop.table(table(train$cdx_cog2, train$mmse_t_w),2)

# Visualize proportion tables
plot(train$cdx_cog2~train$LM1_AB_sscore,col=c("red","blue"))
plot(train$cdx_cog2~train$Age,col=c("red","blue"))
plot(train$cdx_cog2~train$TrailsA_sscore,col=c("red","blue"))
plot(train$cdx_cog2~train$ID_Gender,col=c("red","blue"))
plot(train$cdx_cog2~train$mmse_t_w,col=c("red","blue"))

# Correlation plots for continuous predictors (not including gender)
pairs(train[,c("LM1_AB_sscore","Age","TrailsA_sscore","mmse_t_w")])
my.cor<-cor(train[,c("LM1_AB_sscore","Age","TrailsA_sscore","mmse_t_w")])
my.cor
pairs(train[,c("LM1_AB_sscore","Age","TrailsA_sscore","mmse_t_w")],col=train$cdx_cog2)

# heatmap for correlation plot
heatmap.2(my.cor,col=redgreen(75), 
          density.info="none", trace="none", dendrogram=c("row"), 
          symm=F,symkey=T,symbreaks=T, scale="none")

# Using the summary coefficients we can generate CI for each one in the table and get odds ratios
exp(cbind("Odds ratio" = coef(model), confint.default(model, level = 0.95))) 

# create null model and build it up
model.null<-glm(cdx_cog2 ~ 1, data=train,family = binomial(link="logit"))

#This starts with a null model and then builds up using forward selection up to all the predictors that were specified in my
#main model previously.
step(model.null,
     scope = list(upper=model),
     direction="forward",
     test="Chisq",
     data=train)





