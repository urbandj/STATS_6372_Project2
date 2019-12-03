
# loadings

install.packages("skimr")
install.packages("dplyr")
library(skimr)
library(dplyr)
library(RColorBrewer)
library(glmnet)
library(gplots)
library(ggplot2)
library(leaps)
library(MASS)

# Load data
setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/EDA")
medData <- read.csv(file="data_cut.csv", header=TRUE, sep=',', na.strings=c("", "NA"))

minus_cols <- c(1,7:15,17,32:34,38,41,43,45,47,49,51,53,55,57,59,61,63,65,66,68,70,72,74,76,78,80,104,105,143,157:161)
medData[,-minus_cols]->medData

# replace all NA values with 0; should we get rid of NAs instead?
medData <- na.omit(medData)

# remove factor columns that are practically not useful or Null
reduced <- select (medData2,-c(TrailsBtime, TrailsBerrors, LM1_B2_total, TrailsAerrors, TrailsAtime
                               , LM1_AB_total, LM2_Btotal, LM2_AB_total, LM2_Atotal, cdx_hypertension))


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
reduced <- reduced %>% mutate(cdx_cog=recode(cdx_cog, 
                         `0`=0,
                         `1`=1,
                         `2`=1,
                         `3`=1,
                         `4`=1,
                         `5`=0))

# recode hispanic variable
reduced <- reduced %>% mutate(ID_Hispanic=recode(ID_Hispanic, 
                                               `1`=0,
                                               `2`=1,
                                               `3`=1,
                                               `4`=1,
                                               `5`=1))

# see all the column types
#sapply(reduced4, class)

# removing these two columns because they are very highly correlated with the predictor
# reduced3a <- select (reduced3a,-c(cdx_cog))

# Another removal step to remove 'Age' columns
# reduced4a <- select (reduced3a,-c(IMH_HighBPAge, IMH_HeartDiseaseAge, IMH_StrokeAge, IMH_AnxietyAge, IMH_OsteoporosisAge
                                #, IMH_ArthritisAge, IMH_SeizuresDisorderAge, IMH_ParkinsonAge, IMH_UTIAge, IMH_DepressionAge
                                #, IMH_DementiaAge, IMH_CancerAge, IMH_DiabetesAge, IMH_HighCholesterolAge, IMH_AlzheimersAge
                                #, IMH_ThyroidDiseaseAge, IMH_KidneyDiseaseAge, IMH_OtherMentalHealthAge
                                #, IMH_RhuematoidArthritisAge, IMH_TBIAge))


# test / train data set split
train <- reduced[1:200,]
test <- reduced[201:401,]


# full fit (MLR) before converting response to factor - R2 0.3932
train$cdx_cog<-as.numeric(train$cdx_cog) 
full.model <- lm(cdx_cog ~., data = train)
summary(full.model)

# stepwise selection - LM
step(lm(cdx_cog ~., data = train),direction="both")

# stepwise recommended model - R2 = 0.5666
model.stepwise <- lm(formula = cdx_cog ~ ID_Race_Black + ID_Residence + ID_USlive + 
                       ID_Income + ID_Retire + OM_Pulse1 + OM_Pulse2 + OM_Height + 
                       OM_Weight + OM_BMI + IMH_Alzheimers + IMH_Dementia + IMH_HeartDisease + 
                       IMH_Stroke + IMH_OtherMentalHealth + TrailsA_sscore + TrailsB_sscore + 
                       LM1_AB_sscore + LM2_AB_sscore + mmse_t_w + bw_choltotal + 
                       bw_HDLchol + bw_triglycerides + bw_LDLchol + bw_glucose + 
                       bw_chloride + bw_calcium + bw_protein + bw_Bilirubin + bw_ALKA + 
                       bw_WBC + bw_RBC + bw_hematocrit + bw_MCV + bw_RDW + bw_platelet + 
                       bw_ABneutro + bw_ABlymph + bw_lymphocytes + cdx_hypertension + 
                       cdx_hypothyroid + bw_MCH, 
                     data = train)
summary(model.stepwise)

# LASSO call
x=model.matrix(cdx_cog~.,train)[,-1]
y=(train$cdx_cog)
xtest<-model.matrix(cdx_cog~.,test)[,-1]
ytest<-test$cdx_cog

# Plot LASSO model
grid=10^seq(10,-2, length =608)
lasso.mod=glmnet(x,y,alpha=1, lambda =grid)
par(mfrow=c(1,1))
cv.out=cv.glmnet(x,y,alpha=1) #alpha=1 performs LASSO
plot(cv.out)
bestlambda<-cv.out$lambda.min  #Optimal penalty parameter.  You can make this call visually.
lasso.pred=predict (lasso.mod ,s=bestlambda ,newx=xtest)

# identify they variables selected by LASSO
coef(lasso.mod,s=bestlambda)

# convert dependent variable to factor
train[, 'cdx_cog'] <- as.factor(train[, 'cdx_cog'])
test[, 'cdx_cog'] <- as.factor(test[, 'cdx_cog'])

# manually built model using practical knowledge and LASSO and Stepwise - AIC 97.75
model.manual <- glm(cdx_cog ~ Age + ID_Gender + ID_USlive + OM_BMI + IMH_Stroke + TrailsA_sscore + LM1_AB_sscore + LM2_AB_sscore 
                + mmse_t_w + bw_hemoglobin + bw_ABneutro + bw_ABmono + bw_lymphocytes
               ,family=binomial(link='logit'),data=train)
summary(model.manual)

# Using the summary coefficients we can generate CI for each one in the table and get odds ratios - manual model
exp(cbind("Odds ratio" = coef(model.manual), confint.default(model.manual, level = 0.95)))
