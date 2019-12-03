
# loadings
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

minus_cols<-c(1,7:17,22:24,29:34,36:38,41,43,45,47,48,49,51,53,55,57,59,61,63,64,65,66,68,70,72,74,76,78,80:82,84,85,87,88,90:92,95,99,100,104,105,114,122,123,125,126,136:140,143:151,157:161)
medData[,-minus_cols]->medData

# replace all NA values with 0; should we get rid of NAs instead?
medData <- na.omit(medData)

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
reduced <- medData %>% mutate(cdx_cog=recode(cdx_cog, 
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

# test / train data set split 50/50
train <- reduced[1:200,]
test <- reduced[201:452,]

# full fit (MLR) before converting response to factor - R2 0.3932
train$cdx_cog<-as.numeric(train$cdx_cog) 
full.model <- lm(cdx_cog ~., data = train)
summary(full.model)

# stepwise selection - LM
step(lm(cdx_cog ~., data = train),direction="both")

# stepwise recommended model - R2 = 0.5666
model.stepwise <- lm(formula = cdx_cog ~ Age + ID_Race_White + ID_Residence + ID_Education + 
                       ID_Retire + OM_BMI + IMH_HighBP + IMH_HeartDisease + IMH_ThyroidDisease + 
                       TrailsA_sscore + LM2_AB_sscore + mmse_t_w + bw_hemoglobin + 
                       bw_ABlymph + bw_ABmono, data = train)
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

# manually built model using practical knowledge and LASSO and Stepwise - AIC 101.39
model.manual <- glm(cdx_cog ~ Age + ID_Gender + ID_USlive + OM_BMI + IMH_Stroke + TrailsA_sscore + LM1_AB_sscore + LM2_AB_sscore 
                + mmse_t_w + bw_hemoglobin + + IMH_ThyroidDisease + bw_ABneutro + bw_ABmono + bw_ABlymph + IMH_HighBP + ID_Education 
                + ID_Race_White 
               ,family=binomial(link='logit'),data=train)
summary(model.manual)

# Using the summary coefficients we can generate CI for each one in the table and get odds ratios - manual model
exp(cbind("Odds ratio" = coef(model.manual), confint.default(model.manual, level = 0.95)))
