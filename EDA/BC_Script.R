
# diabetes MLR exploration

library(MASS)

# Load data
#setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/Data")
setwd("C:/Users/croomb/OneDrive - BAT/Desktop/Personal Training/SMU/DS 6372 - Applied Statistics/Projects/Project2/STATS_6372_Project2-master/STATS_6372_Project2-master/Data/")

medData <- read.csv(file="DJ_Project2_Data.csv", header=TRUE)

medData

# -17, -143  IMH_Diabetes  IMH_OtherMentalHealthSpec
reduced<-medData[,-c(17,143)]
reduced
medData[,c(143)]
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


# Load data
#setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/EDA")
setwd("C:/Users/croomb/OneDrive - BAT/Desktop/Personal Training/SMU/DS 6372 - Applied Statistics/Projects/Project2/STATS_6372_Project2-master/STATS_6372_Project2-master/EDA/")


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

# test / train data set split
train <- reduced4a[1:350,]
test <- reduced4a[351:741,]

# logistic regression models - full fit
model  <- glm(cdx_cog2 ~ .,family=binomial(link='logit'),data=train)

# LM2_AB_sscore | LM1_AB_sscore and TrailsA_sscore | TrailsB_sscore  - trails (higher the score the worse)are cognitive test scores
# Partial fit
model  <- glm(cdx_cog2 ~ cdx_depression + LM1_AB_sscore + Age + TrailsA_sscore + IMH_RhuematoidArthritis + IMH_Osteoporosis
              + IMH_Anxiety + IMH_HeartDisease + OM_BP1_SYS + OM_BP1_DIA + ID_Retire + ID_MaritalStatus
              + mmse_t_w,family=binomial(link='logit'),data=train)
summary(model)

# Reduced fit
model  <- glm(cdx_cog2 ~ LM1_AB_sscore + Age + TrailsA_sscore + mmse_t_w + ID_Gender
              ,family=binomial(link='logit'),data=train)
summary(model)

# ANOVA on regression results
anova(model, test="Chisq")

# LDA
mylda <- lda(cdx_cog2 ~., data = train)
mylda <- lda(cdx_cog2 ~ cdx_depression + LM1_AB_sscore + Age + TrailsA_sscore + mmse_t_w + ID_Gender, data = train)

np <- 1
nd.x <- seq(from = min(full$X1), to = max(full$X1), length.out = np)
nd.y <- seq(from = min(full$X2), to = max(full$X2), length.out = np)
nd <- expand.grid(X1 = nd.x, X2 = nd.y)

prd <- as.numeric(predict(mylda, newdata = test)$class)

plot(full[, 1:2], col = full$Response, main="Shift in X2")
points(mylda$means, pch = "+", cex = 2, col = c("black", "red"))
contour(x = nd.x, y = nd.y, z = matrix(prd, nrow = np, ncol = np), 
        levels = c(1, 2), add = TRUE, drawlabels = FALSE)

#######################################################################################################################
## TREE CALCULATIONS
#######################################################################################################################

library(randomForest)
library(tree)
library(ISLR)
library(ROCR)

summary(train$cdx_cog2)
#Need to convert to factor for trees to work correctly
train$cdx_cog2 = factor(train$cdx_cog2)
test$cdx_cog2 = factor(test$cdx_cog2)
summary(train$cdx_cog2)

#Run regular trees
tree.medical=tree(cdx_cog2~.-cdx_cog,train)
plot(tree.medical)
text(tree.medical, pretty = 0)
tree.pred=predict(tree.medical,test,type="class")
table(tree.pred,test$cdx_cog2)

#Prune the trees 
set.seed(3)
par(mfrow=c(1,1))
cv.medical=cv.tree(tree.medical,FUN=prune.misclass)
names(cv.medical)
plot(cv.medical)
#Fit the pruned tree and visualize
prune.medical=prune.misclass(tree.medical,best=9)
plot(prune.medical)
text(prune.medical,pretty=0)

tree.pred=predict(prune.medical,test,type="class")
table(tree.pred,test$cdx_cog2)

#We can verify the overfitting idea by saying to split the tree
#very deep
prune.medical=prune.misclass(tree.medical,best=9)
plot(prune.medical)
text(prune.medical,pretty=0)
tree.pred=predict(prune.medical,test,type="class")
table(tree.pred,test$cdx_cog2)


#For ROC curves on a single decision tree you need predicted probabilities to
#use the previous R scripts.  Lets just use the example here with the last run.
tree.pred=predict(prune.medical,test,type="vector")
head(tree.pred)

library(ROCR)
pred <- prediction(tree.pred[,2], test$cdx_cog2)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#Note in the following code the term "train" means nothing here. 
#I'm just rinsing and repeating code the produces the curve.
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set of a Single Tree")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Execute the random forest
rf.model = randomForest(cdx_cog2~.-cdx_cog,train,subsets=train,importance=T,ntree=100)
fit.pred<-predict(rf.model,newdata=test,type="response")
table(fit.pred,test$cdx_cog2)

#Run ROC curves for RandomForest Model
rf.pred<-predict(rf.model,newdata=test,type="prob")
pred <- prediction(rf.pred[,2], test$cdx_cog2)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#Note in the following code the term "train" means nothing here. 
#I'm just rinsing and repeating code the produces the curve.
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set RF - mtry=5")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#Variable Importance Plots
varImpPlot (rf.model,type=1,main="Variable Importance")
varImpPlot (rf.model,type=2,main="Variable Importance")

#Run regular trees
tree.medical=tree(cdx_cog2~.-cdx_cog,train)
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
