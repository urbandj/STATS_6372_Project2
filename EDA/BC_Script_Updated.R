
# loadings
library(skimr)
library(dplyr)
library(RColorBrewer)
library(glmnet)
library(gplots)
library(ggplot2)
library(leaps)
library(MASS)
library(randomForest)
library(tree)
library(ISLR)
library(ROCR)

# Load data
#setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/EDA")
setwd("C:/Users/croomb/OneDrive - BAT/Desktop/Personal Training/SMU/DS 6372 - Applied Statistics/Projects/Project2/STATS_6372_Project2-master/STATS_6372_Project2-master/")
medData <- read.csv(file="./EDA/data_cut.csv", header=TRUE, sep=',', na.strings=c("", "NA"))

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

skim(reduced) %>% kable()

# test / train data set split 50/50
train <- reduced[1:200,]
test <- reduced[201:452,]

#######################################################################################################################
## TREE CALCULATIONS
#######################################################################################################################

summary(train$cdx_cog)
#Need to convert to factor for trees to work correctly
train$cdx_cog = factor(train$cdx_cog)
test$cdx_cog = factor(test$cdx_cog)
summary(train$cdx_cog)

#Run regular trees
tree.medical=tree(cdx_cog~.,train)
plot(tree.medical)
text(tree.medical, pretty = 0)
tree.pred=predict(tree.medical,test,type="class")
table(tree.pred,test$cdx_cog)

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
table(tree.pred,test$cdx_cog)

#We can verify the overfitting idea by saying to split the tree
#very deep
prune.medical=prune.misclass(tree.medical,best=9)
plot(prune.medical)
text(prune.medical,pretty=0)
tree.pred=predict(prune.medical,test,type="class")
table(tree.pred,test$cdx_cog)


#For ROC curves on a single decision tree you need predicted probabilities to
#use the previous R scripts.  Lets just use the example here with the last run.
tree.pred=predict(prune.medical,test,type="vector")
head(tree.pred)

pred <- prediction(tree.pred[,2], test$cdx_cog)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#Note in the following code the term "train" means nothing here. 
#I'm just rinsing and repeating code the produces the curve.
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set of a Single Tree")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Execute the random forest
rf.model = randomForest(cdx_cog~.,train,subsets=train,importance=T,ntree=100)
fit.pred<-predict(rf.model,newdata=test,type="response")
table(fit.pred,test$cdx_cog)

#Run ROC curves for RandomForest Model
rf.pred<-predict(rf.model,newdata=test,type="prob")
pred <- prediction(rf.pred[,2], test$cdx_cog)
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


