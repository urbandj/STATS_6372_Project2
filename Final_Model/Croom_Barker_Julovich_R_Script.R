# loadings
library(skimr)
library(dplyr)
library(RColorBrewer)
library(glmnet)
library(gplots)
library(ggplot2)
library(leaps)
library(MASS)
library(ggfortify)
library(grid)
library(gridExtra)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

# Load data
setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/EDA")
setwd("C:/Users/daj0079/Desktop/SMU_2nd/Stats_Project_2")
medData1 <- read.csv(file="data_cut.csv", header=TRUE, sep=',', na.strings=c("", "NA"))

## added co_morbid variable which is sum of all morbidites cdx class variable
medData1$cdx_co_morbid<-rowSums(medData1[,144:151])
minus_cols<-c(1,7:15,16,17,22:24,29:34,36:38,41,43,45,47,48,49,51,53,55,57,59,61,63,64,65,66,68,70,72,74,76,78,80:82,84,85,87,88,90:92,95,99,100,104,105,114,122,123,125,126,136:140,143:151,157:161)
medData1[,-minus_cols]->medData

## added co_morbid variable which is sum of all morbidites IMH class
medData$IMH_co_morbid<-rowSums(medData[,16:32] )

## recode of Hispanic level into NO=0 and Yes=1
medData<- medData %>% mutate(ID_Hispanic=dplyr::recode(ID_Hispanic, 
                                                                    `1`=0,
                                                                    `2`=1,
                                                                    `3`=1,
                                                                    `4`=1,
                                                                    `5`=1))
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

medData$cdx_cog_bucket<-ifelse(medData$cdx_cog=='0', 0, ifelse(medData$cdx_cog=='1', 1,
                                                             ifelse(medData$cdx_cog=='2', 1,
                                                                    ifelse(medData$cdx_cog=='3', 1,
                                                                           ifelse(medData$cdx_cog=='4', 1,
                                                                                  ifelse(medData$cdx_cog=='5', 0,0))))))


# replace all NA values with 0; should we get rid of NAs instead?
medData <- na.omit(medData)

#################################################################
# EDA and DATA VIS see appendix for results
#################################################################

##SKIM view missing, type and spread of data. 
skim(medData)

##GGPAIRS  plots ~20 per plot
scol1<- c(1:20)
medData[,scol1]->eda1
mcols1<-c(8)
eda1[,-mcols1]->eda1

scol2<- c(21:40)
medData[,scol2]->eda2

scol3<- c(41:79)
medData[,scol3]->eda3

ggpairs(eda1)
ggpairs(eda2)
ggpairs(eda3)

#CORRELATION PLOTS WITH COLINEARITY
##omited NA, we are LEFT WITH ~400 obs and 79 variables for corr plot.  
##clearly there are some highly correlated variables.  We need to simplfy
##since some the correlated are basically represented are more than on variable

corr_data<-c(1,7:15,17,32:34,38,41,43,45,47,49,51,53,55,57,59,61,63,65,66,68,70,72,74,76,78,80,104,105,143,157:161)
medData1[,-corr_data]->corr_eda
na.omit(corr_eda)->corr_eda

precorr1<-c(1:58)
precorr2<-c(59:118)

corr_eda[,precorr1]->cp11
corr_eda[,precorr2]->cp12

corr_object1 = cor(cp11)
corr_object2= cor(cp12)

##plot was too big needed to break into smaller chuncks
corrplot(corr_object1, method = "number")#use this in write-up
corrplot(corr_object2, method = "number")#use this in write-up
#####################################################
##cleaned Corr plots after removing colinearity  
medData->corr_noNA

corr1<-c(1:35)
corr2<-c(36:79)

corr_noNA[,corr1]->cp1
corr_noNA[,corr2]->cp2

corr_object1 = cor(cp1)
corr_object2= cor(cp2)

##plot was too big needed to break into smaller chuncks
corrplot(corr_object1, method = "number")#use this in write-up
corrplot(corr_object2, method = "number")#use this in write-up

##################################################################
#Question 1 LGR Model
##################################################################
#Remove extra columns needed for other analysis.  
lgr_cols<-c(76:78)
medData[,-lgr_cols]->reduced

# test / train data set split 50/50
train <- reduced[1:200,]
test <- reduced[201:452,]

full.fit <- lm(formula = cdx_cog_bucket ~ ., data = train)

# full fit (MLR) before converting response to factor - R2 0.3932
train$cdx_cog_bucket<-as.numeric(train$cdx_cog_bucket) 
full.model <- lm(cdx_cog_bucket~., data = train)
summary(full.model)

# stepwise selection - LM
step(lm(cdx_cog_bucket~., data = train),direction="both")

# stepwise recommended model - R2 = 0.5003
model.stepwise <- lm(formula = cdx_cog_bucket ~ Age + ID_Race_White + ID_Residence + ID_Education + 
                       ID_Retire + OM_BMI + IMH_HighBP + IMH_HeartDisease + IMH_ThyroidDisease + 
                       TrailsA_sscore + LM2_AB_sscore + mmse_t_w + bw_hemoglobin + 
                       bw_ABlymph + bw_ABmono, data = train)
summary(model.stepwise)

# LASSO call
x=model.matrix(cdx_cog_bucket~.,train)[,-1]
y=(train$cdx_cog_bucket)
xtest<-model.matrix(cdx_cog_bucket~.,test)[,-1]
ytest<-test$cdx_cog_bucket

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
train[, 'cdx_cog_bucket'] <- as.factor(train[, 'cdx_cog_bucket'])
test[, 'cdx_cog_bucket'] <- as.factor(test[, 'cdx_cog_bucket'])

# manually built model using practical knowledge and LASSO and Stepwise - AIC 102.57
model.manual <- glm(cdx_cog_bucket~ Age + ID_Gender + ID_USlive + OM_BMI + IMH_Stroke + TrailsA_sscore + LM1_AB_sscore + LM2_AB_sscore 
                    + mmse_t_w + bw_hemoglobin + IMH_ThyroidDisease + bw_ABneutro + bw_ABmono + IMH_HighBP + ID_Education 
                    ,family=binomial(link='logit'),data=train)
summary(model.manual)

# Using the summary coefficients we can generate CI for each one in the table and get odds ratios - manual model
exp(cbind("Odds ratio" = coef(model.manual), confint.default(model.manual, level = 0.95)))


#################################################################
###DATA SAMPLING to normalize th counts of disease and normal states, Over/ Under /Both 
################################################################
sum(is.na(medData))
set.seed(1975)
data_over_sam <- ovun.sample(cdx_cog_bucket ~., data = medData, method = "over",N = 730)$data
data_under_sam <- ovun.sample(cdx_cog_bucket ~., data = medData, method = "under",N =176)$data
data_both_sam <- ovun.sample(cdx_cog_bucket ~., data = medData, method = "both",N =500)$data

### simple tables that returns count and prop of cdx_cog and cdx_cog_bucket to keep in mind during anaylsis
cog_table<-table( medData$cdx_cog_bucket,medData$cdx_cog)
cog_table
prop.table(cog_table) # row percentages

bucket_table<-table( medData$cdx_cog_bucket, medData$cdx_cog_bucket)
bucket_table
prop.table(bucket_table) # row percentage

table(data_over_sam$cdx_cog_bucket)
table(data_under_sam$cdx_cog_bucket)
table(data_both_sam$cdx_cog_bucket)

#####################################################################
##PCA 
#####################################################################

#removing cat preds and synthetic vars
k_cols<-c(16:33,72:76,77:79)
medData[,-k_cols]->pca_set


#pca with all predictors only continous variables
pc_cont_result<-prcomp(pca_set,scale.=TRUE)
pc_cont_scores<-pc_cont_result$x
pc_cont_result$x

pc_cont_scores<-data.frame(pc_cont_scores)
options("max.print" = 10000)
pca<-pc_cont_result$x
as.data.frame(pca)->pca
pc_cont_result$rotation


#Scree plot  
screeplot(pc_cont_result, type= "l", npcs = 30, main = "Screeplot of the first 15 PCs", ylim=c(0,4))
abline(h = 1, col="red", lty=6)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)

#Cumulative Variance plo
eigenvals<-(pc_cont_result$sdev)^2
plot(1:53,eigenvals/sum(eigenvals),type="l",main="Cumulative Variance plot",ylab="Prop. Var. Explained",ylim=c(0,1),cex.main=2)
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:53,cumulative.prop,lty=2,,abline(v=24,h=.80 ,col="blue"))

#2d scatter plot of groups
library("factoextra")
fviz_pca_ind(pc_cont_result, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = factor(medData$cdx_cog_bucket), 
             col.ind = "black", 
             palette = "wes_palette", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,axes = c(2,3),
             legend.title = "Cognition") +
  ggtitle("2D PCA-plot") +
  theme(plot.title = element_text(hjust = 0.5))

##Estimate the predictors for the highest loading values and compare this is the write to other selection methods
pc_cont_result$rotation->pca_results
(as.data.frame(pca_results))->pca_results
pca_results <- cbind(Row.Names = rownames(pca_results), pca_results)
pca_results

#takes abs of loading for quick comparaison
abs_pca_results<-abs(pca_results[,2:53])

##looks at loadings with high levelof contribution
pc2_vars <- subset(abs_pca_results, PC2 >0.09)
pc2_vars[,c(2,3)]
pc3_vars <- subset(abs_pca_results,  PC3 >0.09)
pc3_vars[,c(2,3)]

#################################################################
#LDA QDA ANALYSIS
####################################################################
##remove the catogorical variable for LDA QDA
qda_split<-c(2:7,16:33,72:76)
medData[,-qda_split]->qda_vars

set.seed(1234)
trainobs=sample(seq(1,dim(qda_vars)[1]),round(.75*dim(qda_vars)[1]),replace=FALSE)
LDA_train=qda_vars[trainobs,]
LDA_test=qda_vars[-trainobs,]

##LDA prediction with 75/25 train test and ROC curve
lda<-lda(cdx_cog_bucket~ Age  + OM_BMI + ID_USlive+ TrailsA_sscore + LM1_AB_sscore + LM2_AB_sscore 
         + mmse_t_w + bw_hemoglobin + bw_ABneutro + bw_ABmono  +ID_Education
         , data = LDA_train)
lda
#Run lda prediction on train set
lda.pred<-predict(lda, LDA_train)
matrix_pred<-lda.pred$class
bucket_train<-as.factor(LDA_train$cdx_cog_bucket)
table(Predicted=lda.pred$class,Cognition=LDA_train$cdx_cog_bucket)

#Print results
confusionMatrix(matrix_pred,bucket_train, dnn = c("Prediction", "Cognition"),  mode = "sens_spec")

# plot out the histogram for eac predicted groups
ldahist(lda.pred$x[,1], g= lda.pred$class,col = 8)

#Run lda on test set
test.lda <- predict(lda, newdata=LDA_test)
matrix_test<-test.lda$class
table(Predicted=test.lda$class, Cognition=LDA_test$cdx_cog_bucket)

bucket_test<-as.factor(LDA_test$cdx_cog_bucket)

#plots the results from the test set
plot(test.lda$x[,1], test.lda$class, col=LDA_test$cdx_cog_bucket+10)
confusionMatrix(matrix_test, bucket_test, dnn = c("Prediction", "Cognition"),  mode = "sens_spec")

#plot ROC of LDA test results
pred <- prediction(test.lda$posterior[,2], LDA_test$cdx_cog_bucket) 
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

##QDA predictions with 75/25 train test and ROC curve
qda<-qda(cdx_cog_bucket~ Age  + OM_BMI + ID_USlive+ TrailsA_sscore + LM1_AB_sscore + LM2_AB_sscore 
          + mmse_t_w + bw_hemoglobin + bw_ABneutro + bw_ABmono  +ID_Education
          , data = LDA_train)
qda
#Use qda to on train set
qda.pred<-predict(qda, LDA_train)
qda_class<-qda.pred$class


#print out training results 
table(Predicted=qda.pred$class,Cognition=LDA_train$cdx_cog_bucket)
confusionMatrix(qda_class ,bucket_train, dnn = c("Prediction", "Cognition"),  mode = "sens_spec")

#run qda on test set
test.qda <- predict(qda, newdata=LDA_test)

#print out QDA test results
qda_matrix_test<-test.qda$class

table(Predicted=test.qda$class, Cognition=LDA_test$cdx_cog_bucket)
plot(test.qda$posterior[,2], test.qda$class, col=bucket_test)
confusionMatrix(qda_matrix_test,bucket_test, dnn = c("Prediction", "Cognition"),  mode = "sens_spec")

#plot ROC of QDA test results
par(mfrow=c(1,1))
pred2 <- prediction(test.qda$posterior[,2], LDA_test$cdx_cog_bucket) 
perf2 <- performance(pred2,"tpr","fpr")
plot(perf2,colorize=TRUE)
auc.train2 <- performance(pred2, measure = "auc")
auc.train2 <- auc.train2@y.values
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train2[[1]],3), sep = ""))


#combined Roc plots on test data for LDA and QDA no real difference is seen
library(pROC)
plot(roc(LDA_test$cdx_cog_bucket,test.lda$posterior[,2]), print.auc = TRUE, col = "blue", main='ROC Plots from Test Set QDA=Red LDA=Blue',
     print.auc.y = 0.3, cex.main=2)
plot(roc(LDA_test$cdx_cog_bucket, test.qda$posterior[,2]), print.auc = TRUE, col = "red", add=TRUE)



#######################################################################################################################
## TREE CALCULATIONS
#######################################################################################################################

#Run with 50/50 train test split
#######################################################################################################################


summary(train$cdx_cog_bucket)
#Need to convert to factor for trees to work correctly
train$cdx_cog_bucket = factor(train$cdx_cog_bucket)
test$cdx_cog_bucket = factor(test$cdx_cog_bucket)
summary(train$cdx_cog_bucket)

#Run regular trees
tree.medical=tree(cdx_cog_bucket~.,train)
plot(tree.medical)
text(tree.medical, pretty = 0)
tree.pred=predict(tree.medical,test,type="class")
table(tree.pred,test$cdx_cog_bucket)

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
table(tree.pred,test$cdx_cog_bucket)

#We can verify the overfitting idea by saying to split the tree
#very deep
prune.medical=prune.misclass(tree.medical,best=9)
plot(prune.medical)
text(prune.medical,pretty=0)
tree.pred=predict(prune.medical,test,type="class")
table(tree.pred,test$cdx_cog_bucket)


#For ROC curves on a single decision tree you need predicted probabilities to
#use the previous R scripts.  Lets just use the example here with the last run.
tree.pred=predict(prune.medical,test,type="vector")
head(tree.pred)

pred <- prediction(tree.pred[,2], test$cdx_cog_bucket)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#Note in the following code the term "train" means nothing here. 
#I'm just rinsing and repeating code the produces the curve.
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set of a Single Tree")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Execute the random forest
rf.model = randomForest(cdx_cog_bucket~.,train,subsets=train,importance=T,ntree=100)
fit.pred<-predict(rf.model,newdata=test,type="response")
table(fit.pred,test$cdx_cog_bucket)

#Run ROC curves for RandomForest Model
rf.pred<-predict(rf.model,newdata=test,type="prob")
pred <- prediction(rf.pred[,2], test$cdx_cog_bucket)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#Note in the following code the term "train" means nothing here. 
#I'm just rinsing and repeating code the produces the curve.
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set RF")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#Variable Importance Plots
varImpPlot (rf.model,type=1,main="Variable Importance")
varImpPlot (rf.model,type=2,main="Variable Importance")

#Run with 75/25 train test split
#######################################################################################################################

set.seed(1234)
trainobs=sample(seq(1,dim(reduced)[1]),round(.75*dim(reduced)[1]),replace=FALSE)
train=reduced[trainobs,]
test=reduced[-trainobs,]

summary(train$cdx_cog_bucket)
#Need to convert to factor for trees to work correctly
train$cdx_cog_bucket = factor(train$cdx_cog_bucket)
test$cdx_cog_bucket = factor(test$cdx_cog_bucket)
summary(train$cdx_cog_bucket)

#Run regular trees
tree.medical=tree(cdx_cog_bucket~.,train)
plot(tree.medical)
text(tree.medical, pretty = 0)
tree.pred=predict(tree.medical,test,type="class")
table(tree.pred,test$cdx_cog_bucket)

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
table(tree.pred,test$cdx_cog_bucket)

#We can verify the overfitting idea by saying to split the tree
#very deep
prune.medical=prune.misclass(tree.medical,best=9)
plot(prune.medical)
text(prune.medical,pretty=0)
tree.pred=predict(prune.medical,test,type="class")
table(tree.pred,test$cdx_cog_bucket)


#For ROC curves on a single decision tree you need predicted probabilities to
#use the previous R scripts.  Lets just use the example here with the last run.
tree.pred=predict(prune.medical,test,type="vector")
head(tree.pred)

pred <- prediction(tree.pred[,2], test$cdx_cog_bucket)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#Note in the following code the term "train" means nothing here. 
#I'm just rinsing and repeating code the produces the curve.
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set of a Single Tree")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))


#Execute the random forest
rf.model = randomForest(cdx_cog_bucket~.,train,subsets=train,importance=T,ntree=100)
fit.pred<-predict(rf.model,newdata=test,type="response")
table(fit.pred,test$cdx_cog_bucket)

#Run ROC curves for RandomForest Model
rf.pred<-predict(rf.model,newdata=test,type="prob")
pred <- prediction(rf.pred[,2], test$cdx_cog_bucket)
roc.perf = performance(pred, measure = "tpr", x.measure = "fpr")
#Note in the following code the term "train" means nothing here. 
#I'm just rinsing and repeating code the produces the curve.
auc.train <- performance(pred, measure = "auc")
auc.train <- auc.train@y.values
plot(roc.perf,main="AUC of Test set RF")
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(auc.train[[1]],3), sep = ""))

#Variable Importance Plots
varImpPlot (rf.model,type=1,main="Variable Importance")
varImpPlot (rf.model,type=2,main="Variable Importance")

###############################################
##K means clustering
####################################
#removed explanitory vaiables 
k_cols<-c(72:75,76,79)
medData[,-k_cols]->km_set

#Normalized the scale for kmeans anaylsis
km<-scale(km_set)

distance <- get_dist(km)
dev.off()
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
k2 <- kmeans(km, centers =2, nstart =25)
fviz_cluster(k2, data = km)

k2
km %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         cogstate= row.names(cdx_cog_bucket)) %>%
  ggplot(aes(cdx_cog_bucket, Murder, color = factor(cluster), label = state)) +
  geom_text()

set.seed(123)
fviz_nbclust(km, kmeans, method = "wss")

set.seed(123)
final <- kmeans(km, 2, nstart = 50)
print(final)



















