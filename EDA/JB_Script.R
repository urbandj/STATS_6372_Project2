
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
                               , LM1_AB_total, LM2_Btotal, LM2_AB_total, LM2_Atotal))


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

# manually built model using practical knowledge - AIC 255.93 - removed Alzheimers because practically it follows that folks with this disease
# will have cognitive impairment
model.manual <- glm(cdx_cog ~ Age + ID_Gender + ID_USlive + OM_BMI + IMH_Stroke + TrailsA_sscore + LM1_AB_sscore + LM2_AB_sscore 
                + mmse_t_w + bw_hemoglobin + bw_ABneutro + bw_ABmono + bw_lymphocytes + cdx_hypertension
               ,family=binomial(link='logit'),data=train)
summary(model.manual)

sapply(train, class)

# Using the summary coefficients we can generate CI for each one in the table and get odds ratios - manual model
exp(cbind("Odds ratio" = coef(model.manual), confint.default(model.manual, level = 0.95))) 

# using aggregate to check the various groupings and summary stats for each predictor
aggregate(cdx_cog2 ~ ID_Race_IndianAlaska,data=train,summary)
aggregate(cdx_cog2 ~ ID_Education_Degree,data=train,summary)
aggregate(cdx_cog2 ~ OM_AbCircumference,data=train,summary)
aggregate(cdx_cog2 ~ OM_Height,data=train,summary)
aggregate(cdx_cog2 ~ OM_Weight,data=train,summary)
aggregate(cdx_cog2 ~ IMH_Anxiety,data=train,summary)
aggregate(cdx_cog2 ~ IMH_Osteoporosis,data=train,summary)
aggregate(cdx_cog2 ~ IMH_SeizureDisorder,data=train,summary)
aggregate(cdx_cog2 ~ TrailsAtime,data=train,summary)
aggregate(cdx_cog2 ~ TrailsAerrors,data=train,summary)
aggregate(cdx_cog2 ~ TrailsBtime,data=train,summary)
aggregate(cdx_cog2 ~ LM1_AB_total,data=train,summary)
aggregate(cdx_cog2 ~ LM1_AB_sscore,data=train,summary)
aggregate(cdx_cog2 ~ LM2_Atotal,data=train,summary)
aggregate(cdx_cog2 ~ LM2_Btotal,data=train,summary)
aggregate(cdx_cog2 ~ LM2_AB_total,data=train,summary)
aggregate(cdx_cog2 ~ mmse_t_w,data=train,summary)
aggregate(cdx_cog2 ~ bw_choltotal,data=train,summary)
aggregate(cdx_cog2 ~ bw_UAbun,data=train,summary)
aggregate(cdx_cog2 ~ bw_eGFRnonAA,data=train,summary)
aggregate(cdx_cog2 ~ bw_eGFRAA,data=train,summary)
aggregate(cdx_cog2 ~ bw_calcium,data=train,summary)
aggregate(cdx_cog2 ~ bw_Bilirubin,data=train,summary)
aggregate(cdx_cog2 ~ bw_hematocrit,data=train,summary)
aggregate(cdx_cog2 ~ bw_platelet,data=train,summary)
aggregate(cdx_cog2 ~ cdx_hypertension,data=train,summary)
aggregate(cdx_cog2 ~ cdx_anemia,data=train,summary)
aggregate(cdx_cog2 ~ cdx_hypothyroid,data=train,summary)
aggregate(cdx_cog2 ~ APOE_4,data=train,summary)
aggregate(cdx_cog2 ~ ID_Hispanic2,data=train,summary)
aggregate(cdx_cog2 ~ ID_Race_White,data=train,summary)
aggregate(cdx_cog2 ~ bw_ALB_GLOB,data=train,summary)
aggregate(cdx_cog2 ~ TrailsA_sscore,data=train,summary)

# proportion tables
prop.ID_Race_IndianAlaska <- prop.table(table(train$cdx_cog2, train$ID_Race_IndianAlaska),2)
prop.ID_Education_Degree <- prop.table(table(train$cdx_cog2, train$ID_Education_Degree),2)
prop.OM_AbCircumference <- prop.table(table(train$cdx_cog2, train$OM_AbCircumference),2)
prop.OM_Height <- prop.table(table(train$cdx_cog2, train$OM_Height),2)
prop.OM_Weight <- prop.table(table(train$cdx_cog2, train$OM_Weight),2)
prop.IMH_Anxiety <- prop.table(table(train$cdx_cog2, train$IMH_Anxiety),2)
prop.IMH_Osteoporosis <- prop.table(table(train$cdx_cog2, train$IMH_Osteoporosis),2)
prop.IMH_SeizureDisorder <- prop.table(table(train$cdx_cog2, train$IMH_SeizureDisorder),2)
prop.TrailsAtime <- prop.table(table(train$cdx_cog2, train$TrailsAtime),2)
prop.TrailsAerrors <- prop.table(table(train$cdx_cog2, train$TrailsAerrors),2)
prop.TrailsBtime <- prop.table(table(train$cdx_cog2, train$TrailsBtime),2)
prop.LM1_AB_total <- prop.table(table(train$cdx_cog2, train$LM1_AB_total),2)
prop.LM1_AB_sscore <- prop.table(table(train$cdx_cog2, train$LM1_AB_sscore),2)
prop.LM2_Atotal <- prop.table(table(train$cdx_cog2, train$LM2_Atotal),2)
prop.LM2_Btotal <- prop.table(table(train$cdx_cog2, train$LM2_Btotal),2)
prop.LM2_AB_total <- prop.table(table(train$cdx_cog2, train$LM2_AB_total),2)
prop.mmse_t_w <- prop.table(table(train$cdx_cog2, train$mmse_t_w),2)
prop.bw_choltotal <- prop.table(table(train$cdx_cog2, train$bw_choltotal),2)
prop.bw_UAbun <- prop.table(table(train$cdx_cog2, train$bw_UAbun),2)
prop.bw_eGFRnonAA <- prop.table(table(train$cdx_cog2, train$bw_eGFRnonAA),2)
prop.bw_eGFRAA <- prop.table(table(train$cdx_cog2, train$bw_eGFRAA),2)
prop.bw_calcium <- prop.table(table(train$cdx_cog2, train$bw_calcium),2)
prop.bw_Bilirubin <- prop.table(table(train$cdx_cog2, train$bw_Bilirubin),2)
prop.bw_hematocrit <- prop.table(table(train$cdx_cog2, train$bw_hematocrit),2)
prop.bw_platelet <- prop.table(table(train$cdx_cog2, train$bw_platelet),2)
prop.cdx_hypertension <- prop.table(table(train$cdx_cog2, train$cdx_hypertension),2)
prop.cdx_hypothyroid <- prop.table(table(train$cdx_cog2, train$cdx_hypothyroid),2)
prop.cdx_anemia <- prop.table(table(train$cdx_cog2, train$cdx_anemia),2)
prop.APOE_4 <- prop.table(table(train$cdx_cog2, train$APOE_4),2)
prop.ID_Hispanic2 <- prop.table(table(train$cdx_cog2, train$ID_Hispanic2),2)
prop.ID_Race_White <- prop.table(table(train$cdx_cog2, train$ID_Race_White),2)
prop.bw_ALB_GLOB <- prop.table(table(train$cdx_cog2, train$bw_ALB_GLOB),2)
prop.TrailsA_sscore <- prop.table(table(train$cdx_cog2, train$TrailsA_sscore),2)

# Visualize proportion tables
plot(train$cdx_cog2~train$Age,col=c("red","blue"))
plot(train$cdx_cog2~train$ID_Gender,col=c("red","blue"))
plot(train$cdx_cog2~train$mmse_t_w,col=c("red","blue"))
plot(train$cdx_cog2~train$ID_Race_IndianAlaska,col=c("red","blue"))
plot(train$cdx_cog2~train$ID_Education_Degree,col=c("red","blue"))
plot(train$cdx_cog2~train$OM_AbCircumference,col=c("red","blue"))
plot(train$cdx_cog2~train$OM_Height,col=c("red","blue"))
plot(train$cdx_cog2~train$OM_Weight,col=c("red","blue"))
plot(train$cdx_cog2~train$IMH_Anxiety,col=c("red","blue"))
plot(train$cdx_cog2~train$IMH_Osteoporosis,col=c("red","blue"))
plot(train$cdx_cog2~train$IMH_SeizureDisorder,col=c("red","blue"))
plot(train$cdx_cog2~train$TrailsAtime,col=c("red","blue"))
plot(train$cdx_cog2~train$TrailsA_sscore,col=c("red","blue"))
plot(train$cdx_cog2~train$TrailsAerrors,col=c("red","blue"))
plot(train$cdx_cog2~train$TrailsBtime,col=c("red","blue"))
plot(train$cdx_cog2~train$LM1_AB_total,col=c("red","blue"))
plot(train$cdx_cog2~train$LM1_AB_sscore,col=c("red","blue"))
plot(train$cdx_cog2~train$LM2_Atotal,col=c("red","blue"))
plot(train$cdx_cog2~train$LM2_Btotal,col=c("red","blue"))
plot(train$cdx_cog2~train$LM2_AB_total,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_choltotal,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_UAbun,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_eGFRnonAA,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_eGFRAA,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_calcium,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_Bilirubin,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_hematocrit,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_platelet,col=c("red","blue"))
plot(train$cdx_cog2~train$cdx_hypertension,col=c("red","blue"))
plot(train$cdx_cog2~train$cdx_hypothyroid,col=c("red","blue"))
plot(train$cdx_cog2~train$cdx_anemia,col=c("red","blue"))
plot(train$cdx_cog2~train$APOE_4,col=c("red","blue"))
plot(train$cdx_cog2~train$ID_Hispanic2,col=c("red","blue"))
plot(train$cdx_cog2~train$ID_Race_White,col=c("red","blue"))
plot(train$cdx_cog2~train$bw_ALB_GLOB,col=c("red","blue"))

# Correlation plots for continuous predictors (not including gender since it's categorical)
pairs(train[,c("LM1_AB_sscore","Age","mmse_t_w", "ID_Race_IndianAlaska", "ID_Education_Degree"
               ,"OM_AbCircumference","OM_Height","OM_Weight","IMH_Anxiety","IMH_Osteoporosis","IMH_SeizureDisorder"
               ,"TrailsAtime","TrailsA_sscore","TrailsAerrors","TrailsBtime","LM1_AB_total", "LM1_AB_sscore"
               ,"LM2_Atotal", "LM2_Btotal","LM2_AB_total","bw_choltotal","bw_UAbun","bw_eGFRnonAA","bw_eGFRAA"
               ,"bw_calcium","bw_Bilirubin","bw_hematocrit","bw_platelet","cdx_hypertension","cdx_hypothyroid"
               ,"cdx_anemia","APOE_4","ID_Hispanic2","ID_Race_White","bw_ALB_GLOB")])

my.cor<-cor(train[,c("LM1_AB_sscore","Age","mmse_t_w", "ID_Race_IndianAlaska", "ID_Education_Degree"
                     ,"OM_AbCircumference","OM_Height","OM_Weight","IMH_Anxiety","IMH_Osteoporosis","IMH_SeizureDisorder"
                     ,"TrailsAtime","TrailsA_sscore","TrailsAerrors","TrailsBtime","LM1_AB_total", "LM1_AB_sscore"
                     ,"LM2_Atotal", "LM2_Btotal","LM2_AB_total","bw_choltotal","bw_UAbun","bw_eGFRnonAA","bw_eGFRAA"
                     ,"bw_calcium","bw_Bilirubin","bw_hematocrit","bw_platelet","cdx_hypertension","cdx_hypothyroid"
                     ,"cdx_anemia","APOE_4","ID_Hispanic2","ID_Race_White","bw_ALB_GLOB")])

pairs(train[,c("LM1_AB_sscore","Age","mmse_t_w", "ID_Race_IndianAlaska", "ID_Education_Degree"
               ,"OM_AbCircumference","OM_Height","OM_Weight","IMH_Anxiety","IMH_Osteoporosis","IMH_SeizureDisorder"
               ,"TrailsAtime","TrailsA_sscore","TrailsAerrors","TrailsBtime","LM1_AB_total", "LM1_AB_sscore"
               ,"LM2_Atotal", "LM2_Btotal","LM2_AB_total","bw_choltotal","bw_UAbun","bw_eGFRnonAA","bw_eGFRAA"
               ,"bw_calcium","bw_Bilirubin","bw_hematocrit","bw_platelet","cdx_hypertension","cdx_hypothyroid"
               ,"cdx_anemia","APOE_4","ID_Hispanic2","ID_Race_White","bw_ALB_GLOB")],col=train$cdx_cog2)

# heatmap for correlation plot
heatmap.2(my.cor,col=redgreen(75), 
          density.info="none", trace="none", dendrogram=c("row"), 
          symm=F,symkey=T,symbreaks=T, scale="none")
