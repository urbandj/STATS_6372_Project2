library("varImp")
library(tidyverse) #general data wrangling tools
library(skimr) #summary stats
library(tangram) #has is.categorical() function, useful for creating tables
library(car) # Regression tools
library(fastDummies) # creates dummy variables
library(rlang)
library(olsrr)
library(forcats) 
library(roperators)
library(corrplot)
library(mefa4)
library(MASS)#studentized residuals
library(caret)
library(dplyr)
library(ISLR)
library(leaps)
library(Hmisc)
library(ggplot2)
library("lindia")
library(GGally)
library(ROSE)
library(dplyr)
library(RColorBrewer)
library(glmnet)
library("tree")
setwd("C:/Users/daj0079/Desktop/SMU_2nd/Stats_Project_2")
#######
##read in CSV 
#######
##with 999 changed to blanks. The data set still contains NAs 
##data needs to be divided into correct modeling sections

lgr_eda<-read.csv("C:/Users/daj0079/Desktop/SMU_2nd/Stats_Project_2/data_cut.csv", header = TRUE, sep=",")

########
##REMOVED VARIABLES
########
##Removed MedId
##Removed 9 races without data and 
##Removed OM_notes with long string
##Removed cdx_date
##Removed ALL IMH_age variables, very high levels of missing.  May want to consider these is looking into a age dervied model.
##Removed APOE variables, These we be good in a smaller model.
##Removed eGRF both variables
##Removed 3rd BP and pluse measurment variables
##Removed cdx_mci, may want to add this back for an MCI model
##Removed IMCOME should run models with and without this variable

minus_cols<-c(1,7:15,32:34,38,41,43,45,47,49,51,53,55,57,59,61,63,66,68,70,72,74,76,78,80,104,105,143,157:161)
apoe_cols<-c(1,7:15,32:34,38,41,43,45,47,49,51,53,55,57,59,61,63,66,68,70,72,74,76,78,80,104,105,143,157,158)
lgr_eda[,-minus_cols]->lgr_eda2
lgr_eda[,-apoe_cols]->lgr_eda_apoe
na.omit(lgr_eda2)->nona_lgr_eda2
na.omit(lgr_eda_apoe)->nona_lgr_eda_apoe

##bucket cdx_cog var for sampling 
cdx_cog_bucket<-ifelse(nona_lgr_eda2$cdx_cog=='0', 0, ifelse(nona_lgr_eda2$cdx_cog=='1', 1,
                                                                  ifelse(nona_lgr_eda2$cdx_cog=='2', 1,
                                                                         ifelse(nona_lgr_eda2$cdx_cog=='3', 1,
                                                                                ifelse(nona_lgr_eda2$cdx_cog=='4', 1,
                                                                                       ifelse(nona_lgr_eda2$cdx_cog=='5', 0,0))))))
##added cdx_cog2 bucket
cbind(nona_lgr_eda2, cdx_cog_bucket)->nona_lgr_eda2
                       
###Over and Under and Both sampling to normalize th counts of disease ad normal states
sum(is.na(nona_lgr_eda2))
set.seed(1975)
data_over_sam <- ovun.sample(cdx_cog_bucket ~., data = nona_lgr_eda2, method = "over",N = 656)$data
data_under_sam <- ovun.sample(cdx_cog_bucket ~., data = nona_lgr_eda2, method = "under",N =146)$data
data_both_sam <- ovun.sample(cdx_cog_bucket ~., data = nona_lgr_eda2, method = "both",N =500)$data

table(nona_lgr_eda2$cdx_cog_bucket)
table(data_over_sam$cdx_cog_bucket)
table(data_under_sam$cdx_cog_bucket)
table(data_both_sam$cdx_cog_bucket)


#######
##SKIM 
#######
#view missing, type and spread of data. 
skim(lgr_eda2)

#######
##GGPAIRS for EDA and correlations plots ~20 per plot
#######
scol1<- c(1:20)
lgr_eda2[,scol1]->eda1
mcols1<-c(8)
eda1[,-mcols1]->eda1

scol2<- c(21:40)
lgr_eda2[,scol2]->eda2

scol3<- c(41:80)
lgr_eda2[,scol3]->eda3

scol4<- c(81:100)
lgr_eda2[,scol4]->eda4

scol5<- c(101:120)
lgr_eda2[,scol5]->eda5

scol6<-c(121:149)
lgr_eda2[,scol6]->eda6

ggpairs(eda1)
ggpairs(eda2)
ggpairs(eda3)
ggpairs(eda4)
ggpairs(eda5)
ggpairs(eda6)

#######
#CORRELATION PLOTS
#######
#remove factors vars
factor_vars<- c(7,39)
lgr_eda2[,-factor_vars]->corr

##omit NA, we are LEFT WITH 400 obs and 117 variables for corr plot.  
##Once we remove correlated variables I think obs will go up.  
na.omit(corr)->corr_noNA

corr1<-c(1:50)
corr2<-c(51:117)

corr_noNA[,corr1]->cp1
corr_noNA[,corr2]->cp2

corr_object1 = cor(cp1)
corr_object2= cor(cp2)

##plot was too big needed to break into smaller chuncks
corrplot(corr_object1, method = "number")#use this in write-up
corrplot(corr_object2, method = "number")#use this in write-up

##clearly there are some highly correlated variables.  We need to simplfy
##since some the correlated are basically represented are more than on variable
##Congnition tests(MMSE, TrailsA/b,LM1/2 ) are showing correlation with cdx_cog
##height, weight BMI
##imunological variables 
## hemogoblin measurments


##PCA on Continous Variables
var_split<-c(2:8,12,13,15,26:46,107:118)
nona_lgr_eda2[,-var_split]->cont_vars
##na.omit(cont_vars)->cont_vars

pc_cont_result<-prcomp(cont_vars,scale.=TRUE)
pc_cont_scores<-pc_cont_result$x
pc_cont_scores<-data.frame(pc_cont_scores)

nona_lgr_eda2


#Scree plot of continous
eigenvals<-(pc_cont_result$sdev)^2
plot(1:76,eigenvals/sum(eigenvals),type="l",main="Scree Plot PC's",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:76,cumulative.prop,lty=2)

##PCA on over/under sampled data
var_split<-c(2:8,12,13,15,26:46,107:119)
data_both_sam[,-var_split]->cont_both_vars
sum(is.na(cont_both_vars))

pc_both_result<-prcomp(cont_both_vars,scale.=TRUE)
pc_both_scores<-pc_both_result$x
pc_both_scores<-data.frame(pc_cont_scores)



#Scree plot of over/under sampled data
eigenvals3<-(pc_both_result$sdev)^2
plot(1:76,eigenvals/sum(eigenvals),type="l",main="Scree Plot PC's",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop<-cumsum(eigenvals/sum(eigenvals))
lines(1:76,cumulative.prop,lty=2)


##Tree spliting on continour vars
cont_tree1<-tree(cdx_cog~.,cont_vars) #default is for atleast 5 observations in the child nodes for split to occur
summary(cont_tree1)
plot(cont_tree1)
text(cont_tree1,pretty=0)

cont_tree2<-tree(cdx_cog_bucket~.,cont_both_vars) #default is for atleast 5 observations in the child nodes for split to occur
summary(cont_tree2)
plot(cont_tree2)
text(cont_tree2,pretty=0)

##PCA on Binary Variables
bi_var_split<-c(2:8,12,13,15,26:46,107:119)
non_bi_vars<-c(6,24)
lgr_eda2[,bi_var_split]->binary_vars
binary_vars[,-non_bi_vars]->binary_vars
na.omit(binary_vars)->binary_vars

apoe_var_split<-c(2:8,12,13,15,26:46,107:122)
non_bi_vars<-c(6,24)
lgr_eda_apoe[,apoe_var_split]->apoe_vars
apoe_vars[,-non_bi_vars]->apoe_vars
na.omit(binary_vars)->apoe_vars

pc_bi_result<-prcomp(binary_vars,scale.=TRUE)
pc_bi_scores<-pc_bi_result$x
pc_bi_scores<-data.frame(pc_bi_scores)
pc_bi_scores
pc.scores$mpg<-newAuto$mpg


bi_tree<-tree(cdx_cog~.,binary_vars) #default is for atleast 5 observations in the child nodes for split to occur
summary(bi_tree)
plot(bi_tree)
text(bi_tree,pretty=0)

apoe_tree<-tree(cdx_cog~.,apoe_vars) #default is for atleast 5 observations in the child nodes for split to occur
summary(apoe_tree)
plot(apoe_tree)
text(apoe_tree,pretty=0)
skim(apoe_vars)
str(apoe_vars)


non_bi_vars<-c(7,24)

lgr_eda2[,-non_bi_vars]->t_vars
total_tree<-tree(cdx_cog~.,t_vars) #default is for atleast 5 observations in the child nodes for split to occur
str(t_vars)
summary(total_tree)
plot(total_tree)
text(total_tree,pretty=0)
#Scree plot of Binary
eigenvals2<-(pc_bi_result$sdev)^2
plot(1:42,eigenvals2/sum(eigenvals2),type="l",main="Scree Plot PC's",ylab="Prop. Var. Explained",ylim=c(0,1))
cumulative.prop2<-cumsum(eigenvals2/sum(eigenvals2))
lines(1:42,cumulative.prop2,lty=2)



######
######
#need to contorl for race, age, sex in our models
######
######

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



ggplot(data = lgr_eda2  , aes(x=lgr_eda2$Age, y=lgr_eda2$cdx_cog, color=cdx_diabetes))+geom_point(stat = "identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Gender, y=lgr_eda2$cdx_cog))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_MaritalStatus, lgr_eda2$cdx_cog, color=cdx_cog))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Race_White, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Race_Black, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")

ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Race_Other, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Race_Specify, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")+ coord_flip()
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Hispanic, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Residence, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_USlive, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Education, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Education_HighSchool, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")

#looks very interesting
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Education_Degree, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
#looks very interesting
#possible log
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Income, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")+xlim(0,200000)
# determine what the difference between the 3 pluse measurements  possible ave
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$ID_Retire, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_Pulse1, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_BP1_SYS, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_BP1_DIA, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_Pulse2, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_BP2_SYS, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_BP2_DIA, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_Pulse3, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_BP3_SYS, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_BP3_DIA, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")

ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_AbCircumference, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_Height, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_Weight, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
#OM_NOTES  review
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$OM_BMI, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")

#Diabetes data 
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$IMH_Diabetes, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$IMH_DiabetesAge, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")

############
#need to keep in mind normal ranges considerations when intperting the model, beware of making implications with extreme values
##############
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$IMH_HighBP, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$IMH_HighBPAge, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$IMH_HighCholesterol, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$IMH_HighCholesterolAge, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_choltotal, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_HDLchol, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_triglycerides, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_LDLchol, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_cholhdlcratio, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_nonhdl, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
hist(log(lgr_eda2$lgr_eda2$cdx_cog))
hist(lgr_eda2$bw_glucose)
hist(log(lgr_eda2$bw_glucose))
ggpairs(subset(df_num, select=c(MonthlyIncome,TotalWorkingYears,YearsAtCompany,YearsInCurrentRole,
                                YearsSinceLastPromotion,YearsWithCurrManager)))


#############
###directly related
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_glucose, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
#############
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_UAbun, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_creatinine, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")+ xlim(0,3)

#does a bucket make sense
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_eGFRnonAA, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_eGFRAA, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_sodium, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")

ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_POTASS, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_chloride, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_carbondiox, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_calcium, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_protein, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ALB, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_GLOB, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ALB_GLOB, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")

#interesting
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_Bilirubin, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ALKA, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
#interesting
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_AST, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ALT, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_TSH, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")+ xlim(0,20)



#############
###directly related
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_T4, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
##############
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_WBC, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_RBC, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_hemoglobin, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_hematocrit, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_MCV, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_MCH, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_MCHC, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_RDW, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_platelet, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ABneutro, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ABlymph, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ABmono, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ABeosin, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_ABbaso, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_neutrophils, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_lymphocytes, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
#maybe
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_monocytes, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_eosinophils, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_baspphils, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_vitaminb12, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")
ggplot(data = lgr_eda2  , aes(x=lgr_eda2$bw_folate, y=(lgr_eda2$cdx_cog)))+geom_histogram(stat ="identity")


write.csv(lgr_eda2, file="11.17.19_clean_cut.csv")
