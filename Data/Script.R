
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

# Load data
setwd("F:/SMU/DS6372/Project 2/STATS_6372_Project2/EDA")
medData2 <- read.csv(file="data_cut.csv", header=TRUE)

summary(medData2)

reduced <- select (medData2,-c(OM_Notes, ID_Race_Specify, cdx_dt))



