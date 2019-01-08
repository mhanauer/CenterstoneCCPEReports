
###### Goal 1 ############################################################
###### Objective E Number of people served by each intervention Overall
# First grab total number of people served use the participant ID include the youth then for each intervention.  Grab the length for both adult and youth from all the data which will give you all the people who have data entered.  
# You don't need to specifcy the baseline data only, because there is no way that someone could not have baseline.  Therefore, the length of the total data set will equal the length of the baseline
###### Goal 1 ############################################################
###### Objective E and F Number of people served by each intervention Overall
# First grab total number of people served use the participant ID include the youth then for each intervention.  Grab the length for both adult and youth from all the data which will give you all the people who have data entered.  



################## Goal 3 Objective A #############################################################
# Grabbing the KNOW_SA variable, because other variables are being used.  Only 17 particpants from pocket which would be better.
Goal3ObjectiveA =  data.frame(gpraAdultAll$KNOW_SA.x, gpraAdultAll$KNOW_SA.y)

Goal3ObjectiveABaseMonth3 = data.frame(Goal3ObjectiveA$gpraAdultAll.KNOW_SA.x, Goal3ObjectiveA$gpraAdultAll.KNOW_SA.y)
Goal3ObjectiveABaseMonth3 = data.frame(apply(Goal3ObjectiveABaseMonth3, 2, function(x){ifelse(x == 98, NA, x)}))
Goal3ObjectiveABaseMonth3 = na.omit(Goal3ObjectiveABaseMonth3)
Goal3ObjectiveABaseMonth3 = data.frame(Goal3ObjectiveABaseMonth3)
## Here is the number of people that completed both
dim(Goal3ObjectiveABaseMonth3)
head(Goal3ObjectiveABaseMonth3)
wilcox.test(Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.y, Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.x, paired = TRUE, alternative  =c("greater"))

count(Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.x)
count(Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.y)
## Need to create a spread for this.
Goal3ObjectiveABaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveABaseMonth3)))
colnames(Goal3ObjectiveABaseMonth3) = c("Base", "Month3")
Goal3ObjectiveABaseMonth3$Difference = (Goal3ObjectiveABaseMonth3$Month3-Goal3ObjectiveABaseMonth3$Base)/Goal3ObjectiveABaseMonth3$Base
Goal3ObjectiveABaseMonth3 = round(Goal3ObjectiveABaseMonth3,2)


########## Goal 3 Objective B | Increase knowledge about HIV and VH by 20%.  KNOW_HIV #########################################################

Goal3ObjectiveB =  data.frame(gpraAdultAll$KNOW_HIV.x, gpraAdultAll$KNOW_HIV.y)

Goal3ObjectiveB = data.frame(apply(Goal3ObjectiveB, 2, function(x){ifelse(x == 98, NA, ifelse(x == 2, NA, x))}))
summary(Goal3ObjectiveB)

Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.x, Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.y)
Goal3ObjectiveBBaseMonth3 = na.omit(Goal3ObjectiveBBaseMonth3)
Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveBBaseMonth3)
## Here is the number of people that completed both
dim(Goal3ObjectiveBBaseMonth3)

head(Goal3ObjectiveBBaseMonth3)
count(Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.x)
count(Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.y)

wilcox.test(Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.y, Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.x, paired = TRUE, alternative  =c("greater"))

Goal3ObjectiveBBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveBBaseMonth3)))
colnames(Goal3ObjectiveBBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveBBaseMonth3$Difference = (Goal3ObjectiveBBaseMonth3$Month3-Goal3ObjectiveBBaseMonth3$Base)/Goal3ObjectiveBBaseMonth3$Base
Goal3ObjectiveBBaseMonth3 = round(Goal3ObjectiveBBaseMonth3, 2)



##### Goal 3 Objective C RSKCIG, RSKMJ, RSKALC create a total compoiste scores ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
Goal3ObjectiveC = data.frame(gpraAdultAll$RSKCIG.x, gpraAdultAll$RSKCIG.y, gpraAdultAll$RSKMJ.x, gpraAdultAll$RSKMJ.y, gpraAdultAll$RSKALC.x, gpraAdultAll$RSKALC.y)
head(Goal3ObjectiveC)
Goal3ObjectiveC  = data.frame(apply(Goal3ObjectiveC, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
head(Goal3ObjectiveC)

Goal3ObjectiveCBaseMonth3= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKCIG.x, Goal3ObjectiveC$gpraAdultAll.RSKCIG.y, Goal3ObjectiveC$gpraAdultAll.RSKMJ.x, Goal3ObjectiveC$gpraAdultAll.RSKMJ.y, Goal3ObjectiveC$gpraAdultAll.RSKALC.x, Goal3ObjectiveC$gpraAdultAll.RSKALC.y)
head(Goal3ObjectiveCBaseMonth3) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth3  = na.omit(Goal3ObjectiveCBaseMonth3)
Goal3ObjectiveCBaseMonth3  = data.frame(Goal3ObjectiveCBaseMonth3)
head(Goal3ObjectiveCBaseMonth3)
## Number for Base and Month3
dim(Goal3ObjectiveCBaseMonth3)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKCIG.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKMJ.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKALC.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum))
colnames(Goal3ObjectiveCBase ) = c("SAHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth3 = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKCIG.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKMJ.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKALC.y)
head(Goal3ObjectiveCMonth3)
summary(Goal3ObjectiveCMonth3)
Goal3ObjectiveCMonth3 = data.frame(apply(Goal3ObjectiveCMonth3, 1, sum))
colnames(Goal3ObjectiveCMonth3 ) = c("SAHarmMonth3")
head(Goal3ObjectiveCMonth3)
dim(Goal3ObjectiveCMonth3)

Goal3ObjectiveCBaseMonth3 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth3)
head(Goal3ObjectiveCBaseMonth3)
wilcox.test(Goal3ObjectiveCMonth3$SAHarmMonth3, Goal3ObjectiveCBase$SAHarmBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveCBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveCBaseMonth3)))
colnames(Goal3ObjectiveCBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveCBaseMonth3$Difference = (Goal3ObjectiveCBaseMonth3$Month3-Goal3ObjectiveCBaseMonth3$Base)/Goal3ObjectiveCBaseMonth3$Base
Goal3ObjectiveCBaseMonth3 = round(Goal3ObjectiveCBaseMonth3, 2)


####### Goal 3 Objective D ######### ###################################################################################################
# Grab these variables and change RSKANYSEX_UNP, RSKSEX_ALCDRG, RSKNDL_SHR change ObjectiveC to ObjectiveD, change SAHarm to HIVHarm 
Goal3ObjectiveD= data.frame(gpraAdultAll$RSKANYSEX_UNP.x, gpraAdultAll$RSKANYSEX_UNP.y, gpraAdultAll$RSKSEX_ALCDRG.x, gpraAdultAll$RSKSEX_ALCDRG.y, gpraAdultAll$RSKNDL_SHR.x, gpraAdultAll$RSKNDL_SHR.y)
head(Goal3ObjectiveD)
Goal3ObjectiveD = data.frame(apply(Goal3ObjectiveD, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
head(Goal3ObjectiveD)

Goal3ObjectiveDBaseMonth3= data.frame(Goal3ObjectiveD$gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveD$gpraAdultAll.RSKANYSEX_UNP.y, Goal3ObjectiveD$gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveD$gpraAdultAll.RSKSEX_ALCDRG.y, Goal3ObjectiveD$gpraAdultAll.RSKNDL_SHR.x, Goal3ObjectiveD$gpraAdultAll.RSKNDL_SHR.y)
head(Goal3ObjectiveDBaseMonth3) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveDBaseMonth3  = na.omit(Goal3ObjectiveDBaseMonth3)
Goal3ObjectiveDBaseMonth3  = data.frame(Goal3ObjectiveDBaseMonth3)
head(Goal3ObjectiveDBaseMonth3)

Goal3ObjectiveDBase = data.frame(Goal3ObjectiveDBaseMonth3$Goal3ObjectiveD.gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveDBaseMonth3$Goal3ObjectiveD.gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveDBaseMonth3$Goal3ObjectiveD.gpraAdultAll.RSKNDL_SHR.x)
head(Goal3ObjectiveDBase)
Goal3ObjectiveDBase = data.frame(apply(Goal3ObjectiveDBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDBase ) = c("HIVHarmBase")
head(Goal3ObjectiveDBase)
dim(Goal3ObjectiveDBase)

Goal3ObjectiveDMonth3 = data.frame(Goal3ObjectiveDBaseMonth3$Goal3ObjectiveD.gpraAdultAll.RSKANYSEX_UNP.y, Goal3ObjectiveDBaseMonth3$Goal3ObjectiveD.gpraAdultAll.RSKSEX_ALCDRG.y, Goal3ObjectiveDBaseMonth3$Goal3ObjectiveD.gpraAdultAll.RSKNDL_SHR.y)
head(Goal3ObjectiveDMonth3)
Goal3ObjectiveDMonth3 = data.frame(apply(Goal3ObjectiveDMonth3, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveDMonth3 ) = c("HIVHarmMonth3")
head(Goal3ObjectiveDMonth3)
dim(Goal3ObjectiveDMonth3)

Goal3ObjectiveDBaseMonth3 = data.frame(Goal3ObjectiveDBase, Goal3ObjectiveDMonth3)
head(Goal3ObjectiveDBaseMonth3)
wilcox.test(Goal3ObjectiveDMonth3$HIVHarmMonth3, Goal3ObjectiveDBase$HIVHarmBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveDBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveDBaseMonth3)))
colnames(Goal3ObjectiveDBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveDBaseMonth3$Difference = (Goal3ObjectiveDBaseMonth3$Month3-Goal3ObjectiveDBaseMonth3$Base)/Goal3ObjectiveDBaseMonth3$Base
Goal3ObjectiveDBaseMonth3 = round(Goal3ObjectiveDBaseMonth3, 2)





###### Goal 3 Objective H ############################################################################################ #########
# CIG30D, TOB30D, VAP30D, ALC30D, BINGE530D, MJ30D, ILL30D, RX30D, SPICE30D, INJECT30D  So sum these to create an overall days used substances in 30 days
Goal3ObjectiveHBaseline3month = data.frame(gpraAdultAll$CIG30D.x,gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x, gpraAdultAll$SPICE30D.x, gpraAdultAll$INJECT30D.x, gpraAdultAll$CIG30D.y,gpraAdultAll$VAP30D.y, gpraAdultAll$ALC30D.y, gpraAdultAll$BINGE530D.y, gpraAdultAll$MJ30D.y, gpraAdultAll$ILL30D.y, gpraAdultAll$RX30D.y, gpraAdultAll$SPICE30D.y, gpraAdultAll$INJECT30D.y)            
Goal3ObjectiveHBaseline3month = data.frame(apply(Goal3ObjectiveHBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveHBaseline3month = data.frame(na.omit(Goal3ObjectiveHBaseline3month))
## Number of people here
dim(Goal3ObjectiveHBaseline3month) 
Goal3ObjectiveHBase =(Goal3ObjectiveHBaseline3month[,1:9])
head(Goal3ObjectiveHBase)
Goal3ObjectiveHBase = data.frame(apply(Goal3ObjectiveHBase, 1, sum))
colnames(Goal3ObjectiveHBase) = c("Goal3ObjectiveHBase")
head(Goal3ObjectiveHBase)
# Here for number of matched people 
dim(Goal3ObjectiveHBase)

Goal3ObjectiveH3month =(Goal3ObjectiveHBaseline3month[,10:18])
head(Goal3ObjectiveH3month)
# Here for number of matched people 
dim(Goal3ObjectiveH3month)
Goal3ObjectiveH3month = data.frame(apply(Goal3ObjectiveH3month, 1, sum))
colnames(Goal3ObjectiveH3month) = c("Goal3ObjectiveH3month")
head(Goal3ObjectiveH3month)

wilcox.test(Goal3ObjectiveH3month$Goal3ObjectiveH3month, Goal3ObjectiveHBase$Goal3ObjectiveHBase, paired = TRUE, alternative = c("less"))

Goal3ObjectiveHBaseMonth3 = data.frame(Goal3ObjectiveHBase, Goal3ObjectiveH3month)
library(psych)
describe(Goal3ObjectiveHBaseMonth3)
Goal3ObjectiveHBaseMonth3 =  data.frame(t(colMeans(Goal3ObjectiveHBaseMonth3)))
colnames(Goal3ObjectiveHBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveHBaseMonth3$Difference = round((Goal3ObjectiveHBaseMonth3$Month3-Goal3ObjectiveHBaseMonth3$Base)/Goal3ObjectiveHBaseMonth3$Base,2)


####################### Objective I ################ ################ ################ ################ ################ ################ ##############
#CNTRL_REFUSEMOOD CNTRL_WAITCNDM CNTRL_TREAT CNTRL_SEXPRAC CNTRL_ASKCNDM CNTRL_REFUSECNDM
Goal3ObjectiveIBaseline3month= data.frame(gpraAdultAll$CNTRL_REFUSEMOOD.x, gpraAdultAll$CNTRL_WAITCNDM.x, gpraAdultAll$CNTRL_TREAT.x, gpraAdultAll$CNTRL_SEXPRAC.x, gpraAdultAll$CNTRL_ASKCNDM.x, gpraAdultAll$CNTRL_REFUSECNDM.x, gpraAdultAll$CNTRL_REFUSEMOOD.y, gpraAdultAll$CNTRL_WAITCNDM.y, gpraAdultAll$CNTRL_TREAT.y, gpraAdultAll$CNTRL_SEXPRAC.y, gpraAdultAll$CNTRL_ASKCNDM.y, gpraAdultAll$CNTRL_REFUSECNDM.y)
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
summary(Goal3ObjectiveIBaseline3month)
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:6])
head(Goal3ObjectiveIBase)
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,7:12])
head(Goal3ObjectiveI3month)
Goal3ObjectiveI3month = data.frame(apply(Goal3ObjectiveI3month, 1, sum))


colnames(Goal3ObjectiveI3month) = c("Goal3ObjectiveI3month")
head(Goal3ObjectiveI3month)

wilcox.test(Goal3ObjectiveI3month$Goal3ObjectiveI3month, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveIBaseMonth3 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveI3month)
Goal3ObjectiveIBaseMonth3 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth3)))
colnames(Goal3ObjectiveIBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveIBaseMonth3$Difference = round((Goal3ObjectiveIBaseMonth3$Month3-Goal3ObjectiveIBaseMonth3$Base)/Goal3ObjectiveIBaseMonth3$Base,2)
write.csv(Goal3ObjectiveIBaseMonth3, "Goal3ObjectiveIBaseMonth3.csv", row.names = FALSE)

#### Goal 3 Objective I Base to Month 6 ########## ######################################################################
# Need to change y to not .y.  Change Month3 to Month6
Goal3ObjectiveIBaselineMonth6= data.frame(gpraAdultAll$CNTRL_REFUSEMOOD.x, gpraAdultAll$CNTRL_WAITCNDM.x, gpraAdultAll$CNTRL_TREAT.x, gpraAdultAll$CNTRL_SEXPRAC.x, gpraAdultAll$CNTRL_ASKCNDM.x, gpraAdultAll$CNTRL_REFUSECNDM.x, gpraAdultAll$CNTRL_REFUSEMOOD, gpraAdultAll$CNTRL_WAITCNDM, gpraAdultAll$CNTRL_TREAT, gpraAdultAll$CNTRL_SEXPRAC, gpraAdultAll$CNTRL_ASKCNDM, gpraAdultAll$CNTRL_REFUSECNDM)
Goal3ObjectiveIBaselineMonth6 = data.frame(apply(Goal3ObjectiveIBaselineMonth6, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaselineMonth6 = data.frame(na.omit(Goal3ObjectiveIBaselineMonth6))
summary(Goal3ObjectiveIBaselineMonth6)
## Number of people here
dim(Goal3ObjectiveIBaselineMonth6) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaselineMonth6[,1:6])
head(Goal3ObjectiveIBase)
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveIMonth6 =(Goal3ObjectiveIBaselineMonth6[,7:12])
head(Goal3ObjectiveIMonth6)
Goal3ObjectiveIMonth6 = data.frame(apply(Goal3ObjectiveIMonth6, 1, sum))


colnames(Goal3ObjectiveIMonth6) = c("Goal3ObjectiveIMonth6")
head(Goal3ObjectiveIMonth6)

wilcox.test(Goal3ObjectiveIMonth6$Goal3ObjectiveIMonth6, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveIBaseMonth6 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveIMonth6)
Goal3ObjectiveIBaseMonth6 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth6)))
colnames(Goal3ObjectiveIBaseMonth6) = c("Base", "Month6")
Goal3ObjectiveIBaseMonth6$Difference = round((Goal3ObjectiveIBaseMonth6$Month6-Goal3ObjectiveIBaseMonth6$Base)/Goal3ObjectiveIBaseMonth6$Base,2)
write.csv(Goal3ObjectiveIBaseMonth6, "Goal3ObjectiveIBaseMonth6.csv", row.names = FALSE)

#### Goal 3 Objective I SIS ############## ######################################################################
#SIS Reflective Post-Test
SISData = read.spss("S:/Indiana Research & Evaluation/CCPE/CCPE SPSS - Datasets/SIS Reflective Post-Test.sav", use.value.labels = FALSE, to.data.frame = TRUE)

SISData = data.frame(na.omit(SISData))
dim(SISData)
summary(SISData)
# Current goes from 3 to 14
SISDataCurrent = SISData[,3:14]
SISDataCurrent = data.frame(apply(SISDataCurrent, 1, mean))
colnames(SISDataCurrent) = c("SISDataCurrent")
head(SISDataCurrent)
# Before is 15 to 26

SISDataBefore= SISData[,15:26]
SISDataBefore= data.frame(apply(SISDataBefore, 1, mean))
colnames(SISDataBefore) = c("SISDataBefore")
head(SISDataBefore)
SISData = data.frame(SISDataBefore, SISDataCurrent)
wilcox.test(SISData$SISDataCurrent, SISData$SISDataBefore, paired = TRUE, alternative = c("greater"))

SISData = data.frame(t(colMeans(SISData)))
SISData$Difference = (SISData$SISDataCurrent-SISData$SISDataBefore) / SISData$SISDataBefore
SISData = round(SISData,2)
write.csv(SISData, "SISData.csv", row.names = TRUE)
#######Goal 3 Objective J ########################################### ########################################### ###########################################
# JAILTIME_N  Change 98 to missing 99 to 0 else to 1.  
Goal3ObjectiveJBaseMonth3 = data.frame(gpraAdultAll$JAILTIME_N.x, gpraAdultAll$JAILTIME_N.y)
Goal3ObjectiveJBaseMonth3 = data.frame(apply(Goal3ObjectiveJBaseMonth3, 2, function(x){ifelse(x == 98, NA, ifelse( x == 99, 0, 1))}))
Goal3ObjectiveJBaseMonth3 = data.frame(na.omit(Goal3ObjectiveJBaseMonth3))
dim(Goal3ObjectiveJBaseMonth3)
head(Goal3ObjectiveJBaseMonth3)
count(Goal3ObjectiveJBaseMonth3$Goal3ObjectiveJBaseMonth3)
summary(Goal3ObjectiveJ)
## There are no new cases of people being in jail from baseline to month 3



####### Goal 3 Objective K ########################################### ########################################### ###########################################
# Use LIFE_RESP_SERV
Goal3ObjectiveKBaseMonth3 = data.frame(gpraAdultAll$LIFE_RESP_SERV.x, gpraAdultAll$LIFE_RESP_SERV.y)
Goal3ObjectiveKBaseMonth3 = data.frame(apply(Goal3ObjectiveKBaseMonth3, 2, function(x){ifelse(x == 98, NA, ifelse(x == 99, NA, x))}))
summary(Goal3ObjectiveKBaseMonth3)
Goal3ObjectiveKBaseMonth3 = data.frame(na.omit(Goal3ObjectiveKBaseMonth3))
colnames(Goal3ObjectiveKBaseMonth3) = c("Base", "Month3")
dim(Goal3ObjectiveKBaseMonth3)
wilcox.test(Goal3ObjectiveKBaseMonth3$Month3, Goal3ObjectiveKBaseMonth3$Base, paired = TRUE, alternative = c("less"))

Goal3ObjectiveKBaseMonth3 =  data.frame(t(colMeans(Goal3ObjectiveKBaseMonth3)))
colnames(Goal3ObjectiveKBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveKBaseMonth3$Difference = round((Goal3ObjectiveKBaseMonth3$Month3-Goal3ObjectiveKBaseMonth3$Base)/Goal3ObjectiveKBaseMonth3$Base,2)
Goal3ObjectiveKBaseMonth3 = round(Goal3ObjectiveKBaseMonth3,2)

##### Objective L ########################## ###############################################################################################################################
## STDHx and subset by year if any data and then compare by year.  No year so need to merge the GPRA with pocket then grab year from GPRA with the one variable here
# LASTSEX_UNP Getting the number of people with unprotected and want to reduce that so 1 =0 and 2 = 1
Goal3ObjectiveMBaseMonth3 = data.frame(gpraAdultAll$LASTSEX_UNP.x, gpraAdultAll$LASTSEX_UNP.y)
Goal3ObjectiveMBaseMonth3 = data.frame(apply(Goal3ObjectiveMBaseMonth3, 2, function(x){ifelse(x == 98, NA, ifelse(x == 99, NA, x))}))
Goal3ObjectiveMBaseMonth3 = data.frame(na.omit(Goal3ObjectiveMBaseMonth3))
Goal3ObjectiveMBaseMonth3 = data.frame(apply(Goal3ObjectiveMBaseMonth3, 2, function(x){ifelse(x == 1, 0, 1)}))
# Here for number of people
dim(Goal3ObjectiveMBaseMonth3)
summary(Goal3ObjectiveMBaseMonth3)
colnames(Goal3ObjectiveMBaseMonth3) = c("Base", "Month3")
wilcox.test(Goal3ObjectiveMBaseMonth3$Month3, Goal3ObjectiveMBaseMonth3$Base, paired = TRUE, alternative = c("less"))
count(Goal3ObjectiveMBaseMonth3$Base)
count(Goal3ObjectiveMBaseMonth3$Month3)

Goal3ObjectiveMBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveMBaseMonth3)))
Goal3ObjectiveMBaseMonth3$Difference = (Goal3ObjectiveMBaseMonth3$Month3-Goal3ObjectiveMBaseMonth3$Base) / Goal3ObjectiveMBaseMonth3$Base
Goal3ObjectiveMBaseMonth3 = round(Goal3ObjectiveMBaseMonth3, 2)


####### Goal 3 Objective M ########################################### ########################################### ###########################################
# Using the pocket screener.  
Goal3ObjectiveMBaseMonth3 = cbind(baseExchange = gpraAdultAll$WHENSEX4STFF_UNP.x, month3Exchange = gpraAdultAll$WHENSEX4STFF_UNP.y)

write.csv(Goal3ObjectiveMBaseMonth3, "Goal3ObjectiveMBaseMonth3.csv", row.names = FALSE)
Goal3ObjectiveMBaseMonth3 = read.csv("Goal3ObjectiveMBaseMonth3.csv", header = TRUE, na.strings = c(NA, 98, 99))
Goal3ObjectiveMBaseMonth3 = na.omit(Goal3ObjectiveMBaseMonth3)
apply(Goal3ObjectiveMBaseMonth3, 2, sum)
##### CCPE Grant Summary Report Data ######## #########################################################

# For Increase confidence in assertive communication skills see Goal 3 Objective I SIS 
# For Increase knowledge about HIV and substance abuse and decrease risky behaviors that could lead to HIV/AIDS and substance abuse.  																			
# use ####### Goal 3 Objective A and B #####
# For Increase protected sex and improve attitudes towards condom use  ##### Objective M ##### 																			

##### Grant Summary Report Demographics ###################################### ##########################################
# Current enrollment totalCCPE to tracking sheet
# Gender Male = 1; Female = 2; Other Gender Identity 3 and 4 
library(plyr)
library(prettyR)
gender = data.frame(gpraAdultAll$GENDER.x)
describe.factor(gender)

# Ethnicity # Need to combine hispanic E_MEXICAN, E_PUERTRICAN, E_CUBAN, E_OTHERHISPAN
# Need to combine all Asian R_ASIAIN_N, R_CHINESE_N, R_JAPAN_N, R_KOR_N, R_VIETNAM_N, R_OTHERASIA_N
# Then grab black and white and get rid of NA's so the total correct
# Need to include this E_NONHISPAN and change 0 to 1 and 1 to 0
n = dim(gpraAdultAll)[1]
hispanic = data.frame(gpraAdultAll$E_NONHISPAN.x)
describe.factor(hispanic)

###### Now Asian ###### ###### ###### ###### ###### 
asian = data.frame(gpraAdultAll$R_ASIAIN_N.x, gpraAdultAll$R_CHINESE_N.x, gpraAdultAll$R_JAPAN_N.x, gpraAdultAll$R_KOR_N.x, gpraAdultAll$R_VIETNAM_N.x, gpraAdultAll$R_OTHERASIA_N.x)
asian = data.frame(apply(asian, 2, function(x){ifelse(x == 98, NA, x)}))
asian = data.frame(apply(asian, 1, sum))
asian = data.frame(apply(asian, 1, function(x){ifelse(x > 0,1, 0)}))
describe.factor(asian)


#### R_BLACK_N ######## ################################################################
black = data.frame(gpraAdultAll$R_BLACK_N.x)
black = data.frame(apply(black, 2, function(x){ifelse(x == 98, NA, x)}))
describe.factor(black)


####### White ################# ################# ####################################################################
white = data.frame(gpraAdultAll$R_WHITE_N.x)
white = data.frame(apply(white, 2, function(x){ifelse(x == 98, NA, x)}))
describe.factor(white)

######################## Education Level ################################################################################ 
# 2 = Middle school
#3 = High school
#4= Community college or technical or trade school
#5= Four-year college
#6= Beyond four-year college


edu = data.frame(gpraAdultAll$EDLEVEL_N.x)
edu = na.omit(edu)
describe.factor(edu, decr.order = FALSE)



########### Age ################################## ########################################################################
ageAdult = data.frame(gpraAdultAll$YOB.x)
ageAdult
colnames(ageAdult) = c("YOB")
age = rbind(ageAdult)
age = data.frame(apply(age, 2, function(x){ifelse(x == 98, NA, x)}))
age = data.frame(age = 2018-age)
age = data.frame(na.omit(age))
mean(age$YOB)

#### Centerstone Metrics  ##### ############################################################
#Objective H for substance and don't have anything else

#### Project Specific outcomes #### #### #### #### #### #### #### #### #### #### #### ####
#Goal3ObjectiveB = Substance Abuse Knowledge
#Goal3ObjectiveH = Youth Communication Skills
#Goal3ObjectiveK = Adult Empowerment
#Goal3ObjectiveM = Reduce unprotected sex

#### SPARS Metrics   ##### ############################################################

## Creating new variable to subset later ########################
gpraAdultBase$Date = as.Date(paste(gpraAdultBase$YEAR, gpraAdultBase$MONTH, gpraAdultBase$DAY, sep = "/"))

gpraAdultBase$Date =  as.Date(gpraAdultBase$Date)
gpraAdultBase$Date = gsub("0018", "2018", gpraAdultBase$Date)
gpraAdultBase$Date = as.Date(gpraAdultBase$Date)
typeof(gpraAdultBase$Date)
write.csv(gpraAdultBase, "gpraAdultBase.csv", row.names = FALSE)
gpraAdultBase = read.csv("gpraAdultBase.csv", header = TRUE)
# Total People no kids, because kids are not tested
QPR1 = data.frame(na.omit(gpraAdultBase$Date))
colnames(QPR1) = c("Date")
head(QPR1$Date)
typeof(QPR1)
QPR1 = as.Date(QPR1$Date)
typeof(QPR1)
QPR1=QPR1[QPR1 >= "2018-10-1" & QPR1 <= "2018-12-31"]
length(QPR1)



##### Young Adults (Age 18-24) in college ##### ##### ##### ##### ##### ##### ##### 
# 1 and 2 are yesses and 0 is no.  
# Can get rid of na's, because I need both variables to identitfy them
# Also need year of birth to subset
gpraAdultBase$Date =  as.Date(gpraAdultBase$Date)
QPR3 = data.frame(AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$Date, gpraAdultBase$COLLEGE)
QPR3 = data.frame(na.omit(QPR3))
colnames(QPR3) = c("AGE", "Date", "College")
head(QPR3)
# subset date first then do the subset for college then do age
# Date for QPR
QPR3 = subset(QPR3, AGE < 25)
QPR3 = subset(QPR3, College == 1 | College == 2)

QPR3=QPR3[QPR3$Date > "2018-10-1" & QPR3$Date <= "2018-12-31",]
dim(QPR3)[1]

######### Young Adults (Age 18-24) not in college ######### ######### ######### ######### 
gpraAdultBase$Date =  as.Date(gpraAdultBase$Date)
QPR4 = data.frame(AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$Date, gpraAdultBase$COLLEGE)
QPR4 = data.frame(na.omit(QPR4))
colnames(QPR4) = c("AGE", "Date", "College")
count(QPR4$College)
# subset date first then do the subset for college then do age
# Date for QPR
QPR4$Date = as.Date(QPR4$Date)
QPR4=QPR4[QPR4$Date >= "2018-10-1" & QPR4$Date <= "2018-12-31",]
QPR4 = subset(QPR4, AGE < 25)
QPR4 = subset(QPR4, College == 0)
dim(QPR4)[1]


######### Older Adults (Age 50 and Over) ######### ######### ######### ######### ######### 
QPR5 = data.frame(AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$Date, gpraAdultBase$COLLEGE)
QPR5 = data.frame(na.omit(QPR5))
colnames(QPR5) = c("AGE", "Date", "College")
count(QPR5$College)
# subset date first then do the subset for college then do age
# Date for QPR
QPR5=QPR5[QPR5$Date >= "2018-10-1" & QPR5$Date <= "2018-12-31",]
QPR5 = subset(QPR5, AGE >= 50)
dim(QPR5)[1]

#########  American Indian/Alaska Natives ######### ######### ######### ######### 
QPR6 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_AMERINALSK_N)
QPR6 = data.frame(na.omit(QPR6))
colnames(QPR6) = c("Date", "R_AMERINALSK_N")
# subset date first then do the subset for college then do age
# Date for QPR
QPR6=QPR6[QPR6$Date >= "2018-10-1" & QPR6$Date <= "2018-12-31",]
QPR6 = subset(QPR6, R_AMERINALSK_N == 1)
dim(QPR6)[1]

#########  Asian American/Pacific Islanders ######### ######### ######### ######### 
# R_ASIAIN_N, R_CHINESE_N, R_FILIP_N, R_JAPAN_N, R_KOR_N, R_VIETNAM_N, R_OTHERASIA_N, R_HAW_N, R_GUAM_N, R_SAMO_N, R_OTHERPI_N

QPR7 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_ASIAIN_N, gpraAdultBase$R_CHINESE_N, gpraAdultBase$R_FILIP_N, gpraAdultBase$R_JAPAN_N, gpraAdultBase$R_KOR_N, gpraAdultBase$R_VIETNAM_N, gpraAdultBase$R_OTHERASIA_N, gpraAdultBase$R_HAW_N, gpraAdultBase$R_GUAM_N, gpraAdultBase$R_OTHERPI_N)
QPR7 = data.frame(na.omit(QPR7))
colnames(QPR7) = c("Date", "R_ASIAIN_N", "R_CHINESE_N", "R_FILIP_N", "R_JAPAN_N", "R_KOR_N", "R_VIETNAM_N", "R_OTHERASIA_N", "R_HAW_N", "R_GUAM_N", "R_OTHERPI_N")
# subset date first then do the subset for college then do age
# Date for QPR
QPR7=QPR7[QPR7$Date >= "2018-10-1" & QPR7$Date <= "2018-12-31",]
QPR7 = subset(QPR7, R_ASIAIN_N == 1 | R_CHINESE_N == 1 | R_FILIP_N ==1 | R_JAPAN_N == 1 | R_KOR_N== 1 | R_VIETNAM_N== 1 | R_OTHERASIA_N==1 | R_HAW_N==1 | R_GUAM_N==1 | R_OTHERPI_N==1)
dim(QPR7)[1]


#########   Black/African American Women #########  #########  
QPR8 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_BLACK_N, gpraAdultBase$GENDER)
QPR8 = data.frame(na.omit(QPR8))
colnames(QPR8) = c("Date", "R_BLACK_N", "GENDER")
# subset date first then do the subset for college then do age
# Date for QPR
QPR8=QPR8[QPR8$Date >= "2018-10-1" & QPR8$Date <= "2018-12-31",]
QPR8 = subset(QPR8, R_BLACK_N == 1 & GENDER == 0)
dim(QPR8)[1]

######### Black/African American Men #########   #########  #########   #########
QPR9 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_BLACK_N, gpraAdultBase$GENDER)
QPR9 = data.frame(na.omit(QPR9))
colnames(QPR9) = c("Date", "R_BLACK_N", "GENDER")
# subset date first then do the subset for college then do age
# Date for QPR
QPR9=QPR9[QPR9$Date >= "2018-10-1" & QPR9$Date <= "2018-12-31",]
QPR9 = subset(QPR9, R_BLACK_N == 1 & GENDER == 1)
dim(QPR9)[1]

######### Latina or Hispanic Women: #########   #########  #########   #########
QPR10 = data.frame(gpraAdultBase$Date, gpraAdultBase$E_NONHISPAN, gpraAdultBase$GENDER)
QPR10 = data.frame(na.omit(QPR10))
colnames(QPR10) = c("Date", "E_NONHISPAN", "GENDER")
# subset date first then do the subset for college then do age
# Date for QPR
QPR10=QPR10[QPR10$Date >= "2018-10-1" & QPR10$Date <= "2018-12-31",]
QPR10 = subset(QPR10, E_NONHISPAN == 0 & GENDER == 0)
dim(QPR10)[1]

######### Latino or Hispanic Men: #########   #########  #########   #########
QPR11 = data.frame(gpraAdultBase$Date, gpraAdultBase$E_NONHISPAN, gpraAdultBase$GENDER)
QPR11 = data.frame(na.omit(QPR11))
colnames(QPR11) = c("Date", "E_NONHISPAN", "GENDER")
QPR11=QPR11[QPR11$Date >= "2018-10-1" & QPR11$Date <= "2018-12-31",]
QPR11 = subset(QPR11, E_NONHISPAN == 0 & GENDER == 1)
dim(QPR11)[1]

#########  Men Having Sex with Men (MSM): #########   #########  #########   #########
QPR12 = data.frame(gpraAdultBase$Date, gpraAdultBase$SEX_MALEEVER, gpraAdultBase$GENDER)
QPR12 = data.frame(na.omit(QPR12))
colnames(QPR12) = c("Date", "SEX_MALEEVER", "GENDER")
QPR12=QPR12[QPR12$Date >= "2018-10-1" & QPR12$Date <= "2018-12-31",]
QPR12 = subset(QPR12, SEX_MALEEVER == 1 & GENDER == 1)
dim(QPR12)[1]

## LGBTQ #########  #########  #########  #########  #########  
QPR13 = data.frame(gpraAdultBase$Date, gpraAdultBase$SEX_PR)
QPR13 = data.frame(na.omit(QPR13))
colnames(QPR13) = c("Date", "SEX_PR")
QPR13=QPR13[QPR13$Date >= "2018-10-1" & QPR13$Date <= "2018-12-31",]
QPR13 = subset(QPR13, SEX_PR == 3 | SEX_PR == 4)
dim(QPR13)[1]

##  Military/Veterans  #########  #########  #########  #########  #########  
# MILSERVENO
QPR14 = data.frame(gpraAdultBase$Date, gpraAdultBase$MILSERVENO)
QPR14 = data.frame(na.omit(QPR14))
colnames(QPR14) = c("Date", "MILSERVENO")
QPR14=QPR14[QPR14$Date >= "2018-10-1" & QPR14$Date <= "2018-12-31",]
QPR14 = subset(QPR14, MILSERVENO == 0)
dim(QPR14)[1]

##  Reentry Populations  #########  #########  #########  #########  #########  
## No data on this

# HOMETYPE_N #########  #########  #########  #########  ######### 
QPR16 = data.frame(gpraAdultBase$Date, gpraAdultBase$HOMETYPE_N)
QPR16 = data.frame(na.omit(QPR16))
colnames(QPR16) = c("Date", "HOMETYPE_N")
QPR16=QPR16[QPR16$Date >= "2018-10-1" & QPR14$Date <= "2018-12-31",]
QPR16 = subset(QPR16, HOMETYPE_N == 6)
dim(QPR16)[1]


## Sex Workers ##############  ############## ############## ##############
# No data on this topic


## Low Income ##############  ############## ############## ############## 
#HINCOMEO_N

QPR18 = data.frame(gpraAdultBase$Date, gpraAdultBase$HINCOMEO_N)
QPR18 = data.frame(na.omit(QPR18))
colnames(QPR18) = c("Date", "HINCOMEO_N")
QPR18=QPR18[QPR18$Date >= "2018-10-1" & QPR14$Date <= "2018-12-31",]
QPR18 = subset(QPR18, HINCOMEO_N == 1)
dim(QPR18)[1]

######## HIV Testing ##############  ############## ############## ##############

#### Total Number of people ##################################
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A)
write.csv(QPR19, "QPR19.csv", row.names = FALSE)
describe.factor(QPR19$gpraAdultBase.INTERVENTION_A)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
dim(QPR19)[1]


### In this reporting period, how many people received an HIV test for the first time using CSAP/MAI funds?

# Need the intersection between those have been and those who have never been tested before
# HIV_RESULTS_N
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
dim(QPR19)[1]

# Received an HIV test using CSAP/MAI grant funds during current reporting period
### GENDER
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$GENDER)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "GENDER")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
describe.factor(QPR19$GENDER)

# Received an HIV test for the first time using CSAP/MAI grant funds during current reporting period
### GENDER
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$HIV_RESULTS_N, gpraAdultBase$GENDER)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "HIV_RESULTS_N", "GENDER")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
describe.factor(QPR19$GENDER)

## Received an HIV test using CSAP/MAI grant funds during current reporting period
## Ethnicity

QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$E_NONHISPAN)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "E_NONHISPAN")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
describe.factor(QPR19$E_NONHISPAN)

## Received an HIV test for the first time using CSAP/MAI grant funds during current reporting period
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$HIV_RESULTS_N, gpraAdultBase$E_NONHISPAN)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A","HIV_RESULTS_N", "E_NONHISPAN")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
describe.factor(QPR19$E_NONHISPAN)

## Received an HIV test using CSAP/MAI grant funds during current reporting period
## Black
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_BLACK_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_BLACK_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
describe.factor(QPR19$R_BLACK_N)

## American Indian or Alaska Native
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_AMERINALSK_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_BLACK_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
describe.factor(QPR19$R_AMERINALSK_N)

## Asian
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_ASIAIN_N, gpraAdultBase$R_CHINESE_N, gpraAdultBase$R_FILIP_N, gpraAdultBase$R_JAPAN_N, gpraAdultBase$R_KOR_N, gpraAdultBase$R_VIETNAM_N, gpraAdultBase$R_OTHERASIA_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_ASIAIN_N", "R_CHINESE_N", "R_FILIP_N", "R_JAPAN_N", "R_KOR_N", "R_VIETNAM_N", "R_OTHERASIA_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
# Can do x == 1, because 1's will only be in the Asian category
QPR19$INTERVENTION_A = NULL
QPR19 = data.frame(apply(QPR19, 1, function(x){ifelse(x == 1, 1, 0)}))
colnames(QPR19) = c("Asian")
describe.factor(QPR19$Asian)

## Native Hawaiian or Other Pacific Islander
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_HAW_N, gpraAdultBase$R_GUAM_N, gpraAdultBase$R_OTHERPI_N)
QPR19 = data.frame(na.omit(QPR19))
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
# Can do x == 1, because 1's will only be in the Asian category
QPR19 = data.frame(apply(QPR19, 2, function(x){ifelse(x == 1, 1, 0)}))
QPR19 = data.frame(apply(QPR19, 1, sum))
colnames(QPR19) = c("Asian")
describe.factor(QPR19$Asian)

## White

QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_WHITE_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_WHITE_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
describe.factor(QPR19$R_WHITE_N)

# Multi just grab columns after deleting missing then sum.  Then if greater than 1 then are 1 if not 0.
QPR19 = data.frame(Date = gpraAdultBase$Date, INTERVENTION_A =gpraAdultBase$INTERVENTION_A, gpraAdultBase[,c(17:20, 22:34)])
head(QPR19)
#QPR19 = data.frame(na.omit(QPR19))
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19$Date = NULL
QPR19$INTERVENTION_A = NULL
QPR19 = data.frame(apply(QPR19, 1, sum))
colnames(QPR19) = c("Multi")
describe.factor(QPR19$Multi)

## Received an HIV test for the first time using CSAP/MAI grant funds during current reporting period
## Black
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_BLACK_N, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_BLACK_N", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
describe.factor(QPR19$R_BLACK_N)

## American Indian or Alaska Native
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_AMERINALSK_N, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_AMERINALSK_N", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
QPR19$INTERVENTION_A = NULL
describe.factor(QPR19$R_AMERINALSK_N)

## Asian
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_ASIAIN_N, gpraAdultBase$R_CHINESE_N, gpraAdultBase$R_FILIP_N, gpraAdultBase$R_JAPAN_N, gpraAdultBase$R_KOR_N, gpraAdultBase$R_VIETNAM_N, gpraAdultBase$R_OTHERASIA_N, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_ASIAIN_N", "R_CHINESE_N", "R_FILIP_N", "R_JAPAN_N", "R_KOR_N", "R_VIETNAM_N", "R_OTHERASIA_N", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
QPR19$INTERVENTION_A = NULL
QPR19 = data.frame(apply(QPR19, 2, function(x){ifelse(x == 1, 1, 0)}))
QPR19 = data.frame(apply(QPR19, 1, sum))
colnames(QPR19) = c("Asian")
describe.factor(QPR19$Asian)

## Native Hawaiian or Other Pacific Islander
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_HAW_N, gpraAdultBase$R_GUAM_N, gpraAdultBase$R_OTHERPI_N, gpraAdultBase$HIV_RESULTS_N)
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_HAW_N", "R_GUAM_N", "R_OTHERPI_N", "HIV_RESULTS_N")
QPR19 = data.frame(na.omit(QPR19))
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
QPR19 = data.frame(apply(QPR19, 2, function(x){ifelse(x == 1, 1, 0)}))
QPR19$INTERVENTION_A = NULL
QPR19 = data.frame(apply(QPR19, 1, sum))

colnames(QPR19) = c("Asian")
describe.factor(QPR19$Asian)

## White

QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$R_WHITE_N, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "R_WHITE_N", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
describe.factor(QPR19$R_WHITE_N)

# Multi just grab columns after deleting missing then sum.  Then if greater than 1 then are 1 if not 0.
QPR19 = data.frame(Date = gpraAdultBase$Date, INTERVENTION_A =gpraAdultBase$INTERVENTION_A, gpraAdultBase[,c(17:20, 22:34)])
#QPR19 = data.frame(na.omit(QPR19))
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
QPR19$Date = NULL
QPR19$INTERVENTION_A = NULL
QPR19 = data.frame(apply(QPR19, 1, sum))
colnames(QPR19) = c("Multi")
describe.factor(QPR19$Multi)


## Received an HIV test using CSAP/MAI grant funds during current reporting period
# Age ### For 12-17 it is 0.

# 18 and older so everyone
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
dim(QPR19)[1]

# 18-24
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, AGE = 2018-gpraAdultBase$YOB)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "AGE")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, AGE >= 18 & AGE <= 24)
dim(QPR19)[1]

# 25-34
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, AGE = 2018-gpraAdultBase$YOB)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "AGE")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, AGE >= 25 & AGE <= 34)
dim(QPR19)[1]

# 35-44
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, AGE = 2018-gpraAdultBase$YOB)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "AGE")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
dim(QPR19)[1]
count(QPR19$AGE)

# Rest are zero

## Received an HIV test using CSAP/MAI grant funds during current reporting period
# Age ### For 12-17 it is 0.

# 18 and older so everyone
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
dim(QPR19)[1]

# 18-24
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "AGE", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, AGE >= 18 & AGE <= 24)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
dim(QPR19)[1]

# 25-34
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "AGE", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, AGE >= 25 & AGE <= 34)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
dim(QPR19)[1]

# 35-44
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "AGE", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
count(QPR19$AGE)

# Rest are zero
# How many were homeless or unstably housed?
# 0

## How many were tested directly by the grantee organization or partner organization?
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
dim(QPR19)[1]

## How many tested positive for HIV?
# 0

## How many were informed of their HIV status? 
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_A, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_A", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2018-10-1" & QPR19$Date <= "2018-12-31",]
QPR19 = subset(QPR19, INTERVENTION_A == 1)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
dim(QPR19)[1]

## Of those who tested positive for HIV, how many were referred to treatment?
# 0
setwd("S:/Indiana Research & Evaluation/CCPE/CCPE SPSS - Datasets/DosageDocs")
### Centerstone average sessions per client use dosage forms
### Grab the duration for each take the means for each one then sum.  This will give you the average duration for the four types of things provided 
DosageI = read.csv("HIV Individual Dosage Upload Template - Updated 10.01.2018.csv", header = TRUE)

# No data
#DosageG = read.csv("HIV Group Dosage Upload Template - Updated 20171116.csv", header = TRUE)
DosageI = DosageI[c("DURATION1", "DURATION2", "DURATION3", "DURATION4")]

DosageI = data.frame(apply(DosageI, 1, sum, na.rm = TRUE))
mean(DosageI$apply.DosageI..1..sum..na.rm...TRUE., na.rm = TRUE)



