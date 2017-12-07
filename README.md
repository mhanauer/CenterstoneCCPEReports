###### Goal 1 ############################################################
###### Objective E Number of people served by each intervention Overall
# First grab total number of people served use the participant ID include the youth then for each intervention.  Grab the length for both adult and youth from all the data which will give you all the people who have data entered.  
# You don't need to specifcy the baseline data only, because there is no way that someone could not have baseline.  Therefore, the length of the total data set will equal the length of the baseline
###### Goal 1 ############################################################
###### Objective E Number of people served by each intervention Overall
# First grab total number of people served use the participant ID include the youth then for each intervention.  Grab the length for both adult and youth from all the data which will give you all the people who have data entered.  

totalAdults = length(gpraAdultAll$PARTID)
totalYouth = length(gpraYouthAll$PARTID)

totalCCPE = sum(totalAdults, totalYouth); totalCCPE

# Now SIS
InternAdult = data.frame(gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
SISAdult = data.frame(subset(InternAdult, gpraAdultAll.INTERVENTION_A.x ==2 | gpraAdultAll.INTERVENTION_B.x == 2 | gpraAdultAll.INTERVENTION_C.x == 2))
SISAdult = dim(SISAdult)
SISAdult = SISAdult[1]

InternYouth = data.frame(gpraYouthAll$INTERVENTION_A.x, gpraYouthAll$INTERVENTION_B.x, gpraYouthAll$INTERVENTION_C.x)
SISYouth = data.frame(subset(InternYouth, gpraYouthAll.INTERVENTION_A.x == 2 | gpraYouthAll.INTERVENTION_B.x == 2 | gpraYouthAll.INTERVENTION_C.x == 2))
SISYouth = dim(SISYouth)
SISYouth = SISYouth[1]

totalSIS = sum(SISAdult, SISYouth); totalSIS

# Now CTR and no CTR for youth
InternAdult = data.frame(gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
CTRAdult = data.frame(subset(InternAdult, gpraAdultAll.INTERVENTION_A.x ==1 | gpraAdultAll.INTERVENTION_B.x == 1 | gpraAdultAll.INTERVENTION_C.x == 1))
CTRAdult = dim(CTRAdult)
CTRAdult = CTRAdult[1]
totalCTR = CTRAdult 


# Lead and Seed 
InternAdult = data.frame(gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
LSAdult = data.frame(subset(InternAdult, gpraAdultAll.INTERVENTION_A.x ==3 | gpraAdultAll.INTERVENTION_B.x == 3 | gpraAdultAll.INTERVENTION_C.x == 3))
LSAdult = dim(LSAdult)
LSAdult = LSAdult[1]

InternYouth = data.frame(gpraYouthAll$INTERVENTION_A.x, gpraYouthAll$INTERVENTION_B.x, gpraYouthAll$INTERVENTION_C.x)
LSYouth = data.frame(subset(InternYouth, gpraYouthAll.INTERVENTION_A.x ==1 | gpraYouthAll.INTERVENTION_B.x == 1 | gpraYouthAll.INTERVENTION_C.x == 1))
LSYouth = dim(LSYouth)
LSYouth = LSYouth[1]
totalLS = LSYouth

# Respect need it for adult and youth
InternAdult = data.frame(gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
RespectAdult = data.frame(subset(InternAdult, gpraAdultAll.INTERVENTION_A.x ==51 | gpraAdultAll.INTERVENTION_B.x == 51 | gpraAdultAll.INTERVENTION_C.x == 51))
RespectAdult = dim(RespectAdult)
RespectAdult = RespectAdult[1]

InternYouth = data.frame(gpraYouthAll$INTERVENTION_A.x, gpraYouthAll$INTERVENTION_B.x, gpraYouthAll$INTERVENTION_C.x)
RespectYouth = data.frame(subset(InternYouth, gpraYouthAll.INTERVENTION_A.x ==51 | gpraYouthAll.INTERVENTION_B.x == 51 | gpraYouthAll.INTERVENTION_C.x == 51))
RespectYouth = dim(RespectYouth)
RespectYouth = RespectYouth[1]

totalRespect = data.frame(sum(RespectAdult, RespectYouth))
colnames(totalRespect) = c("totalRespect")


## Create a nice table for the data
Goal1ObjectiveE= data.frame(totalCCPE, totalAdults , totalYouth, SISAdult, SISYouth, totalSIS, totalCTR, LSAdult, LSYouth, totalLS, RespectAdult, RespectYouth, totalRespect)
setwd("C:/Users/Matthew.Hanauer/Desktop")
write.csv(Goal1ObjectiveE, "Goal1ObjectiveE.csv", row.names = FALSE)


###### Goal 1 ############################################################
###### Objective F Need yearly and quaterly
# Grabing the baseline year and month so that you can subset the year and month of interest, which is greater than 10, because 11-1 is when this quater starts of 2017.
Goal1ObjectiveFAdultData = data.frame(gpraAdultAll$YEAR.x, gpraAdultAll$MONTH.x)
Goal1ObjectiveFAdultYear = subset(Goal1ObjectiveFAdultData, gpraAdultAll.YEAR.x = 2017)
Goal1ObjectiveFAdultYear = dim(Goal1ObjectiveFAdultYear)
Goal1ObjectiveFAdultYear = Goal1ObjectiveFAdultYear[1]

Goal1ObjectiveFAdultQuarter = data.frame(subset(Goal1ObjectiveFAdultData, gpraAdultAll.MONTH.x > 10 | gpraAdultAll.YEAR.x >= 2017))
Goal1ObjectiveFAdultQuarter = dim(Goal1ObjectiveFAdultQuarter)
Goal1ObjectiveFAdultQuarter = Goal1ObjectiveFAdultQuarter[1]

GoalObjectiveF = data.frame(Goal1ObjectiveFAdultYear, Goal1ObjectiveFAdultQuarter)
colnames(GoalObjectiveF) =c("Yearly", "Quarterly")
GoalObjectiveF = t(GoalObjectiveF)
colnames(GoalObjectiveF) = c("Total")
write.csv(GoalObjectiveF, "GoalObjectiveF.csv")

##### Goal 1 Objective G #############  ############# ############# ############# ############# #############
InternAdult = data.frame(gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
RapidAdult = data.frame(subset(InternAdult, gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
RapidAdult = dim(RapidAdult)
RapidAdult = RapidAdult[1]
Goal1ObjectiveG = data.frame(RapidAdult)
colnames(Goal1ObjectiveG) = c("Total HIV Tested")
write.csv(Goal1ObjectiveG, "Goal1ObjectiveG.csv", row.names = FALSE)

################## Goal 3 Objective A #############################################################
# Grabbing the KNOW_SA variable, because other variables are being used.  Only 17 particpants from pocket which would be better.
Goal3ObjectiveA =  data.frame(gpraAdultAll$KNOW_SA.x, gpraAdultAll$KNOW_SA.y, gpraAdultAll$KNOW_SA)

Goal3ObjectiveABaseMonth3 = data.frame(Goal3ObjectiveA$gpraAdultAll.KNOW_SA.x, Goal3ObjectiveA$gpraAdultAll.KNOW_SA.y)
Goal3ObjectiveABaseMonth3 = data.frame(apply(Goal3ObjectiveABaseMonth3, 2, function(x){ifelse(x == 98, NA, x)}))
Goal3ObjectiveABaseMonth3 = na.omit(Goal3ObjectiveABaseMonth3)
Goal3ObjectiveABaseMonth3 = data.frame(Goal3ObjectiveABaseMonth3)
## Here is the number of people that completed both
dim(Goal3ObjectiveABaseMonth3)
head(Goal3ObjectiveABaseMonth3)
wilcox.test(Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.y, Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.x, paired = TRUE, alternative  =c("greater"))

## Need to create a spread for this.
Goal3ObjectiveABaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveABaseMonth3)))
colnames(Goal3ObjectiveABaseMonth3) = c("Base", "Month3")
Goal3ObjectiveABaseMonth3$Difference = (Goal3ObjectiveABaseMonth3$Month3-Goal3ObjectiveABaseMonth3$Base)/Goal3ObjectiveABaseMonth3$Base
Goal3ObjectiveABaseMonth3 = round(Goal3ObjectiveABaseMonth3,2)
write.csv(Goal3ObjectiveABaseMonth3 , "Goal3ObjectiveABaseMonth3.csv", row.names = FALSE)

### Now get the numbers for Baseline versus 6
Goal3ObjectiveA =  data.frame(gpraAdultAll$KNOW_SA.x, gpraAdultAll$KNOW_SA, gpraAdultAll$KNOW_SA)

Goal3ObjectiveABaseMonth6 = data.frame(Goal3ObjectiveA$gpraAdultAll.KNOW_SA.x, Goal3ObjectiveA$gpraAdultAll.KNOW_SA)
Goal3ObjectiveABaseMonth6 = data.frame(apply(Goal3ObjectiveABaseMonth6, 2, function(x){ifelse(x == 98, NA, x)}))
Goal3ObjectiveABaseMonth6 = na.omit(Goal3ObjectiveABaseMonth6)
Goal3ObjectiveABaseMonth6 = data.frame(Goal3ObjectiveABaseMonth6)
## Here is the number of people that completed both
dim(Goal3ObjectiveABaseMonth6)
head(Goal3ObjectiveABaseMonth6)
wilcox.test(Goal3ObjectiveABaseMonth6$Goal3ObjectiveA.gpraAdultAll.KNOW_SA, Goal3ObjectiveABaseMonth6$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.x, paired = TRUE, alternative  =c("greater"))

## Need to create a spread for this.
Goal3ObjectiveABaseMonth6 = data.frame(t(colMeans(Goal3ObjectiveABaseMonth6)))
colnames(Goal3ObjectiveABaseMonth6) = c("Base", "Month6")
Goal3ObjectiveABaseMonth6$Difference = (Goal3ObjectiveABaseMonth6$Month6-Goal3ObjectiveABaseMonth6$Base)/Goal3ObjectiveABaseMonth6
Goal3ObjectiveABaseMonth6 = round(Goal3ObjectiveABaseMonth6,2)
write.csv(Goal3ObjectiveABaseMonth6 , "Goal3ObjectiveABaseMonth6.csv", row.names = FALSE)


########## Goal 3 Objective B | Increase knowledge about HIV and VH by 20%.  KNOW_HIV #########################################################

Goal3ObjectiveB =  data.frame(gpraAdultAll$KNOW_HIV.x, gpraAdultAll$KNOW_HIV.y, gpraAdultAll$KNOW_HIV)

Goal3ObjectiveB = data.frame(apply(Goal3ObjectiveB, 2, function(x){ifelse(x == 98, NA, ifelse(x == 2, NA, x))}))
setwd("C:/Users/Matthew.Hanauer/Desktop")
write.csv(Goal3ObjectiveB , "Goal3ObjectiveB.csv", row.names = FALSE)
Goal3ObjectiveB  = data.frame(read.csv("Goal3ObjectiveB.csv", header = TRUE))
head(Goal3ObjectiveB)

Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.x, Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.y)
Goal3ObjectiveBBaseMonth3 = na.omit(Goal3ObjectiveBBaseMonth3)
Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveBBaseMonth3)
## Here is the number of people that completed both
dim(Goal3ObjectiveBBaseMonth3)
head(Goal3ObjectiveBBaseMonth3)
library(stats)
## Just do Wilcox test for now get McNemar's Test later need to sum the 1 and 0's do baseline versus 3 and 3 months versus 6 months

wilcox.test(Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.y, Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.x, paired = TRUE, alternative  =c("greater"))

Goal3ObjectiveBBaseMonth3Means = data.frame(colMeans(Goal3ObjectiveBBaseMonth3))
colnames(Goal3ObjectiveBBaseMonth3Means) = c("Mean")
head(Goal3ObjectiveBBaseMonth3Means)
Goal3ObjectiveBBaseMonth3Means = (Goal3ObjectiveBBaseMonth3Means[2,1]-Goal3ObjectiveBBaseMonth3Means[1,1])/Goal3ObjectiveBBaseMonth3Means[1,1]
write.csv(Goal3ObjectiveBBaseMonth3Means, "Goal3ObjectiveBBaseMonth3Means.csv", row.names = FALSE)



### Now get the numbers for Baseline versus 6
Goal3ObjectiveBMonth3Month6 = data.frame(Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.x, Goal3ObjectiveB$gpraAdultAll.KNOW_HIV)
Goal3ObjectiveBMonth3Month6 = na.omit(Goal3ObjectiveBMonth3Month6)
Goal3ObjectiveBMonth3Month6 = data.frame(Goal3ObjectiveBMonth3Month6)
## Here is the number of people that completed both
dim(Goal3ObjectiveBMonth3Month6)
head(Goal3ObjectiveBMonth3Month6)
wilcox.test(Goal3ObjectiveBMonth3Month6$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV, Goal3ObjectiveBMonth3Month6$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.x, paired = TRUE, alternative  =c("greater"))
Goal3ObjectiveBBaseMonth6Means = data.frame(colMeans(Goal3ObjectiveBBaseMonth6))
colnames(Goal3ObjectiveBBaseMonth6Means) = c("Mean")
head(Goal3ObjectiveBBaseMonth6Means)
Goal3ObjectiveBBaseMonth6Means = (Goal3ObjectiveBBaseMonth6Means[2,1]-Goal3ObjectiveBBaseMonth6Means[1,1])/Goal3ObjectiveBBaseMonth6Means[1,1]
write.csv(Goal3ObjectiveBBaseMonth6Means, "Goal3ObjectiveBBaseMonth6Means.csv", row.names = FALSE)


##### Goal 3 Objective C RSKCIG, RSKMJ, RSKALC create a total compoiste scores ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
Goal3ObjectiveC = data.frame(gpraAdultAll$RSKCIG.x, gpraAdultAll$RSKCIG.y, gpraAdultAll$RSKCIG, gpraAdultAll$RSKMJ.x, gpraAdultAll$RSKMJ.y, gpraAdultAll$RSKMJ, gpraAdultAll$RSKALC.x, gpraAdultAll$RSKALC.y, gpraAdultAll$RSKALC)
head(Goal3ObjectiveC)
Goal3ObjectiveC  = data.frame(apply(Goal3ObjectiveC, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
head(Goal3ObjectiveC)

Goal3ObjectiveCBaseMonth3= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKCIG.x, Goal3ObjectiveC$gpraAdultAll.RSKCIG.y, Goal3ObjectiveC$gpraAdultAll.RSKMJ.x, Goal3ObjectiveC$gpraAdultAll.RSKMJ.y, Goal3ObjectiveC$gpraAdultAll.RSKALC.x, Goal3ObjectiveC$gpraAdultAll.RSKALC.y)
head(Goal3ObjectiveCBaseMonth3) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth3  = na.omit(Goal3ObjectiveCBaseMonth3)
Goal3ObjectiveCBaseMonth3  = data.frame(Goal3ObjectiveCBaseMonth3)
head(Goal3ObjectiveCBaseMonth3)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKCIG.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKMJ.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKALC.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("SAHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth3 = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKCIG.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKMJ.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKALC.y)
head(Goal3ObjectiveCMonth3)
Goal3ObjectiveCMonth3 = data.frame(apply(Goal3ObjectiveCMonth3, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth3 ) = c("SAHarmMonth3")
head(Goal3ObjectiveCMonth3)
dim(Goal3ObjectiveCMonth3)

Goal3ObjectiveCBaseMonth3 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth3)
head(Goal3ObjectiveCBaseMonth3)
wilcox.test(Goal3ObjectiveCMonth3$SAHarmMonth3, Goal3ObjectiveCBase$SAHarmBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveCBaseMonth3Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth3))
colnames(Goal3ObjectiveCBaseMonth3Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth3Means)
Goal3ObjectiveCBaseMonth3Means = (Goal3ObjectiveCBaseMonth3Means[2,1]-Goal3ObjectiveCBaseMonth3Means[1,1])/Goal3ObjectiveCBaseMonth3Means[1,1]
write.csv(Goal3ObjectiveCBaseMonth3Means, "Goal3ObjectiveCBaseMonth3Means.csv", row.names = FALSE)


## Repeat this process for Baseline and Month6

Goal3ObjectiveCBaseMonth6= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKCIG.x, Goal3ObjectiveC$gpraAdultAll.RSKCIG, Goal3ObjectiveC$gpraAdultAll.RSKMJ.x, Goal3ObjectiveC$gpraAdultAll.RSKMJ, Goal3ObjectiveC$gpraAdultAll.RSKALC.x, Goal3ObjectiveC$gpraAdultAll.RSKALC)
head(Goal3ObjectiveCBaseMonth6) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth6  = na.omit(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6  = data.frame(Goal3ObjectiveCBaseMonth6)
head(Goal3ObjectiveCBaseMonth6)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKCIG.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKMJ.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKALC.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("SAHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth6 = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKCIG, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKMJ, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKALC)
head(Goal3ObjectiveCMonth6)
Goal3ObjectiveCMonth6 = data.frame(apply(Goal3ObjectiveCMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth6 ) = c("SAHarmMonth6")
head(Goal3ObjectiveCMonth6)
dim(Goal3ObjectiveCMonth6)

Goal3ObjectiveCBaseMonth6 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth6)
head(Goal3ObjectiveCBaseMonth6)
wilcox.test(Goal3ObjectiveCMonth6$SAHarmMonth6, Goal3ObjectiveCBase$SAHarmBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveCBaseMonth6Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth6))
colnames(Goal3ObjectiveCBaseMonth6Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth6Means)
Goal3ObjectiveCBaseMonth6Means = (Goal3ObjectiveCBaseMonth6Means[2,1]-Goal3ObjectiveCBaseMonth6Means[1,1])/Goal3ObjectiveCBaseMonth6Means[1,1]
write.csv(Goal3ObjectiveCBaseMonth6Means, "Goal3ObjectiveCBaseMonth6Means.csv", row.names = FALSE)



####### Goal 3 Objective D ######### ###################################################################################################
# Grab these variables and change RSKANYSEX_UNP, RSKSEX_ALCDRG, RSKNDL_SHR change ObjectiveC to ObjectiveD, change SAHarm to HIVHarm 
Goal3ObjectiveC= data.frame(gpraAdultAll$RSKANYSEX_UNP.x, gpraAdultAll$RSKANYSEX_UNP.y, gpraAdultAll$RSKANYSEX_UNP, gpraAdultAll$RSKSEX_ALCDRG.x, gpraAdultAll$RSKSEX_ALCDRG.y, gpraAdultAll$RSKSEX_ALCDRG, gpraAdultAll$RSKNDL_SHR.x, gpraAdultAll$RSKNDL_SHR.y, gpraAdultAll$RSKNDL_SHR)
head(Goal3ObjectiveC)
Goal3ObjectiveC = data.frame(apply(Goal3ObjectiveC, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
head(Goal3ObjectiveC)

Goal3ObjectiveCBaseMonth3= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP.y, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG.y, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR.x, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR.y)
head(Goal3ObjectiveCBaseMonth3) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth3  = na.omit(Goal3ObjectiveCBaseMonth3)
Goal3ObjectiveCBaseMonth3  = data.frame(Goal3ObjectiveCBaseMonth3)
head(Goal3ObjectiveCBaseMonth3)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("HIVHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth3 = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR.y)
head(Goal3ObjectiveCMonth3)
Goal3ObjectiveCMonth3 = data.frame(apply(Goal3ObjectiveCMonth3, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth3 ) = c("SAHarmMonth3")
head(Goal3ObjectiveCMonth3)
dim(Goal3ObjectiveCMonth3)

Goal3ObjectiveCBaseMonth3 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth3)
head(Goal3ObjectiveCBaseMonth3)
wilcox.test(Goal3ObjectiveCMonth3$SAHarmMonth3, Goal3ObjectiveCBase$HIVHarmBase, paired = TRUE, alternative = c("greater"))
Goal3ObjectiveCBaseMonth3Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth3))
colnames(Goal3ObjectiveCBaseMonth3Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth3Means)
Goal3ObjectiveDBaseMonth3Means = (Goal3ObjectiveCBaseMonth3Means[2,1]-Goal3ObjectiveCBaseMonth3Means[1,1])/Goal3ObjectiveCBaseMonth3Means[1,1]
write.csv(Goal3ObjectiveDBaseMonth3Means, "Goal3ObjectiveCBaseMonth3Means.csv", row.names = FALSE) 
## Repeat this process for Baseline and Month6


Goal3ObjectiveCBaseMonth6= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR.x, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR)
head(Goal3ObjectiveCBaseMonth6) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth6  = na.omit(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6  = data.frame(Goal3ObjectiveCBaseMonth6)
head(Goal3ObjectiveCBaseMonth6)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("HIVHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth6 = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR)
head(Goal3ObjectiveCMonth6)
Goal3ObjectiveCMonth6 = data.frame(apply(Goal3ObjectiveCMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth6 ) = c("SAHarmMonth6")
head(Goal3ObjectiveCMonth6)
dim(Goal3ObjectiveCMonth6)

Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth6)
head(Goal3ObjectiveDBaseMonth6 )
wilcox.test(Goal3ObjectiveDBaseMonth6$SAHarmMonth6, Goal3ObjectiveDBaseMonth6$HIVHarmBase, paired = TRUE, alternative = c("greater"))
Goal3ObjectiveCBaseMonth6 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth6)
head(Goal3ObjectiveCBaseMonth6)
wilcox.test(Goal3ObjectiveCMonth6$SAHarmMonth6, Goal3ObjectiveCBase$HIVHarmBase, paired = TRUE, alternative = c("greater"))


Goal3ObjectiveCBaseMonth6Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth6))
colnames(Goal3ObjectiveCBaseMonth6Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth6Means)
Goal3ObjectiveDBaseMonth6Means = (Goal3ObjectiveCBaseMonth6Means[2,1]-Goal3ObjectiveCBaseMonth6Means[1,1])/Goal3ObjectiveCBaseMonth6Means[1,1]
write.csv(Goal3ObjectiveDBaseMonth6Means, "Goal3ObjectiveCBaseMonth6Means.csv", row.names = FALSE)

###### Goal 3 Objective E ###################### ##############################################################################################################
Goal3ObjectiveEFP = data.frame(YOB = 2018-gpraAdultAll$YOB.x,gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
Goal3ObjectiveEFP = data.frame(subset(Goal3ObjectiveEFP, gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
head(Goal3ObjectiveEFP)
Goal3ObjectiveEFP = subset(Goal3ObjectiveEFP, YOB < 25)
Goal3ObjectiveEFP = data.frame(Goal3ObjectiveEFP)
Goal3ObjectiveEFP =dim(Goal3ObjectiveEFP)
Goal3ObjectiveEFP = data.frame(Goal3ObjectiveEFP[1]) 
colnames(Goal3ObjectiveEFP) = c("Tested Focus Population") 
write.csv(Goal3ObjectiveEFP, "Goal3ObjectiveEFP.csv", row.names = FALSE)

######## Goal 3 Objective F ############################################################################################
Goal3ObjectiveF = data.frame(Year = gpraAdultAll$YEAR.x,gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
Goal3ObjectiveF  = data.frame(subset(Goal3ObjectiveF , gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
Goal3ObjectiveF  = data.frame(Goal3ObjectiveF)
Goal3ObjectiveF  = subset(Goal3ObjectiveF, YEAR.x = 2017)
Goal3ObjectiveF  = data.frame(Goal3ObjectiveF)
Goal3ObjectiveF  =dim(Goal3ObjectiveF)
Goal3ObjectiveF  = data.frame(Goal3ObjectiveF[1]) 
colnames(Goal3ObjectiveF) = c("Tested 2017 Population") 
write.csv(Goal3ObjectiveF , "Goal3ObjectiveF.csv", row.names = FALSE)

###### Goal 3 Objective G ############################################################################################ #########
# Need to change the to just 2017 so need the year variable
Goal3ObjectiveG = data.frame(YEAR.x = gpraAdultAll$YEAR.x,gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
Goal3ObjectiveG  = data.frame(subset(Goal3ObjectiveG , gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
Goal3ObjectiveG  = data.frame(Goal3ObjectiveG)
head(Goal3ObjectiveG)

Goal3ObjectiveG2016  = data.frame(subset(Goal3ObjectiveG, YEAR.x == 2016))
Goal3ObjectiveG2016  =dim(Goal3ObjectiveG2016)
Goal3ObjectiveG2016  = Goal3ObjectiveG2016[1]

Goal3ObjectiveG2017  = data.frame(subset(Goal3ObjectiveG, YEAR.x == 2017))
Goal3ObjectiveG2017  =dim(Goal3ObjectiveG2017)
Goal3ObjectiveG2017  = Goal3ObjectiveG2017[1]

Goal3ObjectiveGIncrease = round(data.frame(Goal3ObjectiveGIncrease =(Goal3ObjectiveG2017-Goal3ObjectiveG2016)/ Goal3ObjectiveG2016),2)    
Goal3ObjectiveG = data.frame(Goal3ObjectiveG2016  , Goal3ObjectiveG2017, Goal3ObjectiveGIncrease)
colnames(Goal3ObjectiveG) = c("HIV Tested 2016", "HIV Tested 2017", "HIV Tested Increase") 
write.csv(Goal3ObjectiveG, "Goal3ObjectiveG .csv", row.names = FALSE)


###### Goal 3 Objective H ############################################################################################ #########
# CIG30D, TOB30D, VAP30D, ALC30D, BINGE530D, MJ30D, ILL30D, RX30D, SPICE30D, INJECT30D  So sum these to create an overall days used substances in 30 days
Goal3ObjectiveIBaseline3month = data.frame(gpraAdultAll$CIG30D.x,gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x, gpraAdultAll$SPICE30D.x, gpraAdultAll$INJECT30D.x, gpraAdultAll$CIG30D.y,gpraAdultAll$VAP30D.y, gpraAdultAll$ALC30D.y, gpraAdultAll$BINGE530D.y, gpraAdultAll$MJ30D.y, gpraAdultAll$ILL30D.y, gpraAdultAll$RX30D.y, gpraAdultAll$SPICE30D.y, gpraAdultAll$INJECT30D.y)            
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:9])
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,10:18])
head(Goal3ObjectiveI3month)
Goal3ObjectiveI3month = data.frame(apply(Goal3ObjectiveI3month, 1, sum))


colnames(Goal3ObjectiveI3month) = c("Goal3ObjectiveI3month")
head(Goal3ObjectiveI3month)

wilcox.test(Goal3ObjectiveI3month$Goal3ObjectiveI3month, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("less"))

Goal3ObjectiveIBaseMonth3 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveI3month)
Goal3ObjectiveIBaseMonth3 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth3)))
colnames(Goal3ObjectiveIBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveIBaseMonth3$Difference = round((Goal3ObjectiveIBaseMonth3$Month3-Goal3ObjectiveIBaseMonth3$Base)/Goal3ObjectiveIBaseMonth3$Base,2)
write.csv(Goal3ObjectiveIBaseMonth3, "Goal3ObjectiveHBaseMonth3.csv", row.names = FALSE)

#### Goal 3 Objective H Base to Month 6 ########## ######################################################################
# Need to change y to not .y.  Change Month3 to Month6
Goal3ObjectiveIBaseline3month = data.frame(gpraAdultAll$CIG30D.x,gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x, gpraAdultAll$SPICE30D.x, gpraAdultAll$INJECT30D.x, gpraAdultAll$CIG30D,gpraAdultAll$VAP30D, gpraAdultAll$ALC30D, gpraAdultAll$BINGE530D, gpraAdultAll$MJ30D, gpraAdultAll$ILL30D, gpraAdultAll$RX30D, gpraAdultAll$SPICE30D, gpraAdultAll$INJECT30D)            
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:9])
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,10:18])
head(Goal3ObjectiveI3month)
Goal3ObjectiveI3month = data.frame(apply(Goal3ObjectiveI3month, 1, sum))


colnames(Goal3ObjectiveI3month) = c("Goal3ObjectiveI3month")
head(Goal3ObjectiveI3month)

wilcox.test(Goal3ObjectiveI3month$Goal3ObjectiveI3month, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("less"))

Goal3ObjectiveIBaseMonth6 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveI3month)
Goal3ObjectiveIBaseMonth6 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth6)))
colnames(Goal3ObjectiveIBaseMonth6) = c("Base", "Month6")
Goal3ObjectiveIBaseMonth6$Difference = round((Goal3ObjectiveIBaseMonth6$Month6-Goal3ObjectiveIBaseMonth6$Base)/Goal3ObjectiveIBaseMonth6$Base,2)
write.csv(Goal3ObjectiveIBaseMonth6, "Goal3ObjectiveHBaseMonth6.csv", row.names = FALSE)


####################### Objective I ################ ################ ################ ################ ################ ################ ##############
CNTRL_REFUSEMOOD CNTRL_WAITCNDM CNTRL_TREAT CNTRL_SEXPRAC CNTRL_ASKCNDM CNTRL_REFUSECNDM
Goal3ObjectiveIBaseline3month= data.frame(gpraAdultAll$CNTRL_REFUSEMOOD.x, gpraAdultAll$CNTRL_WAITCNDM.x, gpraAdultAll$CNTRL_TREAT.x, gpraAdultAll$CNTRL_TREAT.x, gpraAdultAll$CNTRL_SEXPRAC.x, gpraAdultAll$CNTRL_ASKCNDM.x, gpraAdultAll$CNTRL_REFUSECNDM.x, gpraAdultAll$CNTRL_REFUSEMOOD.y, gpraAdultAll$CNTRL_WAITCNDM.y, gpraAdultAll$CNTRL_TREAT.y, gpraAdultAll$CNTRL_TREAT.y, gpraAdultAll$CNTRL_SEXPRAC.y, gpraAdultAll$CNTRL_ASKCNDM.y, gpraAdultAll$CNTRL_REFUSECNDM.y)
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
summary(Goal3ObjectiveIBaseline3month)
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:7])
head(Goal3ObjectiveIBase)
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,8:14])
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
Goal3ObjectiveIBaseline3month = data.frame(gpraAdultAll$CIG30D.x,gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x, gpraAdultAll$SPICE30D.x, gpraAdultAll$INJECT30D.x, gpraAdultAll$CIG30D,gpraAdultAll$VAP30D, gpraAdultAll$ALC30D, gpraAdultAll$BINGE530D, gpraAdultAll$MJ30D, gpraAdultAll$ILL30D, gpraAdultAll$RX30D, gpraAdultAll$SPICE30D, gpraAdultAll$INJECT30D)            
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:9])
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,10:18])
head(Goal3ObjectiveI3month)
Goal3ObjectiveI3month = data.frame(apply(Goal3ObjectiveI3month, 1, sum))


colnames(Goal3ObjectiveI3month) = c("Goal3ObjectiveI3month")
head(Goal3ObjectiveI3month)

wilcox.test(Goal3ObjectiveI3month$Goal3ObjectiveI3month, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveIBaseMonth6 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveI3month)
Goal3ObjectiveIBaseMonth6 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth6)))
colnames(Goal3ObjectiveIBaseMonth6) = c("Base", "Month6")
Goal3ObjectiveIBaseMonth6$Difference = round((Goal3ObjectiveIBaseMonth6$Month6-Goal3ObjectiveIBaseMonth6$Base)/Goal3ObjectiveIBaseMonth6$Base,2)
write.csv(Goal3ObjectiveIBaseMonth6, "Goal3ObjectiveHBaseMonth6.csv", row.names = FALSE)


###### Goal 1 ############################################################
###### Objective F Need yearly and quaterly
Goal1ObjectiveFAdultData = data.frame(gpraAdultAll$YEAR.x, gpraAdultAll$MONTH.x)
Goal1ObjectiveFAdultYear = subset(Goal1ObjectiveFAdultData, gpraAdultAll.YEAR.x = 2017)
Goal1ObjectiveFAdultYear = dim(Goal1ObjectiveFAdultYear)
Goal1ObjectiveFAdultYear = Goal1ObjectiveFAdultYear[1]

Goal1ObjectiveFAdultQuarter = data.frame(subset(Goal1ObjectiveFAdultData, gpraAdultAll.MONTH.x > 10 | gpraAdultAll.YEAR.x >= 2017))
Goal1ObjectiveFAdultQuarter = dim(Goal1ObjectiveFAdultQuarter)
Goal1ObjectiveFAdultQuarter = Goal1ObjectiveFAdultQuarter[1]

GoalObjectiveF = data.frame(Goal1ObjectiveFAdultYear, Goal1ObjectiveFAdultQuarter)
colnames(GoalObjectiveF) =c("Yearly", "Quarterly")
GoalObjectiveF = t(GoalObjectiveF)
colnames(GoalObjectiveF) = c("Total")
write.csv(GoalObjectiveF, "GoalObjectiveF.csv", row.names = FALSE)

##### Goal 1 Objective G #############  ############# ############# ############# ############# #############
InternAdult = data.frame(gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
RapidAdult = data.frame(subset(InternAdult, gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
RapidAdult = dim(RapidAdult)
RapidAdult = RapidAdult[1]
totalHIVTesting = data.frame(RapidAdult)
wilcox.test(Goal3ObjectiveA$gpraAdultAll.KNOW_SA.y, Goal3ObjectiveA$gpraAdultAll.KNOW_SA.x , paired = TRUE, alternative  =c("greater"))
colnames(totalHIVTesting) = c("Total HIV Tested")

################## Goal 3 Objective A #############################################################
# Create an average score across the five scores.  Grabe KNOW_SA, because Would you know where to go near where you live to see a health care professoinal regarding a drug or alcohol problem?
##### Overall #####
Goal3ObjectiveA =  data.frame(gpraAdultAll$KNOW_SA.x, gpraAdultAll$KNOW_SA.y, gpraAdultAll$KNOW_SA)

Goal3ObjectiveA = data.frame(apply(Goal3ObjectiveA, 2, function(x){ifelse(x == 98, NA, ifelse(x == 2, NA, x))}))
setwd("C:/Users/Matthew.Hanauer/Desktop")
write.csv(Goal3ObjectiveA , "Goal3ObjectiveA.csv", row.names = FALSE)
Goal3ObjectiveA  = data.frame(read.csv("Goal3ObjectiveA.csv", header = TRUE))
head(Goal3ObjectiveA)

Goal3ObjectiveABaseMonth3 = data.frame(Goal3ObjectiveA$gpraAdultAll.KNOW_SA.x, Goal3ObjectiveA$gpraAdultAll.KNOW_SA.y)
Goal3ObjectiveABaseMonth3 = na.omit(Goal3ObjectiveABaseMonth3)
Goal3ObjectiveABaseMonth3 = data.frame(Goal3ObjectiveABaseMonth3)
## Here is the number of people that completed both
dim(Goal3ObjectiveABaseMonth3)
head(Goal3ObjectiveABaseMonth3)
library(stats)
## Just do Wilcox test for now get McNemar's Test later need to sum the 1 and 0's do baseline versus 3 and 3 months versus 6 months

wilcox.test(Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.y, Goal3ObjectiveABaseMonth3$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.x, paired = TRUE, alternative  =c("greater"))
colMeans(Goal3ObjectiveABaseMonth3)

### Now get the numbers for Baseline versus 6
Goal3ObjectiveAMonth3Month6 = data.frame(Goal3ObjectiveA$gpraAdultAll.KNOW_SA.x, Goal3ObjectiveA$gpraAdultAll.KNOW_SA)
Goal3ObjectiveAMonth3Month6 = na.omit(Goal3ObjectiveAMonth3Month6)
Goal3ObjectiveAMonth3Month6 = data.frame(Goal3ObjectiveAMonth3Month6)
## Here is the number of people that completed both
dim(Goal3ObjectiveAMonth3Month6)
head(Goal3ObjectiveAMonth3Month6)

wilcox.test(Goal3ObjectiveAMonth3Month6$Goal3ObjectiveA.gpraAdultAll.KNOW_SA, Goal3ObjectiveAMonth3Month6$Goal3ObjectiveA.gpraAdultAll.KNOW_SA.x, paired = TRUE, alternative  =c("greater"))# Now see if you can get the percentage difference
colMeans(Goal3ObjectiveAMonth3Month6)

########## Goal 3 Objective B | Increase knowledge about HIV and VH by 20%.  KNOW_HIV #########################################################

Goal3ObjectiveB =  data.frame(gpraAdultAll$KNOW_HIV.x, gpraAdultAll$KNOW_HIV.y, gpraAdultAll$KNOW_HIV)

Goal3ObjectiveB = data.frame(apply(Goal3ObjectiveB, 2, function(x){ifelse(x == 98, NA, ifelse(x == 2, NA, x))}))
setwd("C:/Users/Matthew.Hanauer/Desktop")
write.csv(Goal3ObjectiveB , "Goal3ObjectiveB.csv", row.names = FALSE)
Goal3ObjectiveB  = data.frame(read.csv("Goal3ObjectiveB.csv", header = TRUE))
head(Goal3ObjectiveB)

Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.x, Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.y)
Goal3ObjectiveBBaseMonth3 = na.omit(Goal3ObjectiveBBaseMonth3)
Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveBBaseMonth3)
## Here is the number of people that completed both
dim(Goal3ObjectiveBBaseMonth3)
head(Goal3ObjectiveBBaseMonth3)
library(stats)
## Just do Wilcox test for now get McNemar's Test later need to sum the 1 and 0's do baseline versus 3 and 3 months versus 6 months

wilcox.test(Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.y, Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.x, paired = TRUE, alternative  =c("greater"))

Goal3ObjectiveBBaseMonth3Means = data.frame(colMeans(Goal3ObjectiveBBaseMonth3))
colnames(Goal3ObjectiveBBaseMonth3Means) = c("Mean")
head(Goal3ObjectiveBBaseMonth3Means)
Goal3ObjectiveBBaseMonth3Means = (Goal3ObjectiveBBaseMonth3Means[2,1]-Goal3ObjectiveBBaseMonth3Means[1,1])/Goal3ObjectiveBBaseMonth3Means[1,1]
write.csv(Goal3ObjectiveBBaseMonth3Means, "Goal3ObjectiveBBaseMonth3Means.csv", row.names = FALSE)



### Now get the numbers for Baseline versus 6
Goal3ObjectiveBMonth3Month6 = data.frame(Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.x, Goal3ObjectiveB$gpraAdultAll.KNOW_HIV)
Goal3ObjectiveBMonth3Month6 = na.omit(Goal3ObjectiveBMonth3Month6)
Goal3ObjectiveBMonth3Month6 = data.frame(Goal3ObjectiveBMonth3Month6)
## Here is the number of people that completed both
dim(Goal3ObjectiveBMonth3Month6)
head(Goal3ObjectiveBMonth3Month6)
wilcox.test(Goal3ObjectiveBMonth3Month6$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV, Goal3ObjectiveBMonth3Month6$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.x, paired = TRUE, alternative  =c("greater"))
Goal3ObjectiveBBaseMonth6Means = data.frame(colMeans(Goal3ObjectiveBBaseMonth6))
colnames(Goal3ObjectiveBBaseMonth6Means) = c("Mean")
head(Goal3ObjectiveBBaseMonth6Means)
Goal3ObjectiveBBaseMonth6Means = (Goal3ObjectiveBBaseMonth6Means[2,1]-Goal3ObjectiveBBaseMonth6Means[1,1])/Goal3ObjectiveBBaseMonth6Means[1,1]
write.csv(Goal3ObjectiveBBaseMonth6Means, "Goal3ObjectiveBBaseMonth6Means.csv", row.names = FALSE)


##### Goal 3 Objective C RSKCIG, RSKMJ, RSKALC create a total compoiste scores ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
Goal3ObjectiveC = data.frame(gpraAdultAll$RSKCIG.x, gpraAdultAll$RSKCIG.y, gpraAdultAll$RSKCIG, gpraAdultAll$RSKMJ.x, gpraAdultAll$RSKMJ.y, gpraAdultAll$RSKMJ, gpraAdultAll$RSKALC.x, gpraAdultAll$RSKALC.y, gpraAdultAll$RSKALC)
head(Goal3ObjectiveC)
Goal3ObjectiveC  = data.frame(apply(Goal3ObjectiveC, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
head(Goal3ObjectiveC)

Goal3ObjectiveCBaseMonth3= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKCIG.x, Goal3ObjectiveC$gpraAdultAll.RSKCIG.y, Goal3ObjectiveC$gpraAdultAll.RSKMJ.x, Goal3ObjectiveC$gpraAdultAll.RSKMJ.y, Goal3ObjectiveC$gpraAdultAll.RSKALC.x, Goal3ObjectiveC$gpraAdultAll.RSKALC.y)
head(Goal3ObjectiveCBaseMonth3) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth3  = na.omit(Goal3ObjectiveCBaseMonth3)
Goal3ObjectiveCBaseMonth3  = data.frame(Goal3ObjectiveCBaseMonth3)
head(Goal3ObjectiveCBaseMonth3)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKCIG.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKMJ.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKALC.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("SAHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth3 = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKCIG.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKMJ.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKALC.y)
head(Goal3ObjectiveCMonth3)
Goal3ObjectiveCMonth3 = data.frame(apply(Goal3ObjectiveCMonth3, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth3 ) = c("SAHarmMonth3")
head(Goal3ObjectiveCMonth3)
dim(Goal3ObjectiveCMonth3)

Goal3ObjectiveCBaseMonth3 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth3)
head(Goal3ObjectiveCBaseMonth3)
wilcox.test(Goal3ObjectiveCMonth3$SAHarmMonth3, Goal3ObjectiveCBase$SAHarmBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveCBaseMonth3Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth3))
colnames(Goal3ObjectiveCBaseMonth3Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth3Means)
Goal3ObjectiveCBaseMonth3Means = (Goal3ObjectiveCBaseMonth3Means[2,1]-Goal3ObjectiveCBaseMonth3Means[1,1])/Goal3ObjectiveCBaseMonth3Means[1,1]
write.csv(Goal3ObjectiveCBaseMonth3Means, "Goal3ObjectiveCBaseMonth3Means.csv", row.names = FALSE)


## Repeat this process for Baseline and Month6

Goal3ObjectiveCBaseMonth6= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKCIG.x, Goal3ObjectiveC$gpraAdultAll.RSKCIG, Goal3ObjectiveC$gpraAdultAll.RSKMJ.x, Goal3ObjectiveC$gpraAdultAll.RSKMJ, Goal3ObjectiveC$gpraAdultAll.RSKALC.x, Goal3ObjectiveC$gpraAdultAll.RSKALC)
head(Goal3ObjectiveCBaseMonth6) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth6  = na.omit(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6  = data.frame(Goal3ObjectiveCBaseMonth6)
head(Goal3ObjectiveCBaseMonth6)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKCIG.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKMJ.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKALC.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("SAHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth6 = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKCIG, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKMJ, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKALC)
head(Goal3ObjectiveCMonth6)
Goal3ObjectiveCMonth6 = data.frame(apply(Goal3ObjectiveCMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth6 ) = c("SAHarmMonth6")
head(Goal3ObjectiveCMonth6)
dim(Goal3ObjectiveCMonth6)

Goal3ObjectiveCBaseMonth6 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth6)
head(Goal3ObjectiveCBaseMonth6)
wilcox.test(Goal3ObjectiveCMonth6$SAHarmMonth6, Goal3ObjectiveCBase$SAHarmBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveCBaseMonth6Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth6))
colnames(Goal3ObjectiveCBaseMonth6Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth6Means)
Goal3ObjectiveCBaseMonth6Means = (Goal3ObjectiveCBaseMonth6Means[2,1]-Goal3ObjectiveCBaseMonth6Means[1,1])/Goal3ObjectiveCBaseMonth6Means[1,1]
write.csv(Goal3ObjectiveCBaseMonth6Means, "Goal3ObjectiveCBaseMonth6Means.csv", row.names = FALSE)



####### Goal 3 Objective D ######### ###################################################################################################
# Grab these variables and change RSKANYSEX_UNP, RSKSEX_ALCDRG, RSKNDL_SHR change ObjectiveC to ObjectiveD, change SAHarm to HIVHarm 
Goal3ObjectiveC= data.frame(gpraAdultAll$RSKANYSEX_UNP.x, gpraAdultAll$RSKANYSEX_UNP.y, gpraAdultAll$RSKANYSEX_UNP, gpraAdultAll$RSKSEX_ALCDRG.x, gpraAdultAll$RSKSEX_ALCDRG.y, gpraAdultAll$RSKSEX_ALCDRG, gpraAdultAll$RSKNDL_SHR.x, gpraAdultAll$RSKNDL_SHR.y, gpraAdultAll$RSKNDL_SHR)
head(Goal3ObjectiveC)
Goal3ObjectiveC = data.frame(apply(Goal3ObjectiveC, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
head(Goal3ObjectiveC)

Goal3ObjectiveCBaseMonth3= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP.y, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG.y, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR.x, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR.y)
head(Goal3ObjectiveCBaseMonth3) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth3  = na.omit(Goal3ObjectiveCBaseMonth3)
Goal3ObjectiveCBaseMonth3  = data.frame(Goal3ObjectiveCBaseMonth3)
head(Goal3ObjectiveCBaseMonth3)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("HIVHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth3 = data.frame(Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG.y, Goal3ObjectiveCBaseMonth3$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR.y)
head(Goal3ObjectiveCMonth3)
Goal3ObjectiveCMonth3 = data.frame(apply(Goal3ObjectiveCMonth3, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth3 ) = c("SAHarmMonth3")
head(Goal3ObjectiveCMonth3)
dim(Goal3ObjectiveCMonth3)

Goal3ObjectiveCBaseMonth3 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth3)
head(Goal3ObjectiveCBaseMonth3)
wilcox.test(Goal3ObjectiveCMonth3$SAHarmMonth3, Goal3ObjectiveCBase$HIVHarmBase, paired = TRUE, alternative = c("greater"))
Goal3ObjectiveCBaseMonth3Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth3))
colnames(Goal3ObjectiveCBaseMonth3Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth3Means)
Goal3ObjectiveDBaseMonth3Means = (Goal3ObjectiveCBaseMonth3Means[2,1]-Goal3ObjectiveCBaseMonth3Means[1,1])/Goal3ObjectiveCBaseMonth3Means[1,1]
write.csv(Goal3ObjectiveDBaseMonth3Means, "Goal3ObjectiveCBaseMonth3Means.csv", row.names = FALSE) 
## Repeat this process for Baseline and Month6


Goal3ObjectiveCBaseMonth6= data.frame(Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveC$gpraAdultAll.RSKANYSEX_UNP, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveC$gpraAdultAll.RSKSEX_ALCDRG, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR.x, Goal3ObjectiveC$gpraAdultAll.RSKNDL_SHR)
head(Goal3ObjectiveCBaseMonth6) 
## Filter out NA's, then split apart for the summing then bring back together.
Goal3ObjectiveCBaseMonth6  = na.omit(Goal3ObjectiveCBaseMonth6)
Goal3ObjectiveCBaseMonth6  = data.frame(Goal3ObjectiveCBaseMonth6)
head(Goal3ObjectiveCBaseMonth6)

Goal3ObjectiveCBase = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG.x, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR.x)
head(Goal3ObjectiveCBase)
Goal3ObjectiveCBase = data.frame(apply(Goal3ObjectiveCBase, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCBase ) = c("HIVHarmBase")
head(Goal3ObjectiveCBase)
dim(Goal3ObjectiveCBase)

Goal3ObjectiveCMonth6 = data.frame(Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKANYSEX_UNP, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKSEX_ALCDRG, Goal3ObjectiveCBaseMonth6$Goal3ObjectiveC.gpraAdultAll.RSKNDL_SHR)
head(Goal3ObjectiveCMonth6)
Goal3ObjectiveCMonth6 = data.frame(apply(Goal3ObjectiveCMonth6, 1, sum, na.rm = TRUE))
colnames(Goal3ObjectiveCMonth6 ) = c("SAHarmMonth6")
head(Goal3ObjectiveCMonth6)
dim(Goal3ObjectiveCMonth6)

Goal3ObjectiveDBaseMonth6 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth6)
head(Goal3ObjectiveDBaseMonth6 )
wilcox.test(Goal3ObjectiveDBaseMonth6$SAHarmMonth6, Goal3ObjectiveDBaseMonth6$HIVHarmBase, paired = TRUE, alternative = c("greater"))
Goal3ObjectiveCBaseMonth6 = data.frame(Goal3ObjectiveCBase, Goal3ObjectiveCMonth6)
head(Goal3ObjectiveCBaseMonth6)
wilcox.test(Goal3ObjectiveCMonth6$SAHarmMonth6, Goal3ObjectiveCBase$HIVHarmBase, paired = TRUE, alternative = c("greater"))


Goal3ObjectiveCBaseMonth6Means = data.frame(colMeans(Goal3ObjectiveCBaseMonth6))
colnames(Goal3ObjectiveCBaseMonth6Means) = c("Mean")
head(Goal3ObjectiveCBaseMonth6Means)
Goal3ObjectiveDBaseMonth6Means = (Goal3ObjectiveCBaseMonth6Means[2,1]-Goal3ObjectiveCBaseMonth6Means[1,1])/Goal3ObjectiveCBaseMonth6Means[1,1]
write.csv(Goal3ObjectiveDBaseMonth6Means, "Goal3ObjectiveCBaseMonth6Means.csv", row.names = FALSE)

###### Goal 3 Objective E ###################### ##############################################################################################################
Goal3ObjectiveEFP = data.frame(YOB = 2018-gpraAdultAll$YOB.x,gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
Goal3ObjectiveEFP = data.frame(subset(Goal3ObjectiveEFP, gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
head(Goal3ObjectiveEFP)
Goal3ObjectiveEFP = subset(Goal3ObjectiveEFP, YOB < 25)
Goal3ObjectiveEFP = data.frame(Goal3ObjectiveEFP)
Goal3ObjectiveEFP =dim(Goal3ObjectiveEFP)
Goal3ObjectiveEFP = data.frame(Goal3ObjectiveEFP[1]) 
colnames(Goal3ObjectiveEFP) = c("Tested Focus Population") 
write.csv(Goal3ObjectiveEFP, "Goal3ObjectiveEFP.csv", row.names = FALSE)

######## Goal 3 Objective F ############################################################################################
Goal3ObjectiveF = data.frame(Year = gpraAdultAll$YEAR.x,gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
Goal3ObjectiveF  = data.frame(subset(Goal3ObjectiveF , gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
Goal3ObjectiveF  = data.frame(Goal3ObjectiveF)
Goal3ObjectiveF  = subset(Goal3ObjectiveF, YEAR.x = 2017)
Goal3ObjectiveF  = data.frame(Goal3ObjectiveF)
Goal3ObjectiveF  =dim(Goal3ObjectiveF)
Goal3ObjectiveF  = data.frame(Goal3ObjectiveF[1]) 
colnames(Goal3ObjectiveF) = c("Tested 2017 Population") 
write.csv(Goal3ObjectiveF , "Goal3ObjectiveF.csv", row.names = FALSE)

###### Goal 3 Objective G ############################################################################################ #########
# Need to change the to just 2017 so need the year variable
Goal3ObjectiveG = data.frame(YEAR.x = gpraAdultAll$YEAR.x,gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
Goal3ObjectiveG  = data.frame(subset(Goal3ObjectiveG , gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
Goal3ObjectiveG  = data.frame(Goal3ObjectiveG)
head(Goal3ObjectiveG)

Goal3ObjectiveG2016  = data.frame(subset(Goal3ObjectiveG, YEAR.x == 2016))
Goal3ObjectiveG2016  =dim(Goal3ObjectiveG2016)
Goal3ObjectiveG2016  = Goal3ObjectiveG2016[1]

Goal3ObjectiveG2017  = data.frame(subset(Goal3ObjectiveG, YEAR.x == 2017))
Goal3ObjectiveG2017  =dim(Goal3ObjectiveG2017)
Goal3ObjectiveG2017  = Goal3ObjectiveG2017[1]

Goal3ObjectiveGIncrease = round(data.frame(Goal3ObjectiveGIncrease =(Goal3ObjectiveG2017-Goal3ObjectiveG2016)/ Goal3ObjectiveG2016),2)    
Goal3ObjectiveG = data.frame(Goal3ObjectiveG2016  , Goal3ObjectiveG2017, Goal3ObjectiveGIncrease)
colnames(Goal3ObjectiveG) = c("HIV Tested 2016", "HIV Tested 2017", "HIV Tested Increase") 
write.csv(Goal3ObjectiveG, "Goal3ObjectiveG .csv", row.names = FALSE)


###### Goal 3 Objective H ############################################################################################ #########
# CIG30D, TOB30D, VAP30D, ALC30D, BINGE530D, MJ30D, ILL30D, RX30D, SPICE30D, INJECT30D  So sum these to create an overall days used substances in 30 days
Goal3ObjectiveIBaseline3month = data.frame(gpraAdultAll$CIG30D.x,gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x, gpraAdultAll$SPICE30D.x, gpraAdultAll$INJECT30D.x, gpraAdultAll$CIG30D.y,gpraAdultAll$VAP30D.y, gpraAdultAll$ALC30D.y, gpraAdultAll$BINGE530D.y, gpraAdultAll$MJ30D.y, gpraAdultAll$ILL30D.y, gpraAdultAll$RX30D.y, gpraAdultAll$SPICE30D.y, gpraAdultAll$INJECT30D.y)            
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:9])
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,10:18])
head(Goal3ObjectiveI3month)
Goal3ObjectiveI3month = data.frame(apply(Goal3ObjectiveI3month, 1, sum))


colnames(Goal3ObjectiveI3month) = c("Goal3ObjectiveI3month")
head(Goal3ObjectiveI3month)

wilcox.test(Goal3ObjectiveI3month$Goal3ObjectiveI3month, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("less"))

Goal3ObjectiveIBaseMonth3 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveI3month)
Goal3ObjectiveIBaseMonth3 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth3)))
colnames(Goal3ObjectiveIBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveIBaseMonth3$Difference = round((Goal3ObjectiveIBaseMonth3$Month3-Goal3ObjectiveIBaseMonth3$Base)/Goal3ObjectiveIBaseMonth3$Base,2)
write.csv(Goal3ObjectiveIBaseMonth3, "Goal3ObjectiveHBaseMonth3.csv", row.names = FALSE)

#### Goal 3 Objective H Base to Month 6 ########## ######################################################################
# Need to change y to not .y.  Change Month3 to Month6
Goal3ObjectiveIBaseline3month = data.frame(gpraAdultAll$CIG30D.x,gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x, gpraAdultAll$SPICE30D.x, gpraAdultAll$INJECT30D.x, gpraAdultAll$CIG30D,gpraAdultAll$VAP30D, gpraAdultAll$ALC30D, gpraAdultAll$BINGE530D, gpraAdultAll$MJ30D, gpraAdultAll$ILL30D, gpraAdultAll$RX30D, gpraAdultAll$SPICE30D, gpraAdultAll$INJECT30D)            
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:9])
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,10:18])
head(Goal3ObjectiveI3month)
Goal3ObjectiveI3month = data.frame(apply(Goal3ObjectiveI3month, 1, sum))


colnames(Goal3ObjectiveI3month) = c("Goal3ObjectiveI3month")
head(Goal3ObjectiveI3month)

wilcox.test(Goal3ObjectiveI3month$Goal3ObjectiveI3month, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("less"))

Goal3ObjectiveIBaseMonth6 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveI3month)
Goal3ObjectiveIBaseMonth6 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth6)))
colnames(Goal3ObjectiveIBaseMonth6) = c("Base", "Month6")
Goal3ObjectiveIBaseMonth6$Difference = round((Goal3ObjectiveIBaseMonth6$Month6-Goal3ObjectiveIBaseMonth6$Base)/Goal3ObjectiveIBaseMonth6$Base,2)
write.csv(Goal3ObjectiveIBaseMonth6, "Goal3ObjectiveHBaseMonth6.csv", row.names = FALSE)


####################### Objective I ################ ################ ################ ################ ################ ################ ##############
CNTRL_REFUSEMOOD CNTRL_WAITCNDM CNTRL_TREAT CNTRL_SEXPRAC CNTRL_ASKCNDM CNTRL_REFUSECNDM
Goal3ObjectiveIBaseline3month= data.frame(gpraAdultAll$CNTRL_REFUSEMOOD.x, gpraAdultAll$CNTRL_WAITCNDM.x, gpraAdultAll$CNTRL_TREAT.x, gpraAdultAll$CNTRL_TREAT.x, gpraAdultAll$CNTRL_SEXPRAC.x, gpraAdultAll$CNTRL_ASKCNDM.x, gpraAdultAll$CNTRL_REFUSECNDM.x, gpraAdultAll$CNTRL_REFUSEMOOD.y, gpraAdultAll$CNTRL_WAITCNDM.y, gpraAdultAll$CNTRL_TREAT.y, gpraAdultAll$CNTRL_TREAT.y, gpraAdultAll$CNTRL_SEXPRAC.y, gpraAdultAll$CNTRL_ASKCNDM.y, gpraAdultAll$CNTRL_REFUSECNDM.y)
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
summary(Goal3ObjectiveIBaseline3month)
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:7])
head(Goal3ObjectiveIBase)
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,8:14])
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
Goal3ObjectiveIBaseline3month = data.frame(gpraAdultAll$CIG30D.x,gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x, gpraAdultAll$SPICE30D.x, gpraAdultAll$INJECT30D.x, gpraAdultAll$CIG30D,gpraAdultAll$VAP30D, gpraAdultAll$ALC30D, gpraAdultAll$BINGE530D, gpraAdultAll$MJ30D, gpraAdultAll$ILL30D, gpraAdultAll$RX30D, gpraAdultAll$SPICE30D, gpraAdultAll$INJECT30D)            
Goal3ObjectiveIBaseline3month = data.frame(apply(Goal3ObjectiveIBaseline3month, 2, function(x){ifelse(x == 98, NA, ifelse(x == 97, NA, x))}))
Goal3ObjectiveIBaseline3month = data.frame(na.omit(Goal3ObjectiveIBaseline3month))
## Number of people here
dim(Goal3ObjectiveIBaseline3month) 
Goal3ObjectiveIBase =(Goal3ObjectiveIBaseline3month[,1:9])
Goal3ObjectiveIBase = data.frame(apply(Goal3ObjectiveIBase, 1, sum))
colnames(Goal3ObjectiveIBase) = c("Goal3ObjectiveIBase")
head(Goal3ObjectiveIBase)

Goal3ObjectiveI3month =(Goal3ObjectiveIBaseline3month[,10:18])
head(Goal3ObjectiveI3month)
Goal3ObjectiveI3month = data.frame(apply(Goal3ObjectiveI3month, 1, sum))


colnames(Goal3ObjectiveI3month) = c("Goal3ObjectiveI3month")
head(Goal3ObjectiveI3month)

wilcox.test(Goal3ObjectiveI3month$Goal3ObjectiveI3month, Goal3ObjectiveIBase$Goal3ObjectiveIBase, paired = TRUE, alternative = c("greater"))

Goal3ObjectiveIBaseMonth6 = data.frame(Goal3ObjectiveIBase, Goal3ObjectiveI3month)
Goal3ObjectiveIBaseMonth6 =  data.frame(t(colMeans(Goal3ObjectiveIBaseMonth6)))
colnames(Goal3ObjectiveIBaseMonth6) = c("Base", "Month6")
Goal3ObjectiveIBaseMonth6$Difference = round((Goal3ObjectiveIBaseMonth6$Month6-Goal3ObjectiveIBaseMonth6$Base)/Goal3ObjectiveIBaseMonth6$Base,2)
write.csv(Goal3ObjectiveIBaseMonth6, "Goal3ObjectiveHBaseMonth6.csv", row.names = FALSE)

#######Goal 3 Objective J ########################################### ########################################### ###########################################


####### Goal 3 Objective K ########################################### ########################################### ###########################################

####### Goal 3 Objective L ########################################### ########################################### ###########################################
# Using the pocket screener.  UnproSex
Goal3ObjectiveLBaseMonth3 = data.frame(pocketScreenerAll$UnproSex.x, pocketScreenerAll$UnproSex.y)
Goal3ObjectiveLBaseMonth3 = data.frame(na.omit(Goal3ObjectiveLBaseMonth3))
Goal3ObjectiveLBaseMonth3 = data.frame(apply(Goal3ObjectiveLBaseMonth3, 2, function(x){ifelse(x == "Yes", 1, 0)}))
dim(Goal3ObjectiveLBaseMonth3)
Goal3ObjectiveLBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveLBaseMonth3)))
colnames(Goal3ObjectiveLBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveLBaseMonth3$Difference = (Goal3ObjectiveLBaseMonth3$Month3-Goal3ObjectiveLBaseMonth3$Base)/Goal3ObjectiveLBaseMonth3$Base
Goal3ObjectiveLBaseMonth3 = round(Goal3ObjectiveLBaseMonth3,2)
write.csv(Goal3ObjectiveLBaseMonth3, "Goal3ObjectiveLBaseMonth3.csv", row.names = FALSE)

### Objective L Base to 6 months ########### #################################
Goal3ObjectiveLBaseMonth6 = data.frame(pocketScreenerAll$UnproSex.x, pocketScreenerAll$UnproSex)
Goal3ObjectiveLBaseMonth6 = data.frame(na.omit(Goal3ObjectiveLBaseMonth6))
Goal3ObjectiveLBaseMonth6 = data.frame(apply(Goal3ObjectiveLBaseMonth6, 2, function(x){ifelse(x == "Yes", 1, 0)}))
dim(Goal3ObjectiveLBaseMonth6)
Goal3ObjectiveLBaseMonth6 = data.frame(t(colMeans(Goal3ObjectiveLBaseMonth6)))
colnames(Goal3ObjectiveLBaseMonth6) = c("Base", "Month6")
Goal3ObjectiveLBaseMonth6$Difference = (Goal3ObjectiveLBaseMonth6$Month6-Goal3ObjectiveLBaseMonth6$Base)/Goal3ObjectiveLBaseMonth6$Base
Goal3ObjectiveLBaseMonth6 = round(Goal3ObjectiveLBaseMonth6,2)
write.csv(Goal3ObjectiveLBaseMonth6, "Goal3ObjectiveLBaseMonth6.csv", row.names = FALSE)

##### Objective M ########################## ###############################################################################################################################
## STDHx and subset by year if any data and then compare by year.  No year so need to merge the GPRA with pocket then grab year from GPRA with the one variable here
GPRAPocket = merge(pocketScreenerAll,gpraAdultAll, by = "PARTID", all = TRUE)
GPRAPocket = data.frame(GPRAPocket$YEAR.x, GPRAPocket$STDHx)
count(GPRAPocket)
# No data for 2016 so cannot make comparison yet.
Goal3ObjectiveM = data.frame(pocketScreenerAll$STDHx.x)


