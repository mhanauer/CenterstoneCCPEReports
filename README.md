###### Goal 1 ############################################################
###### Objective E Number of people served by each intervention Overall
# First grab total number of people served use the participant ID include the youth then for each intervention.  Grab the length for both adult and youth from all the data which will give you all the people who have data entered.  
# You don't need to specifcy the baseline data only, because there is no way that someone could not have baseline.  Therefore, the length of the total data set will equal the length of the baseline
###### Goal 1 ############################################################
###### Objective E and F Number of people served by each intervention Overall
# First grab total number of people served use the participant ID include the youth then for each intervention.  Grab the length for both adult and youth from all the data which will give you all the people who have data entered.  

totalAdults = (gpraAdultAll$PARTID)
totalAdults = data.frame(na.omit(totalAdults))
totalYouth = (gpraYouthAll$PARTID)
totalYouth = data.frame(na.omit(totalYouth))

totalAdults = dim(totalAdults)[1]
totalYouth = dim(totalYouth)[1]

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

139/310

##### Goal 1 Objective G #############  ############# ############# ############# ############# #############
### 2016 Testing 


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


########## Goal 3 Objective B | Increase knowledge about HIV and VH by 20%.  KNOW_HIV #########################################################

Goal3ObjectiveB =  data.frame(gpraAdultAll$KNOW_HIV.x, gpraAdultAll$KNOW_HIV.y, gpraAdultAll$KNOW_HIV)

Goal3ObjectiveB = data.frame(apply(Goal3ObjectiveB, 2, function(x){ifelse(x == 98, NA, ifelse(x == 2, NA, x))}))
summary(Goal3ObjectiveB)

Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.x, Goal3ObjectiveB$gpraAdultAll.KNOW_HIV.y)
Goal3ObjectiveBBaseMonth3 = na.omit(Goal3ObjectiveBBaseMonth3)
Goal3ObjectiveBBaseMonth3 = data.frame(Goal3ObjectiveBBaseMonth3)
## Here is the number of people that completed both
dim(Goal3ObjectiveBBaseMonth3)
head(Goal3ObjectiveBBaseMonth3)

wilcox.test(Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.y, Goal3ObjectiveBBaseMonth3$Goal3ObjectiveB.gpraAdultAll.KNOW_HIV.x, paired = TRUE, alternative  =c("greater"))

Goal3ObjectiveBBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveBBaseMonth3)))
colnames(Goal3ObjectiveBBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveBBaseMonth3$Difference = (Goal3ObjectiveBBaseMonth3$Month3-Goal3ObjectiveBBaseMonth3$Base)/Goal3ObjectiveBBaseMonth3$Base
Goal3ObjectiveBBaseMonth3 = round(Goal3ObjectiveBBaseMonth3, 2)
write.csv(Goal3ObjectiveBBaseMonth3, "Goal3ObjectiveBBaseMonth3.csv", row.names = FALSE)



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
write.csv(Goal3ObjectiveCBaseMonth3, "Goal3ObjectiveCBaseMonth3.csv", row.names = FALSE)


####### Goal 3 Objective D ######### ###################################################################################################
# Grab these variables and change RSKANYSEX_UNP, RSKSEX_ALCDRG, RSKNDL_SHR change ObjectiveC to ObjectiveD, change SAHarm to HIVHarm 
Goal3ObjectiveD= data.frame(gpraAdultAll$RSKANYSEX_UNP.x, gpraAdultAll$RSKANYSEX_UNP.y, gpraAdultAll$RSKANYSEX_UNP, gpraAdultAll$RSKSEX_ALCDRG.x, gpraAdultAll$RSKSEX_ALCDRG.y, gpraAdultAll$RSKSEX_ALCDRG, gpraAdultAll$RSKNDL_SHR.x, gpraAdultAll$RSKNDL_SHR.y, gpraAdultAll$RSKNDL_SHR)
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
write.csv(Goal3ObjectiveDBaseMonth3, "Goal3ObjectiveDBaseMonth3.csv", row.names = FALSE)



###### Goal 3 Objective E ###################### ##############################################################################################################
Goal3ObjectiveEFP = data.frame(Year = gpraAdultAll$YEAR.x, YOB = 2018-gpraAdultAll$YOB.x,gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
Goal3ObjectiveEFP = data.frame(subset(Goal3ObjectiveEFP, gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
head(Goal3ObjectiveEFP)
### 2016 First then 2017 and get percentage change.
Goal3ObjectiveEFP2016 = subset(Goal3ObjectiveEFP, YOB < 25 & Year == 2016)
Goal3ObjectiveEFP2016= data.frame(Goal3ObjectiveEFP2016 )
Goal3ObjectiveEFP2016=dim(Goal3ObjectiveEFP2016 )
Goal3ObjectiveEFP2016= data.frame(Goal3ObjectiveEFP2016[1]) 
colnames(Goal3ObjectiveEFP2016) = c("TestedFocusPopulation2016") 

### 2017
Goal3ObjectiveEFP2017= subset(Goal3ObjectiveEFP, YOB < 25 & Year == 2017)
Goal3ObjectiveEFP2017= data.frame(Goal3ObjectiveEFP2017)
Goal3ObjectiveEFP2017=dim(Goal3ObjectiveEFP2017)
Goal3ObjectiveEFP2017= data.frame(Goal3ObjectiveEFP2017 [1]) 
colnames(Goal3ObjectiveEFP2017) = c("TestedFocusPopulation2017")

Goal3ObjectiveE = data.frame(Goal3ObjectiveEFP2017, Goal3ObjectiveEFP2016)
head(Goal3ObjectiveE)
Goal3ObjectiveE$Difference = (Goal3ObjectiveE$TestedFocusPopulation2017- Goal3ObjectiveE$TestedFocusPopulation2016)/ Goal3ObjectiveE$TestedFocusPopulation2016
Goal3ObjectiveE = round(Goal3ObjectiveE,2)

write.csv(Goal3ObjectiveE, "Goal3ObjectiveE.csv", row.names = FALSE)

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

Goal3ObjectiveH3month =(Goal3ObjectiveHBaseline3month[,10:18])
head(Goal3ObjectiveH3month)
Goal3ObjectiveH3month = data.frame(apply(Goal3ObjectiveH3month, 1, sum))
colnames(Goal3ObjectiveH3month) = c("Goal3ObjectiveH3month")
head(Goal3ObjectiveH3month)

wilcox.test(Goal3ObjectiveH3month$Goal3ObjectiveH3month, Goal3ObjectiveHBase$Goal3ObjectiveHBase, paired = TRUE, alternative = c("less"))

Goal3ObjectiveHBaseMonth3 = data.frame(Goal3ObjectiveHBase, Goal3ObjectiveH3month)
Goal3ObjectiveHBaseMonth3 =  data.frame(t(colMeans(Goal3ObjectiveHBaseMonth3)))
colnames(Goal3ObjectiveHBaseMonth3) = c("Base", "Month3")
Goal3ObjectiveHBaseMonth3$Difference = round((Goal3ObjectiveHBaseMonth3$Month3-Goal3ObjectiveHBaseMonth3$Base)/Goal3ObjectiveHBaseMonth3$Base,2)
write.csv(Goal3ObjectiveHBaseMonth3, "Goal3ObjectiveHBaseMonth3.csv", row.names = FALSE)


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
write.csv(Goal3ObjectiveKBaseMonth3, "Goal3ObjectiveKBaseMonth3.csv", row.names = FALSE)

##### Objective L ########################## ###############################################################################################################################
## STDHx and subset by year if any data and then compare by year.  No year so need to merge the GPRA with pocket then grab year from GPRA with the one variable here
# LASTSEX_UNP Getting the number of people with unprotected and want to reduce that so 1 =0 and 2 = 1
Goal3ObjectiveMBaseMonth3 = data.frame(gpraAdultAll$LASTSEX_UNP.x, gpraAdultAll$LASTSEX_UNP.y)
Goal3ObjectiveMBaseMonth3 = data.frame(apply(Goal3ObjectiveMBaseMonth3, 2, function(x){ifelse(x == 98, NA, ifelse(x == 99, NA, x))}))
Goal3ObjectiveMBaseMonth3 = data.frame(na.omit(Goal3ObjectiveMBaseMonth3))
Goal3ObjectiveMBaseMonth3 = data.frame(apply(Goal3ObjectiveMBaseMonth3, 2, function(x){ifelse(x == 1, 0, 1)}))
dim(Goal3ObjectiveMBaseMonth3)
summary(Goal3ObjectiveMBaseMonth3)
colnames(Goal3ObjectiveMBaseMonth3) = c("Base", "Month3")
wilcox.test(Goal3ObjectiveMBaseMonth3$Month3, Goal3ObjectiveMBaseMonth3$Base, paired = TRUE, alternative = c("less"))
Goal3ObjectiveMBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveMBaseMonth3)))
Goal3ObjectiveMBaseMonth3$Difference = (Goal3ObjectiveMBaseMonth3$Month3-Goal3ObjectiveMBaseMonth3$Base) / Goal3ObjectiveMBaseMonth3$Base
Goal3ObjectiveMBaseMonth3 = round(Goal3ObjectiveMBaseMonth3, 2)
write.csv(Goal3ObjectiveMBaseMonth3, "Goal3ObjectiveMBaseMonth3.csv", row.names = FALSE)


####### Goal 3 Objective M ########################################### ########################################### ###########################################
# Using the pocket screener.  
Goal3ObjectiveLBaseMonth3 = data.frame(pocketScreenerAll$STDHx.x, pocketScreenerAll$STDHx.y)
Goal3ObjectiveLBaseMonth3 = data.frame(na.omit(Goal3ObjectiveLBaseMonth3))
Goal3ObjectiveLBaseMonth3 = data.frame(apply(Goal3ObjectiveLBaseMonth3, 2, function(x){ifelse(x == "Yes", 1, 0)}))
dim(Goal3ObjectiveLBaseMonth3)
Goal3ObjectiveLBaseMonth3 = data.frame(t(colMeans(Goal3ObjectiveLBaseMonth3)))
colnames(Goal3ObjectiveLBaseMonth3) = c("Base", "Month3")
wilcox.test(Goal3ObjectiveLBaseMonth3$Month3, Goal3ObjectiveLBaseMonth3$Base, paired = TRUE, alternative = c("less"))
Goal3ObjectiveLBaseMonth3$Difference = (Goal3ObjectiveLBaseMonth3$Month3-Goal3ObjectiveLBaseMonth3$Base)/Goal3ObjectiveLBaseMonth3$Base
Goal3ObjectiveLBaseMonth3 = round(Goal3ObjectiveLBaseMonth3,2)
write.csv(Goal3ObjectiveLBaseMonth3, "Goal3ObjectiveLBaseMonth3.csv", row.names = FALSE)



##### CCPE Grant Summary Report Data ######## ########################################################################################
##### Project Objectives ################### ###############################################################################################
InternAdult = data.frame(gpraAdultAll$YEAR.x, gpraAdultAll$INTERVENTION_A.x, gpraAdultAll$INTERVENTION_B.x, gpraAdultAll$INTERVENTION_C.x)
RapidAdult = data.frame(subset(InternAdult, gpraAdultAll.INTERVENTION_A.x ==78 | gpraAdultAll.INTERVENTION_B.x == 78 | gpraAdultAll.INTERVENTION_C.x == 78))
summary(RapidAdult)
RapidAdult = data.frame(subset(RapidAdult, gpraAdultAll.YEAR.x == 2017))
RapidAdult = dim(RapidAdult)
RapidAdult = RapidAdult[1]
RapidAdult 
# For Increase confidence in assertive communication skills see Goal 3 Objective I SIS 
# For Increase knowledge about HIV and substance abuse and decrease risky behaviors that could lead to HIV/AIDS and substance abuse.  																			
# use ####### Goal 3 Objective A and B #####
# For Increase protected sex and improve attitudes towards condom use  ##### Objective M ##### 																			

##### Grant Summary Report Demographics ###################################### ##########################################
# Current enrollment totalCCPE to tracking sheet
# Gender Male = 1; Female = 2; Other Gender Identity 3 and 4 
library(plyr)
gender = data.frame(gpraAdultAll$GENDER.x)
gender = count(gender)
n = sum(gender$freq)
gender$Percentage = gender$freq / n
gender = round(gender, 2); gender 															

# Ethnicity # Need to combine hispanic E_MEXICAN, E_PUERTRICAN, E_CUBAN, E_OTHERHISPAN
# Need to combine all Asian R_ASIAIN_N, R_CHINESE_N, R_JAPAN_N, R_KOR_N, R_VIETNAM_N, R_OTHERASIA_N
# Then grab black and white and get rid of NA's so the total correct
# Need to include this E_NONHISPAN and change 0 to 1 and 1 to 0
hispanic = data.frame(gpraAdultAll$E_MEXICAN, gpraAdultAll$E_PUERTRICAN, gpraAdultAll$E_CUBAN, gpraAdultAll$E_OTHERHISPAN)
hispanic = data.frame(apply(hispanic, 2, function(x){ifelse(x == 98, NA, x)}))
hispanic = data.frame(apply(hispanic, 1, sum))
hispanic = data.frame(apply(hispanic, 1, function(x){ifelse(x > 0,1, 0)}))
hispanic  = data.frame(count(hispanic))
colnames(hispanic) = c("Hisp", "Count")
hispanic$Percentage = hispanic$Count /n; hispanic

###### Now Asian ###### ###### ###### ###### ###### 
asian = data.frame(gpraAdultAll$R_ASIAIN_N.x, gpraAdultAll$R_CHINESE_N.x, gpraAdultAll$R_JAPAN_N.x, gpraAdultAll$R_KOR_N.x, gpraAdultAll$R_VIETNAM_N.x, gpraAdultAll$R_OTHERASIA_N.x)
asian = data.frame(apply(asian, 2, function(x){ifelse(x == 98, NA, x)}))
asian = data.frame(apply(asian, 1, sum))
asian = data.frame(apply(asian, 1, function(x){ifelse(x > 0,1, 0)}))
asian  = data.frame(count(asian))
colnames(asian) = c("Asian", "Count")
asian$Percentage = asian$Count /n; asian

#### R_BLACK_N ######## ################################################################
black = data.frame(gpraAdultAll$R_BLACK_N.x)
black = data.frame(apply(black, 2, function(x){ifelse(x == 98, NA, x)}))
black  = data.frame(count(black))
colnames(black) = c("Black", "Count")
black$Percentage = black$Count /n; black


####### White ################# ################# ####################################################################
white = data.frame(gpraAdultAll$R_WHITE_N.x)
white = data.frame(apply(white, 2, function(x){ifelse(x == 98, NA, x)}))
white  = data.frame(count(white))
colnames(white) = c("Black", "Count")
white$Percentage = white$Count /n
white

######################## Education Level ################################################################################
edu = data.frame(gpraAdultAll$EDLEVEL_N.x)
edu = na.omit(edu)
eduN = dim(edu)[1]
edu = count(edu)
rownames(edu) = c("High School", "Community College", "Four-year college", "Beyond four-year college", "NA")

edu$Percentage = edu$freq / eduN
edu = round(edu, 2); edu

########### Age ################################## ########################################################################
age = data.frame(gpraAdultAll$YOB.x)
age = data.frame(apply(age, 2, function(x){ifelse(x == 98, NA, x)}))
age = data.frame(age = 2018-age)
age = data.frame(na.omit(age))
write.csv(age, "age.csv", row.names = FALSE)
age = read.csv("age.csv", header = TRUE)
mean(age$gpraAdultAll.YOB.x)

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
gpraAdultBase$Date
as.Date(gpraAdultBase$Date) 
gpraYouthBase$Date = as.Date(paste(gpraYouthBase$YEAR, gpraYouthBase$MONTH, gpraYouthBase$DAY, sep = "/"))
as.Date(gpraYouthBase$Date) 

# Total People

QPR1Youth = data.frame(na.omit(gpraYouthBase$Date))
colnames(QPR1Youth) = c("Date")
QPR1Youth$Date
QPR1Adult = data.frame(na.omit(gpraAdultBase$Date))
colnames(QPR1Adult) = c("Date")

QPR1 = rbind(QPR1Adult, QPR1Youth)
QPR1$Date
write.csv(QPR1, "QPR1.csv")
QPR1=QPR1[QPR1$Date >= "2017-10-01" & QPR1$Date <= "2017-12-31",]
length(QPR1)

#Adolescents (Age 12-17)
QPR2 = data.frame(gpraYouthBase$Date)
QPR2 = data.frame(na.omit(QPR2))
colnames(QPR2) = c("Date")
QPR2=QPR2[QPR2$Date >= "2017-10-01" & QPR2$Date <= "2017-12-31",]
length(QPR2)

##### Young Adults (Age 18-24) in college ##### ##### ##### ##### ##### ##### ##### 
# 1 and 2 are yesses and 0 is no.  
# Can get rid of na's, because I need both variables to identitfy them
# Also need year of birth to subset
QPR3 = data.frame(AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$Date, gpraAdultBase$COLLEGE)
QPR3 = data.frame(na.omit(QPR3))
colnames(QPR3) = c("AGE", "Date", "College")
count(QPR3$College)
# subset date first then do the subset for college then do age
# Date for QPR
QPR3=QPR3[QPR3$Date >= "2017-10-01" & QPR3$Date <= "2017-12-31",]
QPR3 = subset(QPR3, AGE < 25)
QPR3 = subset(QPR3, College == 1 | College == 2)
dim(QPR3)[1]

######### Young Adults (Age 18-24) not in college ######### ######### ######### ######### 
QPR4 = data.frame(AGE = 2018-gpraAdultBase$YOB, gpraAdultBase$Date, gpraAdultBase$COLLEGE)
QPR4 = data.frame(na.omit(QPR4))
colnames(QPR4) = c("AGE", "Date", "College")
count(QPR4$College)
# subset date first then do the subset for college then do age
# Date for QPR
QPR4=QPR4[QPR4$Date >= "2017-10-01" & QPR4$Date <= "2017-12-31",]
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
QPR5=QPR5[QPR5$Date >= "2017-10-01" & QPR5$Date <= "2017-12-31",]
QPR5 = subset(QPR5, AGE >= 50)
dim(QPR5)[1]

#########  American Indian/Alaska Natives ######### ######### ######### ######### 
QPR6 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_AMERINALSK_N)
QPR6 = data.frame(na.omit(QPR6))
colnames(QPR6) = c("Date", "R_AMERINALSK_N")
# subset date first then do the subset for college then do age
# Date for QPR
QPR6=QPR6[QPR6$Date >= "2017-10-01" & QPR6$Date <= "2017-12-31",]
QPR6 = subset(QPR6, R_AMERINALSK_N == 1)
dim(QPR6)[1]

#########  Asian American/Pacific Islanders ######### ######### ######### ######### 
# R_ASIAIN_N, R_CHINESE_N, R_FILIP_N, R_JAPAN_N, R_KOR_N, R_VIETNAM_N, R_OTHERASIA_N, R_HAW_N, R_GUAM_N, R_SAMO_N, R_OTHERPI_N

QPR7 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_ASIAIN_N, gpraAdultBase$R_CHINESE_N, gpraAdultBase$R_FILIP_N, gpraAdultBase$R_JAPAN_N, gpraAdultBase$R_KOR_N, gpraAdultBase$R_VIETNAM_N, gpraAdultBase$R_OTHERASIA_N, gpraAdultBase$R_HAW_N, gpraAdultBase$R_GUAM_N, gpraAdultBase$R_OTHERPI_N)
QPR7 = data.frame(na.omit(QPR7))
colnames(QPR7) = c("Date", "R_ASIAIN_N", "R_CHINESE_N", "R_FILIP_N", "R_JAPAN_N", "R_KOR_N", "R_VIETNAM_N", "R_OTHERASIA_N", "R_HAW_N", "R_GUAM_N", "R_OTHERPI_N")
# subset date first then do the subset for college then do age
# Date for QPR
QPR7=QPR7[QPR7$Date >= "2017-10-01" & QPR7$Date <= "2017-12-31",]
QPR7 = subset(QPR7, R_ASIAIN_N == 1 | R_CHINESE_N == 1 | R_FILIP_N ==1 | R_JAPAN_N == 1 | R_KOR_N== 1 | R_VIETNAM_N== 1 | R_OTHERASIA_N==1 | R_HAW_N==1 | R_GUAM_N==1 | R_OTHERPI_N==1)
dim(QPR7)[1]


#########   Black/African American Women #########  #########  
QPR8 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_BLACK_N, gpraAdultBase$GENDER)
QPR8 = data.frame(na.omit(QPR8))
colnames(QPR8) = c("Date", "R_BLACK_N", "GENDER")
# subset date first then do the subset for college then do age
# Date for QPR
QPR8=QPR8[QPR8$Date >= "2017-10-01" & QPR8$Date <= "2017-12-31",]
QPR8 = subset(QPR8, R_BLACK_N == 1 & GENDER == 0)
dim(QPR8)[1]

######### Black/African American Men #########   #########  #########   #########
QPR9 = data.frame(gpraAdultBase$Date, gpraAdultBase$R_BLACK_N, gpraAdultBase$GENDER)
QPR9 = data.frame(na.omit(QPR9))
colnames(QPR9) = c("Date", "R_BLACK_N", "GENDER")
# subset date first then do the subset for college then do age
# Date for QPR
QPR9=QPR9[QPR9$Date >= "2017-10-01" & QPR9$Date <= "2017-12-31",]
QPR9 = subset(QPR9, R_BLACK_N == 1 & GENDER == 1)
dim(QPR9)[1]

######### Latina or Hispanic Women: #########   #########  #########   #########
QPR10 = data.frame(gpraAdultBase$Date, gpraAdultBase$E_NONHISPAN, gpraAdultBase$GENDER)
QPR10 = data.frame(na.omit(QPR10))
colnames(QPR10) = c("Date", "E_NONHISPAN", "GENDER")
# subset date first then do the subset for college then do age
# Date for QPR
QPR10=QPR10[QPR10$Date >= "2017-10-01" & QPR10$Date <= "2017-12-31",]
QPR10 = subset(QPR10, E_NONHISPAN == 0 & GENDER == 0)
dim(QPR10)[1]

######### Latino or Hispanic Men: #########   #########  #########   #########
QPR11 = data.frame(gpraAdultBase$Date, gpraAdultBase$E_NONHISPAN, gpraAdultBase$GENDER)
QPR11 = data.frame(na.omit(QPR11))
colnames(QPR11) = c("Date", "E_NONHISPAN", "GENDER")
QPR11=QPR11[QPR11$Date >= "2017-10-01" & QPR11$Date <= "2017-12-31",]
QPR11 = subset(QPR11, E_NONHISPAN == 0 & GENDER == 1)
dim(QPR11)[1]

#########  Men Having Sex with Men (MSM): #########   #########  #########   #########
QPR12 = data.frame(gpraAdultBase$Date, gpraAdultBase$SEX_MALEEVER, gpraAdultBase$GENDER)
QPR12 = data.frame(na.omit(QPR12))
colnames(QPR12) = c("Date", "SEX_MALEEVER", "GENDER")
QPR12=QPR12[QPR12$Date >= "2017-10-01" & QPR12$Date <= "2017-12-31",]
QPR12 = subset(QPR12, SEX_MALEEVER == 1 & GENDER == 1)
dim(QPR12)[1]

## LGBTQ #########  #########  #########  #########  #########  
QPR13 = data.frame(gpraAdultBase$Date, gpraAdultBase$SEX_PR)
QPR13 = data.frame(na.omit(QPR13))
colnames(QPR13) = c("Date", "SEX_PR")
QPR13=QPR13[QPR13$Date >= "2017-10-01" & QPR13$Date <= "2017-12-31",]
QPR13 = subset(QPR13, SEX_PR == 3 | SEX_PR == 4)
dim(QPR13)[1]

##  Military/Veterans  #########  #########  #########  #########  #########  
# MILSERVENO
QPR14 = data.frame(gpraAdultBase$Date, gpraAdultBase$MILSERVENO)
QPR14 = data.frame(na.omit(QPR14))
colnames(QPR14) = c("Date", "MILSERVENO")
QPR14=QPR14[QPR14$Date >= "2017-10-01" & QPR14$Date <= "2017-12-31",]
QPR14 = subset(QPR14, MILSERVENO == 1)
dim(QPR14)[1]

##  Reentry Populations  #########  #########  #########  #########  #########  
## No data on this

# HOMETYPE_N #########  #########  #########  #########  ######### 
QPR16 = data.frame(gpraAdultBase$Date, gpraAdultBase$HOMETYPE_N)
QPR16 = data.frame(na.omit(QPR16))
colnames(QPR16) = c("Date", "HOMETYPE_N")
QPR16=QPR16[QPR16$Date >= "2017-10-01" & QPR16$Date <= "2017-12-31",]
QPR16 = subset(QPR16, HOMETYPE_N == 6)
dim(QPR16)[1]


## Sex Workers ##############  ############## ############## ##############
# No data on this topic


## Low Income ##############  ############## ############## ############## 
#HINCOMEO_N

QPR18 = data.frame(gpraAdultBase$Date, gpraAdultBase$HINCOMEO_N)
QPR18 = data.frame(na.omit(QPR18))
colnames(QPR18) = c("Date", "HINCOMEO_N")
QPR18=QPR18[QPR18$Date >= "2017-10-01" & QPR18$Date <= "2017-12-31",]
QPR18 = subset(QPR18, HINCOMEO_N == 1)
dim(QPR18)[1]

######## HIV Testing ##############  ############## ############## ##############

#### Total Number of people ##################################
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
dim(QPR19)[1]


### In this reporting period, how many people received an HIV test for the first time using CSAP/MAI funds?

# Need the intersection between those have been and those who have never been tested before
# HIV_RESULTS_N
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$HIV_RESULTS_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "HIV_RESULTS_N")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
dim(QPR19)[1]

# Received an HIV test using CSAP/MAI grant funds during current reporting period
### GENDER
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$GENDER)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "GENDER")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
count(QPR19$GENDER)

# Received an HIV test for the first time using CSAP/MAI grant funds during current reporting period
### GENDER
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$HIV_RESULTS_N, gpraAdultBase$GENDER)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "HIV_RESULTS_N", "GENDER")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
count(QPR19$GENDER)

## Received an HIV test using CSAP/MAI grant funds during current reporting period
## Ethnicity

QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$E_NONHISPAN)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "E_NONHISPAN")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
count(QPR19$E_NONHISPAN)

## Received an HIV test for the first time using CSAP/MAI grant funds during current reporting period
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$HIV_RESULTS_N, gpraAdultBase$E_NONHISPAN)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "E_NONHISPAN")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
QPR19 = subset(QPR19, HIV_RESULTS_N == 0)
count(QPR19$E_NONHISPAN)

## Received an HIV test using CSAP/MAI grant funds during current reporting period
## Black
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$R_BLACK_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "R_BLACK_N")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
count(QPR19$R_BLACK_N)

## American Indian or Alaska Native
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$R_AMERINALSK_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "R_BLACK_N")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
count(QPR19$R_AMERINALSK_N)

## Asian
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$R_ASIAIN_N, gpraAdultBase$R_CHINESE_N, gpraAdultBase$R_FILIP_N, gpraAdultBase$R_JAPAN_N, gpraAdultBase$R_KOR_N, gpraAdultBase$R_VIETNAM_N, gpraAdultBase$R_OTHERASIA_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "R_ASIAIN_N", "R_CHINESE_N", "R_FILIP_N", "R_JAPAN_N", "R_KOR_N", "R_VIETNAM_N", "R_OTHERASIA_N")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
# Can do x == 1, because 1's will only be in the Asian category
QPR19 = data.frame(apply(QPR19, 2, function(x){ifelse(x == 1, 1, 0)}))
QPR19 = data.frame(apply(QPR19, 1, sum))
colnames(QPR19) = c("Asian")
count(QPR19$Asian)

## Native Hawaiian or Other Pacific Islander
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$R_HAW_N, gpraAdultBase$R_GUAM_N, gpraAdultBase$R_OTHERPI_N)
QPR19 = data.frame(na.omit(QPR19))
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
# Can do x == 1, because 1's will only be in the Asian category
QPR19 = data.frame(apply(QPR19, 2, function(x){ifelse(x == 1, 1, 0)}))
QPR19 = data.frame(apply(QPR19, 1, sum))
colnames(QPR19) = c("Asian")
count(QPR19$Asian)

## White

QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase$R_WHITE_N)
QPR19 = data.frame(na.omit(QPR19))
colnames(QPR19) = c("Date", "INTERVENTION_B", "R_WHITE_N")
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
count(QPR19$R_WHITE_N)

# Multi just grab columns after deleting missing then sum.  Then if greater than 1 then are 1 if not 0.
QPR19 = data.frame(gpraAdultBase$Date, gpraAdultBase$INTERVENTION_B, gpraAdultBase[,17:34])
QPR19 = data.frame(na.omit(QPR19))
QPR19=QPR19[QPR19$Date >= "2017-10-01" & QPR19$Date <= "2017-12-31",]
QPR19 = subset(QPR19, INTERVENTION_B == 78)
count(QPR19$R_WHITE_N)



