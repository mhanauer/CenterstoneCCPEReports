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
SISYouth = data.frame(subset(InternYouth, gpraYouthAll.INTERVENTION_A.x ==2 | gpraYouthAll.INTERVENTION_B.x == 2 | gpraYouthAll.INTERVENTION_C.x == 2))
SISYouth = dim(SISYouth)
SISYouth = SISYouth[1]

totalSIS = sum(SISAdult, SISYouth); totalSIS

# Now CIR
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

# #### Objective E for the quarter ####################################################################
gpraAdultAll$MONTH.x = as.numeric(gpraAdultAll$MONTH.x)
gpraAdultQuarter = subset(gpraAdultAll, MONTH.x > 10 | gpraAdultAll$YEAR.x >= 2017)
gpraYouthAll$MONTH.x = as.numeric(gpraYouthAll$MONTH.x)
gpraYouthQuarter = subset(gpraYouthAll, MONTH.x > 10 | gpraYouthAll$YEAR.x >= 2017)

quaterAdults = length(gpraAdultQuarter$PARTID)
quaterYouth = length(gpraYouthQuarter$PARTID)

quaterCCPE = sum(quaterAdults, quaterYouth); quaterCCPE

# Now SIS
InternAdult = data.frame(gpraAdultQuarter$INTERVENTION_A.x, gpraAdultQuarter$INTERVENTION_B.x, gpraAdultQuarter$INTERVENTION_C.x)
SISAdult = data.frame(subset(InternAdult, gpraAdultQuarter.INTERVENTION_A.x ==2 | gpraAdultQuarter.INTERVENTION_B.x == 2 | gpraAdultQuarter.INTERVENTION_C.x == 2))
SISAdult = dim(SISAdult)
SISAdult = SISAdult[1]

InternYouth = data.frame(gpraYouthQuarter$INTERVENTION_A.x, gpraYouthQuarter$INTERVENTION_B.x, gpraYouthQuarter$INTERVENTION_C.x)
SISYouth = data.frame(subset(InternYouth, gpraYouthQuarter.INTERVENTION_A.x ==2 | gpraYouthQuarter.INTERVENTION_B.x == 2 | gpraYouthQuarter.INTERVENTION_C.x == 2))
SISYouth = dim(SISYouth)
SISYouth = SISYouth[1]

quaterSIS = sum(SISAdult, SISYouth); quaterSIS

# Now CIR
InternAdult = data.frame(gpraAdultQuarter$INTERVENTION_A.x, gpraAdultQuarter$INTERVENTION_B.x, gpraAdultQuarter$INTERVENTION_C.x)
CTRAdult = data.frame(subset(InternAdult, gpraAdultQuarter.INTERVENTION_A.x ==1 | gpraAdultQuarter.INTERVENTION_B.x == 1 | gpraAdultQuarter.INTERVENTION_C.x == 1))
CTRAdult = dim(CTRAdult)
CTRAdult = CTRAdult[1]
quaterCTR =  CTRAdult


# Lead and Seed
InternAdult = data.frame(gpraAdultQuarter$INTERVENTION_A.x, gpraAdultQuarter$INTERVENTION_B.x, gpraAdultQuarter$INTERVENTION_C.x)
LSAdult = data.frame(subset(InternAdult, gpraAdultQuarter.INTERVENTION_A.x ==3 | gpraAdultQuarter.INTERVENTION_B.x == 3 | gpraAdultQuarter.INTERVENTION_C.x == 3))
LSAdult = dim(LSAdult)
LSAdult = LSAdult[1]

InternYouth = data.frame(gpraYouthQuarter$INTERVENTION_A.x, gpraYouthQuarter$INTERVENTION_B.x, gpraYouthQuarter$INTERVENTION_C.x)
LSYouth = data.frame(subset(InternYouth, gpraYouthQuarter.INTERVENTION_A.x ==1 | gpraYouthQuarter.INTERVENTION_B.x == 1 | gpraYouthQuarter.INTERVENTION_C.x == 1))
LSYouth = dim(LSYouth)
LSYouth = LSYouth[1]

quaterLS = sum(LSAdult, LSYouth)

## Create a nice table for the data
totalQuarterObjectiveE = t(data.frame(totalAdults, totalYouth, totalCCPE, totalSIS, totalCTR, totalLS))
colnames(totalQuarterObjectiveE) = c("Total")
write.csv(totalQuarterObjectiveE, "totalQuarterObjectiveE.csv", row.names = FALSE)


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
colnames(totalHIVTesting) = c("Total HIV Tested")

################## Goal 3 Objective A #############################################################
# Create an average score across the five scores.  Grabe KNOW_SA, because Would you know where to go near where you live to see a health care professoinal regarding a drug or alcohol problem?
##### Overall #####
Goal3ObjectiveA =  data.frame(gpraAdultAll$KNOW_SA.x, gpraAdultAll$KNOW_SA.y, gpraAdultAll$KNOW_SA.x)
Goal3ObjectiveA
Goal3ObjectiveA = data.frame(apply(Goal3ObjectiveA, 2, function(x){ifelse(x == 98, NA, x)}))
install.packages("plyr")
count(Goal3ObjectiveA$gpraAdultAll.KNOW_SA.x)
