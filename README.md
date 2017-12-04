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
Goal1ObjectiveFAdult = data.frame(gpraAdultAll$YEAR.x, gpraAdultAll$MONTH.x)
Goal1ObjectiveFAdultYear = subset(Goal1ObjectiveFAdult, gpraAdultAll.YEAR.x = 2017)
Goal1ObjectiveFAdultYear = dim(Goal1ObjectiveFAdultYear)
Goal1ObjectiveFAdultYear = Goal1ObjectiveFAdultYear[1]

Goal1ObjectiveFAdultQuarter = subset(Goal1ObjectiveFAdult, MONTH.x > 10, YEAR.X >= 2017)

#### CCPE Reports ######  ###### ###### ###### ###### ###### ###### ###### ######
# Demographics Overall: Gender Adult and Youth 

# This is for the  statistics, need to repeat this process for 

gpraAdultAll$GENDER.x # Need to break this down by the categories, could just turn this into a factor and see how it breaks it down
# Then I get a count for each type and rename the variable
# Follow this procedure for each one
# So what is the total enrollment?  Is this the enrollment that we have baseline data on? I would assume so 
overallAdultGender = data.frame(count(gpraAdultAll$GENDER.x))
colnames(overallAdultGender) = c("Gender", "Frequency")
overallYouthGender = data.frame(count(gpraYouthAll$GENDER.x))
colnames(overallYouthGender) = c("Gender", "Frequency")
write.csv(overallAdultGender, "overallAdultGender.csv", row.names = FALSE)
write.csv(overallYouthGender, "overallYouthGender.csv", row.names = FALSE)

# Demographics Overall: Age Adult and Youth
# # Now I need to subtract 2017 from year of birth.  That is as close as I can get without a month.
# Then I need to create the categories that are the same as in the table, but for adults only need years and older and 18 to 24
overallAdultAge = 2017- gpraAdultAll$YOB.x 
overallAdultAge = ifelse(overallAdultAge >= 25, "25 years or older", "18 – 24 years")
count(overallAdultAge)

# For youth, we are just getting anything that is not NA
overallYouthAge = 2017- gpraYouthAll$YOB.x 
overallYouthAge = ifelse(overallYouthAge > 0, "17 years or younger", NA)
count(overallYouthAge)

# Demographics Overall: Race Adult ######  ###### ###### ###### ###### ###### ###### ###### #
overallRaceAdult = data.frame(cbind(gpraAdultAll$R_WHITE_N.x,gpraAdultAll$R_BLACK_N.x,gpraAdultAll$R_AMERINALSK_N.x,gpraAdultAll$R_ASIAIN_N.x,gpraAdultAll$R_CHINESE_N.x,gpraAdultAll$R_FILIP_N.x,gpraAdultAll$R_JAPAN_N.x,gpraAdultAll$R_KOR_N.x,gpraAdultAll$R_VIETNAM_N.x,gpraAdultAll$R_OTHERASIA_N.x,gpraAdultAll$R_HAW_N.x,gpraAdultAll$R_OTHERPI_N.x))
names(overallRaceAdult) = c("White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander")
head(overallRaceAdult)
White = count(overallRaceAdult$White)

(overallRaceAdult)
# Grab the second row of data, because that has the 2's which are the yeses
White = White[2,2]
White
names(White) =c("White")
White

Black = count(overallRaceAdult$'Black or African American')
Black = Black[2,2]
Black
names(Black) =c("Black of African American")
Black

AmericanIndian = count(overallRaceAdult$`American Indian or Alaska Native`)
AmericanIndian = AmericanIndian[2,2]
names(AmericanIndian) =c("American Indian or Alaska Native")
AmericanIndian

AsianIndian = count(overallRaceAdult$`Asian Indian`)
AsianIndian = AsianIndian[2,2]
names(AsianIndian) =c("Asian Indian")
AsianIndian

Chinese = count(overallRaceAdult$Chinese)
Chinese = Chinese[2,2]
names(Chinese) =c("Chinese")
Chinese

Filipino = count(overallRaceAdult$Filipino)
Filipino  = Filipino [2,2]
names(Filipino ) =c("Filipino ")
Filipino

Japanese = count(overallRaceAdult$Japanese)
Japanese  = Japanese [2,2]
names(Japanese ) =c("Japanese ")
Japanese

Koren = count(overallRaceAdult$Korean)
Koren  = Koren [2,2]
names(Koren ) =c("Koren ")
Koren 

Vietnamese = count(overallRaceAdult$Vietnamese)
Vietnamese  = Vietnamese [2,2]
names(Vietnamese ) =c("Vietnamese ")
Vietnamese 

OtherAsian = count(overallRaceAdult$`Other Asian`)
OtherAsian  = OtherAsian [2,2]
names(OtherAsian ) =c("OtherAsian ")
OtherAsian 

NativeHawaiian = count(overallRaceAdult$`Native Hawaiian`)
NativeHawaiian  = NativeHawaiian [2,2]
names(NativeHawaiian ) =c("NativeHawaiian ")
NativeHawaiian 

OtherPacificIslander = count(overallRaceAdult$`Other Pacific Islander`)
OtherPacificIslander  = OtherPacificIslander [2,2]
names(OtherPacificIslander ) =c("OtherPacificIslander")
OtherPacificIslander 

overallRaceAdult = data.frame(cbind(White, Black, AmericanIndian, AsianIndian, Chinese, Filipino, Japanese, Vietnamese, OtherAsian, NativeHawaiian, OtherPacificIslander))
overallRaceAdult =  t(overallRaceAdult)
colnames(overallRaceAdult) = c("Overall")

write.csv(overallRaceAdult, "overallRaceAdult.csv")

# Demographics Overall: Race Youth  ######  ###### ###### ###### ###### ###### ###### ###### #
overallRaceYouth = data.frame(cbind(gpraYouthAll$R_WHITE_N.x,gpraYouthAll$R_BLACK_N.x,gpraYouthAll$R_AMERNALSK_N.x,gpraYouthAll$R_ASIAIN_N.x,gpraYouthAll$R_CHINESE_N.x,gpraYouthAll$R_FILIP_N.x,gpraYouthAll$R_JAPAN_N.x,gpraYouthAll$R_KOR_N.x,gpraYouthAll$R_VIETNAM_N.x,gpraYouthAll$R_OTHERASIA_N.x,gpraYouthAll$R_HAW_N.x,gpraYouthAll$R_OTHERRPI_N.x))
names(overallRaceYouth) = c("White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander")
head(overallRaceYouth)
White = count(overallRaceYouth$White)
# Grab the second row of data, because that has the 2's which are the yeses
White = White[2,2]
White
names(White) =c("White")
White

Black = count(overallRaceYouth$'Black or African American')
Black = Black[2,2]
Black
names(Black) =c("Black of African American")
Black

AmericanIndian = count(overallRaceYouth$`American Indian or Alaska Native`)
AmericanIndian = AmericanIndian[2,2]
names(AmericanIndian) =c("American Indian or Alaska Native")
AmericanIndian

AsianIndian = count(overallRaceYouth$`Asian Indian`)
AsianIndian = AsianIndian[2,2]
names(AsianIndian) =c("Asian Indian")
AsianIndian

Chinese = count(overallRaceYouth$Chinese)
Chinese = Chinese[2,2]
names(Chinese) =c("Chinese")
Chinese

Filipino = count(overallRaceYouth$Filipino)
Filipino  = Filipino [2,2]
names(Filipino ) =c("Filipino ")
Filipino

Japanese = count(overallRaceYouth$Japanese)
Japanese  = Japanese [2,2]
names(Japanese ) =c("Japanese ")
Japanese

Koren = count(overallRaceYouth$Korean)
Koren  = Koren [2,2]
names(Koren ) =c("Koren ")
Koren 

Vietnamese = count(overallRaceYouth$Vietnamese)
Vietnamese  = Vietnamese [2,2]
names(Vietnamese ) =c("Vietnamese ")
Vietnamese 

OtherAsian = count(overallRaceYouth$`Other Asian`)
OtherAsian  = OtherAsian [2,2]
names(OtherAsian ) =c("OtherAsian ")
OtherAsian 

NativeHawaiian = count(overallRaceYouth$`Native Hawaiian`)
NativeHawaiian  = NativeHawaiian [2,2]
names(NativeHawaiian ) =c("NativeHawaiian ")
NativeHawaiian 

OtherPacificIslander = count(overallRaceYouth$`Other Pacific Islander`)
OtherPacificIslander  = OtherPacificIslander [2,2]
names(OtherPacificIslander ) =c("OtherPacificIslander")
OtherPacificIslander 

overallRaceYouth = data.frame(cbind(White, Black, AmericanIndian, AsianIndian, Chinese, Filipino, Japanese, Vietnamese, OtherAsian, NativeHawaiian, OtherPacificIslander))
overallRaceYouth =  t(overallRaceYouth)
colnames(overallRaceYouth) = c("Overall")

write.csv(overallRaceYouth, "overallRaceYouth.csv")

# Demographics Overall: Ethnicity Adult  ######  ###### ###### ###### ###### ###### ###### ###### #
overallAdultEth = data.frame(cbind(gpraAdultAll$E_MEXICAN.x, gpraAdultAll$E_PUERTRICAN.x, gpraAdultAll$E_CUBAN.x, gpraAdultAll$E_OTHERHISPAN.x)) 
names(overallAdultEth) = c("Mexican, Mexican American, Chicano(a)", "Puerto Rican", "Cuban", "Other Hispanic, Latino(a), Spanish")

Mexican = count(overallAdultEth$`Mexican, Mexican American, Chicano(a)`)
Mexican  = Mexican [2,2]
names(Mexican ) =c("Mexican ")
Mexican 

PuertoRican = count(overallAdultEth$`Puerto Rican`)
PuertoRican  = PuertoRican [2,2]
names(PuertoRican ) =c("Puerto Rican")
PuertoRican 

OtherHis = count(overallAdultEth$`Other Hispanic, Latino(a), Spanish`)
OtherHis  =  OtherHis [2,2]
names(OtherHis) =c("Other Hispanic, Latino(a), Spanish")
OtherHis 

overallEthAdult = t(cbind(Mexican, PuertoRican, OtherHis))
colnames(overallEthAdult) = c("Ethnicity")
write.csv(overallEthAdult, "overallEthAdult.csv", row.names = TRUE)
# Demographics Overall: Eth Youth ######  ###### ###### ###### ###### ###### ###### ###### #
overallYouthEth = data.frame(cbind(gpraYouthAll$E_MEXICAN.x, gpraYouthAll$E_PUERTRICAN.x, gpraYouthAll$E_CUBAN.x, gpraYouthAll$E_OTHERHISPAN.x)) 
names(overallYouthEth) = c("Mexican, Mexican American, Chicano(a)", "Puerto Rican", "Cuban", "Other Hispanic, Latino(a), Spanish")

Mexican = count(overallYouthEth$`Mexican, Mexican American, Chicano(a)`)
Mexican  = Mexican [2,2]
names(Mexican ) =c("Mexican ")
Mexican 

PuertoRican = count(overallYouthEth$`Puerto Rican`)
PuertoRican  = PuertoRican [2,2]
names(PuertoRican ) =c("Puerto Rican")
PuertoRican 

OtherHis = count(overallYouthEth$`Other Hispanic, Latino(a), Spanish`)
OtherHis  =  OtherHis [2,2]
names(OtherHis) =c("Other Hispanic, Latino(a), Spanish")
OtherHis 

overallEthYouth = t(cbind(Mexican, PuertoRican, OtherHis))
colnames(overallEthYouth) = c("Ethnicity")
write.csv(overallEthYouth, "overallEthYouth.csv", row.names = TRUE)

# Demographics Quarter: Gender Adult and Youth  ######  ###### ###### ###### ###### ###### ###### ###### ######
# So It.  Last quarter was 10-31-17.  Need to replace All with Quarter and overall with quarter.  Because this is multiyear I need to have the month as well as the year.  So for this coming quarter 2018.  Because this quater moves into two year I need both 2017 and 2018.  Grab the baseline data because this will include the new people for baseline and their future data will be included  
gpraAdultAll$MONTH.x = as.numeric(gpraAdultAll$MONTH.x)
gpraAdultQuarter = subset(gpraAdultAll, MONTH.x > 10 | gpraAdultAll$YEAR.x >= 2017)
gpraYouthAll$MONTH.x = as.numeric(gpraYouthAll$MONTH.x)
gpraYouthQuarter = subset(gpraYouthAll, MONTH.x > 10 | gpraYouthAll$YEAR.x >= 2017)
setwd("C:/Users/Matthew.Hanauer/Desktop")

QuarterAdultGender = data.frame(count(gpraAdultQuarter$GENDER.x))
colnames(QuarterAdultGender) = c("Gender", "Frequency")
QuarterYouthGender = data.frame(count(gpraYouthQuarter$GENDER.x))
colnames(QuarterYouthGender) = c("Gender", "Frequency")
write.csv(QuarterAdultGender, "QuarterAdultGender.csv", row.names = FALSE)
write.csv(QuarterYouthGender, "QuarterYouthGender.csv", row.names = FALSE)



# Demographics Quarter: Gender Adult and Youth ######  ###### ###### ###### ###### ###### ###### ###### ######
QuarterAdultAge = 2017- gpraAdultQuarter$YOB.x 
QuarterAdultAge = ifelse(QuarterAdultAge >= 25, "25 years or older", "18 – 24 years")
count(QuarterAdultAge)
write.csv(QuarterAdultAge,"QuarterAdultAge.csv.csv", row.names = FALSE)
# For youth, we are just getting anything that is not NA
QuarterYouthAge = 2017- gpraYouthQuarter$YOB.x 
QuarterYouthAge = ifelse(QuarterYouthAge > 0, "17 years or younger", NA)
count(QuarterYouthAge)
write.csv(QuarterYouthAge, "QuarterYouthAge.csv", row.names = FALSE)

# Change Quarter and Quarter to Quarter
# Demographics Overall: Race Adult ######  ###### ###### ###### ###### ###### ###### ###### #
QuarterRaceAdult = data.frame(cbind(gpraAdultQuarter$R_WHITE_N.x,gpraAdultQuarter$R_BLACK_N.x,gpraAdultQuarter$R_AMERINALSK_N.x,gpraAdultQuarter$R_ASIAIN_N.x,gpraAdultQuarter$R_CHINESE_N.x,gpraAdultQuarter$R_FILIP_N.x,gpraAdultQuarter$R_JAPAN_N.x,gpraAdultQuarter$R_KOR_N.x,gpraAdultQuarter$R_VIETNAM_N.x,gpraAdultQuarter$R_OTHERASIA_N.x,gpraAdultQuarter$R_HAW_N.x,gpraAdultQuarter$R_OTHERPI_N.x))
names(QuarterRaceAdult) = c("White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander")
head(QuarterRaceAdult)
White = count(QuarterRaceAdult$White)

(QuarterRaceAdult)
# Grab the second row of data, because that has the 2's which are the yeses
White = White[2,2]
White
names(White) =c("White")
White

Black = count(QuarterRaceAdult$'Black or African American')
Black = Black[2,2]
Black
names(Black) =c("Black of African American")
Black

AmericanIndian = count(QuarterRaceAdult$`American Indian or Alaska Native`)
AmericanIndian = AmericanIndian[2,2]
names(AmericanIndian) =c("American Indian or Alaska Native")
AmericanIndian

AsianIndian = count(QuarterRaceAdult$`Asian Indian`)
AsianIndian = AsianIndian[2,2]
names(AsianIndian) =c("Asian Indian")
AsianIndian

Chinese = count(QuarterRaceAdult$Chinese)
Chinese = Chinese[2,2]
names(Chinese) =c("Chinese")
Chinese

Filipino = count(QuarterRaceAdult$Filipino)
Filipino  = Filipino [2,2]
names(Filipino ) =c("Filipino ")
Filipino

Japanese = count(QuarterRaceAdult$Japanese)
Japanese  = Japanese [2,2]
names(Japanese ) =c("Japanese ")
Japanese

Koren = count(QuarterRaceAdult$Korean)
Koren  = Koren [2,2]
names(Koren ) =c("Koren ")
Koren 

Vietnamese = count(QuarterRaceAdult$Vietnamese)
Vietnamese  = Vietnamese [2,2]
names(Vietnamese ) =c("Vietnamese ")
Vietnamese 

OtherAsian = count(QuarterRaceAdult$`Other Asian`)
OtherAsian  = OtherAsian [2,2]
names(OtherAsian ) =c("OtherAsian ")
OtherAsian 

NativeHawaiian = count(QuarterRaceAdult$`Native Hawaiian`)
NativeHawaiian  = NativeHawaiian [2,2]
names(NativeHawaiian ) =c("NativeHawaiian ")
NativeHawaiian 

OtherPacificIslander = count(QuarterRaceAdult$`Other Pacific Islander`)
OtherPacificIslander  = OtherPacificIslander [2,2]
names(OtherPacificIslander ) =c("OtherPacificIslander")
OtherPacificIslander 

QuarterRaceAdult = data.frame(cbind(White, Black, AmericanIndian, AsianIndian, Chinese, Filipino, Japanese, Vietnamese, OtherAsian, NativeHawaiian, OtherPacificIslander))
QuarterRaceAdult =  t(QuarterRaceAdult)
colnames(QuarterRaceAdult) = c("Overall")

write.csv(QuarterRaceAdult, "QuarterRaceAdult.csv")

# Demographics Overall: Race Youth  ######  ###### ###### ###### ###### ###### ###### ###### #
QuarterRaceYouth = data.frame(cbind(gpraYouthQuarter$R_WHITE_N.x,gpraYouthQuarter$R_BLACK_N.x,gpraYouthQuarter$R_AMERNALSK_N.x,gpraYouthQuarter$R_ASIAIN_N.x,gpraYouthQuarter$R_CHINESE_N.x,gpraYouthQuarter$R_FILIP_N.x,gpraYouthQuarter$R_JAPAN_N.x,gpraYouthQuarter$R_KOR_N.x,gpraYouthQuarter$R_VIETNAM_N.x,gpraYouthQuarter$R_OTHERASIA_N.x,gpraYouthQuarter$R_HAW_N.x,gpraYouthQuarter$R_OTHERRPI_N.x))
names(QuarterRaceYouth) = c("White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander")
head(QuarterRaceYouth)
White = count(QuarterRaceYouth$White)
# Grab the second row of data, because that has the 2's which are the yeses
White = White[2,2]
White
names(White) =c("White")
White

Black = count(QuarterRaceYouth$'Black or African American')
Black = Black[2,2]
Black
names(Black) =c("Black of African American")
Black

AmericanIndian = count(QuarterRaceYouth$`American Indian or Alaska Native`)
AmericanIndian = AmericanIndian[2,2]
names(AmericanIndian) =c("American Indian or Alaska Native")
AmericanIndian

AsianIndian = count(QuarterRaceYouth$`Asian Indian`)
AsianIndian = AsianIndian[2,2]
names(AsianIndian) =c("Asian Indian")
AsianIndian

Chinese = count(QuarterRaceYouth$Chinese)
Chinese = Chinese[2,2]
names(Chinese) =c("Chinese")
Chinese

Filipino = count(QuarterRaceYouth$Filipino)
Filipino  = Filipino [2,2]
names(Filipino ) =c("Filipino ")
Filipino

Japanese = count(QuarterRaceYouth$Japanese)
Japanese  = Japanese [2,2]
names(Japanese ) =c("Japanese ")
Japanese

Koren = count(QuarterRaceYouth$Korean)
Koren  = Koren [2,2]
names(Koren ) =c("Koren ")
Koren 

Vietnamese = count(QuarterRaceYouth$Vietnamese)
Vietnamese  = Vietnamese [2,2]
names(Vietnamese ) =c("Vietnamese ")
Vietnamese 

OtherAsian = count(QuarterRaceYouth$`Other Asian`)
OtherAsian  = OtherAsian [2,2]
names(OtherAsian ) =c("OtherAsian ")
OtherAsian 

NativeHawaiian = count(QuarterRaceYouth$`Native Hawaiian`)
NativeHawaiian  = NativeHawaiian [2,2]
names(NativeHawaiian ) =c("NativeHawaiian ")
NativeHawaiian 

OtherPacificIslander = count(QuarterRaceYouth$`Other Pacific Islander`)
OtherPacificIslander  = OtherPacificIslander [2,2]
names(OtherPacificIslander ) =c("OtherPacificIslander")
OtherPacificIslander 

QuarterRaceYouth = data.frame(cbind(White, Black, AmericanIndian, AsianIndian, Chinese, Filipino, Japanese, Vietnamese, OtherAsian, NativeHawaiian, OtherPacificIslander))
QuarterRaceYouth =  t(QuarterRaceYouth)
colnames(QuarterRaceYouth) = c("Overall")

write.csv(QuarterRaceYouth, "QuarterRaceYouth.csv")

# Demographics Overall: Ethnicity Adult  ######  ###### ###### ###### ###### ###### ###### ###### #
QuarterAdultEth = data.frame(cbind(gpraAdultQuarter$E_MEXICAN.x, gpraAdultQuarter$E_PUERTRICAN.x, gpraAdultQuarter$E_CUBAN.x, gpraAdultQuarter$E_OTHERHISPAN.x)) 
names(QuarterAdultEth) = c("Mexican, Mexican American, Chicano(a)", "Puerto Rican", "Cuban", "Other Hispanic, Latino(a), Spanish")

Mexican = count(QuarterAdultEth$`Mexican, Mexican American, Chicano(a)`)
Mexican  = Mexican [2,2]
names(Mexican ) =c("Mexican ")
Mexican 

PuertoRican = count(QuarterAdultEth$`Puerto Rican`)
PuertoRican  = PuertoRican [2,2]
names(PuertoRican ) =c("Puerto Rican")
PuertoRican 

OtherHis = count(QuarterAdultEth$`Other Hispanic, Latino(a), Spanish`)
OtherHis  =  OtherHis [2,2]
names(OtherHis) =c("Other Hispanic, Latino(a), Spanish")
OtherHis 

QuarterEthAdult = t(cbind(Mexican, PuertoRican, OtherHis))
colnames(QuarterEthAdult) = c("Ethnicity")
write.csv(QuarterEthAdult, "QuarterEthAdult.csv", row.names = TRUE)
# Demographics Overall: Eth Youth ######  ###### ###### ###### ###### ###### ###### ###### #
QuarterYouthEth = data.frame(cbind(gpraYouthQuarter$E_MEXICAN.x, gpraYouthQuarter$E_PUERTRICAN.x, gpraYouthQuarter$E_CUBAN.x, gpraYouthQuarter$E_OTHERHISPAN.x)) 
names(QuarterYouthEth) = c("Mexican, Mexican American, Chicano(a)", "Puerto Rican", "Cuban", "Other Hispanic, Latino(a), Spanish")

Mexican = count(QuarterYouthEth$`Mexican, Mexican American, Chicano(a)`)
Mexican  = Mexican [2,2]
names(Mexican ) =c("Mexican ")
Mexican 

PuertoRican = count(QuarterYouthEth$`Puerto Rican`)
PuertoRican  = PuertoRican [2,2]
names(PuertoRican ) =c("Puerto Rican")
PuertoRican 

OtherHis = count(QuarterYouthEth$`Other Hispanic, Latino(a), Spanish`)
OtherHis  =  OtherHis [2,2]
names(OtherHis) =c("Other Hispanic, Latino(a), Spanish")
OtherHis 

QuarterEthYouth = t(cbind(Mexican, PuertoRican, OtherHis))
colnames(QuarterEthYouth) = c("Ethnicity")
write.csv(QuarterEthYouth, "QuarterEthYouth.csv", row.names = TRUE)

###### Figure 1 ##################################################################################################
# Need to subset the people who were tested for HIV.  Just get the adults.  6 = rapid testing.  6 is only on intervention b so just grabbing intervention b.  Remember that two is is yes.  Male = 1; Female = 2. 
# Male to Female Transgender = 3, Female to Male Transgender =4, NA =5
overallFigure1 = data.frame(cbind(gpraAdultQuarter$INTERVENTION_B.x, 2018-gpraAdultAll$YOB.x, gpraAdultAll$R_WHITE_N.x,gpraAdultAll$R_BLACK_N.x,gpraAdultAll$R_AMERINALSK_N.x,gpraAdultAll$R_ASIAIN_N.x,gpraAdultAll$R_CHINESE_N.x,gpraAdultAll$R_FILIP_N.x,gpraAdultAll$R_JAPAN_N.x,gpraAdultAll$R_KOR_N.x,gpraAdultAll$R_VIETNAM_N.x,gpraAdultAll$R_OTHERASIA_N.x,gpraAdultAll$R_HAW_N.x,gpraAdultAll$R_OTHERPI_N.x, gpraAdultAll$GENDER.x)) 
colnames(overallFigure1) = c("Rapid", "Age", "White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander", "Gender")

#Overall Gender
overallFigure1Gender = data.frame(cbind(overallFigure1$Rapid, overallFigure1$Gender))
colnames(overallFigure1Gender) = c("Rapid", "Gender")
overallFigure1Gender = subset(overallFigure1Gender, overallFigure1Gender$Rapid == 6)
overallFigure1Gender = overallFigure1Gender$Gender
overallFigure1Gender = ifelse(overallFigure1Gender == 1, "Male", ifelse(overallFigure1Gender == 2, "Female", ifelse(overallFigure1Gender == 3,"Transgender", ifelse(overallFigure1Gender == 4, "Transgender", NA))))
head(overallFigure1Gender)
overallFigure1Gender = count(overallFigure1Gender)
colnames(overallFigure1Gender) = c("Gender", "Count")
overallFigure1Gender$Percent = round(overallFigure1Gender$Count / sum(overallFigure1Gender$Count),2)
write.csv(overallFigure1Gender, "overallFigure1Gender.csv", row.names = FALSE)

# Overall: Age
# Need to get 17 and younger
overallFigure1Age = data.frame(cbind(overallFigure1$Rapid,overallFigure1$Age)) 
colnames(overallFigure1Age) = c("Rapid", "Age")
overallFigure1Age = subset(overallFigure1Age, overallFigure1Age$Rapid == 6)
overallFigure1Age = overallFigure1Age$Age
overallFigure1Age = ifelse(overallFigure1Age >= 25, 25, ifelse(overallFigure1Age <= 17, 17, overallFigure1Age))
overallFigure1Age = as.data.frame(overallFigure1Age)
overallFigure1Age = count(overallFigure1Age)
colnames(overallFigure1Age) = c("Age", "Count")
overallFigure1Age$Percent = round(overallFigure1Age$Count / sum(overallFigure1Age$Count),2)
overallFigure1AgeTest = overallFigure1Age[order(-overallFigure1Age$Percent),]

# Overall Race
overallFigure1Race = data.frame(overallFigure1$Rapid, overallFigure1[c(3:14)])
head(overallFigure1Race)
overallFigure1Race = subset(overallFigure1Race, overallFigure1Race$overallFigure1.Rapid == 6)
overallFigure1Race = overallFigure1Race[c(-1)]

White = count(overallFigure1Race$White)
White = White[2,2]
White
names(White) =c("White")
White

Black = count(overallFigure1Race$Black.or.African.American)
Black = Black[2,2]
Black
names(Black) =c("Black of African American")
Black

AmericanIndian = count(overallFigure1Race$American.Indian.or.Alaska.Native)
AmericanIndian = AmericanIndian[2,2]
names(AmericanIndian) =c("American Indian or Alaska Native")
AmericanIndian

AsianIndian = count(overallFigure1Race$Asian.Indian)
AsianIndian = AsianIndian[2,2]
names(AsianIndian) =c("Asian Indian")
AsianIndian

Chinese = count(overallFigure1Race$Chinese)
Chinese = Chinese[2,2]
names(Chinese) =c("Chinese")
Chinese

Filipino = count(overallFigure1Race$Filipino)
Filipino  = Filipino [2,2]
names(Filipino ) =c("Filipino ")
Filipino

Japanese = count(overallFigure1Race$Japanese)
Japanese  = Japanese [2,2]
names(Japanese ) =c("Japanese ")
Japanese

Koren = count(overallFigure1Race$Korean)
Koren  = Koren [2,2]
names(Koren ) =c("Koren ")
Koren 

Vietnamese = count(overallFigure1Race$Vietnamese)
Vietnamese  = Vietnamese [2,2]
names(Vietnamese ) =c("Vietnamese ")
Vietnamese 

OtherAsian = count(overallFigure1Race$Other.Asian)
OtherAsian  = OtherAsian [2,2]
names(OtherAsian ) =c("OtherAsian ")
OtherAsian 

NativeHawaiian = count(overallFigure1Race$Native.Hawaiian)
NativeHawaiian  = NativeHawaiian [2,2]
names(NativeHawaiian ) =c("NativeHawaiian ")
NativeHawaiian 

OtherPacificIslander = count(overallFigure1Race$Other.Pacific.Islander)
OtherPacificIslander  = OtherPacificIslander [2,2]
names(OtherPacificIslander ) =c("OtherPacificIslander")
OtherPacificIslander 

overallFigure1Race = data.frame(cbind(White, Black, AmericanIndian, AsianIndian, Chinese, Filipino, Japanese, Vietnamese, OtherAsian, NativeHawaiian, OtherPacificIslander))
overallFigure1Race =  t(overallFigure1Race)
colnames(overallFigure1Race) = c("Overall")

quarterFigure1 = data.frame(cbind(gpraAdultQuarter$INTERVENTION_B.x, 2018-gpraAdultQuarter$YOB.x, gpraAdultQuarter$R_WHITE_N.x,gpraAdultQuarter$R_BLACK_N.x,gpraAdultQuarter$R_AMERINALSK_N.x,gpraAdultQuarter$R_ASIAIN_N.x,gpraAdultQuarter$R_CHINESE_N.x,gpraAdultQuarter$R_FILIP_N.x,gpraAdultQuarter$R_JAPAN_N.x,gpraAdultQuarter$R_KOR_N.x,gpraAdultQuarter$R_VIETNAM_N.x,gpraAdultQuarter$R_OTHERASIA_N.x,gpraAdultQuarter$R_HAW_N.x,gpraAdultQuarter$R_OTHERPI_N.x, gpraAdultQuarter$GENDER.x)) 
colnames(quarterFigure1) = c("Rapid", "Age", "White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander", "Gender")

#Quarter Gender
quarterFigure1Gender = data.frame(cbind(quarterFigure1$Rapid, quarterFigure1$Gender))
colnames(quarterFigure1Gender) = c("Rapid", "Gender")
quarterFigure1Gender = subset(quarterFigure1Gender, quarterFigure1Gender$Rapid == 6)
quarterFigure1Gender = quarterFigure1Gender$Gender
quarterFigure1Gender = ifelse(quarterFigure1Gender == 1, "Male", ifelse(quarterFigure1Gender == 2, "Female", ifelse(quarterFigure1Gender == 3,"Transgender", ifelse(quarterFigure1Gender == 4, "Transgender", NA))))
head(quarterFigure1Gender)
quarterFigure1Gender = count(quarterFigure1Gender)
colnames(quarterFigure1Gender) = c("Gender", "Count")
quarterFigure1Gender$Percent = round(quarterFigure1Gender$Count / sum(quarterFigure1Gender$Count),2)
write.csv(quarterFigure1Gender, "quarterFigure1Gender.csv", row.names = FALSE)

# Quarter: Age
# Need to get 17 and younger
quarterFigure1Age = data.frame(cbind(quarterFigure1$Rapid,quarterFigure1$Age)) 
colnames(quarterFigure1Age) = c("Rapid", "Age")
quarterFigure1Age = subset(quarterFigure1Age, quarterFigure1Age$Rapid == 6)
quarterFigure1Age = quarterFigure1Age$Age
quarterFigure1Age = ifelse(quarterFigure1Age >= 25, 25, ifelse(quarterFigure1Age <= 17, 17, quarterFigure1Age))
quarterFigure1Age = as.data.frame(quarterFigure1Age)
quarterFigure1Age = count(quarterFigure1Age)
colnames(quarterFigure1Age) = c("Age", "Count")
quarterFigure1Age$Percent = round(quarterFigure1Age$Count / sum(quarterFigure1Age$Count),2)
quarterFigure1AgeTest = quarterFigure1Age[order(-quarterFigure1Age$Percent),]

# Quarter Race
quarterFigure1Race = data.frame(quarterFigure1$Rapid, quarterFigure1[c(3:14)])
head(quarterFigure1Race)
quarterFigure1Race = subset(quarterFigure1Race, quarterFigure1Race$quarterFigure1.Rapid == 6)
quarterFigure1Race = quarterFigure1Race[c(-1)]

White = count(quarterFigure1Race$White)
White = White[2,2]
White
names(White) =c("White")
White

Black = count(quarterFigure1Race$Black.or.African.American)
Black = Black[2,2]
Black
names(Black) =c("Black of African American")
Black

AmericanIndian = count(quarterFigure1Race$American.Indian.or.Alaska.Native)
AmericanIndian = AmericanIndian[2,2]
names(AmericanIndian) =c("American Indian or Alaska Native")
AmericanIndian

AsianIndian = count(quarterFigure1Race$Asian.Indian)
AsianIndian = AsianIndian[2,2]
names(AsianIndian) =c("Asian Indian")
AsianIndian

Chinese = count(quarterFigure1Race$Chinese)
Chinese = Chinese[2,2]
names(Chinese) =c("Chinese")
Chinese

Filipino = count(quarterFigure1Race$Filipino)
Filipino  = Filipino [2,2]
names(Filipino ) =c("Filipino ")
Filipino

Japanese = count(quarterFigure1Race$Japanese)
Japanese  = Japanese [2,2]
names(Japanese ) =c("Japanese ")
Japanese

Koren = count(quarterFigure1Race$Korean)
Koren  = Koren [2,2]
names(Koren ) =c("Koren ")
Koren 

Vietnamese = count(quarterFigure1Race$Vietnamese)
Vietnamese  = Vietnamese [2,2]
names(Vietnamese ) =c("Vietnamese ")
Vietnamese 

OtherAsian = count(quarterFigure1Race$Other.Asian)
OtherAsian  = OtherAsian [2,2]
names(OtherAsian ) =c("OtherAsian ")
OtherAsian 

NativeHawaiian = count(quarterFigure1Race$Native.Hawaiian)
NativeHawaiian  = NativeHawaiian [2,2]
names(NativeHawaiian ) =c("NativeHawaiian ")
NativeHawaiian 

OtherPacificIslander = count(quarterFigure1Race$Other.Pacific.Islander)
OtherPacificIslander  = OtherPacificIslander [2,2]
names(OtherPacificIslander ) =c("OtherPacificIslander")
OtherPacificIslander 

quarterFigure1Race = data.frame(cbind(White, Black, AmericanIndian, AsianIndian, Chinese, Filipino, Japanese, Vietnamese, OtherAsian, NativeHawaiian, OtherPacificIslander))
quarterFigure1Race =  t(quarterFigure1Race)
colnames(quarterFigure1Race) = c("Quarter")
write.csv(quarterFigure1Race, 'quarterFigure1Race.csv', row.names = FALSE)

#### Figure 2 HIV/AIDS Knowledge of Adult Participants: Percent of Participants with the Correct Answer ##########################
# Need to find the data for these questions. Break them out by baseline, 3 month, and 6 month
#GPRA HIV knowledge sum them together, get means and see if there is a difference between them.  Not enough data to look at all three.  Use McNair test, but need to get the counts of how is right and wrong for each group.

figure2 = data.frame(gpraAdultAll$PARTID, gpraAdultAll$HIV_SICK_N.x, gpraAdultAll$HIV_SICK_N.y, gpraAdultAll$HIV_SICK_N, gpraAdultAll$HIV_GAYSEX_N.x, gpraAdultAll$HIV_GAYSEX_N.y, gpraAdultAll$HIV_GAYSEX_N, gpraAdultAll$HIV_BCPILL_N.x, gpraAdultAll$HIV_BCPILL_N.y, gpraAdultAll$HIV_BCPILL_N, gpraAdultAll$HIV_DRGS_N.x, gpraAdultAll$HIV_DRGS_N.y, gpraAdultAll$HIV_DRGS_N, gpraAdultAll$HIV_CURE_N.x, gpraAdultAll$HIV_CURE_N.y, gpraAdultAll$HIV_CURE_N) 


summary(figure2)

# Now need to get the answers that are correct so change them where 1 equals correct and 0 equals incorrect
# Sick 0 is 1, else zero or NA, gay 0 is 1 else zero, BCP 0 is 1, all others leave alone
# Need to change factors.  Also there are some 2's that should not be there.  97's should be incorrect, because if you don't know the answer then you don't know the correct answer.
# Need to add the one and zero, because you making everything that is not defined NA.  Need to have NA's in parthensis to not turn everything into NAs
# Need to have someone go back and get rid of 2's they should not be there.
# Don't know if fine, because you are grabbing the ones where don't know would be considered incorrect and where 0 is an incorrect answer.  Try subsetting at the end to get rid of the twos.
# In True equals one the don't knows equal 0, because they will be wrong
HIVTrueEqualsOne = figure2[c(11:16)]
head(HIVTrueEqualsOne)
HIVTrueEqualsOneTest = figure2[c(11:16)]
write.csv(HIVTrueEqualsOneTest, "HIVTrueEqualsOneTest.csv", row.names = FALSE)
HIVTrueEqualsOne = data.frame(apply(HIVTrueEqualsOne, 2, function(x){ifelse(x == "True", 1, ifelse(x == "False", 0, ifelse(x == "Don't know",0, x)))}))
head(HIVTrueEqualsOne)
write.csv(HIVTrueEqualsOne, "HIVTrueEqualsOne.csv", row.names = FALSE)
HIVTrueEqualsOne = read.csv("HIVTrueEqualsOne.csv", header = TRUE)
HIVTrueEqualsOne = data.frame(apply(HIVTrueEqualsOne, 2, function(x){ifelse(x == 2, NA, ifelse(x == 97, 0, x))}))
write.csv(HIVTrueEqualsOne, "HIVTrueEqualsOne.csv", row.names = FALSE)
# The above are correct
# Need to change factors.  Also there are some 2's that should not be there.  Take an average for baseline 3month, and 6month.  Here I am saying that 0 is the correct reponse so it is getting changed to 1.
HIVFalseEqualsOne = figure2[c(2:10)]
HIVFalseEqualsOneTest = figure2[c(2:10)]
write.csv(HIVFalseEqualsOneTest, "HIVFalseEqualsOneTest.csv", row.names = FALSE)

# Keep trues and falses the same i.e. True =1, because you are going to reverse everything at the next point.
HIVFalseEqualsOne = data.frame(apply(HIVFalseEqualsOne, 2, function(x){ifelse(x == "Don't know", 1, ifelse(x == "True", 1, ifelse(x == "False", 0, x)))}))
summary(HIVFalseEqualsOneTest)
write.csv(HIVFalseEqualsOne, "HIVFalseEqualsOne.csv", row.names = FALSE)
HIVFalseEqualsOne = read.csv("HIVFalseEqualsOne.csv", header = TRUE)

# Need's 1's to 0, because 1 is wrong and 0's to equal 1's, because 0's are right.  Also, get 
HIVFalseEqualsOne = data.frame(apply(HIVFalseEqualsOne, 2, function(x){ifelse(x == 0, 1, ifelse(x == 1, 0, ifelse(x == 97, NA,  x)))}))
write.csv(HIVFalseEqualsOne, "HIVFalseEqualsOne.csv", row.names = FALSE)

# Not picking up the zeros may need to change 

setwd("C:/Users/Matthew.Hanauer/Desktop")
figure2 = data.frame(figure2$gpraAdultAll.PARTID, HIVTrueEqualsOne, HIVFalseEqualsOne)
write.csv(figure2, "figure2.csv", row.names = FALSE)
figure2 = read.csv("figure2.csv", header = TRUE)
summary(figure2)
# Getting rid of the twos, because I couldn't earlier.
figure2 = data.frame(apply(figure2, 2, function(x){ifelse(x == 2, NA, x)}))


summary(figure2)
head(figure2)
# Now I need to sum them for each of their time frames.  Need to get them into the baseline, 3 month and 6 month versions.  So the average of the quetsions that they did answer.

baseline = data.frame(figure2$gpraAdultAll.HIV_DRGS_N.x, figure2$gpraAdultAll.HIV_CURE_N.x, figure2$gpraAdultAll.HIV_SICK_N.x, figure2$gpraAdultAll.HIV_GAYSEX_N.x, figure2$gpraAdultAll.HIV_BCPILL_N.x) 
write.csv(baseline, "baseline.csv", row.names = FALSE)
baseline = data.frame(apply(baseline, 1, mean, na.rm = TRUE))
head(baseline)


Month3 = data.frame(figure2$gpraAdultAll.HIV_SICK_N.y, figure2$gpraAdultAll.HIV_GAYSEX_N.y, figure2$gpraAdultAll.HIV_BCPILL_N.y, figure2$gpraAdultAll.HIV_DRGS_N.y, figure2$gpraAdultAll.HIV_CURE_N.y) 
Month3 = data.frame(apply(Month3, 1, mean, na.rm = TRUE))
head(Month3)

Month6 =data.frame(figure2$gpraAdultAll.HIV_SICK_N, figure2$gpraAdultAll.HIV_GAYSEX_N, figure2$gpraAdultAll.HIV_BCPILL_N, figure2$gpraAdultAll.HIV_DRGS_N, figure2$gpraAdultAll.HIV_CURE_N) 
Month6 = data.frame(apply(Month6, 1, mean, na.rm = TRUE))
head(Month6)

figure2 = data.frame(baseline, Month3, Month6)
colnames(figure2) = c("BaselineHIVScore", "Month3HIVScore", "Month6HIVScore")
head(figure2)
# It is right here, because person two has a 100% which is correct. 
figure2BaseTo3 = data.frame(BaselineHIVScore = figure2$BaselineHIVScore,Month3HIVScore  = figure2$Month3HIVScore)
figure2BaseTo3 = data.frame(na.omit(figure2BaseTo3)) 
dim(figure2BaseTo3)
wilcox.test(figure2BaseTo3$Month3HIVScore, figure2BaseTo3$BaselineHIVScore , paired = TRUE, correct=FALSE, alternative = c("greater"))
Figure2BaseTo3ColMeans = data.frame(Means = colMeans(figure2BaseTo3))
Figure2BaseTo3ColMeans = round(Figure2BaseTo3ColMeans,2)
write.csv(Figure2BaseTo3ColMeans, "Figure2BaseTo3ColMeans.csv")

figure2BaseTo6 = data.frame(figure2$BaselineHIVScore, figure2$Month6HIVScore)
figure2BaseTo6 = data.frame(na.omit(figure2BaseTo6)) 
dim(figure2BaseTo6)

# Research more about this test.
wilcox.test(figure2BaseTo6$figure2.Month6HIVScore, figure2BaseTo6$figure2.BaselineHIVScore , paired = TRUE, correct=FALSE, alternative = c("greater"))
Figure2BaseTo6ColMeans = data.frame(Means = colMeans(figure2BaseTo6))
Figure2BaseTo6ColMeans = round(Figure2BaseTo6ColMeans,2)
write.csv(Figure2BaseTo6ColMeans, "Figure2BaseTo6ColMeans.csv")

#dim(figure2)
# Right now when you delete missing data, you have no data, which means that we have zero people who completed all five questions across all three time points.
# To do the McNar test, I am ok, because we are only including people with data.  I need to put it in a format with the counts of 1's and zeros for base and 3 month or six month

### Figure 5  #########################################################################################################
# Grab all data, change to numeric values, conduct paired wilxcon test for all of them with baseline to 3 months then baseline to 6 months.  Make a nice table
figure5Month6 = data.frame(gpraAdultAll$CIG30D, gpraAdultAll$TOB30D , gpraAdultAll$VAP30D, gpraAdultAll$ALC30D , gpraAdultAll$BINGE530D, gpraAdultAll$MJ30D, gpraAdultAll$ILL30D, gpraAdultAll$RX30D)

figure5Baseline = data.frame(gpraAdultAll$CIG30D.x, gpraAdultAll$TOB30D.x, gpraAdultAll$VAP30D.x, gpraAdultAll$ALC30D.x, gpraAdultAll$BINGE530D.x, gpraAdultAll$MJ30D.x, gpraAdultAll$ILL30D.x, gpraAdultAll$RX30D.x)
figure5BaselineMeans = round(data.frame(apply(figure5Baseline,  2, mean, na.rm = TRUE)),0)
colnames(figure5BaselineMeans) = c("Mean number of days")
figure5BaselineMeans

figure5Month3 = data.frame(gpraAdultAll$CIG30D.y, gpraAdultAll$TOB30D.y, gpraAdultAll$VAP30D.y, gpraAdultAll$ALC30D.y, gpraAdultAll$BINGE530D.y, gpraAdultAll$MJ30D.y, gpraAdultAll$ILL30D.y, gpraAdultAll$RX30D.y)
figure5Baseline
# Need to get rid of the 98 and 97
figure5BaseMonth3 = data.frame(figure5Baseline, figure5Month3)
figure5BaseMonth3 = data.frame(apply(figure5BaseMonth3, 2, function(x){ifelse(x == 97, NA, ifelse(x == 98, NA, x))}))
figure5BaseMonth3 = data.frame(na.omit(figure5BaseMonth3))
dim(figure5BaseMonth3)

wilcox.test(figure5BaseMonth3$gpraAdultAll.CIG30D.y,figure5BaseMonth3$gpraAdultAll.CIG30D.x , paired = TRUE, correct=FALSE, alternative = c("less"))
figure5BaseMonth3Means = data.frame(apply(figure5BaseMonth3, 2, mean)) 
figure5BaseMonth3Means

write.csv(figure5, "figure5.csv", row.names = FALSE)
figure5 = read.csv("figure5.csv", header= TRUE)

#as.factor(figure5$gpraAdultAll.CIG30D.x) This changes it to a correct factor.

## Figure 5 #############################################################################################################
