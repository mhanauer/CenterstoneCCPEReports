#### CCPE Reports ######  ###### ###### ###### ###### ###### ###### ###### ######
# Demographics Overall: Gender Adult and Youth 

# This is for the  statistics, need to repeat this process for 
library(WriteXLS)
gpraAdultAll$GENDER.x # Need to break this down by the categories, could just turn this into a factor and see how it breaks it down
# Then I get a count for each type and rename the variable
# Follow this procedure for each one
# So what is the total enrollment?  Is this the enrollment that we have baseline data on? I would assume so 
overallAdultGender = data.frame(count(gpraAdultAll$GENDER.x))
colnames(overallAdultGender) = c("Gender", "Frequency")
overallYouthGender = data.frame(count(gpraYouthAll$GENDER.x))
colnames(overallYouthGender) = c("Gender", "Frequency")
overallAdultGender
overallYouthGender

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
overallAdultEth = data.frame(cbind(gpraAdultAll$E_NONHISPAN.x, gpraAdultAll$E_MEXICAN.x, gpraAdultAll$E_PUERTRICAN.x, gpraAdultAll$E_CUBAN.x, gpraAdultAll$E_OTHERHISPAN.x)) 


# Demographics Quarter: Gender Adult and Youth ######  ###### ###### ###### ###### ###### ###### ###### ######
# So I t.  Last quarter was 10-31-17
gpraAdultAll$MONTH.x = as.numeric(gpraAdultAll$MONTH.x)
gpraAdultQuarter = subset(gpraAdultAll, MONTH.x > 10)







# Demographics Quarter: Gender Youth
grpaYouthAll$MONTH.x = as.numeric(grpaYouthAll$MONTH.x)
gpraYouthQuarter = subset(grpaYouthAll, MONTH.x > 10)


