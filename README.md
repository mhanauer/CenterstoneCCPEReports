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

# Demographics Overall: Race Adult and Youth
overallRaceAdult = data.frame(cbind(gpraAdultAll$R_WHITE_N.x,gpraAdultAll$R_BLACK_N.x,gpraAdultAll$R_AMERINALSK_N.x,gpraAdultAll$R_ASIAIN_N.x,gpraAdultAll$R_CHINESE_N.x,gpraAdultAll$R_FILIP_N.x,gpraAdultAll$R_JAPAN_N.x,gpraAdultAll$R_KOR_N.x,gpraAdultAll$R_VIETNAM_N.x,gpraAdultAll$R_OTHERASIA_N.x,gpraAdultAll$R_HAW_N.x,gpraAdultAll$R_OTHERPI_N.x))
names(overallRaceAdult) = c("White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander")
head(overallRaceAdult)
White = count(overallRaceAdult$White)
# Grab the second row of data, because that has the 2's which are the yeses
White = White[2,2]
White
names(White) =c("White", "Count")
White
Black = count(overallRaceAdult$'Black or African American')
BlackTest = Black[2,2]
BlackTest
names(Black) =c("Black of African American", "Count")
Black
AmericanIndian = count(overallRaceAdult$`American Indian or Alaska Native`)

AsianIndian = count(overallRaceAdult$`Asian Indian`)

Chinese = count(overallRaceAdult$Chinese)

Filipino = count(overallRaceAdult$Filipino)

Japanese = count(overallRaceAdult$Japanese)

Koren = count(overallRaceAdult$Korean)

Vietnamese = count(overallRaceAdult$Vietnamese)

OtherAsian = count(overallRaceAdult$`Other Asian`)

NativeHawaiian = count(overallRaceAdult$`Native Hawaiian`)

OtherPacificIslander = count(overallRaceAdult$`Other Pacific Islander`)


overallRaceYouth = data.frame(cbind(gpraYouthAll$R_WHITE_N.x,gpraYouthAll$R_BLACK_N.x,gpraYouthAll$R_AMERNALSK_N.x,gpraYouthAll$R_ASIAIN_N.x,gpraYouthAll$R_CHINESE_N.x,gpraYouthAll$R_FILIP_N.x,gpraYouthAll$R_JAPAN_N.x,gpraYouthAll$R_KOR_N.x,gpraYouthAll$R_VIETNAM_N.x,gpraYouthAll$R_OTHERASIA_N.x,gpraYouthAll$R_HAW_N.x,gpraYouthAll$R_OTHERASIA_N.x))

  
names(overallRaceYouth) = c("White", "Black or African American", "American Indian or Alaska Native", "Asian Indian", "Chinese", "Filipino", "Japanese", "Korean", "Vietnamese", "Other Asian", "Native Hawaiian", "Other Pacific Islander")
head(overallRaceYouth)

# Demographics Quarter: Gender Adult and Youth ######  ###### ###### ###### ###### ###### ###### ###### ######
# So I t.  Last quarter was 10-31-17
gpraAdultAll$MONTH.x = as.numeric(gpraAdultAll$MONTH.x)
gpraAdultQuarter = subset(gpraAdultAll, MONTH.x > 10)







# Demographics Quarter: Gender Youth
grpaYouthAll$MONTH.x = as.numeric(grpaYouthAll$MONTH.x)
gpraYouthQuarter = subset(grpaYouthAll, MONTH.x > 10)


