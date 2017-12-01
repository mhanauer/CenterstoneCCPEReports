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
# So I t.  Last quarter was 10-31-17.  Need to replace All with Quarter and overall with quarter
gpraAdultAll$MONTH.x = as.numeric(gpraAdultAll$MONTH.x)
gpraAdultQuarter = subset(gpraAdultAll, MONTH.x > 10)
gpraYouthAll$MONTH.x = as.numeric(gpraYouthAll$MONTH.x)
gpraYouthQuarter = subset(gpraYouthAll, MONTH.x > 10)
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
#GPRA HIV knowledge sum them together, get means and see if there is a difference between them.

figure2 = data.frame(gpraAdultAll$HIV_SICK_N.x, gpraAdultAll$HIV_SICK_N.y, gpraAdultAll$HIV_SICK_N, gpraAdultAll$HIV_GAYSEX_N.x, gpraAdultAll$HIV_GAYSEX_N.y, gpraAdultAll$HIV_GAYSEX_N, gpraAdultAll$HIV_BCPILL_N.x, gpraAdultAll$HIV_BCPILL_N.y, gpraAdultAll$HIV_BCPILL_N, gpraAdultAll$HIV_DRGS_N.x, gpraAdultAll$HIV_DRGS_N.y, gpraAdultAll$HIV_DRGS_N, gpraAdultAll$HIV_CURE_N.x, gpraAdultAll$HIV_CURE_N.y, gpraAdultAll$HIV_CURE_N) 
# Now need to get the answers that are correct so change them where 1 equals correct and 0 equals incorrect
# Sick 0 is 1, else zero or NA, gay 0 is 1 else zero, BCP 0 is 1, all others leave alone
# Need to change factors.  Also there are some 2's that should not be there.
HIVFalseEqualsOne = figure2[c(1:9)]

figure2 = data.frame(apply(figure2, 2, function(x){ifelse(x == "True", 1, ifelse(x == "False", 0, ifelse(x == "Don't know", NA,ifelse(x == 97, NA, NA))))}))
summary(figure2)

# Need to change factors.  Also there are some 2's that should not be there.
HIVFalseEqualsOne = figure2[c(1:9)]
HIVFalseEqualsOne = data.frame(apply(HIVFalseEqualsOne, 2, function(x){ifelse(x == 0, 1, x)}))
figure2 = figure2[c(10:15)]
figure2 = data.frame(HIVFalseEqualsOne, figure2)
head(figure2)
# Now I need to sum them for each of their time frames.  Need to get them into the baseline, 3 month and 6 month versions
baseline = data.frame(figure2$gpraAdultAll.HIV_SICK_N.x, figure2$gpraAdultAll.HIV_GAYSEX_N.x, figure2$gpraAdultAll.HIV_BCPILL_N.x, figure2$gpraAdultAll.HIV_DRGS_N.x, figure2$gpraAdultAll.HIV_CURE_N.x)
#baseline = data.frame(apply(baseline, 1, sum, na.rm = TRUE))

Month3 = data.frame(figure2$gpraAdultAll.HIV_SICK_N.y, figure2$gpraAdultAll.HIV_GAYSEX_N.y, figure2$gpraAdultAll.HIV_BCPILL_N.y, figure2$gpraAdultAll.HIV_DRGS_N.y, figure2$gpraAdultAll.HIV_CURE_N.y)
#Month3 = data.frame(apply(Month3, 1, sum, na.rm = TRUE))


Month6 =data.frame(figure2$gpraAdultAll.HIV_SICK_N, figure2$gpraAdultAll.HIV_GAYSEX_N, figure2$gpraAdultAll.HIV_BCPILL_N, figure2$gpraAdultAll.HIV_DRGS_N, figure2$gpraAdultAll.HIV_CURE_N)
#Month6 = data.frame(apply(Month6, 1, sum, na.rm = TRUE))

figure2 = data.frame(baseline, Month3, Month6)
figure2 = data.frame(na.omit(figure2))
dim(figure2)

### Table 2 #########################################################################################################


## Figure 5 #############################################################################################################
