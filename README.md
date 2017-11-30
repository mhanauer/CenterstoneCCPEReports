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
overallYouthGender = data.frame(count(grpaYouthAll$GENDER.x))
colnames(overallYouthGender) = c("Gender", "Frequency")
overallAdultGender
overallYouthGender

# Demographics Quarter: Gender Adult and Youth ######  ###### ###### ###### ###### ###### ###### ###### ######
# So I t.  Last quarter was 10-31-17
gpraAdultAll$MONTH.x = as.numeric(gpraAdultAll$MONTH.x)

subset(gpraAdultAll,MONTH.x == "November" | "December")
#as.numeric(gpraAdultAll$MONTH.x)

gpraAdultAll
