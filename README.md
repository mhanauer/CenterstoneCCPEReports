---
title: "CCPE Final Report"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Library some packages
```{r}
library(psych)
library(prettyR)
####CCPE New GPRA
setwd("S:/Indiana Research & Evaluation/CCPE/NewGPRAData")
base = read.csv("baseline.csv", header = TRUE, na.strings = c(-99, -98, -97))
month3 = read.csv("month6.csv", header = TRUE, na.strings = c(-99, -98, -97))
```


Cleaning data need to merge them
```{r}
base$ID= base$participant_id
month3$ID = month3$redcap_survey_identifier
matchedDat = merge(base, month3, by = "ID", all.y = TRUE)
## Dim Check
dim(month3)
dim(matchedDat)
### ID Check
month3 = month3[order(month3$ID),]
matchedDat = matchedDat[order(matchedDat$ID),]
matchedDat$ID == month3$ID
```
Goal Three Objective A: Increase knowledge about SA by 20%.

know_sa
Would you know where to go near where you live to see a health care professional regarding a drug or alcohol problem?
Yes = 1
No = 0
```{r}
G3OA = data.frame(SABase = matchedDat$know_sa.x, SAMonth3 = matchedDat$know_sa.y)
dim(G3OA)
G3OAComplete = na.omit(G3OA)
dim(G3OAComplete)
describe(G3OAComplete)
describe.factor(G3OAComplete$SABase)
describe.factor(G3OAComplete$SAMonth3)


SABaseMean = round(mean(G3OAComplete$SABase),3)
SAMonth3Mean = round(mean(G3OAComplete$SAMonth3),3)
G3OA_p_change = round((SAMonth3Mean - SABaseMean)/ SABaseMean,3)


G3OA_results = data.frame(N = dim(G3OAComplete)[1], SABaseMean, SAMonth3Mean, G3OA_p_change)
G3OA_results
```
Demographics

what_is_your_race_one_or_m___2.x
What is your race? (One or more categories may be selected)
1, White
2, Black or African American
3, American Indian or Alaska Native
4, Asian
5, Native Hawaiian or other Pacific Islander

gender
How do you describe yourself? 
1, Male
2, Female
3, Transgender
5, I do not identify as male, female, or transgender

sex_pr
Which of the following do you consider yourself to be?
1, Straight/heterosexual
2, Gay/Lesbian
3, Bisexual
4, Other
5, Prefer not to say

```{r}
describe.factor(matchedDat$what_is_your_race_one_or_m___2.x)
describe.factor(matchedDat$gender.x)
describe.factor(matchedDat$sex_pr.x)
```

