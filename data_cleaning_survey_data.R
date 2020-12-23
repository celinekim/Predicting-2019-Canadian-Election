#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from the 2019 Canadian 
# Election Study 
# Author: Celine Kim
# Data: 9 December 2020
# Contact: celinekim.kim@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/celinekim/Desktop/project")
# Read in the raw data
raw_data <- read_dta("inputs/2019_survey.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Select necessary variables
reduced_data <- 
  raw_data %>% 
  select(cps19_age, 
         cps19_education, 
         cps19_province,
         cps19_gender,
         cps19_ethnicity_23,
         cps19_ethnicity_24,
         cps19_ethnicity_25,
         cps19_ethnicity_26,
         cps19_ethnicity_27,
         cps19_ethnicity_28,
         cps19_ethnicity_29,
         cps19_ethnicity_30,
         cps19_ethnicity_31,
         cps19_ethnicity_32,
         cps19_ethnicity_33,
         cps19_ethnicity_34,
         cps19_ethnicity_35,
         cps19_ethnicity_36,
         cps19_ethnicity_37,
         cps19_ethnicity_38,
         cps19_ethnicity_39,
         cps19_ethnicity_40,
         cps19_employment,
         cps19_marital,
         cps19_income_cat,
         cps19_bornin_canada,
         cps19_language_69, # French
         cps19_votechoice, # Canadian citizen & answered certainly or likely to vote
         cps19_vote_unlikely, # Canadian citizen & answered unlikely to vote
         cps19_vote_lean, # Canadian citizen & answered don't know or prefer not to answer
         pes19_votechoice2019)

### LANGUAGE - Whether the respondent speaks French
reduced_data<-
  reduced_data %>%
  mutate(speak_French = ifelse(!(is.na(cps19_language_69)), 1, 0))

### GENDER_SEX
# If respondent identifies as a man, 1.
# If respondent identifies as a woman, 2.
# If respondent identifies as other (e.g. Trans, non-binary, two-spirit, gender-queer), 3
reduced_data<-
  reduced_data %>%
  mutate(gender_sex =
           ifelse(cps19_gender==1, 1, cps19_gender)) %>%
  filter(gender_sex != 3)

### BORN IN CANADA
# If respondent was born in Canada, 1 and 0 otherwise
reduced_data<-
  reduced_data %>%
  mutate(bornin_canada =
           ifelse(cps19_bornin_canada=="Yes", 1, 0)) 

### EDUCATION
# If respondent answered "Don't know/ Prefer not to answer", 0.
# No schooling or below secondary school diploma corresponds to 1.
# Secondary (high) school diploma or equivalency certificate corresponds to 2.
# Some or completed technical, community college, CEGEP, College Classique corresponds to 3.
# Some university or Bachelor's degree corresponds to 4.
# Master's degree corresponds to 5.
# Professional degree or doctorate corresponds to 6.
reduced_data<-
  reduced_data %>%
  mutate(education =
           ifelse(cps19_education==1, 1, cps19_education)) %>%
  mutate(education =
           ifelse(education==1 | education==2 | education==3 | education==4, 
                  1, education)) %>%
  mutate(education =
           ifelse(education==5, 2, education)) %>%
  mutate(education =
           ifelse(education==6 | education==7, 3, education)) %>% 
  mutate(education =
           ifelse(education==8 | education==9, 4, education)) %>%  
  mutate(education =
           ifelse(education==10, 5, education)) %>%
  mutate(education =
           ifelse(education==11, 6, education)) %>%
  mutate(education =
           ifelse(education==12, 0, education))

# # Remove ages below 18 and divide into 4 age groups which are ages18to29, 
# # ages30to44, ages45to59 and ages60plus.
reduced_data$age <- as.integer(reduced_data$cps19_age)
reduced_data <-
  reduced_data %>%
  filter(age >= 18)

# reduced_data <-
#   reduced_data %>%
#   mutate(age_group =
#            ifelse((age>=18 & age<=19), 101,
#                   ifelse((age>=20 & age<=24), 102,
#                          ifelse((age>=25 & age<=29), 103,
#                                 ifelse((age>=30 & age<=34), 104,
#                                        ifelse((age>=35 & age<=39), 105,
#                                               ifelse((age>=40 & age<=44), 106,
#                                                      ifelse((age>=45 & age<=49), 107,
#                                                             ifelse((age>=50 & age<=54), 108,
#                                                                    ifelse((age>=55 & age<=59), 109,
#                                                                           ifelse((age>=60 & age<=64), 110,
#                                                                                  ifelse((age>=65 & age<=69), 111,
#                                                                                         ifelse((age>=70 & age<=74), 112,
#                                                                                                ifelse((age>=75 & age<=79), 113,
#                                                                                                       ifelse((age>=80 & age<=84), 114,
#                                                                                                              ifelse((age>=85), 115, age)
#                                                                                                       )
#                                                                                                )
#                                                                                         )
#                                                                                  )
#                                                                           )
#                                                                    )
#                                                             )
#                                                      )
#                                               )
#                                        )
#                                 )
#                          )
#                   )
#            )
#   ) %>%
#   filter(age_group != "NA")

reduced_data <-
  reduced_data %>%
  mutate(age_group =
           ifelse((age>=18 & age<=29), 101,
                  ifelse((age>=30 & age<=39), 102,
                         ifelse((age>=40 & age<=49), 103,
                                ifelse((age>=50 & age<=59), 104,
                                       ifelse((age>=60), 105, age)
                                )
                         )
                  )
           )
  ) %>%
  filter(age_group != "NA")
                  

reduced_data <-
  reduced_data %>%
  mutate(cps19_vote_liberal =
           ifelse(cps19_votechoice=="Liberal Party" | 
                    cps19_vote_unlikely=="Liberal Party" |
                    cps19_vote_lean=="Liberal Party", 1, 0)) %>%
  mutate(cps19_vote_liberal = ifelse(!(is.na(cps19_vote_liberal)), 
                                     cps19_vote_liberal, 0))

reduced_data <-
  reduced_data %>%
  mutate(cps19_vote_conservative =
           ifelse(cps19_votechoice=="Conservative Party" | 
                    cps19_vote_unlikely=="Conservative Party" |
                    cps19_vote_lean=="Conservative Party", 1, 0)) %>%
  mutate(cps19_vote_conservative = ifelse(!(is.na(cps19_vote_conservative)), 
                                          cps19_vote_conservative, 0))

reduced_data <-
  reduced_data %>%
  mutate(pes19_vote_liberal =
           ifelse(pes19_votechoice2019=="Liberal Party", 1, 0)) %>%
  mutate(pes19_vote_liberal = ifelse(!(is.na(pes19_vote_liberal)), 
                                   pes19_vote_liberal, 0))

reduced_data <-
  reduced_data %>%
  mutate(pes19_vote_conservative =
           ifelse(pes19_votechoice2019=="Conservative Party", 1, 0)) %>%
  mutate(pes19_vote_conservative = ifelse(!(is.na(pes19_vote_conservative)), 
                                        pes19_vote_conservative, 0))

### EMPLOYMENT
# If working for pay full-time, part-time or self-employed, is_working is 1 and 0 otherwise.
# what about 9, 10, 11
reduced_data <-
  reduced_data %>%
  mutate(is_working =
           ifelse(cps19_employment==1, 1, cps19_employment)) %>%
  mutate(is_working =
           ifelse(is_working==1 | is_working==2 | is_working==3 | 
                    is_working==9 | is_working==10 | is_working==11 , 1, 0))

### PROVINCE
reduced_data$province <- as.integer(reduced_data$cps19_province)
reduced_data<-
  reduced_data %>%
  mutate(province =
           ifelse(province==6 | province==8 | province==13, 70, province)) %>%
  mutate(province = ifelse(province==12, 47, province)) %>%
  mutate(province = ifelse(province==11, 24, province)) %>%
  mutate(province = ifelse(province==10, 11, province)) %>%
  mutate(province = ifelse(province==9, 35, province)) %>%
  mutate(province = ifelse(province==7, 12, province)) %>%
  mutate(province = ifelse(province==5, 10, province)) %>%
  mutate(province = ifelse(province==4, 13, province)) %>%
  mutate(province = ifelse(province==3, 46, province)) %>%
  mutate(province = ifelse(province==2, 59, province)) %>%
  mutate(province = ifelse(province==1, 48, province))

# If respondent's ethnicity is Aboriginal/First Nations, is_Aboriginal_or_FN is 1, otherwise 0.
reduced_data <-
  reduced_data %>%
  mutate(is_Aboriginal_or_FN = ifelse(!(is.na(cps19_ethnicity_23)), 1, 0))

# If respondent is Inuk/ Inuit, is_Inuk_or_Inuit is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Inuk_or_Inuit = ifelse(!(is.na(cps19_ethnicity_33)), 1, 0))

# If respondent is Metis, is_Metis is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Metis = ifelse(!(is.na(cps19_ethnicity_36)), 1, 0))

# If respondent is Aboriginal/FN or Inuk/Inuit or Metis, is_Aboriginal is 1, 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Aboriginal = ifelse(is_Aboriginal_or_FN==1 | is_Inuk_or_Inuit==1 |
                                  is_Metis ==1, 1, 0))

# If respondent is British, is_British is 1, otherwise 0.
reduced_data <-
  reduced_data %>%
  mutate(is_British = ifelse(!(is.na(cps19_ethnicity_24)), 1, 0))

# If respondent is Chinese, is_Chinese 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Chinese = ifelse(!(is.na(cps19_ethnicity_25)), 1, 0))

# If respondent is Dutch, is_Dutch is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Dutch = ifelse(!(is.na(cps19_ethnicity_26)), 1, 0))

# If respondent is English, is_English is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_English = ifelse(!(is.na(cps19_ethnicity_27)), 1, 0))

# If respondent is French, is_French is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_French = ifelse(!(is.na(cps19_ethnicity_28)), 1, 0))

# If respondent is French Canadian, is_French_Canadian is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_French_Canadian = ifelse(!(is.na(cps19_ethnicity_29)), 1, 0))

# If respondent is German, is_German is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_German = ifelse(!(is.na(cps19_ethnicity_30)), 1, 0))

# If respondent is Hispanic, is_Hispanic is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Hispanic = ifelse(!(is.na(cps19_ethnicity_31)), 1, 0))

# If respondent is Indian, is_Indian is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Indian = ifelse(!(is.na(cps19_ethnicity_32)), 1, 0))

# If respondent is Irish, is_Irish is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Irish = ifelse(!(is.na(cps19_ethnicity_34)), 1, 0))

# If respondent is Italian, is_Italian is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Italian = ifelse(!(is.na(cps19_ethnicity_35)), 1, 0))

# If respondent is Polish, is_Polish is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Polish = ifelse(!(is.na(cps19_ethnicity_37)), 1, 0))

# If respondent is Quebecois, is_Quebecois is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Quebecois = ifelse(!(is.na(cps19_ethnicity_38)), 1, 0))

# If respondent is Scottish, is_Scottish is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Scottish = ifelse(!(is.na(cps19_ethnicity_39)), 1, 0))

# If respondent is Ukrainian, is_Ukrainian is 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_Ukrainian = ifelse(!(is.na(cps19_ethnicity_40)), 1, 0))

### HOUSEHOLD INCOME
reduced_data <-
  reduced_data %>%
  mutate(household_income =
           ifelse(cps19_income_cat==1, 1, cps19_income_cat)) %>%
  mutate(household_income =
           ifelse(household_income==1 | household_income==2, 1, household_income)) %>%
  mutate(household_income =
           ifelse(household_income==3, 2, household_income)) %>%
  mutate(household_income =
           ifelse(household_income==4, 3, household_income)) %>%
  mutate(household_income =
           ifelse(household_income==5, 4, household_income)) %>%
  mutate(household_income =
           ifelse(household_income==6, 5, household_income)) %>%
  mutate(household_income =
           ifelse(household_income==7, 6, household_income)) %>%
  mutate(household_income =
           ifelse(household_income==8, 7, household_income)) %>%
  mutate(household_income =
           ifelse(household_income==9, 0, household_income))

reduced_data <-
  reduced_data %>%
  filter(household_income != "NA")

reduced_data <-
  reduced_data %>%
  mutate(marital_status =
           ifelse(cps19_marital=="Never Married", 6, cps19_marital) + 100) %>%
  mutate(marital_status =
           ifelse(marital_status==101, 2, marital_status)) %>%
  mutate(marital_status =
           ifelse(marital_status==102, 3, marital_status)) %>%
  mutate(marital_status =
           ifelse(marital_status==103, 5, marital_status)) %>%
  mutate(marital_status =
           ifelse(marital_status==104, 4, marital_status)) %>%
  mutate(marital_status =
           ifelse(marital_status==105, 6, marital_status)) %>%
  mutate(marital_status =
           ifelse(marital_status==106, 1, marital_status))

reduced_data <-
  reduced_data %>%
  filter(marital_status != 107)

reduced_data <-
  reduced_data %>%
  select(age_group,
         education, 
         province,
         gender_sex,
         is_Aboriginal,
         is_Chinese,
         is_working,
         marital_status,
         household_income,
         bornin_canada,
         speak_French,
         cps19_vote_liberal,
         cps19_vote_conservative)

Newfoundland_data <- 
  reduced_data %>%
  filter(province == 10)

PEY_data <- 
  reduced_data %>%
  filter(province == 11)

NovaScotia_data <- 
  reduced_data %>%
  filter(province == 12)

NewBrunswick_data <- 
  reduced_data %>%
  filter(province == 13)

Quebec_data <- 
  reduced_data %>%
  filter(province == 24)

Ontario_data <- 
  reduced_data %>%
  filter(province == 35)

Manitoba_data <- 
  reduced_data %>%
  filter(province == 46)

Sas_data <- 
  reduced_data %>%
  filter(province == 47)

Alberta_data <- 
  reduced_data %>%
  filter(province == 48)

BC_data <- 
  reduced_data %>%
  filter(province == 59)

NorthernCanada_data <- 
  reduced_data %>%
  filter(province == 70)

# Saving the survey/sample data as a csv file in my working directory
write_csv(reduced_data, "outputs/survey_data.csv")
write_csv(Newfoundland_data, "outputs/survey_data_Newfoundland.csv")
write_csv(PEY_data, "outputs/survey_data_PEY.csv")
write_csv(NovaScotia_data, "outputs/survey_data_NovaScotia.csv")
write_csv(NewBrunswick_data, "outputs/survey_data_NewBrunswick.csv")
write_csv(Quebec_data, "outputs/survey_data_Quebec.csv")
write_csv(Ontario_data, "outputs/survey_data_Ontario.csv")
write_csv(Manitoba_data, "outputs/survey_data_Manitoba.csv")
write_csv(Sas_data, "outputs/survey_data_Sas.csv")
write_csv(Alberta_data, "outputs/survey_data_Alberta.csv")
write_csv(BC_data, "outputs/survey_data_BC.csv")
write_csv(NorthernCanada_data, "outputs/survey_data_NorthernCanada.csv")



