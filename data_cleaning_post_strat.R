#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from the American 
# Community Surveys
# Author: Celine Kim
# Data: 2 November 2020
# Contact: celinekim.kim@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/celinekim/Desktop/project")
raw_data <- read.csv("inputs/AAOMcpOa.csv")

# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(agegrp,
         sex,
         pob,
         pr,
         hhinc,
         hdgree,
         kol,
         marsth,
         dpgrsum,
         cow)

reduced_data <-
  reduced_data %>%
  filter(agegrp >= 7) %>%
  filter(agegrp != 88)

# reduced_data<-
#   reduced_data %>%
#   mutate(age_group =
#            ifelse(agegrp==7, 101, agegrp)) %>%
#   mutate(age_group =
#            ifelse(age_group==8, 102, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==9, 103, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==10, 104, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==11, 105, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==12, 106, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==13, 107, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==14, 108, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==15, 109, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==16, 110, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==17, 111, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==18, 112, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==19, 113, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==20, 114, age_group)) %>%
#   mutate(age_group =
#            ifelse(age_group==21, 115, age_group)) 

reduced_data<-
  reduced_data %>%
  mutate(age_group =
           ifelse(agegrp==7 | agegrp==8 | agegrp==9, 101, agegrp)) %>%
  mutate(age_group =
           ifelse(age_group==10 | age_group==11, 102, age_group)) %>%
  mutate(age_group =
           ifelse(age_group==12 | age_group==13, 103, age_group)) %>%
  mutate(age_group =
           ifelse(age_group==14 | age_group==15, 104, age_group)) %>%
  mutate(age_group =
           ifelse(age_group>=16 & age_group<=21 , 105, age_group))

reduced_data <-
  reduced_data %>%
  mutate(gender_sex = ifelse(sex==2, 2, sex) + 100) %>%
  mutate(gender_sex = ifelse(gender_sex==102, 1, gender_sex)) %>%
  mutate(gender_sex = ifelse(gender_sex==101, 2, gender_sex))

reduced_data <-
  reduced_data %>%
  mutate(bornin_canada = ifelse(pob==1, 1, 0))
  
reduced_data <-
  reduced_data %>%
  mutate(household_income = ifelse(hhinc>=1 & hhinc <=10, 1, hhinc)) %>%
  mutate(household_income = ifelse(household_income>=11 & household_income <=16,
                                   2, household_income)) %>%
  mutate(household_income = ifelse(household_income>=17 & household_income <=22, 
                                   3, household_income)) %>%
  mutate(household_income = ifelse(household_income>=23 & household_income <=25, 
                                   4, household_income)) %>%
  mutate(household_income = ifelse(household_income>=26 & household_income <=29, 
                                   5, household_income)) %>%
  mutate(household_income = ifelse(household_income==30 | household_income==31, 
                                   6, household_income)) %>%
  mutate(household_income = ifelse(household_income==32 | household_income==33,
                                   7, household_income)) %>%
  mutate(household_income = ifelse(household_income==88, 0, household_income))
  
reduced_data <-
  reduced_data %>%
  mutate(education = ifelse(hdgree>=3 & hdgree <=7, 3, hdgree)) %>%
  mutate(education = ifelse(education>=8 & education<=10, 4, education)) %>%
  mutate(education = ifelse(education==12, 5, education)) %>%
  mutate(education = ifelse(education==11 | education==13, 6, education)) %>%
  mutate(education = ifelse(education==88 | education==99, 0, education))

reduced_data <-
  reduced_data %>%
  mutate(speak_French = ifelse(kol==2 | kol==3, 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(is_Aboriginal = ifelse(dpgrsum == 15, 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(is_Chinese = ifelse(dpgrsum == 3, 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(is_working =
           ifelse(cow>=1 & cow<=6, 1, 0))

reduced_data$province <- reduced_data$pr
reduced_data$marital_status <- reduced_data$marsth

reduced_data <-
  reduced_data %>%
  select(age_group,
         gender_sex,
         bornin_canada,
         province,
         household_income,
         education,
         speak_French,
         marital_status,
         is_Aboriginal,
         is_Chinese,
         is_working)

write_csv(reduced_data, "outputs/not_grouped.csv")

# Proportion Calculation
total_data <- length(reduced_data$is_working)

reduced_data <-
  reduced_data %>%
  group_by(province) %>%
  mutate(province_total=n()) %>%
  select(is_working, gender_sex, is_Aboriginal, is_Chinese, age_group,
         bornin_canada, household_income, education, speak_French,
         marital_status, province, province_total)

reduced_data <-
  reduced_data %>%
  group_by(province, age_group, is_working, gender_sex, is_Aboriginal,
           is_Chinese, bornin_canada, household_income, education, speak_French,
           marital_status, province_total) %>%
  summarise(number=n())

reduced_data <-
  reduced_data %>%
  mutate(cell_prop_of_division_total = number/total_data) %>%
  mutate(cell_prop_of_division_total_state = number/province_total)

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

# Saving the census data as a csv file in my working directory
write_csv(reduced_data, "outputs/census_data.csv")
write_csv(Newfoundland_data, "outputs/census_data_Newfoundland.csv")
write_csv(PEY_data, "outputs/census_data_PEY.csv")
write_csv(NovaScotia_data, "outputs/census_data_NovaScotia.csv")
write_csv(NewBrunswick_data, "outputs/census_data_NewBrunswick.csv")
write_csv(Quebec_data, "outputs/census_data_Quebec.csv")
write_csv(Ontario_data, "outputs/census_data_Ontario.csv")
write_csv(Manitoba_data, "outputs/census_data_Manitoba.csv")
write_csv(Sas_data, "outputs/census_data_Sas.csv")
write_csv(Alberta_data, "outputs/census_data_Alberta.csv")
write_csv(BC_data, "outputs/census_data_BC.csv")
write_csv(NorthernCanada_data, "outputs/census_data_NorthernCanada.csv")



