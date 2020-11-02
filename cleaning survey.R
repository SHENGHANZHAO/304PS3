#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from [...UPDATE ME!!!!!]
# Author: Rohan Alexander and Sam Caetano [CHANGE THIS TO YOUR NAME!!!!]
# Data: 22 October 2020
# Contact: rohan.alexander@utoronto.ca [PROBABLY CHANGE THIS ALSO!!!!]
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/12913/Downloads")
raw_data <- read_dta("ns20200618.dta")

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(registration,vote_intention,gender, age, household_income,state,education,race_ethnicity,vote_2020)
         
data <- na.omit(reduced_data)
data <- filter(data,registration == 1 & vote_intention == 1&(vote_2020 == 1 | vote_2020 == 2))
data <- mutate(data,vote_2020 = ifelse(vote_2020==1,"Trump",  vote_2020))
data <- mutate(data,vote_2020 = ifelse(vote_2020==2,"Biden",  vote_2020))
data <- mutate(data,gender = ifelse(gender==1,"Female",  gender))
data <- mutate(data,gender = ifelse(gender==2,"Male",  gender))
data <- mutate(data,household_income = ifelse(household_income == 1 | household_income == 2  ,"Below or near poverty level",  household_income))
data <- mutate(data,household_income = ifelse(household_income == 3 | household_income == 4 
                                              |household_income == 5 |household_income == 6 |household_income == 7,"Low income",  household_income))
data <- mutate(data,household_income = ifelse(household_income == 8 | household_income == 9 
                                              |household_income == 10 |household_income == 11 |household_income == 12|
                                              household_income == 14 | household_income == 13 
                                              |household_income == 15 |household_income == 16 |household_income == 17
                                              |household_income == 18 |household_income == 19 |household_income == 20,"Middle class",  household_income))
data <- mutate(data,household_income = ifelse(household_income == 21 | household_income == 22  ,"High income",  household_income))
data <- mutate(data,household_income = ifelse(household_income == 23 | household_income == 24  ,"Highest tax brackets",  household_income))
data <- mutate(data,race_ethnicity = ifelse(race_ethnicity == 1,"White",  race_ethnicity))
data <- mutate(data,race_ethnicity = ifelse(race_ethnicity == 2,"Black",  race_ethnicity))
data <- mutate(data,race_ethnicity = ifelse(race_ethnicity == 4 | race_ethnicity == 5 |race_ethnicity == 6 |race_ethnicity == 7 |
                                              race_ethnicity == 8 |race_ethnicity == 9 |race_ethnicity == 10,"Asian",  race_ethnicity))
data <- mutate(data,race_ethnicity = ifelse(race_ethnicity == 3 | race_ethnicity == 11 |race_ethnicity == 12 
                                            |race_ethnicity == 13 | race_ethnicity == 14 |race_ethnicity == 15 ,"Other",  race_ethnicity))
data <- mutate(data,education = ifelse(education==1|education==2|education==3|education==4|education==5,"High school or lower",  education))
data <- mutate(data,education = ifelse(education==6|education==7|education==8,"College degree or some college",  education))
data <- mutate(data,education = ifelse(education==9|education==10|education==11,"Postgraduate",  education))
write_csv(data, "C:/Users/12913/Downloads/survey_data.csv")



         