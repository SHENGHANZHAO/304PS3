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
raw_data_census <- read_dta("usa_00005.dta")

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_census <- 
  raw_data_census %>% 
  select(statefip,sex,age,race,citizen,educd,perwt,citizen,hhincome)
na.omit(reduced_data_census)
census <- filter(reduced_data_census, age >= 18)
census <- filter(census, citizen == 1 | citizen == 2)
census <- mutate(census,sex = ifelse(sex==1,"Male",  sex))
census <- mutate(census,sex = ifelse(sex==2,"Female",  sex))
census <- mutate(census,race = ifelse(race==1,"White",  race))
census <- mutate(census,race = ifelse(race==2,"Black",  race))
census <- mutate(census,race = ifelse(race==4 | race == 5 | race == 6,"Asian",  race))
census <- mutate(census,race = ifelse(race==3 | race == 7 | race == 8 | race == 9,"Other",  race))
census <- mutate(census,educd = ifelse(educd <= 64 ,"High school or lower",  educd))
census <- mutate(census,educd = ifelse(educd == 65 | educd == 70 | educd == 71|
                                       educd == 80 | educd == 81 | educd == 82|
                                       educd == 83 | educd == 90 | educd == 100|
                                       educd == 101 | educd == 110 | educd == 111|
                                       educd == 112 | educd == 113 ,"College degree or some college",  educd))
census <- mutate(census,educd = ifelse(educd == 114 | educd == 115 | educd == 116,"Postgraduate",  educd))
census <- filter(census, hhincome != 9999999)
census <- census %>% mutate(hhincome = case_when(census$hhincome <= 20000 ~ "Below or near poverty level",
          census$hhincome <= 44999 & census$hhincome >= 20000 ~ "Low income",
          census$hhincome <= 149999 & census$hhincome >= 45000 ~ "Middle class",
          census$hhincome <= 199999 & census$hhincome >= 150000 ~ "High income",
          census$hhincome  >= 200000 ~ "Highest tax brackets"))
census <- mutate(census, statefip = ifelse(statefip == 1, "AL", statefip))
census <- mutate(census, statefip = ifelse(statefip == 2, "AK", statefip))
census <- mutate(census, statefip = ifelse(statefip == 4, "AZ", statefip))
census <- mutate(census, statefip = ifelse(statefip == 5, "AR", statefip))
census <- mutate(census, statefip = ifelse(statefip == 6, "CA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 8, "CO", statefip))
census <- mutate(census, statefip = ifelse(statefip == 9, "CT", statefip))
census <- mutate(census, statefip = ifelse(statefip == 10, "DE", statefip))
census <- mutate(census, statefip = ifelse(statefip == 11, "DC", statefip))
census <- mutate(census, statefip = ifelse(statefip == 12, "FL", statefip))
census <- mutate(census, statefip = ifelse(statefip == 13, "GA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 15, "HI", statefip))
census <- mutate(census, statefip = ifelse(statefip == 16, "ID", statefip))
census <- mutate(census, statefip = ifelse(statefip == 17, "IL", statefip))
census <- mutate(census, statefip = ifelse(statefip == 18, "IN", statefip))
census <- mutate(census, statefip = ifelse(statefip == 19, "IA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 20, "KS", statefip))
census <- mutate(census, statefip = ifelse(statefip == 21, "KY", statefip))
census <- mutate(census, statefip = ifelse(statefip == 22, "LA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 23, "ME", statefip))
census <- mutate(census, statefip = ifelse(statefip == 24, "MD", statefip))
census <- mutate(census, statefip = ifelse(statefip == 25, "MA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 26, "MI", statefip))
census <- mutate(census, statefip = ifelse(statefip == 27, "MN", statefip))
census <- mutate(census, statefip = ifelse(statefip == 28, "MS", statefip))
census <- mutate(census, statefip = ifelse(statefip == 29, "MO", statefip))
census <- mutate(census, statefip = ifelse(statefip == 30, "MT", statefip))
census <- mutate(census, statefip = ifelse(statefip == 31, "NE", statefip))
census <- mutate(census, statefip = ifelse(statefip == 32, "NV", statefip))
census <- mutate(census, statefip = ifelse(statefip == 33, "NH", statefip))
census <- mutate(census, statefip = ifelse(statefip == 34, "NJ", statefip))
census <- mutate(census, statefip = ifelse(statefip == 35, "NM", statefip))
census <- mutate(census, statefip = ifelse(statefip == 36, "NY", statefip))
census <- mutate(census, statefip = ifelse(statefip == 37, "NC", statefip))
census <- mutate(census, statefip = ifelse(statefip == 38, "ND", statefip))
census <- mutate(census, statefip = ifelse(statefip == 39, "OH", statefip))
census <- mutate(census, statefip = ifelse(statefip == 40, "OK", statefip))
census <- mutate(census, statefip = ifelse(statefip == 41, "OR", statefip))
census <- mutate(census, statefip = ifelse(statefip == 42, "PA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 44, "RI", statefip))
census <- mutate(census, statefip = ifelse(statefip == 45, "SC", statefip))
census <- mutate(census, statefip = ifelse(statefip == 46, "SD", statefip))
census <- mutate(census, statefip = ifelse(statefip == 47, "TN", statefip))
census <- mutate(census, statefip = ifelse(statefip == 48, "TX", statefip))
census <- mutate(census, statefip = ifelse(statefip == 49, "UT", statefip))
census <- mutate(census, statefip = ifelse(statefip == 50, "VT", statefip))
census <- mutate(census, statefip = ifelse(statefip == 51, "VA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 53, "WA", statefip))
census <- mutate(census, statefip = ifelse(statefip == 54, "WV", statefip))
census <- mutate(census, statefip = ifelse(statefip == 55, "WI", statefip))
census <- mutate(census, statefip = ifelse(statefip == 56, "WY", statefip))

write_csv(census, "C:/Users/12913/Downloads/census_data.csv")



         