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
raw_data <- read_dta("usa_00001.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(sex, 
         age,
         race, 
         hinscaid ,
         hinscare,
         educ,
         ind,
         incwage)
         

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

# group by sex
sex_data <- 
  reduced_data %>% 
  count(sex) %>%
  group_by(sex)

# group by race
race_data <-
  reduced_data %>%
  count(race) %>%
  group_by(race)

# group by age

age_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

age_data$age <- as.integer(age_data$age)

age_data <-
  age_data %>%
  filter(age >= 18) %>%
  count(age)%>%
  group_by(age)


# group by age group
reduced_data$age <- as.integer(reduced_data$age)

age_group_data <-
  reduced_data %>%
  filter(age>=18) %>%
  mutate(age_group = case_when(age <= 29 ~ "18-29 year olds",
                               age %in% c(30:44) ~ "30-44 year olds",
                               age %in% c(45:64) ~ "45-64 year olds",
                               age >=65 ~ "65 years and older")) %>%
  count(age_group) %>%
  group_by(age_group)

# group by health insurance (Medicaid)
medicaid_data <- 
  reduced_data %>% 
  count(hinscaid) %>%
  group_by(hinscaid)

#group by health insurance (Medicare)
medicare_data <- 
  reduced_data %>% 
  count(hinscare) %>%
  group_by(hinscare)

# group by education attainment
education_data <- 
  reduced_data %>% 
  count(educ) %>%
  group_by(educ)

# group by industry
industry_data <- 
  reduced_data %>% 
  mutate(industry = case_when(ind %in% c(170:490) ~ "Agriculture, Forestry, Fishing, Hunting, and Mining",
                              ind == 770 ~ "Construction",
                              ind %in% c(1070:3990) ~ "Manufacturing",
                              ind %in% c(4070:4590) ~ "Wholesale Trade",
                              ind %in% c(4670:5790) ~ "Retail Trade",
                              ind %in% c(6070:6390) ~ "Transportation and Warehousing",
                              ind %in% c(570:690) ~ "Utilities",
                              ind %in% c(6470:6780) ~ "Information",
                              ind %in% c(6870:6992) ~ "Finance and Insurance",
                              ind %in% c(7071:7190) ~ "Real Estate and Rental/Leasing",
                              ind %in% c(7270:7490) ~ "Professional, Scientific, and Technical Services",
                              ind == 7570 ~ "Management of companies and enterprises",
                              ind %in% c(7580:7790) ~ "Administrative and support and waste management services",
                              ind %in% c(7860:7890) ~ "Educational Services",
                              ind %in% c(7970:8470) ~ "Health Care and Social Assistance",
                              ind %in% c(8561:8590) ~ "Arts, Entertainment, and Recreation",
                              ind %in% c(8660:8690) ~ "Accommodation and Food Services",
                              ind %in% c(8770:9290) ~ "Other Services, Except Public Administration",
                              ind %in% c(9370:9590) ~ "Public Administration",
                              ind %in% c(9670:9870) ~ "Military",
                              ind == 9920 ~ "Unemployed or never worked")) %>%
  count(industry) %>%
  group_by(industry)

# group by wage
wage_data <-
  reduced_data %>%
  filter(incwage != 999999, incwage != 999998) %>%
  mutate(income = case_when(incwage %in% c(0:9999) ~ "Less than $10,000",
                            incwage %in% c(10000:14999) ~ "$10,000 - $14,999",
                            incwage %in% c(15000:19999) ~ "$15,000 - $19,999",
                            incwage %in% c(20000:29999) ~ "$20,000 - $29,999",
                            incwage %in% c(30000:39999) ~ "$30,000 - $39,999",
                            incwage %in% c(40000:49999) ~ "$40,000 - $49,999",
                            incwage %in% c(50000:74999) ~ "$50,000 - $74,999",
                            incwage %in% c(75000:99999) ~ "$75,000 - $99,999",
                            incwage %in% c(100000:149999) ~ "$100,000 - $149,999",
                            incwage >= 150000 ~ "$150,000 and over")) %>%
  count(income) %>%
  group_by(income)
           
# Saving the census data as a csv file in my
# working directory
# write_csv(_data, "_census_data.csv")



         