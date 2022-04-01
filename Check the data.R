#Check the data
#install.packages("tidyverse")
#install.packages("rlang")
library(tidyverse)
library(haven)

load.path <- "C:\\Users\\ben-l\\OneDrive\\Documenten\\2021-2022\\semester 2\\bachelorproject\\our project\\" #change this so it's how you want it
save.path <- "C:\\Users\\ben-l\\OneDrive\\Documenten\\2021-2022\\semester 2\\bachelorproject\\our project\\" #change this so it's how you want it

raw_data <- read_stata(paste(load.path, "data_Deming_2008_0217.dta", sep ="")) #loading in the data

glimpse(raw_data)
attach(raw_data)

#1)
#First step in a replication are the descriptive tables.
###############################################################################################
#First create rules that establish sample eligibility;
#Rule is 5 years old by 1990, so that they will be 19 by 2004;
#Next restrict the sample to families with at least 2 age-eligible children;
#Finally restrict to families where at least one (but not all) children were in Head Start;
###############################################################################################

#change all NA values to 0:
raw_data[is.na(raw_data)] <- 0

#Rule is 5 years old by 1990, so that they will be 19 by 2004;  
#age needs to be > 4 or >= 5 by 1990 so they are > 18 or >= 19 by 2004: use a filter to select the correct values
#Replace missing age values with PPVT Age or age reported from the nearest survey year instead;
age_mo_given_104 <- raw_data %>%   #sometimes the Age_Mo is missing, always go back to the previous one and check
  filter(Age_Mo104 > 0) %>%
  mutate(Age_correct = Age_Mo104)   #Age_correct is the age in months in 2004

age_mo_given_102 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 > 0) %>%
  mutate(Age_correct = Age_Mo102 + 2*12)

age_mo_given_100 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 > 0) %>%
  mutate(Age_correct = Age_Mo100 + 4*12)

age_mo_given_98 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 == 0, Age_Mo98 > 0) %>%
  mutate(Age_correct = Age_Mo98 + 6*12)

age_mo_given_96 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 == 0, Age_Mo98 == 0, Age_Mo96 > 0) %>%
  mutate(Age_correct = Age_Mo96 + 8*12)

age_mo_given_94 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 == 0, Age_Mo98 == 0, Age_Mo96 == 0, Age_Mo94 > 0) %>%
  mutate(Age_correct = Age_Mo94 + 10*12)

age_mo_given_92 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 == 0, Age_Mo98 == 0, Age_Mo96 == 0, Age_Mo94 == 0, Age_Mo92 > 0) %>%
  mutate(Age_correct = Age_Mo92 + 12*12)

age_mo_given_90 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 == 0, Age_Mo98 == 0, Age_Mo96 == 0, Age_Mo94 == 0, Age_Mo92 == 0, Age_Mo90 > 0) %>%
  mutate(Age_correct = Age_Mo90 + 14*12)

age_mo_given_88 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 == 0, Age_Mo98 == 0, Age_Mo96 == 0, Age_Mo94 == 0, Age_Mo92 == 0, Age_Mo90 == 0,
         Age_Mo88 > 0) %>%
  mutate(Age_correct = Age_Mo88 + 16*12)

age_mo_given_86 <- raw_data %>%
  filter(Age_Mo104 == 0, Age_Mo102 == 0, Age_Mo100 == 0, Age_Mo98 == 0, Age_Mo96 == 0, Age_Mo94 == 0, Age_Mo92 == 0, Age_Mo90 == 0,
         Age_Mo88 == 0, Age_Mo86 > 0) %>%
  mutate(Age_correct = Age_Mo86 + 18*12)


age_PPVT_given_86 <-raw_data %>%   #if the age isn't given, take the PPVTAge and convert it to 2004
  filter(Age_Mo86 == 0, PPVTAge86 > 0) %>%
  mutate(Age_correct = PPVTAge86 + 18*12)

age_PPVT_given_88 <-raw_data %>%   #if the age isn't given, take the PPVTAge and convert it to 2004
  filter(Age_Mo88 == 0, PPVTAge86 == 0, PPVTAge88 > 0) %>%   #the PPVTAge of the previous one also has to be 0, else we will add the age twice
  mutate(Age_correct = PPVTAge88 + 16*12)

age_PPVT_given_90 <-raw_data %>%
  filter(Age_Mo90 == 0, PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 > 0) %>%
  mutate(Age_correct = PPVTAge90 + 14*12)

age_PPVT_given_92 <-raw_data %>%
  filter(Age_Mo92 == 0, PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 == 0, PPVTAge92 > 0) %>%
  mutate(Age_correct = PPVTAge92 + 12*12)

age_PPVT_given_94 <-raw_data %>%
  filter(Age_Mo94 == 0, PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 == 0, PPVTAge92 == 0, PPVTAge94 > 0) %>%
  mutate(Age_correct = PPVTAge94 + 10*12)

age_PPVT_given_96 <-raw_data %>%
  filter(Age_Mo96 == 0, PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 == 0, PPVTAge92 == 0, PPVTAge94 == 0, PPVTAge96 > 0) %>%
  mutate(Age_correct = PPVTAge96 + 8*12)

age_PPVT_given_98 <-raw_data %>%
  filter(Age_Mo98 == 0, PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 == 0, PPVTAge92 == 0, PPVTAge94 == 0, PPVTAge96 == 0, PPVTAge98 > 0) %>%
  mutate(Age_correct = PPVTAge98 + 6*12)

age_PPVT_given_100 <-raw_data %>%
  filter(Age_Mo100 == 0, PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 == 0, PPVTAge92 == 0, PPVTAge94 == 0, PPVTAge96 == 0, PPVTAge98 == 0, 
         PPVTAge100 > 0) %>%
  mutate(Age_correct = PPVTAge100 + 4*12)

age_PPVT_given_102 <-raw_data %>%
  filter(Age_Mo102 == 0, PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 == 0, PPVTAge92 == 0, PPVTAge94 == 0, PPVTAge96 == 0, PPVTAge98 == 0, 
         PPVTAge100 == 0, PPVTAge102 > 0) %>%
  mutate(Age_correct = PPVTAge102 + 2*12)

age_PPVT_given_104 <-raw_data %>%
  filter(Age_Mo104 == 0,PPVTAge86 == 0, PPVTAge88 == 0, PPVTAge90 == 0, PPVTAge92 == 0, PPVTAge94 == 0, PPVTAge96 == 0, PPVTAge98 == 0, 
         PPVTAge100 == 0, PPVTAge102 == 0, PPVTAge104 > 0) %>%
  mutate(Age_correct = PPVTAge104)

#combining all these tables in to 1 table
age_combined <- bind_rows(age_mo_given_104, age_mo_given_102, age_mo_given_100, age_mo_given_98, age_mo_given_96, age_mo_given_94,
                          age_mo_given_92, age_mo_given_90, age_mo_given_88, age_mo_given_86, age_PPVT_given_86, age_PPVT_given_88, 
                          age_PPVT_given_90, age_PPVT_given_92, age_PPVT_given_94, age_PPVT_given_96, age_PPVT_given_98, 
                          age_PPVT_given_100, age_PPVT_given_102, age_PPVT_given_104, .id = NULL)

age_restriction <- age_combined %>%   #ages are in months so 19*12 = 228
  filter(Age_correct >= 228)
#still 5022 observations left, this is possible


#Next restrict the sample to families with at least 2 age-eligible children;
#families with at least 2 age-eligible children, group_by mother
at_least_two_restriction <- age_restriction%>%
  group_by(MotherID)%>%
  count()%>%
  filter(n >= 2)%>%
  ungroup() #doesn't work
#also, to little already


#for covariates: select all the needed ones and group_by


#2)
#Check whether the types of variables are correctly identified. (Starting point for robustness analysis.)
#Check for outliers and potentially influential observations. (Starting point for robustness analysis.)
#Check basic associations between the variables.

