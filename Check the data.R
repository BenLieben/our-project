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

#Want to account in some way for the fact that people are surveyed at different times of the year
#So I chose 4 years, 6 months - these kids are almost certainly too old to enroll if they are not already in the program
#Estimates in the paper are not sensitive to this rule
age_restriction <- age_combined %>%   #ages are in months and they chose 4.5 years --> 18.5*12 = 222
  filter(Age_correct >= 222)
#still 5365 observations left, this is possible


#Next restrict the sample to families with at least 2 age-eligible children;
#families with at least 2 age-eligible children, group_by mother
grouped_by_mother <- age_restriction %>%
  group_by(MotherID) %>%
  mutate(count = n()) %>%
  ungroup()

at_least_2_children_restriction <- grouped_by_mother %>%
  filter(count >= 2)
#3967 still now, needs to be higher than 3698, so still possible (269 too many)

#Exclude small number of kids who died prior to eligibility;
#86: 68, 88: 77, 90: 70, 104: 92
#why 104? 
excluded_the_ones_that_died <- at_least_2_children_restriction %>%
  filter(Res86 != 8) %>%
  filter(Res88 != 8) %>%
  filter(Res90 != 8)
#still 3879 after this --> too many, needs to be 3698

#Oversample?
#HowLong_HS

#still some more
final_data_set <- excluded_the_ones_that_died

#for covariates: select all the needed ones and use group_by for the categories at the top
#Race_Child: 1 Hispanic en 3 White samen, 2 voor Black
Hispanic <- excluded_the_ones_that_died %>%
  filter(Race_Child == 1) %>%
  mutate(Race_Child_Correct = "Hispanic")

White <- excluded_the_ones_that_died %>%
  filter(Race_Child == 3) %>%
  mutate(Race_Child_Correct = "White")

Black <- excluded_the_ones_that_died %>%
  filter(Race_Child == 2) %>%
  mutate(Race_Child_Correct = "Black")

races_combined <- bind_rows(Hispanic, White, Black, .id = NULL)

#Ever_HS, Ever_Preschool, None???
#Ever_HS --> 1, Ever_Preschool --> 1, None: bij ze alle twee 0????
HeadStart_Correct <- races_combined %>%
  mutate(HeadStart = Ever_HS88 + Ever_HS90) %>%
  filter(HeadStart >= 1) %>%
  mutate(Education = "Head Start")

Pre_School_Correct <- races_combined %>%
  mutate(Pre_School = Ever_Preschool88 + Ever_Preschool90) %>%
  filter(Pre_School >= 1) %>%
  mutate(Education = "Pre-School")
  
No_Headstart_or_Preschool <- races_combined %>%
  mutate(HeadStart = Ever_HS88 + Ever_HS90) %>%
  mutate(Pre_School = Ever_Preschool88 + Ever_Preschool90) %>%
  filter(HeadStart == 0) %>%
  filter(Pre_School == 0) %>%
  mutate(Education = "None")

education_combined <- bind_rows(HeadStart_Correct, Pre_School_Correct, No_Headstart, No_Preschool, .id = NULL)
#too much, something went wrong

#correct data:
#for table 2 and 5 (not necessary)
extra_data <- read_stata(paste(load.path, "Deming_cleaned_data_Table 2 and 5.dta", sep ="")) #loading in the data
attach(extra_data)
glimpse(extra_data)

#for table 1? and 4 (THIS ISN'T 3698 as mentioned in the paper) (age 18 also???)
correct_data <- read_stata(paste(load.path, "Deming_cleaned_data_up_to_Table 4.dta", sep ="")) #loading in the data
attach(correct_data)
glimpse(correct_data)

#HOW do we get to the numbers, just means and sd in a table?
#okay to replace all NA's with 0's?
#also need sample size

#variables on the left: Permanent Income, Mother<HS, Mother some college, Maternal AFQT, Grandmother's Education:
#Permanent Income = PermInc_std ???
#Mother<HS = MomHS ???
#Mother some college = MomSomeColl
#Maternal AFQT = impAFQT_std ???
#Grandmother's education = GMom_0to3_miss OR GMom_0to_3_imp ???

#grouping by: black/non-black (Black)
#HS/pre-school/none (new variable Education: HS2_FE90 == 1, Pre2_FE90 == 1, HS2_FE90 == 0, Pre2_FE90 == 0)


#fixed-effect: siblings differentially participate in Head Start, other preschools, or no preschool.
#first group_by mother, then add up all education, and two need to be 1 or higher???

#extra column: Head start—none diff. (in SD units) ???







#2)
#Check whether the types of variables are correctly identified. (Starting point for robustness analysis.)
#Check for outliers and potentially influential observations. (Starting point for robustness analysis.)
#Check basic associations between the variables.

