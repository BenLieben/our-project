#to update R:
#install.packages("installr")
#library(installr)
#updateR()
#install.packages("tidyverse")
#install.packages("haven")
#install.packages("rlang")
#install.packages("stargazer")
#install.packages("AER")
#install.packages("plm")
#install.packages("specr")
#install.packages("tidygraph")
library(tidyverse)
library(haven)
library(stargazer)
library(AER)
library(plm)
library(specr)
library(tidygraph)


load.path <- "C:\\Users\\ben-l\\OneDrive\\Documenten\\2021-2022\\semester 2\\bachelorproject\\our project\\" #change this so it's how you want it
save.path <- "C:\\Users\\ben-l\\OneDrive\\Documenten\\2021-2022\\semester 2\\bachelorproject\\our project\\" #change this so it's how you want it

raw_data <- read_stata(paste(load.path, "data_Deming_2008_0217.dta", sep ="")) #loading in the data
glimpse(raw_data)

#Get the correct data for the descriptive tables
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

#for covariates: select all the needed ones and use group_by for the categories at the top
#Race_Child: 1 for Hispanic and 3 for White (together), 2 for Black
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

#Head start: Ever_HS --> 1, Preschool: Ever_Preschool --> 1, None: if there's a 0 for both of them
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

education_combined <- bind_rows(HeadStart_Correct, Pre_School_Correct, No_Headstart_or_Preschool, .id = NULL)
#too much, something went wrong






#CORRECT DATA:
#for table 3 and 4 (still for table 4)
deming_table_4_data <- read_stata(paste(load.path, "Deming_cleaned_data_up_to_Table 4.dta", sep ="")) #loading in the data
glimpse(deming_table_4_data)

#FOR TABLE 1: (also for table 2 and 5)
#variables on the left: Permanent Income, Mother<HS, Mother some college, Maternal AFQT, Grandmother's Education:
#grouping by race and education:
deming <- read_stata(paste(load.path, "Deming_cleaned_data_Table 2 and 5.dta", sep ="")) #loading in the data
dim(deming)   #11470x1230
deming_new <- deming %>%
  mutate(Race = case_when(Black == 1 ~ "Black", NonBlack == 1 ~ "White/Hispanic"),
    preschool_status = case_when(HS2_90 == 1 ~ "Head Start", Pre2_90 == 1 ~ "Preschool", None2_90 == 1 ~ "None")) %>%
  select(Race, preschool_status, PermInc, MomDropout, MomSomeColl, AgeAFQT_std, HighGrade_GMom79) %>% #change name with mutate first?
  drop_na()
#should be 3698 observations


#table for mean:
deming_table1_mean <- deming_new %>%
  group_by(Race, preschool_status) %>%
  summarise(across(where(is.numeric), ~ mean(.x)), n = n())
print(deming_table1_mean)
#because of rounding, grandmother's education with preschool slightly different

#table for sd:
deming_table1_sd <- deming_new %>%
  group_by(Race, preschool_status) %>%
  summarise(across(where(is.numeric), ~ sd(.x)), n = n())
print(deming_table1_sd)
#because of rounding, mother some coll with HS (white) slightly different


#fixed-effect: siblings differently participate in Head Start, other preschools, or no preschool.
fixed_effects <- deming %>%
  filter(Elig2_90 == 1)

fixed_effects_new <- fixed_effects %>%
  mutate(Race = case_when(Black == 1 ~ "Black", NonBlack == 1 ~ "White/Hispanic"),
         preschool_status = case_when(HS2_FE90 == 1 ~ "Head Start", Pre2_FE90 == 1 ~ "Preschool", None2_FE90 == 1 ~ "None")) %>%
  select(Race, preschool_status, PermInc, MomDropout, MomSomeColl, AgeAFQT_std, HighGrade_GMom79) %>%
  drop_na()
#should be 1663 observations


#table for mean:
fixed_effects_table1_mean <- fixed_effects_new %>%
  group_by(Race, preschool_status) %>%
  summarise(across(where(is.numeric), ~ mean(.x)), n = n())
print(fixed_effects_table1_mean)
#because of rounding, grandmother's education with preschool slightly different

#table for sd:
fixed_effects_table1_sd <- fixed_effects_new %>%
  group_by(Race, preschool_status) %>%
  summarise(across(where(is.numeric), ~ sd(.x)), n = n())
print(fixed_effects_table1_sd)

#We used excel to put it in 1 big table like they have in the paper and we compare the values
#All the values are pretty much the same


#FOR TABLE 2: deming data set, different names, each row is a regression
#The paper filters the data for a specific subsample.
glimpse(deming)

#for Attrit and pretreatmentindex: (like in the Stata code)
deming_table_2_Attrit_and_pretreatmentindex <- deming %>%
  filter((is.na(HS2_FE90) == F) & MotherID != 12 & (Age2_Yr104>=19 | (DOB_Yr_Child==1985 & DOB_Mo_Child<9)))
dim(deming_table_2_Attrit_and_pretreatmentindex) #1722x1230


#for all the rest: (like in the Stata code)
deming_table_2_rest <- deming %>%
  filter(Sample90_2 == 1)
dim(deming_table_2_rest) #1251x1230


#Attrited: (correct)
table2_Attrit <- plm(data = deming_table_2_Attrit_and_pretreatmentindex,
                       Attrit ~ HS2_FE90 + Pre2_FE90,
                       model = "within",
                       index = c("MotherID"),
                       stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef1 <- coeftest(table2_Attrit, vcov. = vcovHC(table2_Attrit, type = "sss", cluster = "group"))
print(coef1)

#PPVT at age 3: (correct) (3 extra variables, like in the Stata code)
table2_PPVT_at3 <- plm(data = deming_table_2_rest,
                    PPVTat3 ~ HS2_FE90 + Pre2_FE90 + Male + FirstBorn + Age2_Mo90,
                    model = "within",
                    index = c("MotherID"),
                    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef2 <- coeftest(table2_PPVT_at3, vcov. = vcovHC(table2_PPVT_at3, type = "sss", cluster = "group"))
print(coef2)
#NO STAR FOR PRE, while they do have a star for pre in the paper
#slight differences in the sd, probably due to rounding

#log BW: (correct)
table2_logBW <- plm(data = deming_table_2_rest,
                    logBW ~ HS2_FE90 + Pre2_FE90,
                    model = "within",
                    index = c("MotherID"),
                    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef3 <- coeftest(table2_logBW, vcov. = vcovHC(table2_logBW, type = "sss", cluster = "group"))
print(coef3)

#very low BW: (correct)
table2_very_low_BW <- plm(data = deming_table_2_rest,
                    VLow_BW ~ HS2_FE90 + Pre2_FE90,
                    model = "within",
                    index = c("MotherID"),
                    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef4 <- coeftest(table2_very_low_BW, vcov. = vcovHC(table2_very_low_BW, type = "sss", cluster = "group"))
print(coef4)

#ln mother's HH 0-3: (= Res_0to3) (correct)
table2_ln_mother_HH_0to3 <- plm(data = deming_table_2_rest,
                          Res_0to3 ~ HS2_FE90 + Pre2_FE90,
                          model = "within",
                          index = c("MotherID"),
                          stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef5 <- coeftest(table2_ln_mother_HH_0to3, vcov. = vcovHC(table2_ln_mother_HH_0to3, type = "sss", cluster = "group"))
print(coef5)

#pre-existing limitation: (correct)
table2_pre_existing_lim <- plm(data = deming_table_2_rest,
                                HealthCond_before ~ HS2_FE90 + Pre2_FE90,
                                model = "within",
                                index = c("MotherID"),
                                stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef6 <- coeftest(table2_pre_existing_lim, vcov. = vcovHC(table2_pre_existing_lim, type = "sss", cluster = "group"))
print(coef6)

#firstborn: (correct)
table2_firstborn <- plm(data = deming_table_2_rest,
                               FirstBorn ~ HS2_FE90 + Pre2_FE90,
                               model = "within",
                               index = c("MotherID"),
                               stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef7 <- coeftest(table2_firstborn, vcov. = vcovHC(table2_firstborn, type = "sss", cluster = "group"))
print(coef7)

#male: (correct)
table2_male <- plm(data = deming_table_2_rest,
                        Male ~ HS2_FE90 + Pre2_FE90,
                        model = "within",
                        index = c("MotherID"),
                        stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef8 <- coeftest(table2_male, vcov. = vcovHC(table2_male, type = "sss", cluster = "group"))
print(coef8)

#age in 2004 (in years): (correct)
table2_age_in_2004 <- plm(data = deming_table_2_rest,
                   Age2_Yr104 ~ HS2_FE90 + Pre2_FE90,
                   model = "within",
                   index = c("MotherID"),
                   stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef9 <- coeftest(table2_age_in_2004, vcov. = vcovHC(table2_age_in_2004, type = "sss", cluster = "group"))
print(coef9)

#HOME score, age 3: (correct)
table2_HOMEscore_at3 <- plm(data = deming_table_2_rest,
                          HOME_Pct_0to3 ~ HS2_FE90 + Pre2_FE90,
                          model = "within",
                          index = c("MotherID"),
                          stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef10 <- coeftest(table2_HOMEscore_at3, vcov. = vcovHC(table2_HOMEscore_at3, type = "sss", cluster = "group"))
print(coef10)
#slight differences in the sd for preschool, probably due to rounding

#Father in HH, 0–3: (correct)
table2_father_in_HH_0to3 <- plm(data = deming_table_2_rest,
                            Father_HH_0to3 ~ HS2_FE90 + Pre2_FE90,
                            model = "within",
                            index = c("MotherID"),
                            stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef11 <- coeftest(table2_father_in_HH_0to3, vcov. = vcovHC(table2_father_in_HH_0to3, type = "sss", cluster = "group"))
print(coef11)

#Grandmother in HH, 0–3: (correct)
table2_GMother_in_HH_0to3 <- plm(data = deming_table_2_rest,
                                GMom_0to3 ~ HS2_FE90 + Pre2_FE90,
                                model = "within",
                                index = c("MotherID"),
                                stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef12 <- coeftest(table2_GMother_in_HH_0to3, vcov. = vcovHC(table2_GMother_in_HH_0to3, type = "sss", cluster = "group"))
print(coef12)

#Maternal care, age 0–3: (correct)
table2_maternal_care_0to3 <- plm(data = deming_table_2_rest,
                                 MomCare ~ HS2_FE90 + Pre2_FE90,
                                 model = "within",
                                 index = c("MotherID"),
                                 stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef13 <- coeftest(table2_maternal_care_0to3, vcov. = vcovHC(table2_maternal_care_0to3, type = "sss", cluster = "group"))
print(coef13)

#Relative care, age 0–3: (correct)
table2_relative_care_0to3 <- plm(data = deming_table_2_rest,
                                 RelCare ~ HS2_FE90 + Pre2_FE90,
                                 model = "within",
                                 index = c("MotherID"),
                                 stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef14 <- coeftest(table2_relative_care_0to3, vcov. = vcovHC(table2_relative_care_0to3, type = "sss", cluster = "group"))
print(coef14)

#Nonrelative care, age 0–3: (correct)
table2_non_relative_care_0to3 <- plm(data = deming_table_2_rest,
                                 NonRelCare ~ HS2_FE90 + Pre2_FE90,
                                 model = "within",
                                 index = c("MotherID"),
                                 stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef15 <- coeftest(table2_non_relative_care_0to3, vcov. = vcovHC(table2_non_relative_care_0to3, type = "sss", cluster = "group"))
print(coef15)

#Breastfed: (correct)
table2_breastfed <- plm(data = deming_table_2_rest,
                                     Breastfed ~ HS2_FE90 + Pre2_FE90,
                                     model = "within",
                                     index = c("MotherID"),
                                     stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef16 <- coeftest(table2_breastfed, vcov. = vcovHC(table2_breastfed, type = "sss", cluster = "group"))
print(coef16)

#Regular doctor’s visits, age 0–3: (correct)
table2_regular_doctor <- plm(data = deming_table_2_rest,
                        Doctor_0to3 ~ HS2_FE90 + Pre2_FE90,
                        model = "within",
                        index = c("MotherID"),
                        stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef17 <- coeftest(table2_regular_doctor, vcov. = vcovHC(table2_regular_doctor, type = "sss", cluster = "group"))
print(coef17)

#Ever been to dentist, age 0–3: (correct)
table2_ever_dentist <- plm(data = deming_table_2_rest,
                             Dentist_0to3 ~ HS2_FE90 + Pre2_FE90,
                             model = "within",
                             index = c("MotherID"),
                             stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef18 <- coeftest(table2_ever_dentist, vcov. = vcovHC(table2_ever_dentist, type = "sss", cluster = "group"))
print(coef18)

#Weight change during pregnancy: (correct)
table2_weight_change <- plm(data = deming_table_2_rest,
                           Moth_WeightChange ~ HS2_FE90 + Pre2_FE90,
                           model = "within",
                           index = c("MotherID"),
                           stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef19 <- coeftest(table2_weight_change, vcov. = vcovHC(table2_weight_change, type = "sss", cluster = "group"))
print(coef19)
#slight differences in the sd preschool, probably due to rounding

#Child illness, age 0–1: (correct)
table2_child_illness <- plm(data = deming_table_2_rest,
                            Illness_1stYr ~ HS2_FE90 + Pre2_FE90,
                            model = "within",
                            index = c("MotherID"),
                            stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef20 <- coeftest(table2_child_illness, vcov. = vcovHC(table2_child_illness, type = "sss", cluster = "group"))
print(coef20)

#Premature birth: (correct)
table2_premature_birth <- plm(data = deming_table_2_rest,
                            Premature ~ HS2_FE90 + Pre2_FE90,
                            model = "within",
                            index = c("MotherID"),
                            stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef21 <- coeftest(table2_premature_birth, vcov. = vcovHC(table2_premature_birth, type = "sss", cluster = "group"))
print(coef21)

#Private health insurance, age 0–3: (correct)
table2_health_insurance <- plm(data = deming_table_2_rest,
                              Insurance_0to3 ~ HS2_FE90 + Pre2_FE90,
                              model = "within",
                              index = c("MotherID"),
                              stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef22 <- coeftest(table2_health_insurance, vcov. = vcovHC(table2_health_insurance, type = "sss", cluster = "group"))
print(coef22)

#Medicaid, age 0–3: (correct)
table2_medicaid_0to3 <- plm(data = deming_table_2_rest,
                               Medicaid_0to3 ~ HS2_FE90 + Pre2_FE90,
                               model = "within",
                               index = c("MotherID"),
                               stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef23 <- coeftest(table2_medicaid_0to3, vcov. = vcovHC(table2_medicaid_0to3, type = "sss", cluster = "group"))
print(coef23)

#ln (income), age 0–3: (correct)
table2_log_income_0to3 <- plm(data = deming_table_2_rest,
                            LogInc_0to3 ~ HS2_FE90 + Pre2_FE90,
                            model = "within",
                            index = c("MotherID"),
                            stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef24 <- coeftest(table2_log_income_0to3, vcov. = vcovHC(table2_log_income_0to3, type = "sss", cluster = "group"))
print(coef24)

#ln (income), age 3: (correct)
table2_log_income_at3 <- plm(data = deming_table_2_rest,
                              LogIncAt3 ~ HS2_FE90 + Pre2_FE90,
                              model = "within",
                              index = c("MotherID"),
                              stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef25 <- coeftest(table2_log_income_at3, vcov. = vcovHC(table2_log_income_at3, type = "sss", cluster = "group"))
print(coef25)

#Mom average hours worked, year before birth: (correct)
table2_avg_hours_worked_bef_birth <- plm(data = deming_table_2_rest,
                             Moth_HrsWorked_BefBirth ~ HS2_FE90 + Pre2_FE90,
                             model = "within",
                             index = c("MotherID"),
                             stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef26 <- coeftest(table2_avg_hours_worked_bef_birth, vcov. = vcovHC(table2_avg_hours_worked_bef_birth, type = "sss", cluster = "group"))
print(coef26)
#slight differences in the sd, probably due to rounding

#Mom average hours worked, age 0–1: (correct)
table2_avg_hours_worked_0to1 <- plm(data = deming_table_2_rest,
                                         Moth_HrsWorked_0to1 ~ HS2_FE90 + Pre2_FE90,
                                         model = "within",
                                         index = c("MotherID"),
                                         stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef27 <- coeftest(table2_avg_hours_worked_0to1, vcov. = vcovHC(table2_avg_hours_worked_0to1, type = "sss", cluster = "group"))
print(coef27)

#Mom smoked before birth: (correct)
table2_smoked_bef_birth <- plm(data = deming_table_2_rest,
                                    Moth_Smoke_BefBirth ~ HS2_FE90 + Pre2_FE90,
                                    model = "within",
                                    index = c("MotherID"),
                                    stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef28 <- coeftest(table2_smoked_bef_birth, vcov. = vcovHC(table2_smoked_bef_birth, type = "sss", cluster = "group"))
print(coef28)

#Mom drank before birth: (correct)
table2_drank_bef_birth <- plm(data = deming_table_2_rest,
                               Alc_BefBirth ~ HS2_FE90 + Pre2_FE90,
                               model = "within",
                               index = c("MotherID"),
                               stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef29 <- coeftest(table2_drank_bef_birth, vcov. = vcovHC(table2_drank_bef_birth, type = "sss", cluster = "group"))
print(coef29)

#Pre-treatment index: (correct)
table2_pre_treatment_index <- plm(data = deming_table_2_rest,
                              PreTreatIndex ~ HS2_FE90 + Pre2_FE90,
                              model = "within",
                              index = c("MotherID"),
                              stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
coef30 <- coeftest(table2_pre_treatment_index, vcov. = vcovHC(table2_pre_treatment_index, type = "sss", cluster = "group"))
print(coef30)

#FOR TABLE 3 and 4: (stargazer) (should be 4687/1251 sample size) (deming_table_4_data)
#TABLE 3:
#effect on test scores, by all HS_x and Pre_x, PermInc_std, impAFQT_std, MomHS, MomSomeColl
for_table_3 <- deming_table_4_data %>%
  select(Test_std, HS_5to6, HS_7to10, HS_11to14, Pre_5to6, Pre_7to10, Pre_11to14, PermInc_std, impAFQT_std, MomHS, MomSomeColl,
         HS2_FE90, Pre2_FE90, AgeTest_Yr, year, Group_5to6, Group_7to10, Group_11to14, MotherID,
         Attrit, PPVTat3_imp, logBW_imp, VLow_BW_imp, Res_0to3_imp, HealthCond_before_imp, FirstBorn_imp, Male, Age2_Yr104, HOME_Pct_0to3_imp,
         Father_HH_0to3_imp, GMom_0to3_imp, MomCare_imp, RelCare_imp, NonRelCare_imp, Breastfed_imp, Doctor_0to3_imp, Dentist_0to3_imp,
         Moth_WeightChange_imp, Illness_1stYr_imp, Premature_imp, Insurance_0to3_imp, Medicaid_0to3_imp, LogInc_0to3_imp, LogIncAt3_imp,
         Moth_HrsWorked_BefBirth_imp, Moth_HrsWorked_Avg_0to3_imp, Moth_HrsWorked_0to1_imp, Moth_Smoke_BefBirth_imp, Alc_BefBirth_imp, PreTreatIndex) %>%
  drop_na()
#In mother’s HH, 0–3 can't find it, describe it, is it Res_0to3_imp??? (Living in Mother's HouseHold 0-3 vs. Residence 0 - 3???)
#describe difficulty finding the variables


#COLUMN 1:
mod1 <- lm(data = for_table_3, Test_std ~ HS_5to6 + HS_7to10 + HS_11to14 + Pre_5to6 + Pre_7to10 + Pre_11to14 +
             Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr),
           index = c("MotherID"),
           stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1))

mod1_coeftest <- coeftest(mod1, vcov. = vcovCL, cluster =~ MotherID)
#We use a cluster robust se because they also used this in the paper

summary(mod1)
stargazer(mod1_coeftest, type = "text", digits =3)
#completely the same

#COLUMN 2:
mod2 <- lm(data = for_table_3, Test_std ~ HS_5to6 + HS_7to10 + HS_11to14 + Pre_5to6 + Pre_7to10 + Pre_11to14 +
             Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
             Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
             HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
             Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
             Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
             Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
           index = c("MotherID"),
           stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1))

mod2_coeftest <- coeftest(mod2, vcov. = vcovCL, cluster =~ MotherID)

summary(mod2)
stargazer(mod2_coeftest, type = "text", digits =3)
#some slight differences (less than 0.02 most often), 1 star wrong

#COLUMN 3:
mod3 <- lm(data = for_table_3, Test_std ~ HS_5to6 + HS_7to10 + HS_11to14 + Pre_5to6 + Pre_7to10 + Pre_11to14 +
             PermInc_std + impAFQT_std + MomHS + MomSomeColl +
             Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
             Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
             HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
             Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
             Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
             Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
           index = c("MotherID"),
           stars = c('***' = 0.01, '**' = 0.05, '*' = 0.1))

mod3_coeftest <- coeftest(mod3, vcov. = vcovCL, cluster =~ MotherID)

summary(mod3)
stargazer(mod3_coeftest, type = "text", digits =3)
#some slight differences (less than 0.02 most often), 1 star wrong

#COLUMN 4:
mod4 <- plm(data = for_table_3, Test_std ~ HS_5to6 + HS_7to10 + HS_11to14 + Pre_5to6 + Pre_7to10 + Pre_11to14 +
              Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr),
            model = "within",
            effect = "individual",
            index = c("MotherID"),
            stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))

mod4_coeftest <- coeftest(mod4, vcov. = vcovHC(mod4, type = "sss", cluster = "group"))
#type = "sss" employs the small sample correction as used by Stata
#Observations may be clustered by "group" (or "time") to account for serial (cross-sectional) correlation

summary(mod4)
stargazer(mod4_coeftest, type = "text", digits = 3)
#completely the same
  
#COLUMN 5:
mod5 <- plm(data = for_table_3, Test_std ~ HS_5to6 + HS_7to10 + HS_11to14 + Pre_5to6 + Pre_7to10 + Pre_11to14 +
              Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
             Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
             HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
             Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
             Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
             Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
            model = "within",
            effect = "individual",
            index = c("MotherID"),
            stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))

mod5_coeftest <- coeftest(mod5, vcov. = vcovHC(mod5, type = "sss", cluster = "group"))

summary(mod5)
stargazer(mod5_coeftest, type = "text", digits = 3)
#some slight differences (less than 0.02 most often), 1 star wrong

#FULL TABLE 3:
stargazer(mod1_coeftest, mod2_coeftest, mod3_coeftest, mod4_coeftest, mod5_coeftest, type = "text", digits =3)
#column 1 and 4 completely the same, 2, 3 and 5 a bit off. Just difference between Stata and Rstudio?? We did everything the same.


#TABLE 4:
#HS_5to6 + HS_7to10 + HS_11to14 in a new variable HS_5to14, Pre_5to6 + Pre_7to10 + Pre_11to14 in a new variable Pre_5to14
#same for by race, by gender, by maternal AFQT

for_table_4 <- deming_table_4_data %>%
  mutate(HS_5to14 = HS_5to6 + HS_7to10 + HS_11to14, Pre_5to14 = Pre_5to6 + Pre_7to10 + Pre_11to14,
         HS_Black_5to14 = HS_Black_5to6 + HS_Black_7to10 + HS_Black_11to14,
         HS_NonBlack_5to14 = HS_NonBlack_5to6 + HS_NonBlack_7to10 + HS_NonBlack_11to14,
         HS_Male_5to14 = HS_Male_5to6 + HS_Male_7to10 + HS_Male_11to14,
         HS_NonMale_5to14 = HS_NonMale_5to6 + HS_NonMale_7to10 + HS_NonMale_11to14,
         HS_lowAFQT_5to_14 = HS_lowAFQT_5to6 + HS_lowAFQT_7to10 + HS_lowAFQT_11to14,
         HS_NonlowAFQT_5to14 = HS_NonlowAFQT_5to6, HS_NonlowAFQT_7to10, HS_NonlowAFQT_11to14) %>%
  select(Test_std, HS_5to6, HS_7to10, HS_11to14, HS_5to14, Pre_5to6, Pre_7to10, Pre_11to14, Pre_5to14, PermInc_std, impAFQT_std, MomHS, MomSomeColl,
         HS_Black_5to6, HS_Black_7to10, HS_Black_11to14, HS_Black_5to14, HS_NonBlack_5to6, HS_NonBlack_7to10, HS_NonBlack_11to14, HS_NonBlack_5to14,
         HS_Male_5to6, HS_Male_7to10, HS_Male_11to14, HS_Male_5to14, HS_NonMale_5to6, HS_NonMale_7to10, HS_NonMale_11to14, HS_NonMale_5to14,
         HS_lowAFQT_5to6, HS_lowAFQT_7to10, HS_lowAFQT_11to14, HS_lowAFQT_5to_14, HS_NonlowAFQT_5to6, HS_NonlowAFQT_7to10, HS_NonlowAFQT_11to14, HS_NonlowAFQT_5to14,
         HS2_FE90, Pre2_FE90, AgeTest_Yr, year, Group_5to6, Group_7to10, Group_11to14, MotherID,
         Attrit, PPVTat3_imp, logBW_imp, VLow_BW_imp, Res_0to3_imp, HealthCond_before_imp, FirstBorn_imp, Male, Age2_Yr104, HOME_Pct_0to3_imp,
         Father_HH_0to3_imp, GMom_0to3_imp, MomCare_imp, RelCare_imp, NonRelCare_imp, Breastfed_imp, Doctor_0to3_imp, Dentist_0to3_imp,
         Moth_WeightChange_imp, Illness_1stYr_imp, Premature_imp, Insurance_0to3_imp, Medicaid_0to3_imp, LogInc_0to3_imp, LogIncAt3_imp,
         Moth_HrsWorked_BefBirth_imp, Moth_HrsWorked_0to1_imp, Moth_Smoke_BefBirth_imp, Alc_BefBirth_imp, PreTreatIndex) %>%
  drop_na()
attach(for_table_4)


#4A overall:
#for 5 - 6, 7 - 10, 11 - 14:
mod1_for_4a <- plm(data = for_table_4, Test_std ~ HS_5to6 + HS_7to10 + HS_11to14 + Pre_5to6 + Pre_7to10 + Pre_11to14 +
                     Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
                     Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
                     HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
                     Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
                     Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
                     Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
                   model = "within",
                   effect = "individual",
                   index = c("MotherID"),
                   stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod1_for_4a)

mod1_for_4a_coeftest <- coeftest(mod1_for_4a, vcov. = vcovHC(mod1_for_4a, type = "sss", cluster = "group"))
stargazer(mod1_for_4a_coeftest , type = "text", digits = 3)

#for 5 - 14:
mod2_for_4a <- plm(data = for_table_4, Test_std ~ HS_5to14 + Pre_5to14 +
                    Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
                    Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
                    HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
                    Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
                    Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
                    Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
                  model = "within",
                  effect = "individual",
                  index = c("MotherID"),
                  stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod2_for_4a)

mod2_for_4a_coeftest <- coeftest(mod2_for_4a, vcov. = vcovHC(mod2_for_4a, type = "sss", cluster = "group"))
stargazer(mod2_for_4a_coeftest , type = "text", digits = 3)

#combined in 1:
stargazer(mod1_for_4a_coeftest, mod2_for_4a_coeftest , type = "text", digits = 3)


#4B by race:
#for 5 - 6, 7 - 10, 11 - 14:
mod1_for_4b <- plm(data = for_table_4, Test_std ~ HS_Black_5to6 + HS_Black_7to10 + HS_Black_11to14 + HS_NonBlack_5to6 + HS_NonBlack_7to10 + HS_NonBlack_11to14 +
              Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
              Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
              HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
              Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
              Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
              Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
            model = "within",
            effect = "individual",
            index = c("MotherID"),
            stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod1_for_4b)

mod1_for_4b_coeftest <- coeftest(mod1_for_4b, vcov. = vcovHC(mod1_for_4b, type = "sss", cluster = "group"))
stargazer(mod1_for_4b_coeftest , type = "text", digits = 3)

#for 5 - 14
mod2_for_4b <- plm(data = for_table_4, Test_std ~ HS_Black_5to14 + HS_NonBlack_5to14 +
                     Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
                     Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
                     HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
                     Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
                     Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
                     Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
                   model = "within",
                   effect = "individual",
                   index = c("MotherID"),
                   stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod2_for_4b)

mod2_for_4b_coeftest <- coeftest(mod2_for_4b, vcov. = vcovHC(mod2_for_4b, type = "sss", cluster = "group"))
stargazer(mod2_for_4b_coeftest , type = "text", digits = 3)

#combined in 1:
stargazer(mod1_for_4b_coeftest, mod2_for_4b_coeftest , type = "text", digits = 3)


#4C by gender:
#for 5 - 6, 7 - 10, 11 - 14:
mod1_for_4c <- plm(data = for_table_4, Test_std ~ HS_Male_5to6 + HS_Male_7to10 + HS_Male_11to14 + HS_NonMale_5to6 + HS_NonMale_7to10 + HS_NonMale_11to14 +
                     Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
                     Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
                     HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
                     Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
                     Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
                     Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
                   model = "within",
                   effect = "individual",
                   index = c("MotherID"),
                   stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod1_for_4c)

mod1_for_4c_coeftest <- coeftest(mod1_for_4c, vcov. = vcovHC(mod1_for_4c, type = "sss", cluster = "group"))
stargazer(mod1_for_4c_coeftest , type = "text", digits = 3)

#for 5 - 14
mod2_for_4c <- plm(data = for_table_4, Test_std ~ HS_Male_5to14 + HS_NonMale_5to14 +
                     Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
                     Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
                     HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
                     Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
                     Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
                     Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
                   model = "within",
                   effect = "individual",
                   index = c("MotherID"),
                   stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod2_for_4c)

mod2_for_4c_coeftest <- coeftest(mod2_for_4c, vcov. = vcovHC(mod2_for_4c, type = "sss", cluster = "group"))
stargazer(mod2_for_4c_coeftest , type = "text", digits = 3)

#combined in 1:
stargazer(mod1_for_4c_coeftest, mod2_for_4c_coeftest , type = "text", digits = 3)


#4D by AFQT:
#for 5 - 6, 7 - 10, 11 - 14:
mod1_for_4d <- plm(data = for_table_4, Test_std ~ HS_lowAFQT_5to6 + HS_lowAFQT_7to10 + HS_lowAFQT_11to14 + HS_NonlowAFQT_5to6 + HS_NonlowAFQT_7to10 + HS_NonlowAFQT_11to14 +
                     Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
                     Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
                     HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
                     Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
                     Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
                     Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
                   model = "within",
                   effect = "individual",
                   index = c("MotherID"),
                   stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod1_for_4d)

mod1_for_4d_coeftest <- coeftest(mod1_for_4d, vcov. = vcovHC(mod1_for_4d, type = "sss", cluster = "group"))
stargazer(mod1_for_4d_coeftest , type = "text", digits = 3)

#for 5 - 14
mod2_for_4d <- plm(data = for_table_4, Test_std ~ HS_lowAFQT_5to_14 + HS_NonlowAFQT_5to14 +
                     Male + factor(year) + Group_7to10 + Group_11to14 + factor(AgeTest_Yr) +
                     Attrit + PPVTat3_imp + logBW_imp + VLow_BW_imp + Res_0to3_imp + HealthCond_before_imp + FirstBorn_imp + Male + Age2_Yr104 +
                     HOME_Pct_0to3_imp + Father_HH_0to3_imp + GMom_0to3_imp + MomCare_imp + RelCare_imp + NonRelCare_imp + Breastfed_imp +
                     Doctor_0to3_imp + Dentist_0to3_imp + Moth_WeightChange_imp + Illness_1stYr_imp + Premature_imp + Insurance_0to3_imp +
                     Medicaid_0to3_imp + LogInc_0to3_imp + LogIncAt3_imp + Moth_HrsWorked_BefBirth_imp + Moth_HrsWorked_0to1_imp +
                     Moth_Smoke_BefBirth_imp + Alc_BefBirth_imp + PreTreatIndex,
                   model = "within",
                   effect = "individual",
                   index = c("MotherID"),
                   stars = c('*' = 0.10, '**' = 0.05, '***' = 0.01))
summary(mod2_for_4d)

mod2_for_4d_coeftest <- coeftest(mod2_for_4d, vcov. = vcovHC(mod2_for_4d, type = "sss", cluster = "group"))
stargazer(mod2_for_4d_coeftest , type = "text", digits = 3)

#combined in 1:
stargazer(mod1_for_4d_coeftest, mod2_for_4d_coeftest , type = "text", digits = 3)

#We combined this all in 1 big table in excel
#For the values, again almost always less than 0.02 away, some differences in stars (highlighted in excel)


#FOR SPECIFICATION CURVE ANALYSIS:

#For column 5 of table 3: plm-model
#need to put MotherID as first column and ChildID as the second column
for_specification_curve <- deming_table_4_data %>%
  mutate(year = as.factor(year), AgeTest_Yr = as.factor(AgeTest_Yr), MotherID = as.factor(MotherID), ChildID = as.factor(ChildID)) %>%
  select(MotherID, ChildID, year, Test_std, HS_5to6, HS_7to10, HS_11to14, Pre_5to6, Pre_7to10, Pre_11to14, Male, Group_7to10, Group_11to14, AgeTest_Yr,
         Attrit, PPVTat3_imp, logBW_imp, VLow_BW_imp, Res_0to3_imp, HealthCond_before_imp, FirstBorn_imp, Male, Age2_Yr104,
         HOME_Pct_0to3_imp, Father_HH_0to3_imp, GMom_0to3_imp, MomCare_imp, RelCare_imp, NonRelCare_imp, Breastfed_imp,
         Doctor_0to3_imp, Dentist_0to3_imp, Moth_WeightChange_imp, Illness_1stYr_imp, Premature_imp, Insurance_0to3_imp,
         Medicaid_0to3_imp, LogInc_0to3_imp, LogIncAt3_imp, Moth_HrsWorked_BefBirth_imp, Moth_HrsWorked_0to1_imp,
         Moth_Smoke_BefBirth_imp, Alc_BefBirth_imp, PreTreatIndex)
attach(for_specification_curve)

#remove duplicates
final <- for_specification_curve %>%
  filter(!duplicated(for_specification_curve[,1:2]))

#works, but just a lm model
#then, this will be column 2 of table 3
results_v2 <- run_specs(df = for_table_3, 
                        y = c("Test_std"), 
                        x = c("HS_5to6", "HS_7to10", "HS_11to14", "Pre_5to6", "Pre_7to10", "Pre_11to14"), 
                        model = c("lm"),
                        controls = c("Male", "factor(year)", "Group_7to10", "Group_11to14", "factor(AgeTest_Yr)",
                                     "Attrit", "PPVTat3_imp", "logBW_imp", "VLow_BW_imp", "Res_0to3_imp", "HealthCond_before_imp", "FirstBorn_imp",
                                     "Male", "Age2_Yr104", "HOME_Pct_0to3_imp", "Father_HH_0to3_imp", "GMom_0to3_imp", "MomCare_imp",
                                     "RelCare_imp", "NonRelCare_imp", "Breastfed_imp", "Doctor_0to3_imp", "Dentist_0to3_imp",
                                     "Moth_WeightChange_imp", "Illness_1stYr_imp", "Premature_imp", "Insurance_0to3_imp",
                                     "Medicaid_0to3_imp", "LogInc_0to3_imp", "LogIncAt3_imp", "Moth_HrsWorked_BefBirth_imp",
                                     "Moth_HrsWorked_0to1_imp", "Moth_Smoke_BefBirth_imp", "Alc_BefBirth_imp", "PreTreatIndex"))

plot_specs(results_v2)

#copied this function from the R-code provided by the prof
plm_entity_fe <- function(formula, data) {
                    plm(formula = formula,
                        data = data,
                        model = "within",
                        effect = "individual")
                    }

#works, this is column 5 of table 3
results_v1 <-run_specs(df = final, 
                     y = c("Test_std"), 
                     x = c("HS_5to6", "HS_7to10", "HS_11to14", "Pre_5to6", "Pre_7to10", "Pre_11to14"), 
                     model = c("plm_entity_fe"),
                     controls = c("Male", "year", "Group_7to10", "Group_11to14", "AgeTest_Yr",
                                  "Attrit", "PPVTat3_imp", "logBW_imp", "VLow_BW_imp", "Res_0to3_imp", "HealthCond_before_imp", "FirstBorn_imp",
                                  "Male", "Age2_Yr104", "HOME_Pct_0to3_imp", "Father_HH_0to3_imp", "GMom_0to3_imp", "MomCare_imp",
                                  "RelCare_imp", "NonRelCare_imp", "Breastfed_imp", "Doctor_0to3_imp", "Dentist_0to3_imp",
                                  "Moth_WeightChange_imp", "Illness_1stYr_imp", "Premature_imp", "Insurance_0to3_imp",
                                  "Medicaid_0to3_imp", "LogInc_0to3_imp", "LogIncAt3_imp", "Moth_HrsWorked_BefBirth_imp",
                                  "Moth_HrsWorked_0to1_imp", "Moth_Smoke_BefBirth_imp", "Alc_BefBirth_imp", "PreTreatIndex"))

plot_specs(results_v1)


#end of the specification curve analysis




#2)
#Check whether the types of variables are correctly identified. (Starting point for robustness analysis.)
#Check for outliers and potentially influential observations. (Starting point for robustness analysis.)
#Check basic associations between the variables.