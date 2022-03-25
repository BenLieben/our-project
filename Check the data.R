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

variable_list <- list(ChildID-hhID)
#missing variables als NA laten??

#First create rules that establish sample eligibility;
#Rule is 5 years old by 1990, so that they will be 19 by 2004;
#Next restrict the sample to families with at least 2 age-eligible children;
#Finally restrict to families where at least one (but not all) children were in Head Start;

#Replace missing age values with PPVT Age or age reported from the nearest survey year instead;

#gen in stata is mutate in R
#replace(x, list, values)     x = vactor haing some values, list = this can be an index vector, Values = the replacement values
#if (test_expression) {
#statement
#}
for(variable in variable_list){
  a
}
  






#2)
#Check whether the types of variables are correctly identified. (Starting point for robustness analysis.)
#Check for outliers and potentially influential observations. (Starting point for robustness analysis.)
#Check basic associations between the variables.

