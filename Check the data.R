#Check the data
install.packages("tidyverse")
install.packages("rlang")
library(tidyverse)
library(haven)

load.path <- "C:\\Users\\ben-l\\OneDrive\\Documenten\\2021-2022\\semester 2\\bachelorproject\\our project\\" #change this so it's how you want it
save.path <- "C:\\Users\\ben-l\\OneDrive\\Documenten\\2021-2022\\semester 2\\bachelorproject\\our project\\" #change this so it's how you want it

raw_data <- read_stata(paste(load.path, "data_Deming_2008_0217.dta", sep ="")) #loading in the data

#Check whether the types of variables are correctly identified.
#Check for outliers and potentially influential observations.
#Check basic associations between the variables.

glimpse(raw_data)

