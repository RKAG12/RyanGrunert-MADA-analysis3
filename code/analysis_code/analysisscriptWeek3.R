###############################
# Analysis script 3
#The following is the analysis script for week 3 of this project.
#This is for Module 11: Machine Learning Models I

#load needed packages. make sure they are installed.
library(tidyverse)
library(tidymodels)

#path to data
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
df <- readRDS(data_location)

#Pre-processing

###Feature/Variable removal
#Removing the yes/no variables for Weakness, Cough, and Myalgia
df2 <- subset(df, select = -c(CoughYN, WeaknessYN, CoughYN2, MyalgiaYN))

#Coding the three ordered factors Weakness, CoughIntensity, Myalgia
 df2 <- mutate(df2, Weakness = factor(Weakness, levels = c("None", "Mild",
                                                              "Moderate","Severe"),ordered = TRUE))
 df2 <- mutate(df2, CoughIntensity = factor(CoughIntensity, levels = c("None", "Mild",
                                                             "Moderate","Severe"),ordered = TRUE))
 df2 <- mutate(df2, Myalgia = factor(Myalgia, levels = c("None", "Mild",
                                                             "Moderate","Severe"),ordered = TRUE))
   
#Removing unbalanced binary predictors with <50 entries in one category
df2 <- subset(df2, select = -c(Hearing, Vision))

data_location2 <- here::here("data","processed_data","PrePanalysisdata.rds")
saveRDS(df2, file = data_location2)









