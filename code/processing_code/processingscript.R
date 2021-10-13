###############################
# Processing script
#
#this script loads the raw data, processes and cleans it 
#and saves it as Rds file in the processed_data folder

#load needed packages. make sure they are installed.
library(tidyverse)
library(here) #to set paths

#path to data
data_location <- here::here("data","raw_data","SympAct_Any_Pos.Rda") #Specifies the path to the data
df <- readRDS(file = data_location) #Loads the data

#Below selects the columns with names that do not include the six text strings, 
#and not the unique visit column,
#then removes any rows that have NA values. 
df2 <- df %>%
      select(-contains(c("Score", "Total", "FluA", "FluB", "Dxname", "Activity")), -(Unique.Visit)) %>%
      na.omit(df)

#Saves the cleaned and filtered dataset
save_data_location <- here::here("data", "processed_data", "processeddata.rds")
saveRDS(df2, file = save_data_location)

