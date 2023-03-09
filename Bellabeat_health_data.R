# Install packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("dplyr")
install.packages("lubridate")
install.packages("ggpubr")

# import library
library(tidyverse)
library(tidyr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(ggpubr)
getwd()

#====================================================================
# STEP 1: COLLECT DATA
#====================================================================

activity <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/dailyActivity_merged.csv"))
calories <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/dailyCalories_merged.csv"))
intensities <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/dailyIntensities_merged.csv"))
intensity_hour <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/hourlyIntensities_merged.csv"))
steps <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/dailySteps_merged.csv"))
calories_hour <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/hourlyCalories_merged.csv"))
sleep <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/sleepDay_merged.csv"))

str(activity)
str(calories)
str(intensities)
str(intensity_hour)
str(steps)
str(calories_hour)
str(sleep)

#====================================================================
# STEP 2: FORMAT DATA 
#====================================================================
"OBSERVATION: The format of recorded date in those file
is chr -> Need to changed"

View(activity)
View(calories)
View(intensities)
View(steps)
View(calories_hour)
View(sleep)
View(intensity_hour)

# Format the date for all the activity date in all the dataset
# For activity 
activity$ActivityDate <- as.Date(activity$ActivityDate, format="%m/%d/%y")
#for calories
calories$ActivityDay <- as.Date(calories$ActivityDay, format="%m/%d/%y")
#for intensities
intensities$ActivityDay <- as.Date(intensities$ActivityDay, format="%m/%d/%y")
# for steps
steps$ActivityDay <- as.Date(steps$ActivityDay, format="%m/%d/%y")

# for calories_hour: convert and split date and time
calories_hour$ActivityHour <- as.POSIXct(calories_hour$ActivityHour, 
                                         format="%m/%d/%Y %I:%M:%S %p", tz="GMT") 
str(calories_hour$ActivityHour)
# create a column for date only
calories_hour$date <- as.Date(calories_hour$ActivityHour, format="%m/%d/%y")
#create a column for time only
calories_hour$time <- format(calories_hour$ActivityHour, format="%H:%M:%S")

# for sleep
sleep$SleepDay <- as.Date(sleep$SleepDay, format="%m/%d/%y")

# format date and split into date and time
intensity_hour$ActivityHour <- as.POSIXct(intensity_hour$ActivityHour, 
                                          format="%m/%d/%Y %I:%M:%S %p", tz="GMT")

intensity_hour$date <- as.Date(intensity_hour$ActivityHour, format="%m/%d/%y")
intensity_hour$time <- format(intensity_hour$ActivityHour, format="%H:%M:%S")

str(intensity_hour$time)



