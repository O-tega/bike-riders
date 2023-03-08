# Install packages
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("ggpubr")

# import library
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
getwd()

#====================================================================
# STEP 1: COLLECT DATA
#====================================================================

Daily_activity_data <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/dailyActivity_merged.csv"))
Daily_calories_data <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/dailyCalories_merged.csv"))
Daily_intensities_data <- as_tibble(read_csv("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/dailyIntensities_merged.csv"))


