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


#====================================================================
# STEP 3: EXPLORE AND CLEAN THE DATA 
#====================================================================

# explore all the data 
# create a function to check all through the distinct values
distinct_values <- function(n){
  for (i in n){
    print(n_distinct(i))
  }
}

values <- c(data.frame(activity$Id), data.frame(intensity_hour$Id),
            data.frame(intensities$Id), data.frame(calories$Id),
                  data.frame(calories_hour$Id), data.frame(sleep$Id),
            data.frame(steps$Id))

distinct_values(values)


# check if there are duplicates
sum(duplicated(activity))
sum(duplicated(calories))
sum(duplicated(intensities))
sum(duplicated(steps))
sum(duplicated(sleep))

# remove the duplicate in sleep
sleep <- unique(sleep)

sum(duplicated(sleep))
head(sleep)

# check the total number of distinct values of each dataset to combine them
table(activity$Id)
table(calories$Id)
table(intensities$Id)
table(intensity_hour$Id)
table(calories_hour$Id)
table(steps$Id)
table(sleep$id)

#====================================================================
# STEP 4: ANALYZE PHASE 
#====================================================================

# we need to make sense of all important target in the each dataset
head(activity)
head(calories)
head(calories_hour)
head(intensities)
head(intensity_hour)
head(steps)
head(sleep)

# find correlation in each dataset
# correlation in activity
cor(activity$TotalSteps, activity$TotalDistance,
    method = c("pearson","kendall", "spearman" ))

# lets get the summary some fields in the datasets
summary(activity$TotalDistance)
summary(calories$Calories)
colnames(intensities)
intensities %>%
  select(SedentaryMinutes, FairlyActiveMinutes, VeryActiveMinutes, LightlyActiveMinutes)%>%
  summary()
colnames(sleep)
sleep%>%
  select(TotalSleepRecords, TotalMinutesAsleep)%>%
  summary()
summary(steps$StepTotal)

#Observation

"Activites from the intensities dataset shows that most of the acticvities where
from the sedentary minutes then lightly active minutes

the observation shows that people using the device sit most of the time and just 
do minimal activity 

The average number of calories burn is in line with the standard avergae burnt
a day
"

# Next I want to figure out what time that people are most active,
#Average intensities by time
intensities_avg_by_time <- intensity_hour %>%
  group_by(time) %>%
  drop_na()%>%
  summarise(mean_intensity = mean(TotalIntensity))

print(intensities_avg_by_time)
dim(intensities_avg_by_time)

#Average calories burnt by time
calories_avg_by_time <- calories_hour %>%
  group_by(time)%>%
  drop_na()%>%
  summarise(mean_calories = mean(Calories))

View(calories_avg_by_time)

# combine intensities_avg_by_time and calories_avg_by_time fields together

avg_intensities_calories_by_time <- cbind(intensities_avg_by_time,
                                          mean_calories = calories_avg_by_time$mean_calories)

View(avg_intensities_calories_by_time)

write.csv(avg_intensities_calories_by_time,
 file="C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/Fitabase_Data/avg_intensities_calories_by_time.csv" )

# Explore sleep patterns if there are any strange occurence 

sleep$weekday <- wday(sleep$SleepDay, label = TRUE)

sleep_avg_by_weekday <- sleep%>%
  group_by(weekday)%>%
  summarise(mean_sleep_weekday = mean(TotalMinutesAsleep))

#find the average calories burnt by weekdays

calories$weekday <- wday(calories$ActivityDay, label=TRUE)
View(calories)

avg_calories_weekday <- calories%>%
  group_by(weekday)%>%
  summarise(mean_calories_weekday = mean(Calories))

avg_calories_weekday

#====================================================================
# STEP 5: VISUALIZE THE DATA 
#====================================================================

image01 <- intensities_avg_by_time%>%
  ggplot(aes(x=time, y=mean_intensity))+
  geom_col(aes(x=time, y=mean_intensity, fill=mean_intensity))+
  theme(axis.text.x = element_text(angle = 90, color ="blue"))+
  theme(axis.text.y = element_text(color = "blue"))+
  labs(title= "Average intensity by time")

image01

image02 <- calories_avg_by_time %>%
  ggplot(aes(x=time, y=mean_calories))+
  geom_col(aes(x=time, y=mean_calories, fill=mean_calories))+
  theme(axis.text.x = element_text(angle = 90, color = "blue"))+
  theme(axis.text.y = element_text(color="blue"))+
  labs(title = "Average calories by time")

image02
View(activity)

# create pie chart that shows the average intensities

mean_sedenary_minutes <- mean(intensities$SedentaryMinutes)
mean_Lightly_active_minutes <- mean(intensities$LightlyActiveMinutes)
mean_fairly_active_minutes <- mean(intensities$FairlyActiveMinutes)
mean_very_active_minutes <- mean(intensities$VeryActiveMinutes)

data <- data.frame(
  mean_values = c(mean_sedenary_minutes, mean_Lightly_active_minutes,
                  mean_fairly_active_minutes, mean_very_active_minutes),
  data_label=c("mean_sedenary_minutes", "mean_Lightly_active_minutes",
          "mean_fairly_active_minutes", "mean_very_active_minutes")
)

data

data <- data%>%
  arrange(desc(label))%>%
  mutate(percent_mean = mean_values/sum(data$mean_values) *100)%>%
  mutate(ypos = cumsum(percent_mean)-0.5*percent_mean)
  
data
data%>%
  ggplot(aes(x="", y=percent_mean, fill=data_label))+
  geom_bar(stat="identity", width=1, color="white")+
  coord_polar("y", start=0)+
  theme_void()