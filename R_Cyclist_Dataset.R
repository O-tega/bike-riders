# Install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggpubr")


# import library
library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggpubr)
getwd()
setwd("C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset")


#====================================================================
# STEP 1: COLLECT DATA
#====================================================================

Q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
Q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
Q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")
Q1_2020 <- read_csv("Divvy_Trips_2020_Q1.csv")

#====================================================================
# STEP 2: WRANGLE DATA AND COMBINE INTO A SINGLE FILE
#====================================================================

colnames(Q1_2020)
colnames(Q2_2019)
colnames(Q3_2019)
colnames(Q4_2019)

# Rename the column names
Q4_2019 <- rename(Q4_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_id = from_station_id,
                  start_station_name = from_station_name,
                  start_station_id = from_station_id,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

Q3_2019 <- rename(Q3_2019,
                  ride_id = trip_id,
                  rideable_type = bikeid,
                  started_at = start_time,
                  ended_at = end_time,
                  start_station_id = from_station_id,
                  start_station_name= from_station_name,
                  end_station_name = to_station_name,
                  end_station_id = to_station_id,
                  member_casual = usertype)

Q2_2019 <- rename(Q2_2019,
                  ride_id = "01 - Rental Details Rental ID" ,
                  rideable_type = "01 - Rental Details Bike ID",
                  started_at = "01 - Rental Details Local Start Time" ,
                  ended_at = "01 - Rental Details Local End Time" ,
                  start_station_name=  "03 - Rental Start Station Name" ,
                  start_station_id = "03 - Rental Start Station ID",
                  end_station_name =  "02 - Rental End Station Name" ,
                  end_station_id =  "02 - Rental End Station Name" ,
                  member_casual =  "User Type")


# Inspect dataframes for each dataset

str(Q4_2019)
str(Q3_2019)
str(Q2_2019)
str(Q1_2020)

# convert ride_id and rideable_type to charaacter so they can stack prorperly

Q4_2019 <- mutate(Q4_2019, ride_id= as.character(ride_id),
                  rideable_type=as.character(rideable_type))

Q3_2019 <- mutate(Q3_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))

Q2_2019 <- mutate(Q2_2019, ride_id = as.character(ride_id),
                  rideable_type = as.character(rideable_type))
str(Q2_2019)
str(Q1_2020)

Q1_2020 <- mutate(Q1_2020, end_station_id = as.character(end_station_id))
Q4_2019 <- mutate(Q4_2019, end_station_id = as.character(end_station_id))
Q3_2019 <- mutate(Q3_2019, end_station_id = as.character(end_station_id))
Q2_2019 <- mutate(Q2_2019, end_station_id = as.character(end_station_id))


# stack individual dataframe into one big dataframe
all_trips <- bind_rows(Q2_2019, Q3_2019, Q4_2019, Q1_2020)
colnames(all_trips)

# drop columns that are not useful to the analysis
all_trips <- all_trips %>%
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, 
            "01 - Rental Details Duration In Seconds Uncapped", 
            "05 - Member Details Member Birthday Year", "Member Gender",
            "tripduration"))

View(all_trips)

colnames(all_trips)

#====================================================================
# STEP 3: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#====================================================================

# get the total number of rows
nrow(all_trips)
# to get the dimension of the data
dim(all_trips)
# to get the number of rows specified from the top
head(all_trips)
summary(all_trips)

# get the unique characters of members_causual in the all_trips dataset
unique(all_trips$member_casual)
table(all_trips$member_casual)

# We just need two distinct usertype which is casual and member
# convert all customer to casual and subscriber to members
all_trips <- all_trips %>%
  mutate(member_casual = recode(member_casual,
                                "Subscriber" = "member",
                                "Customer" = "casual"))

table(all_trips$member_casual)

# for clearer description, lets add date, months and time to the dataset
# add date
all_trips$Date <- as.Date(all_trips$started_at)
all_trips$Month <- format(as.Date(all_trips$Date), "%m")
all_trips$Day <- format(as.Date(all_trips$Date), "%d")
all_trips$Year <- format(as.Date(all_trips$Date), "%Y")
all_trips$Week_day <- format(as.Date(all_trips$Date), "%A")

# Get the ride length between time started and time ended for the ride
all_trips$ride_length <- difftime(all_trips$ended_at, all_trips$started_at)
str(all_trips)


# convert the ride_length from factor to numeric so we can
#perform calculations on them
is.factor(all_trips$ride_length)
is.numeric(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

#Remove Bad data
# we will drops rows in the data set that has negative ride_length and quality
# we will have to create a new dataframe so as not to discard the all_trips
# dataframe

all_trips_v2 <- all_trips[!(all_trips$start_station_name=="HQ QR"
                            | all_trips$ride_length<0),]

View(all_trips_v2)


#====================================================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#====================================================================

# First lets get the mean, median, max and min of the ride length
mean(all_trips_v2$ride_length)
median(all_trips_v2$ride_length)
max(all_trips_v2$ride_length)
min(all_trips_v2$ride_length)

# we can get the mean, median, max and min using summary
summary(all_trips_v2$ride_length)


# COmpare casual and members
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN=min)

# See the average ride time by each day
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$Week_day, FUN=mean)


# Lets reorder the days of the week
all_trips_v2$Week_day <- ordered(all_trips_v2$Week_day, levels=c(
  "Sunday","Monday", "Tuesday", "Wednesday", "Thursday", "Friday",
  "Saturday"))

aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual 
          + all_trips_v2$Week_day, FUN=(mean))

# Analyze ridership data by type and weekdays
all_trips_v2$newStarted_at<- as.Date(all_trips_v2$started_at)

#The function below is suppose to create a new week date using
#the mutate funtion
#all_trips_v2 %>%
#  mutate(weekDay = week(newStarted_at, label=TRUE)) %>% week()

all_trips_v2$weekDay <- week(as.Date(all_trips_v2$newStarted_at,
                                  format="%Y-%d-%m"))  
table(all_trips_v2$weekDay)

# Group by 
trip_summary<-all_trips_v2 %>%
  group_by(member_casual, weekDay) %>%
  summarise(number_of_rides = n(),
            average_duration = mean(ride_length))%>%
  arrange(member_casual, weekDay)

View(trip_summary)

#====================================================================
# STEP 5: VISUALIZATION
#====================================================================

# Using the ggplot to visualize
# create a visualize the number of rides
nr <- trip_summary %>%
  ggplot(aes(x = weekDay, y = number_of_rides, fill=member_casual)) + geom_col(
    position = "dodge"
  )

# Create visualization for avergae duration
ad <- trip_summary%>%
  ggplot(aes(x=weekDay, y=average_duration, fill=member_casual))+geom_col(
    position="dodge")

# represent both graph on a single line graph

sl <- trip_summary%>%
  ggplot( aes(x=weekDay))+geom_line(
    aes(y=number_of_rides), color="darkred"
  )+ geom_line(
    aes(y=average_duration), color="steelblue"
  )

# using a bar chart
bc <- trip_summary%>%
  ggplot(aes(x=weekDay, y=number_of_rides, fill=member_casual ))+
  geom_bar( stat="identity")

# using ggpubr to put all the plot in one graph

ggarrange(nr, ad, sl, bc +rremove("x.text"),
          labels = c("a", "b", "c", "d"),
          ncol=2, nrow=2)

#====================================================================
# STEP 5: EXPORT FOR FURTHER ANALYSIS
#====================================================================
# create a csv file that will be visualized in other visualization 

counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual
                    +all_trips_v2$Week_day, FUN=mean)
counts
View(counts)

# Export the counts as a csv file
write.csv(counts,
          file='C:/Users/user/Desktop/DS-Project/Cyclist dataset/CSV_dataset/avg_ride_length.csv')
