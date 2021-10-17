
#--------------------------------------------------------------------------
# STEP-1: Installation of required packages & change of directory for ease.
#--------------------------------------------------------------------------

# Install required packages

install.packages("ggplot2")
install.packages("skimr")
install.packages("lubridate")
install.packages("janitor")
install.packages("dplyr")
install.packages("tidyverse")


library(skimr)#helps summaries data
library(janitor) #helps clean data
library(dplyr)
library(tidyverse)  #helps wrangle data
library(lubridate)  #helps wrangle date attributes
library(ggplot2)  #helps visualize data
getwd() #displays your working directory
setwd("C:/Users/abc/Desktop/Nainil_Bike_Share") 


#----------------------------
# STEP-2: Data import into R.
#----------------------------
# Upload Divvy datasets (csv files) here
#read csv() imports data from .csv files.

April_2020 <- read_csv("202004-divvy-tripdata.csv")
May_2020 <- read.csv("202005-divvy-tripdata.csv")
June_2020 <- read.csv("202006-divvy-tripdata.csv")
July_2020 <- read.csv("202007-divvy-tripdata.csv")
August_2020 <- read.csv("202008-divvy-tripdata.csv")
September_2020 <- read.csv("202009-divvy-tripdata.csv")
October_2020 <- read.csv("202010-divvy-tripdata.csv")
November_2020 <- read.csv("202011-divvy-tripdata.csv")
December_2020 <- read.csv("202012-divvy-tripdata.csv")
April_2020 <- read.csv("202004-divvy-tripdata.csv")
January_2021 <- read.csv("202101-divvy-tripdata.csv")
February_2021 <- read.csv("202102-divvy-tripdata.csv")
March_2021 <- read.csv("202103-divvy-tripdata.csv")
April_2021 <- read.csv("202104-divvy-tripdata.csv")
May_2021 <- read.csv("202105-divvy-tripdata.csv")
June_2021 <- read.csv("202106-divvy-tripdata.csv")
July_2021 <- read.csv("202107-divvy-tripdata.csv")
August_2021 <- read.csv("202108-divvy-tripdata.csv")
September_2021 <- read.csv("202109-divvy-tripdata.csv")

#------------------------------------------------------
# STEP-3: Comparing column names for each of the files.
#------------------------------------------------------
# Compare column names each of the files
# While the names don't have to be in the same order, they DO need to match perfectly before we can use a command to join them into one file

#colnames() displays the names of all the columns in the dataset.

colnames(April_2020)
colnames(May_2020)
colnames(June_2020)
colnames(July_2020)
colnames(August_2020)
colnames(September_2020)
colnames(October_2020)
colnames(November_2020)
colnames(December_2020)
colnames(January_2021)
colnames(February_2021)
colnames(March_2021)
colnames(April_2021)
colnames(May_2021)
colnames(June_2021)
colnames(July_2021)
colnames(August_2021)
colnames(September_2021)

#---------------------------------------------------------------
# STEP-4: Inspecting the data frames and look for incongruencies.
#---------------------------------------------------------------
#str() gives the data type info of data in the data set.

str(April_2020)
str(May_2020)
str(June_2020)
str(July_2020)
str(August_2020)
str(September_2020)
str(October_2020)
str(November_2020)
str(December_2020)
str(January_2021)
str(February_2021)
str(March_2021)
str(April_2021)
str(May_2021)
str(June_2021)
str(July_2021)
str(August_2021)
str(September_2021)


#--------------------------------------------------------------------------
# STEP-5: Making consistent Data types for columns in each of the datasets.
#--------------------------------------------------------------------------
#We compare column datatype across all dataframe by using compare_df_cols() when we have large dataset, that would be more easy

compare_df_cols(April_2020,May_2020,
                June_2020, 
                July_2020, 
                August_2020, 
                September_2020, 
                October_2020, 
                November_2020, 
                December_2020, 
                April_2020, 
                January_2021, 
                February_2021,
                March_2021, 
                April_2021, 
                May_2021, 
                June_2021, 
                July_2021, 
                August_2021, 
                September_2021, return="mismatch")


#On inspecting the data types of all the columns, we found that some columns have mismatched data types.
#For further ease in working we need to combine all datasets into one, and to do that all columns must have identical data type.
#mutate() is used to change the data type of a column from one to another.
#Convert end_station_id and start_station_id to character so that they can stack correctly

January_2021 <-  mutate(January_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#January_2021 <-  mutate(January_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

February_2021 <-  mutate(February_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#February_2021 <-  mutate(February_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

March_2021 <-  mutate(March_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#March_2021 <-  mutate(March_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

April_2021 <-  mutate(April_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#April_2021 <-  mutate(April_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

May_2021 <-  mutate(May_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#May_2021 <-  mutate(May_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

June_2021 <-  mutate(June_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#June_2021 <-  mutate(June_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

July_2021 <-  mutate(July_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#July_2021 <-  mutate(July_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

August_2021 <-  mutate(August_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#August_2021 <-  mutate(August_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

September_2021 <-  mutate(September_2021, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#September_2021 <-  mutate(September_2021, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))


April_2020 <-  mutate(April_2020, start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#April_2020 <-  mutate(April_2020, start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

May_2020  <-  mutate(May_2020 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#May_2020  <-  mutate(May_2020 , start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

June_2020  <-  mutate(June_2020 , start_station_id = as.character(start_station_id), end_station_id = as.character(end_station_id))
#June_2020  <-  mutate(June_2020 , start_station_id = as.double(start_station_id), end_station_id = as.double(end_station_id))

July_2020  <-  mutate(July_2020 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#July_2020  <-  mutate(July_2020 , start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

August_2020  <-  mutate(August_2020 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#August_2020  <-  mutate(August_2020 , start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

September_2020  <-  mutate(September_2020 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#September_2020  <-  mutate(September_2020 , start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

October_2020  <-  mutate(October_2020 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#October_2020  <-  mutate(October_2020 , start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

November_2020  <-  mutate(November_2020 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#November_2020  <-  mutate(November_2020 , start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))

December_2020  <-  mutate(December_2020 , start_station_id = as.character(start_station_id),end_station_id = as.character(end_station_id))
#December_2020  <-  mutate(December_2020 , start_station_id = as.double(start_station_id),end_station_id = as.double(end_station_id))


#--------------------------------------------------------
# STEP-6: Combining all the datasets into one Data Frame.
#--------------------------------------------------------
#bind_rows() combines rows from various datasets into one data frame.

all_trips <- bind_rows(
  April_2020,
  May_2020, 
  June_2020, 
  July_2020,
  August_2020, 
  September_2020, 
  October_2020, 
  November_2020, 
  December_2020, 
  April_2020, 
  January_2021, 
  February_2021,
  March_2021, 
  April_2021, 
  May_2021, 
  June_2021, 
  July_2021, 
  August_2021, 
  September_2021 )


#------------------------------------------------------------
# STEP-7: Removal of Unnecessary Columns from the Data Frame.
#------------------------------------------------------------
#select(-argument) remvoes selected columns from the dataset.
# Remove lat, long, birthyear, and gender fields as this data was dropped beginning in 2020

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng))

all_trips <- all_trips %>%  
  select(-c(start_lat, start_lng, end_lat, end_lng, birthyear, gender, "01 - Rental Details Duration In Seconds Uncapped", "05 - Member Details Member Birthday Year", "Member Gender", "tripduration"))


#------------------------------------------------------
# STEP-8: CLEAN UP AND ADD DATA TO PREPARE FOR ANALYSIS
#------------------------------------------------------
# Inspect the new table that has been created

colnames(all_trips)  #List of column names
nrow(all_trips)  #How many number of rows are there in the dataframe?
dim(all_trips)  #Dimensions of the data frame (rows x columns)?
head(all_trips)  #See the first 6 rows of data frame.
str(all_trips)  #See list of columns and data types.
summary(all_trips)  #Statistical summary of data. Mainly for numerics.

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2020 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                                ,"Subscriber" = "member"
                                ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link


all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

#--------------------------------------------------
# STEP-10: Calculating Trip Duration for each ride.
#--------------------------------------------------
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

#----------------------------------------------
# STEP-11: Removing Irrelevant/Incomplete Data.
#----------------------------------------------
# Made a new version of the file in case we need to comeback to the data.

is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#Remove "bad" data
#The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative

skim(all_trips_v2)
summary(all_trips_v2$ride_length)

#-----------------------------------------------------------------------
# STEP-12: Descriptive analysis on ride_length (all figures in seconds).
#-----------------------------------------------------------------------

summary(all_trips_v2$ride_length)

#---------------------------------------------------------------------
# STEP-13: Comparing Member and casual users (all figures in seconds).
#---------------------------------------------------------------------
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

#--------------------------------------------------------------------------------------------
# STEP-14: Average ride time by each day for members vs casual users (all figures in seconds).
#---------------------------------------------------------------------------------------------

all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

#-----------------------------------------------------------------------------
# STEP-15: Analyzing riders data by type and weekday (all figures in seconds).
#-----------------------------------------------------------------------------

all_trips_v2 %>%
  mutate(weekday = wday(started_at, label = TRUE)) %>%
  group_by(member_casual, weekday) %>%
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>%
  arrange(member_casual, weekday)

#################################
#Visualize
#################################

#-------------------------------------------------------------------------
# STEP-16: Visualizing the number of rides in each weekday by member type.
#-------------------------------------------------------------------------
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  scale_fill_manual(values = c("#b40c1c", "#e2a32d"))+
  geom_col(position = "dodge") +
  labs(title = "Number of rides by Member type during eachday of the week")

#-------------------------------------------------------------------------------------
# STEP-17: Visualization for average duration of rides in each weekday by member type.
#-------------------------------------------------------------------------------------
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(), average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Average duration of rides in each weekday by member type")

#------------------------------------------------------------------------------
# STEP-18: Visualization for number of rides in each ride type by member types.
#-----------------------------------------------------------------------------
all_trips_v2 %>% 
  group_by(member_casual, rideable_type) %>% 
  summarise(number_of_rides = n()) %>% 
  arrange(member_casual)  %>% 
  ggplot(aes(x = rideable_type, y=number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Number of rides in each ride type by member types")


#Export file as .csv 
write.csv(all_trips_v2, file = 'C:/Users/abc/Desktop/Nainil_Bike_Share/avg_ride_length_1.csv')