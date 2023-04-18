library(tidyverse)
library(lubridate)
library(shiny)
library(geosphere)
library(dplyr)
library(magrittr)
library(viridis)

rm(list = ls())

#read in csv files
df_april_data <- read.csv("../uber-rides-data/uber-raw-data-apr14.csv")

df_may_data <- read.csv("../uber-rides-data/uber-raw-data-may14.csv")

df_june_data <- read.csv("../uber-rides-data/uber-raw-data-jun14.csv")

df_july_data <- read.csv("../uber-rides-data/uber-raw-data-jul14.csv")

df_august_data <- read.csv("../uber-rides-data/uber-raw-data-aug14.csv")

df_september_data <- read.csv("../uber-rides-data/uber-raw-data-sep14.csv")

df_uber_data <- rbind(df_april_data, df_may_data, df_june_data, df_july_data, df_august_data, df_september_data)

#save data into rds so it runs faster
saveRDS(df_uber_data, "df_uber_data.rds")#

#read the rds
df_rds_data <- readRDS("df_uber_data.rds")

#for running script faster
df_subset <- sample_n(df_rds_data, 50000)

df_rds_data[c('Date', 'Time')] <- str_split_fixed(df_rds_data$Date.Time, ' ', 2)
df_subset[c('Date', 'Time')] <- str_split_fixed(df_subset$Date.Time, ' ', 2)

#Create valid date schema for analysis
df_rds_data <- df_subset %>% #df_rds_data %>% 
  mutate(Date.Time = as.Date(Date.Time, format = "%m/%d/%Y")) %>%
  mutate(Day = day(Date.Time)) %>%
  mutate(Week = week(Date.Time)) %>%
  mutate(Month = month(Date.Time)) %>%
  mutate(Month = ifelse(Month == "4", "April", Month)) %>%
  mutate(Month = ifelse(Month == "5", "May", Month)) %>%
  mutate(Month = ifelse(Month == "6", "June", Month)) %>%
  mutate(Month = ifelse(Month == "7", "July", Month)) %>%
  mutate(Month = ifelse(Month == "8", "August", Month)) %>%
  mutate(Month = ifelse(Month == "9", "September", Month)) %>%
  mutate(DayofWeek = weekdays(Date.Time)) %>%
  mutate(Hour = substr(Time, 1, 2)) %>%
  mutate(Hour = str_remove(Hour, ":"))

#create df's for plotting within the shiny app to answer varius querys
df_rides_by_hour <-  df_rds_data %>%
  group_by(Hour) %>%
  mutate(ridesPerHour = length(Hour))

df_distinct_rides_by_hour <- df_rides_by_hour %>%
  distinct(ridesPerHour) %>%
  mutate(Hour = ifelse(Hour == "0", "12 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "1", "1 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "2", "2 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "3", "3 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "4", "4 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "5", "5 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "6", "6 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "7", "7 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "8", "8 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "9", "9 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "10", "10 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "11", "11 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "12", "12 am", Hour)) %>%
  mutate(Hour = ifelse(Hour == "13", "1 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "14", "2 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "15", "3 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "16", "4 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "17", "5 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "18", "6 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "19", "7 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "20", "8 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "21", "9 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "22", "10 pm", Hour)) %>%
  mutate(Hour = ifelse(Hour == "23", "11 pm", Hour))

df_rides_by_month <- df_rds_data %>%
  group_by(Month, Day) %>%
  mutate(ridesPerMonth = length(Month)) %>%
  mutate(Week = Day / 8) %>%
  mutate(Week = ceiling(Week))

df_rides_by_day <- df_rds_data %>%
  group_by(DayofWeek) %>%
  mutate(ridesPerDay = length(DayofWeek))

df_rides_by_day_of_week <- df_rds_data %>%
  group_by(Day) %>%
  mutate(ridesPerDay = length(Day))

df_rides_by_base_day_of_week <- df_rds_data %>%
  group_by(Base, Day) %>%
  mutate(ridesPerBase = length(Base))

df_distinct_rides_by_day <- df_rds_data %>%
  group_by(Day) %>%
  mutate(ridesPerDay = length(Day)) %>%
  distinct(ridesPerDay)

df_rides_by_base <- df_rds_data %>%
  group_by(Base, Day) %>%
  mutate(ridesPerBase = length(Base))

#Write rds and csv files for transfer to shiny scripts
write_rds(df_rds_data, "../uber-rides-data/clean_data/df_formatted_rds_data.rds")
write_csv(df_rides_by_day,"../uber-rides-data/clean_data/df_rides_by_day.csv")
write_csv(df_rides_by_day_of_week,"../uber-rides-data/clean_data/df_rides_by_day_of_week.csv")
write_csv(df_rides_by_base_day_of_week,"../uber-rides-data/clean_data/df_rides_by_base_day_of_week.csv")
write_csv(df_rides_by_hour,"../uber-rides-data/clean_data/df_rides_by_hour.csv")
write_csv(df_rides_by_base,"../uber-rides-data/clean_data/df_rides_by_base.csv")
write_csv(df_distinct_rides_by_hour,"../uber-rides-data/clean_data/df_distinct_rides_by_hour.csv")
write_csv(df_distinct_rides_by_day,"../uber-rides-data/clean_data/df_distinct_rides_by_day.csv")
write_csv(df_rides_by_month,"../uber-rides-data/clean_data/df_rides_by_month.csv")


