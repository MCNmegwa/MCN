library(tidyverse)
library(readr)
library(ggplot2)
library(lubridate)
library(skimr)
library(janitor)
library(stringr)
library(dplyr)


df_jun_2021 <- read.csv('202106-divvy-tripdata.csv')
df_jul_2021 <- read.csv('202107-divvy-tripdata.csv')
df_aug_2021 <- read.csv('202108-divvy-tripdata.csv')
df_sep_2021 <- read.csv('202109-divvy-tripdata.csv')
df_oct_2021 <- read.csv('202110-divvy-tripdata.csv')
df_nov_2021 <- read.csv('202111-divvy-tripdata.csv')
df_dec_2021 <- read.csv('202112-divvy-tripdata.csv')
df_jan_2022 <- read.csv('202201-divvy-tripdata.csv')
df_feb_2022 <- read.csv('202202-divvy-tripdata.csv')
df_mar_2022 <- read.csv('202203-divvy-tripdata.csv')
df_apr_2022 <- read.csv('202204-divvy-tripdata.csv')
df_may_2022 <- read.csv('202205-divvy-tripdata.csv')


df_jun_2021 <- mutate(df_jun_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_jul_2021 <- mutate(df_jul_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_aug_2021 <- mutate(df_aug_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_sep_2021 <- mutate(df_sep_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_oct_2021 <- mutate(df_oct_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_nov_2021 <- mutate(df_nov_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_dec_2021 <- mutate(df_dec_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_jul_2021 <- mutate(df_jul_2021, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_jan_2022 <- mutate(df_jan_2022, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_feb_2022 <- mutate(df_feb_2022, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_mar_2022 <- mutate(df_mar_2022, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_apr_2022 <- mutate(df_mar_2022, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))
df_may_2022 <- mutate(df_may_2022, started_at = as_datetime(started_at),
                      ended_at = as_datetime(ended_at))

all_rides <- bind_rows(df_jun_2021, df_jul_2021, df_aug_2021,
                       df_sep_2021, df_oct_2021, df_nov_2021,
                       df_dec_2021, df_jan_2022, df_feb_2022,
                       df_mar_2022, df_apr_2022, df_may_2022)

all_rides <- all_rides %>%
  rename(bike_type = rideable_type,
         membership_type = member_casual)

all_rides$ride_length <- difftime(all_rides$ended_at,
                                  all_rides$started_at)

all_rides$date <- as.Date(all_rides$started_at)
all_rides$day <- format(as.Date(all_rides$date), "%d")
all_rides$day_of_week <- format(as.Date(all_rides$date), "%A")
all_rides$month <- format(as.Date(all_rides$date), "%m")
all_rides$year <- format(as.Date(all_rides$date), "%Y")
all_rides$hour <- as.numeric(format(all_rides$started_at, "%H"))


all_rides$ride_length <- as.numeric(all_rides$ride_length)

all_rides2 <- all_rides %>%
  filter(!(ride_length<60)) %>%
  filter(!(ride_length>86400))

all_rides2 <- all_rides2 %>%
  filter(!(is.na(end_lat)))%>%
  filter(!(is.na(end_lng)))

all_rides2 %>%
  summarise(mean = mean(ride_length), maximum = max(ride_length),
            minimum = min(ride_length), median = median(ride_length))

aggregate(all_rides2$ride_length ~ all_rides2$membership_type, FUN = mean)
aggregate(all_rides2$ride_length ~ all_rides2$membership_type, FUN = max)
aggregate(all_rides2$ride_length ~ all_rides2$membership_type, FUN = min)
aggregate(all_rides2$ride_length ~ all_rides2$membership_type, FUN = median)

all_rides2 %>%
  group_by(membership_type)%>%
  summarise(number_of_rides = n())

all_rides2 <- all_rides2 %>%
  mutate(day_of_week = factor(day_of_week, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")))

all_rides2 %>%
  group_by(membership_type, day_of_week)%>%
  summarise(number_of_ride = n()) %>%
  arrange(membership_type)

aggregate(all_rides2$ride_length ~ all_rides2$membership_type + all_rides2$day_of_week,
          FUN = mean)

all_rides2 <- all_rides2 %>%
  mutate(month = month(started_at, label = TRUE)) %>%
  group_by(membership_type, month)%>%
  summarise(number_of_rides = n()) %>%
  arrange(membership_type)

    


str(all_rides2)