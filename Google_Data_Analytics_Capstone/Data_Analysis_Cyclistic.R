#install packages
install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")
install.packages("data.table")
install.packages("readr")
install.packages("psych")
install.packages("hrbrthemes")
install.packages("ggplot2")

#load packages
library(tidyverse)
library(lubridate)
library(janitor)
library(data.table)
library(readr)
library(psych)
library(hrbrthemes)
library(ggplot2)
 
#import data
january_2023 <- read.csv("data/202301-divvy-tripdata.csv")
february_2023 <- read.csv("data/202302-divvy-tripdata.csv")
march_2023 <- read.csv("data/202303-divvy-tripdata.csv")
april_2023 <- read.csv("data/202304-divvy-tripdata.csv")
may_2023 <- read.csv("data/202305-divvy-tripdata.csv")
june_2023 <- read.csv("data/202306-divvy-tripdata.csv")
july_2023 <- read.csv("data/202307-divvy-tripdata.csv")
august_2023 <- read.csv("data/202308-divvy-tripdata.csv")
september_2023 <- read.csv("data/202309-divvy-tripdata.csv")
october_2023 <- read.csv("data/202310-divvy-tripdata.csv")
november_2023 <- read.csv("data/202311-divvy-tripdata.csv")
december_2023 <- read.csv("data/202312-divvy-tripdata.csv")

#data validation
colnames(january_2023)
colnames(february_2023)
colnames(march_2023)
colnames(april_2023)
colnames(may_2023)
colnames(june_2023)
colnames(july_2023)
colnames(august_2023)
colnames(september_2023)
colnames(october_2023)
colnames(november_2023)
colnames(december_2023)

#total number of rows
sum(nrow(january_2023) + nrow(february_2023) + nrow(march_2023) + 
  nrow(april_2023) + nrow(may_2023) + nrow(june_2023) + nrow(july_2023)+
  nrow(august_2023) + nrow(september_2023) + nrow(october_2023) + 
  nrow(november_2023) + nrow(december_2023))

#combine data of 12 month into one for smooth work flow
tripfinal <- rbind(january_2023, february_2023, march_2023, april_2023,
                   may_2023, june_2023, july_2023, august_2023, september_2023,
                   october_2023,november_2023,december_2023)

#save the combined files
write.csv(tripfinal, file = "data/tripfinal.csv", row.names = FALSE)

#final data validation
str(tripfinal)
View(head(tripfinal))
View(tail(tripfinal))
dim(tripfinal)
summary(tripfinal)
names(tripfinal)

#count rows with "na" values
colSums(is.na(tripfinal))

#remove missing 
clean_trip_final <- tripfinal[complete.cases(tripfinal), ]

#remove duplicates
clean_trip_final <- distinct(clean_trip_final)

#remove "na"
clean_trip_final <- drop_na(clean_trip_final)
clean_trip_final <- remove_empty(clean_trip_final)
clean_trip_final <- remove_missing(clean_trip_final)

#remove data which greater start_at then end_at 
clean_trip_final <- clean_trip_final %>%
  filter(started_at < ended_at)

#remaining column for better context
clean_trip_final <- rename(clean_trip_final, costumer_type = member_casual,
                           bike_type = rideable_type)

#separate date in date, month, year for better analysis
clean_trip_final$date <- as.Date(clean_trip_final$started_at)
clean_trip_final$week_day <- format(as.Date(clean_trip_final$date), "%A")
clean_trip_final$month <- format(as.Date(clean_trip_final$date), "%b_%y")
clean_trip_final$year <- format(clean_trip_final$date, "%Y")

#separate column for time 
clean_trip_final$time <- as.POSIXct(clean_trip_final$started_at , format = "%Y-%m-%d %H:%M:%S")  
clean_trip_final$time <- format(clean_trip_final$time, format = "%H:%M")

#add right length column
clean_trip_final$ride_length <- difftime(clean_trip_final$ended_at, clean_trip_final$started_at, units = "mins")

#select the data we are going to use
clean_trip_final <- clean_trip_final %>%
  select(bike_type, costumer_type, month, year, time, started_at, week_day, ride_length)

#remove stolen bikes
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length>1440,]
clean_trip_final <- clean_trip_final[!clean_trip_final$ride_length<5,]

#checked cleaned data
colSums(is.na(clean_trip_final))
View(filter(clean_trip_final, clean_trip_final$started_at > clean_trip_final$ended_at))
View(filter(clean_trip_final, clean_trip_final$ride_length>1440 | clean_trip_final < 5))

#save the cleaned data
write.csv(clean_trip_final, file = "clean_trip_final.csv", row.names = FALSE)

#import the cleaned data
clean_trip_final <- read.csv("clean_trip_final.csv")
str(clean_trip_final)
names(clean_trip_final)

#order the data
clean_trip_final$month <- ordered(clean_trip_final$month,levels=c("Jan_23","Feb_23","Mar_23", 
                                                                  "Apr_23","May_23","Jun_23","Jul_23", 
                                                                  "Aug_23","Sep_23","Oct_23","Nov_23","Dec_23"))

clean_trip_final$week_day <- ordered(clean_trip_final$week_day,levels=c("Sunday", "Monday", "Tuesday", 
                                     "Wednesday", "Thursday", 
                                     "Friday", "Saturday"))

#analysis:- min, max, median, average
View(describe(clean_trip_final, fast=TRUE))

#total number of customer
View(table(clean_trip_final$costumer_type))

#total rides for each customer type in minutes
View(setNames(aggregate(ride_length ~ costumer_type, clean_trip_final, sum ),c("Customer_type", "total_ride_len(mins)")))

  
#difference between members and casual riders in terms of length of ride
View(clean_trip_final %>% 
       group_by(costumer_type) %>% 
       summarise(min_length_mins = min(ride_length), max_length_min = max(ride_length),
                 median_length_mins = median(ride_length), mean_length_min = mean(ride_length)))

#average ride_length for users by day_of_week and numbers of total riders by day_of_week
View(clean_trip_final %>%
       group_by(week_day) %>%
       summarise(Avg_length = mean(ride_length),
                 number_of_ride = n()))

#average ride length comparison by each week day according to each customer type
View(aggregate(clean_trip_final$ride_length ~ clean_trip_final$costumer_type+
                 clean_trip_final$week_day, FUN = mean))

#analyze rider length data by customer type and weekday
View(clean_trip_final %>% 
       group_by(costumer_type, week_day) %>% 
       summarise(number_of_ride = n(),
                 avgerage_duration = mean(ride_length),
                 median_duration = median(ride_length),
                 max_duration = max(ride_length),
                 min_duration = min(ride_length)))

#save the data for data visualization
write.csv(clean_trip_final,file = "clean_trip_final_tableau.csv",row.names = FALSE)



      