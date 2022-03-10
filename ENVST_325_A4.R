install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
library(dplyr)
library(ggplot2)
library(lubridate)
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")
sensorLog <- read.csv("/cloud/project/activity04/Sensor log.csv",
                      na.strings = "#N/A")

average <- function(x){
  x.no = na.omit(x)
  sum(x.no)/length(x.no)
}

average(weather$AirTemp)

#--Prompt 1--
# parse date
weather$dateF <- mdy_hm(weather$Date)
# create a day of year column
weather$doy <- yday(weather$dateF)
# create a year column
weather$year <- year(weather$dateF)


weather$precip.QC <- ifelse(weather$doy >= 121 &
                              weather$doy <= 188 &
                              weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip)
# value if false: uses original precipitation observation


weather$FreezeFlag <- ifelse(weather$AirTemp <= 0, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero


ggplot(data = weather[weather$doy >= 121 & weather$doy <= 181 ,], aes(x = dateF, y = SolRad)) + 
  geom_line()

#--Prompt 2--
int_length(weather$dateF[1] %--% weather$dateF[2])

intervals <- weather$dateF[-length(weather$dateF)] %--% weather$dateF[-1]
# [-length(weather$dateF) returns a new vector without the last observation
# weather$dateF[-1] returns a new vector without the first observation
# calculate interval times
interval_times <- int_length(intervals)
# check interval times
intervals[interval_times != 900]

timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)
