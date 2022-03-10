#HW4
library(dplyr)
library(ggplot2)
library(lubridate)
weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                    na.strings = "#N/A")

#--Q1--
weather$precip.clinton <- ifelse(weather$AirTemp < 0 | 
                              (weather$XLevel > 2 &
                              weather$YLevel > 2), 
                            NA, # value if true
                            weather$Precip)

#Function that counts the number of observations with NA
count_NA<- function(x){
  x.no = na.omit(x)
  length(x) - length(x.no)
}

count_NA(weather$precip.clinton)

#--Q2--
weather$BatteryFlag <- ifelse(weather$BatVolt <= 8.5,
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero

#--Q3--
tempcheck <- function(data){
  filter(data, data$AirTemp <=-30 | data$AirTemp >=50)
}
tempcheck(weather)

solarcheck <- function(data){
  filter(data, data$SolRad <0 | data$SolRad > 1000)
}
solarcheck(weather)

#--Q4--
ggplot(data = weather[weather$doy >= 1 & weather$doy <= 90 & weather$year == 2021,], aes(x = dateF, y = AirTemp)) +
  geom_line() + 
  labs(x = "Date", y = "Air Temperature (Celcius)",
       title = "Air Temperature from January - March 2021")
