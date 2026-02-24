# In Class Demo -----------------------------------------------------------
#install packages
install.packages(c("dplyr", "lubridate", "ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)


weather <- read.csv("/cloud/project/activity04/campus_weather.csv",
                  na.strings = c("#N/A","NA"))

metaDat <- read.csv("/cloud/project/activity04/meter_weather_metadata.csv",
                    na.strings = "#N/A")

#parse dates 

weather$dateF <- mdy_hm(weather$Date)
weather$dateET <- mdy_hm(weather$Date, tz="America/New_York")

weatherCheck <- weather %>%
  filter(is.na(weather$dateET))

weather$dateF[2] %--% weather$dateF[3]

int_length(weather$dateF[2] %--% weather$dateF[3])

test <- weather$dateF[1:10]
test
#removes the first observation - fast way to change formatting
test[-1]

# x is a date vector
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}

timeCheck900(weather$dateF)

soilFiles <- list.files("/cloud/project/activity04/soil")

#set up variable to be used in for loop
soilList <- list()

for(i in 1:length(soilFiles)){
  soillist[[i]]<-read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}

str(soillist)

soilData <- do.call("rbind", soilList)

#calculate moving average
airMA <- numeric()

for(i in 8:length(weather$AirTemp)){
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}

weather$airMA <- airMA


#prompt 2
May_June <- weather %>%
  filter(month(weather$dateF)==5 |
           month(weather$dateF)==6




# In Class Prompts --------------------------------------------------------



# Homeworks Prompts -------------------------------------------------------


