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
#inspect first file
head(soillist)

soilData <- do.call("rbind", soilList)


#calculate moving average
airMA <- numeric()

for(i in 8:length(weather$AirTemp)){
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}

weather$airMA <- airMA





# In Class Prompts --------------------------------------------------------

#prompt 2
May_June <- weather %>%
  filter(month(weather$dateF)==5 |
           month(weather$dateF)==6)
ggplot(data=May_June, aes(x=dateF, y=SolRad))+
  geom_line()

# Homeworks Prompts -------------------------------------------------------

#prompt 1
Clinton <- weather %>%
  filter(RHSensorTemp>0 &
           XLevel<2 &
           YLevel<2) %>%
  #filter for months w/ bird activity and without spike
  filter(dateET<"2021-05-01" | dateET>"2021-06-30")

sum(is.na(weather$Precip))
  
#prompt 2 
#create voltage flag
weather$voltageflag <- ifelse(weather$BatVolt <= 8.5, #if true:set flag to 1
                              1, 0) #if false: set flag to 0


#prompt 3 
check_temp_rad <- function (weather, tempcol ="AirTemp", 
                           radcol="SolRad") 
  #define temp limits
{mintemp <- -35
  maxtemp <- 43
  minrad <- 0
  maxrad <- 1200
  #flag unrealistic values
  

#prompt 4
#create filter 
Jan_March <- weather %>%
  filter(dateET>="2021-01-01", dateET <="2021-03-31")
  
#plot Jan-March
ggplot(data=Jan_March,
       aes(x=dateET, y=AirTemp)) +
  geom_line()


#prompt 5 
March_April <- weather %>%
  filter(dateET>="2021-03-01", dateET <= "2021-04-30")

# start an empty list
MarchAprillist <- list()

#create for loop
for (i in 2:nrow(March_April)) { 
  if (March_April$AirTemp[i]< 1.6667 | March_April$AirTemp[i-1] < 1.6667)
March_April$Precip[i] <- NA
  }

#check number of NA values
sum(!is.na(March_April$Precip))


#prompt 6

set up variable to be used in for loop
soilList <- list()

for(i in 1:length(soilFiles)){
  soillist[[i]]<-read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}
#inspect first file
head(soillist)

soilData <- do.call("rbind", soilList)
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
}

#check sum 
sum(!is.na(March_April$Precip))
