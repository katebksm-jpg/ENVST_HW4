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
weather$doy <- yday(weather$dateF)
weather$year <- year(weather$dateF)

weather

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

#read in soil files
soilFiles <- list.files("/cloud/project/activity04/soil")

#set up variable to be used in for loop
soilList <- list()

for(i in 1:length(soilFiles)){
  soillist[[i]]<-read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}
#inspect first file
head(soillist)

soilData <- do.call("rbind", soillist)


#calculate moving average
airMA <- numeric()

for(i in 8:length(weather$AirTemp)){
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}

weather$airMA <- airMA





# In Class Prompts --------------------------------------------------------

#prompt 1 
#completed above 

#prompt 2
May_June <- weather %>%
  filter(month(weather$dateF)==5 |
           month(weather$dateF)==6)
ggplot(data=May_June, aes(x=dateF, y=SolRad))+
  geom_line()

#prompt 3


# Homework Prompts -------------------------------------------------------

#prompt 1
#convert perception below freezing, impacted by bird excrement, 
#or with X/Y values above to NA values 

weatherQC <- weather
#bird excrement period (May or June)
weatherQC$Precip <- ifelse(weatherQC$doy >= 121 & weatherQC$doy <= 188 
                           & weatherQC$year == 2021, 
                           NA, weatherQC$Precip) 
#temperatures below 0
weatherQC$Precip <- ifelse(weatherQC$AirTemp<=0, NA, weatherQC$Precip)

#x and y level 
weatherQC$Precip <- ifelse(weatherQC$XLevel>2 |
                             weatherQC$YLevel>2, NA, weatherQC$Precip)

#find the total NA values
sum(is.na(weatherQC$Precip))


#prompt 2 
#create voltage flag
weather$voltageflag <- ifelse(weather$BatVolt <= 8.5, #if true:set flag to "warning",
                              #if false set flag to "good"
                              "battery warning", "good")


#prompt 3 
# function that checks for observations that are in unrealistic data ranges 
#in air temperature and solar radiation

check_temp_rad <- function (weather)
  #define temp limits
{mintemp <- -35 #manual indicates lowest readable value is 50, but this is a more realistic value for Clinton
  maxtemp <- 50 #122 degrees F, manual indicates 60 but Clinton does not typically get so hot
  maxrad <- 1750 #max reading from sensor manual
  #flag unrealistic values
  #if unrealistic will be "Unrealistic," if realistic "Realistic"
weather$tempcheck <- ifelse(weather$AirTemp<=mintemp | weather$AirTemp>=maxtemp,
"Unrealistic", "Realistic")
weather$radcheck <- ifelse(weather$SolRad<0 #solar rad can't be negative 
                           | weather$SolRad >= maxrad, 
                            "Unrealistic", "Realistic")
return(weather)
}

#run funciton on weather
weather_check <- check_temp_rad(weather)

#prompt 4
#create filter for January and March
Jan_March <- weather %>%
  filter(dateET>="2021-01-01", dateET <="2021-03-31")
  
#plot Jan-March
ggplot(data=Jan_March,
       aes(x=dateET, y=AirTemp)) +
  geom_line()+
  labs(title="January-March Air Temperatures (2021)",
    x="Date", y="Air Temperature (Celsius)")


#prompt 5 
#create new data frame that summarizes total precipitation and minimum air temperature
March_April <- weather %>%
  filter(year==2021) %>%
  #filter for March and April
  filter(dateET>="2021-03-01", dateET <= "2021-04-30") %>%
  group_by(doy)%>%
  summarise(totPrecip=sum(Precip), mintemp=min(AirTemp))

# start an empty numeric vector
precip.check<-as.numeric(NA)

for(i in 2:nrow(March_April)){precip.check[i]<-ifelse(March_April$mintemp[i]<=1.7|
                                                         March_April$mintemp[i-1]<=1.7,
                                                       NA, March_April$totPrecip[i])}

March_April$precip.check <- precip.check

#check number of NA values
sum(!is.na(precip.check))


#prompt 6
soilFiles <- list.files("/cloud/project/activity04/soil")

#set up variable to be used in for loop
soilList <- list()

for(i in 1:length(soilFiles)){
  soillist[[i]]<-read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
}
#inspect first file
head(soillist)

soilData <- do.call("rbind", soillist)

#parse dates and times through lubridate
soilData$dateTime <- ymd_hm(soilData$Timestamp)
soilData$dateET <- ymd_hm(soilData$Timestamp, tz="America/New_York")

#create user defined functionn
timeCheck <- function(x,t){intervals <- x[-length(x)] %--% x[-1]
 interval_times<-int_length(intervals)
 intervals[interval_times!= t]
  }

#use function to check for an interval of 3600 sec (1 hour)
#use ET as this weather station is at Hamilton College
timeCheck(soilData$dateET, 3600)

