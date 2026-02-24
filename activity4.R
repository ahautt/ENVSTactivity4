

#read in data, edit NA's
weather <- read.csv("/cloud/project/campus_weather.csv",
                    na.strings = "#N/A")
metaDat <- read.csv("/cloud/project/meter_weather_metadata.csv",
                    na.strings = "#N/A")


#UTC = Coordinated Universal Time
# - make sure time zones match between data 
# - interpret data in local time
# - some time measurements are not reliable 

weather$dateF <- mdy_hm(weather$Date)
weather$dateET <- mdy_hm(weather$Date, tz="America/New_York")

#finding an error
weatherCheck <- weather %>%
  filter(is.na(weather$dateET))

#making an interval
weather$dateF[2] %--% weather$dateF[3]
int_length(weather$dateF[2] %--% weather$dateF[3])

#vector subsets
test <- weather$dateF[1:10]
test
#start with the second object of the first vector
test[-1]

#create function for checking for irregular intervals that
# deviate from 900 seconds
# only argument is x, a vector of POSIXct formatted dates to be checked
# with an expected interval of 900 seconds
# the output is all observations that deviate from 900 seconds
timeCheck900 <- function(x){
  intervals <- x[-length(x)] %--% x[-1]
  interval_times <- int_length(intervals)
  intervals[interval_times != 900]
  
}
# run on weather data
timeCheck900(weather$dateF)

#for loops

soilFiles <- list.files("/cloud/project/activity04/soil")
# start an empty list
soilList <- list()

# start an empty list
for(i in 1:length(soilFiles)){
  soilList[[i]] <- read.csv(paste0("/cloud/project/activity04/soil/", soilFiles[i]))
  
}
# inspect the beginning of the first file
head(soilList[[1]])
# get info about your list
str(soilList)

#bind the lists into one data frame
soilData <- do.call("rbind", soilList)
str(soilData)




#class prompts

#1
#using the weather variable, calculate a rolling avg of 
#air temps over 8 15 minute measurements
# in Jan 2022 using a 4 loop, then plot

airMA <- numeric()
for(i in 8:length(weather$AirTemp)) {
  airMA[i] <- mean(weather$AirTemp[(i-7):i])
}
airMA[1:20]

weather$airMA <- airMA

#2
# examine precipitation using a bar plot
ggplot(data=weather[weather$doy > 121 & weather$doy < 274 ,],
       aes(x=dateF,
           y=SolRad))+
  geom_col(color="royalblue4")+
  theme_classic()