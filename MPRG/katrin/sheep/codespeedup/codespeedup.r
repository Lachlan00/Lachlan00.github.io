library(suncalc)

setwd("~/Development/other_people_help/Katrin")
df <- read.csv("studyperiod_na.csv")

# clean data
df$X <- NULL
df$date <- NULL
df$datetime <- NULL
df$datetimeACST <- NULL

# Convert the datetime column to POSIXct datetime data type
df$datetimeUTC <- as.POSIXct(df$datetimeUTC, tz='UTC')

calcToD <- function(dt, lat, lon){
  # so this is afunction to get the time of day categories
  # let's get the suntimes
  sun <- getSunlightTimes(date=as.Date(dt), lat=lat, lon=lon)
  if (as.numeric(dt - sun$sunrise) >= 0 & as.numeric(dt - sun$sunrise) <= 4){
    ToD <- 1
  } else if (as.numeric(dt - sun$solarNoon) >= -2 & as.numeric(dt - sun$solarNoon) <= 2){
    ToD <- 2
  } else if (as.numeric(dt - sun$sunset) >= -2 & as.numeric(dt - sun$sunset) <= 0){
    ToD <- 3
  } else {
    ToD <- 0
  }
  return(ToD)
}

# make subset for testing
df_test <- head(df, 10000)
df_test <- df # just using df_test as the variable so I don't have to redit the code

#####################################
# Make dummy data
# n=10000 #number of samples
# ddata <- as.data.frame(cbind(date = sample(seq(as.POSIXct('2017-01-01'), as.POSIXct('2018-10-31'), by="1 hour", tz="UTC"), n),
#                        lat = sample(0.001:55.001, n, replace=T),
#                        lon = sample(0.0001:90.000, n, replace=T)))
# ddata$date <- as.POSIXct(ddata$date, origin = "1970-01-01")

#######################################################
## Your function
#######################################################
###
#get start time
t1.1 <- Sys.time()
#run yor code
ToD <- mapply(calcToD, df_test$datetimeUTC, df_test$Latitude, df_test$Longitude)
#get end time
t1.2 <- Sys.time()
t.l <- t2.1-t1.1

#######################################################
## Simple version
#######################################################

#get start time
t1<-Sys.time()
ddata <- df_test[c('datetimeUTC','Latitude','Longitude')]
#keep posix format
datetime <- ddata$datetimeUTC
#change to date for sunlight function
ddata$date <- as.Date(ddata$date)
ddata$lon <- ddata$Longitude
ddata$lat <- ddata$Latitude
ddata <- ddata[c('date', 'lat', 'lon')]
#get times
sun <- getSunlightTimes(data=ddata)
#create variables
ddata$sinceSunrise <- as.numeric(datetime-sun$sunrise)/3600 #this is in seconds, so you have to divide by 3600 to get hours
ddata$sinceSolarNoon <- as.numeric(datetime-sun$solarNoon)/3600
ddata$sinceSunset <- as.numeric(datetime-sun$sunset)/3600

#create ToD
ddata$ToD <- 0

ddata$ToD[ddata$sinceSunrise > 0 & ddata$sinceSunrise <= 4] <- 1
ddata$ToD[ddata$sinceSolarNoon >= -2 & ddata$sinceSolarNoon <= 2] <- 2
ddata$ToD[ddata$sinceSunset >= -4 & ddata$sinceSunset <= 0] <- 3

#get end time
t2<-Sys.time()
t.s <- t2-t1
#######################################################
##Benchmark
#######################################################


format(t.l,"s")
message(paste("Your version took:",round(t.l,0),"s for", n,"samples"))
message(paste("My version took:",round(t.s,0),"s for", n,"samples"))

