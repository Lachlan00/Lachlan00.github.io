# Load libraries
library(suncalc)
library(lubridate)
# setwd and load in dataset
setwd("~/Development/other_people_help/Katrin/sheep/more_categories")
load("heatwave1_na_ToD.RData")
# renaming dataframe for more readable code
df <- heatwave1_na
# removing uneeded columns
df$timeofday <- NULL
df <- head(df, 5000) # subset for testing

# Testing original 3 category function
calcToD_3 <- function(dt, lat, lon){
  sun <- getSunlightTimes(date=as.Date(dt), lat=lat, lon=lon)
  if (difftime(dt, sun$dawn, units='hours') > 24){
    sun <- getSunlightTimes(date=as.Date(dt)+1, lat=lat, lon=lon)
  }
  if (as.numeric(difftime(dt, sun$dawn, units='hours')) >= 0 & as.numeric(difftime(dt, sun$dawn, units='hours')) <= 4){
    ToD <- 1
  } else if (as.numeric(difftime(dt, sun$solarNoon, units='hours')) >= -2 & as.numeric(difftime(dt, sun$solarNoon, units='hours')) <= 2){
    ToD <- 2
  } else if (as.numeric(difftime(dt, sun$dusk, units='hours')) >= -4 & as.numeric(difftime(dt, sun$dusk, units='hours')) <= 0){
    ToD <- 3
  } else {
    ToD <- NA
  }
  return(ToD)
}

# run 3 category test
df$ToD <- NULL
df$ToD <- mapply(calcToD_3, df$datetime, df$Latitude, df$Longitude)
df3 <- df

# works so let's make the 5 category function
calcToD_5 <- function(dt, lat, lon){
  sun <- getSunlightTimes(date=as.Date(dt), lat=lat, lon=lon)
  if (difftime(dt, sun$dawn, units='hours') > 21){
    sun <- getSunlightTimes(date=as.Date(dt)+1, lat=lat, lon=lon)
  }
  if (as.numeric(difftime(dt, sun$dawn, units='hours')) <= 0 & as.numeric(difftime(dt, sun$dawn, units='hours')) >= -3){
    ToD <- 1
  } else if (as.numeric(difftime(dt, sun$dawn, units='hours')) >= 0 & as.numeric(difftime(dt, sun$dawn, units='hours')) <= 3){
    ToD <- 2
  } else if (as.numeric(difftime(dt, sun$solarNoon, units='hours')) >= -1.5 & as.numeric(difftime(dt, sun$solarNoon, units='hours')) <= 1.5){
    ToD <- 3
  } else if (as.numeric(difftime(dt, sun$dusk, units='hours')) >= -3 & as.numeric(difftime(dt, sun$dusk, units='hours')) <= 0){
    ToD <- 4
  } else if (as.numeric(difftime(dt, sun$dusk, units='hours')) <= 3 & as.numeric(difftime(dt, sun$dusk, units='hours')) >= 0){
    ToD <- 5
  } else {
    ToD <- NA
  }
  return(ToD)
}

# run 5 category test
df$ToD <- NULL
df$ToD <- mapply(calcToD_5, df$datetime, df$Latitude, df$Longitude)
df5 <- df

# Visual check function
visual_check <- function(df, title){
  localTime <- df$datetime.local
  localTime <- force_tz(localTime, tzone='UTC')
  localTime <- strftime(localTime, format="%H:%M:%S")
  localTime <- as.POSIXct(localTime, format="%H:%M:%S")
  plot(localTime, as.factor(df$ToD), col=as.factor(df$ToD), xlab="Time", ylab="ToD Category", main=title)
}
# run check
visual_check(df3, "3-categories")
visual_check(df5, "5-categories (3 hour blocks)")
