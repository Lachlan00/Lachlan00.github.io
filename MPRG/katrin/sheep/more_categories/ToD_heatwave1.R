### Time of day for heatwave1_na ###

## CATEGORISE TIME OF DAY INTO 3 PERIODS
# 1: FIRST 4 HOURS AFTER DAWN
# 2: 2 HOURS BEFORE AND AFTER SUN IS AT ITS PEAK POSITION
# 3: 4 HOURS BEFORE DUSK

### Method 1: loop for 3 periods
heatwave1_na$timeofday <- ifelse(
  difftime(time1=heatwave1_na$datetime.local,
           time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S",tz="Australia/Adelaide"),
                                             lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                                             keep=c("dawn"),tz="Australia/Adelaide")[,4]),
           tz="Australia/Adelaide",
           units=c("mins") ) > 0 &
    difftime(time1=heatwave1_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                                               keep=c("dawn"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) < 240,
  1,
  ifelse(
    difftime(time1=heatwave1_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                                               keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) > -120 &
      difftime(time1=heatwave1_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                                                 keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) < 120,
    2,
    ifelse(
      difftime(time1=heatwave1_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                                                 keep=c("dusk"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) > -240 &
        difftime(time1=heatwave1_na$datetime.local,
                 time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                   lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                                                   keep=c("dusk"),tz="Australia/Adelaide")[,4]),
                 tz="Australia/Adelaide",
                 units=c("mins") ) < 0,
      3, NA)
  )
)


### Method 2: 3 periods
# -> only calculating time of day categories for periods 2 and 3

calcToD <- function(dt, lat, lon){
  sun <- getSunlightTimes(date=as.Date(dt), lat=lat, lon=lon)
  if (as.numeric(dt - sun$dawn, units=c("mins")) >= 0 & as.numeric(dt - sun$dawn, units=c("mins")) <= 240){
    ToD <- 1
  } else if (as.numeric(dt - sun$solarNoon,units=c("mins")) >= -120 & as.numeric(dt - sun$solarNoon, units=c("mins")) <= 120){
    ToD <- 2
  } else if (as.numeric(dt - sun$dusk, units=c("mins")) >= -240 & as.numeric(dt - sun$dusk, units=c("mins")) <= 0){
    ToD <- 3
  } else {
    ToD <- NA
  }
  return(ToD)
}


heatwave1_na$ToD <- mapply(calcToD, heatwave1_na$datetime, heatwave1_na$Latitude, heatwave1_na$Longitude)


#### NEW ####
### Method 2: 5 periods
# -> only calculating time of day categories for periods 3, 4 and 5

## CATEGORISE TIME OF DAY INTO 5 PERIODS
# 1: 4 HOURS Before DAWN
# 2: 4 HOURS After DAWN
# 3: 2 HOURS BEFORE AND AFTER SUN IS AT ITS PEAK POSITION
# 4: 4 HOURS BEFORE DUSK
# 5: 4 HOURS After DUSK

calcToD <- function(dt, lat, lon){
  sun <- getSunlightTimes(date=as.Date(dt), lat=lat, lon=lon)
  if (as.numeric(dt - sun$dawn, units=c("mins")) >= -240 & as.numeric(dt - sun$dawn, units=c("mins")) <= 0){
    ToD <- 1
  } else if (as.numeric(dt - sun$dawn, units=c("mins")) >= 0 & as.numeric(dt - sun$dawn, units=c("mins")) <= 240){
    ToD <- 2
  } else if (as.numeric(dt - sun$solarNoon, units=c("mins")) >= -120 & as.numeric(dt - sun$solarNoon, units=c("mins")) <= 120){
    ToD <- 3
  } else if (as.numeric(dt - sun$dusk, units=c("mins")) >= -240 & as.numeric(dt - sun$dusk, units=c("mins")) <= 0){
    ToD <- 4
  } else if (as.numeric(dt - sun$dusk, units=c("mins")) >= 0 & as.numeric(dt - sun$dusk, units=c("mins")) <= 240){
    ToD <- 5
  } else {
    ToD <- NA
  }
  return(ToD)
}


heatwave1_na$ToD <- mapply(calcToD, heatwave1_na$datetime, heatwave1_na$Latitude, heatwave1_na$Longitude)

save(heatwave1_na, file="heatwave1_na_ToD.RData")
