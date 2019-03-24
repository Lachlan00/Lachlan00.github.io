library(suncalc)
# Check out graph with angles for definition of dawn, dusk, etc, but need better reference than wiki in paper
# Earliest
getSunlightTimes(date=as.Date(min(heatwave1_na$datetime),format="%Y-%m-%d %H:%M:%S"),
                 lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                 keep=c("sunrise", "dawn", "nauticalDawn", "sunset", "dusk", "nauticalDusk", "solarNoon"),tz="Australia/Adelaide")
# Latest
getSunlightTimes(date=as.Date(max(heatwave1_na$datetime),format="%Y-%m-%d %H:%M:%S"),
                 lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                 keep=c("sunrise", "dawn", "nauticalDawn", "sunset", "dusk", "nauticalDusk", "solarNoon"),tz="Australia/Adelaide")


# Need to have periods of same length, but daylight hours shift over time.
# Hence I defined 3 periods of same length informed by total length between dawn and dusk, and solarNoon
# Length = 4h

### DO NOT RUN
# Create smaller df for testing
heatwave1_na.df <- heatwave1_na

heatwave1_na <- subset(heatwave1_na, ID==10)
###  END DO NOT RUN

### h1 ###
# Check timezone of variable
attributes(heatwave1_na$datetime)$tzone

# Change timezone to local Adelaide time
heatwave1_na$datetime.local <- heatwave1_na$datetime
attributes(heatwave1_na$datetime.local)$tzone <-"Australia/Adelaide" 
attributes(heatwave1_na$datetime.local)$tzone

## CATEGORISE TIME OF DAY INTO 3 PERIODS
# 1: FIRST 4 HOURS AFTER DAWN
# 2: 2 HOURS BEFORE AND AFTER SUN IS AT ITS PEAK POSITION
# 3: 4 HOURS BEFORE DUSK

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

### h2 ###  
# Change timezone to local Adelaide time
heatwave2_na$datetime.local <- heatwave2_na$datetime
attributes(heatwave2_na$datetime.local)$tzone <-"Australia/Adelaide" 
attributes(heatwave2_na$datetime.local)$tzone

## CATEGORISE TIME OF DAY INTO 3 PERIODS
# 1: FIRST 4 HOURS AFTER DAWN
# 2: 2 HOURS BEFORE AND AFTER SUN IS AT ITS PEAK POSITION
# 3: 4 HOURS BEFORE DUSK

heatwave2_na$timeofday <- ifelse(
  difftime(time1=heatwave2_na$datetime.local,
           time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S",tz="Australia/Adelaide"),
                                             lat=heatwave2_na$Latitude[1],lon=heatwave2_na$Longitude[1],
                                             keep=c("dawn"),tz="Australia/Adelaide")[,4]),
           tz="Australia/Adelaide",
           units=c("mins") ) > 0 &
    difftime(time1=heatwave2_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=heatwave2_na$Latitude[1],lon=heatwave2_na$Longitude[1],
                                               keep=c("dawn"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) < 240,
  1,
  ifelse(
    difftime(time1=heatwave2_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=heatwave2_na$Latitude[1],lon=heatwave2_na$Longitude[1],
                                               keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) > -120 &
      difftime(time1=heatwave2_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=heatwave2_na$Latitude[1],lon=heatwave2_na$Longitude[1],
                                                 keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) < 120,
    2,
    ifelse(
      difftime(time1=heatwave2_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=heatwave2_na$Latitude[1],lon=heatwave2_na$Longitude[1],
                                                 keep=c("dusk"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) > -240 &
        difftime(time1=heatwave1_na$datetime.local,
                 time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                   lat=heatwave2_na$Latitude[1],lon=heatwave2_na$Longitude[1],
                                                   keep=c("dusk"),tz="Australia/Adelaide")[,4]),
                 tz="Australia/Adelaide",
                 units=c("mins") ) < 0,
      3, NA)
  )
)

### c1 ###
# Change timezone to local Adelaide time
coldwave1_na$datetime.local <- coldwave1_na$datetime
attributes(coldwave1_na$datetime.local)$tzone <-"Australia/Adelaide" 
attributes(coldwave1_na$datetime.local)$tzone

## CATEGORISE TIME OF DAY INTO 3 PERIODS
# 1: FIRST 4 HOURS AFTER DAWN
# 2: 2 HOURS BEFORE AND AFTER SUN IS AT ITS PEAK POSITION
# 3: 4 HOURS BEFORE DUSK

coldwave1_na$timeofday <- ifelse(
  difftime(time1=coldwave1_na$datetime.local,
           time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S",tz="Australia/Adelaide"),
                                             lat=coldwave1_na$Latitude[1],lon=coldwave1_na$Longitude[1],
                                             keep=c("dawn"),tz="Australia/Adelaide")[,4]),
           tz="Australia/Adelaide",
           units=c("mins") ) > 0 &
    difftime(time1=coldwave1_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=coldwave1_na$Latitude[1],lon=coldwave1_na$Longitude[1],
                                               keep=c("dawn"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) < 240,
  1,
  ifelse(
    difftime(time1=coldwave1_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=coldwave1_na$Latitude[1],lon=coldwave1_na$Longitude[1],
                                               keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) > -120 &
      difftime(time1=coldwave1_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=coldwave1_na$Latitude[1],lon=coldwave1_na$Longitude[1],
                                                 keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) < 120,
    2,
    ifelse(
      difftime(time1=coldwave1_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=coldwave1_na$Latitude[1],lon=coldwave1_na$Longitude[1],
                                                 keep=c("dusk"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) > -240 &
        difftime(time1=coldwave1_na$datetime.local,
                 time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                   lat=coldwave1_na$Latitude[1],lon=coldwave1_na$Longitude[1],
                                                   keep=c("dusk"),tz="Australia/Adelaide")[,4]),
                 tz="Australia/Adelaide",
                 units=c("mins") ) < 0,
      3, NA)
  )
)

### c2 ###
# Change timezone to local Adelaide time
coldwave2_na$datetime.local <- coldwave2_na$datetime
attributes(coldwave2_na$datetime.local)$tzone <-"Australia/Adelaide" 
attributes(coldwave2_na$datetime.local)$tzone

## CATEGORISE TIME OF DAY INTO 3 PERIODS
# 1: FIRST 4 HOURS AFTER DAWN
# 2: 2 HOURS BEFORE AND AFTER SUN IS AT ITS PEAK POSITION
# 3: 4 HOURS BEFORE DUSK

coldwave2_na$timeofday <- ifelse(
  difftime(time1=coldwave2_na$datetime.local,
           time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S",tz="Australia/Adelaide"),
                                             lat=coldwave2_na$Latitude[1],lon=coldwave2_na$Longitude[1],
                                             keep=c("dawn"),tz="Australia/Adelaide")[,4]),
           tz="Australia/Adelaide",
           units=c("mins") ) > 0 &
    difftime(time1=coldwave2_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=coldwave2_na$Latitude[1],lon=coldwave2_na$Longitude[1],
                                               keep=c("dawn"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) < 240,
  1,
  ifelse(
    difftime(time1=coldwave2_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=coldwave2_na$Latitude[1],lon=coldwave2_na$Longitude[1],
                                               keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) > -120 &
      difftime(time1=coldwave2_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=coldwave2_na$Latitude[1],lon=coldwave2_na$Longitude[1],
                                                 keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) < 120,
    2,
    ifelse(
      difftime(time1=coldwave2_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=coldwave2_na$Latitude[1],lon=coldwave2_na$Longitude[1],
                                                 keep=c("dusk"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) > -240 &
        difftime(time1=coldwave1_na$datetime.local,
                 time2=as.POSIXct(getSunlightTimes(date=as.Date(coldwave2_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                   lat=coldwave2_na$Latitude[1],lon=coldwave2_na$Longitude[1],
                                                   keep=c("dusk"),tz="Australia/Adelaide")[,4]),
                 tz="Australia/Adelaide",
                 units=c("mins") ) < 0,
      3, NA)
  )
)

  
### DO NOT RUN ##
### CHECKING OUTCOME OF CODE ###
heatwave1_na$diff2dawn <- difftime(time1=heatwave1_na$datetime.local,
           time2=as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S",tz="Australia/Adelaide"),
                                             lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                                             keep=c("dawn"),tz="Australia/Adelaide")[,4]),
           tz="Australia/Adelaide",
           units=c("mins") )


heatwave1_na$dawn <- as.POSIXct(getSunlightTimes(date=as.Date(heatwave1_na$datetime.local,format="%Y-%m-%d %H:%M:%S",tz="Australia/Adelaide"),
                            lat=heatwave1_na$Latitude[1],lon=heatwave1_na$Longitude[1],
                            keep=c("dawn"),tz="Australia/Adelaide")[,4])

### END DO NOT RUN ###


### Add time of day to whole STUDYPERIOD ###
# Change timezone to local Adelaide time
studyperiod_na$datetime.local <- studyperiod_na$datetime
attributes(studyperiod_na$datetime.local)$tzone <-"Australia/Adelaide" 
attributes(studyperiod_na$datetime.local)$tzone

## CATEGORISE TIME OF DAY INTO 3 PERIODS
# 1: FIRST 4 HOURS AFTER DAWN
# 2: 2 HOURS BEFORE AND AFTER SUN IS AT ITS PEAK POSITION
# 3: 4 HOURS BEFORE DUSK

studyperiod_na$timeofday <- ifelse(
  difftime(time1=studyperiod_na$datetime.local,
           time2=as.POSIXct(getSunlightTimes(date=as.Date(studyperiod_na$datetime.local,format="%Y-%m-%d %H:%M:%S",tz="Australia/Adelaide"),
                                             lat=studyperiod_na$Latitude[1],lon=studyperiod_na$Longitude[1],
                                             keep=c("dawn"),tz="Australia/Adelaide")[,4]),
           tz="Australia/Adelaide",
           units=c("mins") ) > 0 &
    difftime(time1=studyperiod_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(studyperiod_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=studyperiod_na$Latitude[1],lon=studyperiod_na$Longitude[1],
                                               keep=c("dawn"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) < 240,
  1,
  ifelse(
    difftime(time1=studyperiod_na$datetime.local,
             time2=as.POSIXct(getSunlightTimes(date=as.Date(studyperiod_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                               lat=studyperiod_na$Latitude[1],lon=studyperiod_na$Longitude[1],
                                               keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
             tz="Australia/Adelaide",
             units=c("mins") ) > -120 &
      difftime(time1=studyperiod_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(studyperiod_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=studyperiod_na$Latitude[1],lon=studyperiod_na$Longitude[1],
                                                 keep=c("solarNoon"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) < 120,
    2,
    ifelse(
      difftime(time1=studyperiod_na$datetime.local,
               time2=as.POSIXct(getSunlightTimes(date=as.Date(studyperiod_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                 lat=studyperiod_na$Latitude[1],lon=studyperiod_na$Longitude[1],
                                                 keep=c("dusk"),tz="Australia/Adelaide")[,4]),
               tz="Australia/Adelaide",
               units=c("mins") ) > -240 &
        difftime(time1=studyperiod_na$datetime.local,
                 time2=as.POSIXct(getSunlightTimes(date=as.Date(studyperiod_na$datetime.local,format="%Y-%m-%d %H:%M:%S", tz="Australia/Adelaide"),
                                                   lat=studyperiod_na$Latitude[1],lon=studyperiod_na$Longitude[1],
                                                   keep=c("dusk"),tz="Australia/Adelaide")[,4]),
                 tz="Australia/Adelaide",
                 units=c("mins") ) < 0,
      3, NA)
  )
)

write.csv(studyperiod_na, "studyperiod_na.csv")

