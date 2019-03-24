library(suncalc)
library(parallel)
setwd("~/Development/other_people_help/Katrin")

# Read in dataset
df <- read.csv("studyperiod_na.csv")

# Clean data
df$X <- NULL
df$date <- NULL
df$datetime <- NULL
df$datetimeACST <- NULL

# Convert the datetime column to POSIXct datetime data type
df$datetimeUTC <- as.POSIXct(df$datetimeUTC, tz='UTC')

# calcToD function
calcToD <- function(dt, lat, lon){
  sun <- getSunlightTimes(date=as.Date(dt), lat=lat, lon=lon)
  if (as.numeric(dt - sun$dawn) >= 0 & as.numeric(dt - sun$dawn) <= 4){
    ToD <- 1
  } else if (as.numeric(dt - sun$solarNoon) >= -2 & as.numeric(dt - sun$solarNoon) <= 2){
    ToD <- 2
  } else if (as.numeric(dt - sun$dusk) >= -4 & as.numeric(dt - sun$dusk) <= 0){
    ToD <- 3
  } else {
    ToD <- NA
  }
  return(ToD)
}

# run function
df$ToD <- mapply(calcToD, df$datetimeUTC, df$Latitude, df$Longitude)

# run function in paralellised form (remember to change core number to reflect your machine)
dfm <- as.matrix(df[,c("datetimeUTC", "Latitude", "Longitude")])
result <- mclapply(seq_len(nrow(dfm)),
                   function(x) do.call(calcToD, as.list(dfm[x,])),
                   mc.cores=6L)
df$ToD <- unlist(result)

# Check data
df_test <- head(df, 3000)
localTime <- as.POSIXlt(df_test$datetimeUTC, tz='Australia/Adelaide')
localTime <- strftime(localTime, format="%H:%M:%S")
localTime <- as.POSIXct(localTime, format="%H:%M:%S")
plot(localTime, as.factor(df_test$ToD), col=as.factor(df_test$ToD), xlab="Time", ylab="ToD Category")
