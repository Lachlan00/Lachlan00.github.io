# Load libraries
library(suncalc)
library(parallel)

# Set working directory to repository containing data
setwd("~/Development/other_people_help/Katrin")

# Read in your dataset
df <- read.csv("studyperiod_na.csv")

# Remove annoying 'index column' that got saved. TIP: to stop this from happening
# you can set `row.names=FALSE` as an argument in the write.csv function.
df$X <- NULL

# Let's remove the 'date' column as we have that information already in the datetime stamps
# and it's just taking up space (2 million points!) We'll also get rid of the `datetime` column
# for this reason as it's a duplicate of `datetimneUTC`. Also we'll be doin our calculations 
# using UTC (best practice) so let's drop the local timestamps.
df$date <- NULL
df$datetime <- NULL
df$datetimeACST <- NULL

# Convert the datetime column to POSIXct datetime data type
df$datetimeUTC <- as.POSIXct(df$datetimeUTC, tz='UTC')

# Cool so let's go and do that calculation and get the sunlight data.
# Instead of using a for loop we're going to try and vectorise this to make
# it faster. So to do this we're going to make our own function and then
# call it on the dataset using `mapply()`. 
calcToD <- function(dt, lat, lon){
  # so this is afunction to get the time of day categories
  # let's get the suntimes
  sun <- getSunlightTimes(date=as.Date(dt), lat=lat, lon=lon)
  # now let's check which category the data fits into by calculating the
  # time deltas (difference in time) between the timestamps and the dt
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

# Cool so now we have our function written let's apply it to the dataframe
# and save the results as a new column. Note that this is much quicker than 
# a for loop but will still take a while. About 6-7 hours... so...
# *DON'T* run this line yet! There's still a faster way.
df$ToD <- mapply(calcToD, df$datetimeUTC, df$Latitude, df$Longitude)

# Because the above code takes so long we might consider parallelising the code
# so it splits the computations up on different cores. We can do that by trasnforming
# the dataframe into a matrix and the runnign the function in a paralellised form 
# with the following function from the `parallel` library.
dfm <- as.matrix(df[,c("datetimeUTC", "Latitude", "Longitude")])
result <- mclapply(seq_len(nrow(dfm)),
                   function(x) do.call(calcToD, as.list(dfm[x,])),
                   mc.cores=4L) # here we are giving the function 6 cores (my computer has 8)
df$ToD <- unlist(result)

# So let's do a check to make sure we did everything right! Let's make a simple plot
# from a subset of the dataframe to check the categories are roughly in the right spot
df_test <- head(df, 500000)
# so let's make a list of local timestamps
localTime <- as.POSIXlt(df_test$datetimeUTC, tz='Australia/Adelaide')
# let's strip away the date component and add a random year
localTime <- strftime(localTime, format="%H:%M:%S")
localTime <- as.POSIXct(localTime, format="%H:%M:%S")
# now let's plot this against out ToD varibable to see if it looks about right
plot(localTime, as.factor(df_test$ToD), col=as.factor(df_test$ToD), xlab="Time", ylab="ToD Category")

# Notes:
# If you want to display a UTC timestamp in Adelaide's time zone, just use: 
# `as.POSIXlt(dt, tz="Australia/Adelaide")`. It's bad practice to do calculations 
# on non UTC timestamps and in my opinion you should ALWAYS store POSIX
# datetimes as UTC and only convert to local time when you need to display 
# these values. Will save you a ton of pain using this approach. 

