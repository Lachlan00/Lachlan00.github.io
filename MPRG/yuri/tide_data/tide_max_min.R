library(lubridate)

# read in data
df <- read.csv('FortDenison_sealevel_hourly.csv')

# lists to capture data
dt_max_ls <- list()
dt_min_ls <- list()
max_ls <- list()
min_ls <- list()

# slow nested for loops but can't be bothered vectorising
# fast enough for this
k <- 0
year_ls <- split(df, df$year)
for (df_year in year_ls) {
  month_ls <- split(df_year, df_year$month)
  for (df_month in month_ls) {
    day_ls <- split(df_month, df_month$day)
    for (df_day in day_ls) {
      # get indexes of max and min
      i <- which(df_day$sea_level==max(df_day$sea_level))
      j <- which(df_day$sea_level==min(df_day$sea_level))
      max_ls[k] <- df_day$sea_level[i]
      min_ls[k] <- df_day$sea_level[j]
      dt_max_ls[k] <- make_datetime(year=df_day$year[1],month=df_day$month[1],
                              day=df_day$day[1],hour=df_day$hour[i])
      dt_min_ls[k] <- make_datetime(year=df_day$year[1],month=df_day$month[1],
                              day=df_day$day[1],hour=df_day$hour[j])
      k <- k + 1
      print(paste("Processing day:",k))
    }  
  }
}
# hacky way to make a dataframe from lists
df_out <- as.data.frame(unlist(dt_max_ls))
df_out$dt_high <- unlist(dt_max_ls)
df_out$high <- unlist(max_ls)
df_out$dt_low <- unlist(dt_min_ls)
df_out$low <- unlist(min_ls)
df_out[[1]] <- NULL

# write to file
write.csv(df_out, 'FortDenison_tides_highlow.csv', row.names=FALSE)
