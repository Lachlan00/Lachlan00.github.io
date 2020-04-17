library(ggplot2)
library(lubridate)
library(gridExtra)
library(stringr)
# set working directory
setwd("~/Development/other_people_help/Vanessa/bird_call_analysis")

###############################
# 1. read in and process data #
###############################
# function for reading in and processing V's data
read_V_csv <- function(fn){
  df <- read.csv(fn)
  colnames(df) <- c('pair','behaviour','behaviour_type','date','month','time','hour','daylight')
  df$behaviour_type <- as.factor(sapply(df$behaviour_type, function(x) gsub(" \xa0","",x)))
  df$dtAEST <- as.POSIXct(paste(df$date, df$time),'%d/%m/%Y %H:%M:%S', tz='Australia/Sydney')
  df[,c('date','month','time','hour')] <- NULL
  df$hour <- as.factor(paste0(str_pad(hour(df$dtAEST), 2, pad='0'),'h'))
  return(df)
}

###############################################
# 2. Function for plotting behavioural types #
###############################################
plot_behaviour <- function(df, behaviours, split_months=FALSE){
  # make sure behaviour is a factor
  df$behaviour_type <- as.factor(df$behaviour_type)
  # filter for behaviours
  df <- df[df$behaviour_type %in% behaviours,]
  # set factor levels
  df$behaviour_type <- factor(df$behaviour_type, levels=behaviours)
  # if not splitting months make plot for all times
  if (!split_months){
    p <- plot_behaviour_sub(df, 'Behaviour Frequencies')
  } else {
    # split data frame by month
    df_ls <- split(df, month(df$dtAEST))
    # make each plot using a for loop
    for (i in 1:length(df_ls)){
      title <- paste(month(df_ls[[i]]$dtAEST[1], label=TRUE, abbr=FALSE),'Behaviour Frequencies')
      df_ls [[i]] <- plot_behaviour_sub(df_ls[[i]], title)
    }
    # make a grid
    ncol <- floor(sqrt(length(df_ls)))
    p <- do.call("grid.arrange", c(df_ls, ncol=ncol))
  }
  return(p)
}

###################################################
# 2.1 Sub function for plotting behavioural types #
###################################################
plot_behaviour_sub <- function(df, title){
  p <- ggplot(df, aes(x=hour, fill=behaviour_type)) + 
    geom_bar() +
    scale_x_discrete(drop=FALSE) +
    scale_fill_discrete(drop=FALSE) +
    labs(fill='Behaviour Type') +
    ggtitle(title)
  return(p)
}
