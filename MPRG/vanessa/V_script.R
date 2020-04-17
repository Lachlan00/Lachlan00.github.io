# set working directory
setwd("~/Development/other_people_help/Vanessa/bird_call_analysis")

# load your functions
source('./V_functions.R')

# load your data with super efficient function
df <- read_V_csv('./ALL_VA_&_BB.csv')

# make some plots and save them to a pdf file
plot_behaviour(df, c('A - ooom', 'B - ooom'))
plot_behaviour(df, c('A - ooom', 'B - ooom'), split_months=TRUE)
plot_behaviour(df, c('Circling', 'Pecking', 'Ruffle feathers', 'Wing display', 'Jump Fly'))
plot_behaviour(df, c('Circling', 'Pecking', 'Ruffle feathers', 'Wing display', 'Jump Fly'), split_months=TRUE)
# note the last one you might want to click the "zoom" button in the plot window to see 
# it properly as it is squashed in the small R-Studio plot window.