# Test build some rolling average attributes for season

# Load required packages
library(zoo)

# Must run function-season-data first to have season data matrix available or must be passed.

for_name_test <- "Penrith"

season_2018_team <- subset(season_2018_datamatrix, for_name == for_name_test )



rollapply(season_2018_team[7:10],3,mean, by.column = TRUE, fill = NA)

sapply(season_2018_team,class)



