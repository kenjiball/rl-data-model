# Test build some rolling average attributes for season

# Load required packages
library(zoo)

# Must run function-season-data first to have season data matrix available or must be passed.

roll_mean_data <- function(data, team, window){

    # team <- "Penrith"
    # columns <- "[7:80,82:155]" 
    # window <- 5
  
    season_2018_team <- subset(data, for_name == for_name_test )
    
    header_team <- season_2018_team[c(1:6,81)]
    data_roll <- season_2018_team[c(7:80,82:155)] # season_2018_team[c(7:80,82:155)]
    
    rollmean_games <- rollapply(data_roll,window,mean, by.column = TRUE, partial = TRUE, align = "right")
    
    colnames(rollmean_games) <- paste(colnames(rollmean_games),"last3", sep = "_")
    
    cbind(header_team,rollmean_games)
    
    return(rollmean_games)

}

roll_mean_data(season_2018_datamatrix,"Penrith",3)


class_list <- sapply(season_2018_team,class)

# check if all columns in data_roll are numeric
length(lapply(data_roll, is.numeric)) == sum(unlist(lapply(data_roll, is.numeric)))




# playing with correlation
cor_test <- cor(data_roll)
write.csv(cor_test, "../correlation_test.csv")

plot(season_2018_datamatrix$for_tries,season_2018_datamatrix$for_completion_rate)


