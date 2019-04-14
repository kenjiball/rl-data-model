# Test build some rolling average attributes for season

# Load required packages
library(zoo)

# Must run function-season-data first to have season data matrix available or must be passed.

roll_mean_data <- function(data, team, window, app_name,header){
      
    #data <- season_2018_datamatrix
    #team <- "Penrith"
    #window <- 5
  
    data <- subset(data, for_name == for_name_test )
    
    header_team <- data[,which(!sapply(data,is.numeric))]
    data_roll <- data[,which(sapply(data,is.numeric))] # season_2018_team[c(7:80,82:155)]
    
    rollmean_games <- rollapply(data_roll,window,mean, by.column = TRUE, partial = TRUE, align = "right", fill = 0)
    
    colnames(rollmean_games) <- paste(colnames(rollmean_games),app_name, sep = "_")
    
    if(header == TRUE){
      result <- data.frame(header_team,rollmean_games)
    }else{
      result <-  as.data.frame(rollmean_games)
    }
    
    return(result)

}

# Roll over all team
team_names <- levels(as.factor(season_2018_datamatrix$for_name))

# for loop through the list of team names

# Roll over a single team
roll1_data <- roll_mean_data(season_2018_datamatrix,"Penrith",list(-1:-1),"last1",FALSE)
roll3_data <- roll_mean_data(season_2018_datamatrix,"Penrith",list(-1:-3),"last3",FALSE)
roll5_data <- roll_mean_data(season_2018_datamatrix,"Penrith",list(-1:-5),"last5",FALSE)
rollseason_data <- roll_mean_data(season_2018_datamatrix,"Penrith",list(-1:-30),"seasonavg",FALSE)

panthers <-subset(data, for_name == "Penrith" )
team_rollingave_df <- data.frame(panthers,roll1_data, roll3_data, roll5_data, rollseason_data)

dim(team_rollingave_df)

write.csv(team_rollingave_df, "../panthers_avg.csv")

# Need to create an extra blank row for the upcming match with the prev avgs but no match outcome


# testing below
panthers <-subset(data, for_name == "Penrith" )
write.csv(team_rollingave_df, "../panthers_avg.csv")

rollapply(panthers$for_errors,list(-1:-3),mean, by.column = TRUE, partial = TRUE, align = "right", fill = 0)
season_2018_datamatrix[,numeric_list]
class_list <- sapply(season_2018_team,class)










