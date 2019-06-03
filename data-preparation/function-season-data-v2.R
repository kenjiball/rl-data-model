# Create a all time file which prepares data for the NRL ML Model
# Version 2: function-season-data-v2
# Major Overhaul: Improve function by using tidyverse packages

# Call packages
source("./load-packages.R")

# Set working directory
setwd(wd_data)
getwd()

# Function to run season data into a single file
create.statsmatrix <-function(match_vector){
   #match_vector <- season_2018
  
  for(i in 1:length(match_vector)){  
  #i <- 2

  # Load a json file for a single game and build to data frame
  jsonFileName <- match_vector[i]
  jsonMatchData <- fromJSON(paste("matchdata_",jsonFileName, ".json",sep = ""))
  
  # Unload the json format into a data frame
  # Get match ID and match information
  
  match_id <- data.frame(jsonMatchData$match_id)
  names(match_id) <- c("match_id") 
  
  match_info <- match_id %>% 
                    mutate(season = substr(match_id, 4, 7),
                          round = substr(match_id, 8, 9),
                          match = substr(match_id, 10, 11)
                          ) 
  match_info <- match_info %>% mutate_if(sapply(match_info,is.factor), as.character)
    
  ##### Get stats for Team A
  teamA_stats <- jsonlite:::simplify(jsonMatchData$team_A$stats, flatten = TRUE)
  
  # Remove columns which are not required: rm_cols
  rm_cols <- c("momentum", "period", "completion_rate", "possession_time" )
  rm_cols_ind <- names(teamA_stats) %in% rm_cols
  teamA_stats_df <- teamA_stats[!rm_cols_ind]
  teamA_stats_df <- as.data.frame(teamA_stats_df)
  
  # Add team name to data frame
  teamA_stats_df <- teamA_stats_df %>% 
                    mutate( name = as.character(jsonMatchData$team_A$name) ) %>%
                    select(name, everything())
  
  # Append column names with teamA_
  colnames(teamA_stats_df) <- paste("teamA", colnames(teamA_stats_df), sep = "_")

  
  #### Get stats for Team B
  teamB_stats <- jsonlite:::simplify(jsonMatchData$team_B$stats, flatten = TRUE)
  
  # Remove columns which are not required: rm_cols
  rm_cols <- c("momentum", "period", "completion_rate", "possession_time" )
  rm_cols_ind <- names(teamB_stats) %in% rm_cols
  teamB_stats_df <- teamB_stats[!rm_cols_ind]
  teamB_stats_df <- as.data.frame(teamB_stats_df)
  
  # Add team name to data frame
  teamB_stats_df <- teamB_stats_df %>% 
    mutate( name = as.character(jsonMatchData$team_B$name) ) %>%
    select(name, everything())
  
  # Append column names with teamA_
  colnames(teamB_stats_df) <- paste("teamB", colnames(teamB_stats_df), sep = "_")
  
  
  #### Get Stat Comparisons Data
  stats_comparison <- jsonMatchData$stat_comparisons %>% 
                      gather(team, measure_name, -stat_name) %>%
                      spread(stat_name, measure_name  ) %>%
                      as.data.frame()
  
  # Filter for team A and rename columns
  teamA_sc <- stats_comparison %>% filter(team == "team_A_value") %>% select(-team)
  names(teamA_sc) <- paste("teamA",names(teamA_sc),sep="_")
  names(teamA_sc) <- gsub(" ", "_", names(teamA_sc), fixed=TRUE)  
  teamA_sc
  
  # Filter for team A and rename columns
  teamB_sc <- stats_comparison %>% filter(team == "team_B_value") %>% select(-team)
  names(teamB_sc) <- paste("teamB",names(teamB_sc),sep="_")
  names(teamB_sc) <- gsub(" ", "_", names(teamB_sc), fixed=TRUE)  
  teamB_sc
  
  
  #### MUTATE to fix formats of existing values
  
  teamA_stats_df <- teamA_stats_df %>% 
                         mutate(teamA_goal_made = as.numeric(strsplit(as.character(teamA_stats_df$teamA_goal_rate), " from ")[[1]][1]),
                                teamA_goal_attempted = as.numeric(strsplit(as.character(teamA_stats_df$teamA_goal_rate), " from ")[[1]][2]),
                                )
                                
  teamB_stats_df <- teamB_stats_df %>% 
                         mutate(teamB_goal_made = as.numeric(strsplit(as.character(teamB_stats_df$teamB_goal_rate), " from ")[[1]][1]),
                                teamB_goal_attempted = as.numeric(strsplit(as.character(teamB_stats_df$teamB_goal_rate), " from ")[[1]][2])
                                )
  
  
  teamA_stats_df$teamA_goal_rate <- NULL
  teamB_stats_df$teamB_goal_rate <- NULL 
  
  #### MUTATE to create additional metrics in the data frame
  
  teamA_stats_df <- teamA_stats_df %>% 
    mutate(teamA_tackle_success_rate = as.numeric(1 - teamA_stats_df$teamA_missed_tackles / teamA_stats_df$teamA_tackles)
           ,teamA_completion_rate = as.numeric(teamA_stats_df$teamA_complete_sets / teamA_stats_df$teamA_total_sets)
           ,teamA_effective_offload_rate = as.numeric(teamA_stats_df$teamA_effective_offloads / teamA_stats_df$teamA_off_loads)
           ,teamA_oppHalf_tackle_percent = teamA_stats_df$teamA_tackle_opp_half / teamA_stats_df$teamA_play_the_balls
           ,teamA_redzone_tackle_percent = teamA_stats_df$teamA_tackledOpp20 / teamA_stats_df$teamA_play_the_balls
           ,teamA_redzone_conversion = teamA_stats_df$teamA_tries / teamA_stats_df$teamA_tackledOpp20
           ,teamA_linebreak_try_ratio = teamA_stats_df$teamA_tries / teamA_stats_df$teamA_line_breaks
           ,teamA_average_kick_metres = teamA_stats_df$teamA_kick_metres / teamA_stats_df$teamA_kicks
           ,teamA_average_run_metres = teamA_stats_df$teamA_run_metres / teamA_stats_df$teamA_runs
           ,teamA_run_metres_success_rate = teamA_stats_df$teamA_runs_8plus_meters / teamA_stats_df$teamA_runs
           ,teamA_penalty_ratio = teamA_stats_df$teamA_penaltiesAwarded / (teamA_stats_df$teamA_penalties_conceded + teamA_stats_df$teamA_penaltiesAwarded)
           ,teamA_tackle_busts_per_run = teamA_stats_df$teamA_tackle_busts / teamA_stats_df$teamA_runs
           ,teamA_gang_tackle_ratio = teamB_stats_df$teamB_tackles / teamA_stats_df$teamA_runs
           ,teamA_match_result = ifelse(teamA_stats_df$teamA_points > teamB_stats_df$teamB_points, "Win",
                                        ifelse(teamA_stats_df$teamA_points < teamB_stats_df$teamB_points, "Lose",
                                               ifelse(teamA_stats_df$teamA_points == teamB_stats_df$teamB_points,"Draw",NA)))
          )
  
  teamB_stats_df <- teamB_stats_df %>% 
    mutate(teamB_tackle_success_rate = as.numeric(1 - teamB_stats_df$teamB_missed_tackles / teamB_stats_df$teamB_tackles)
           ,teamB_completion_rate = as.numeric(teamB_stats_df$teamB_complete_sets / teamB_stats_df$teamB_total_sets)
           ,teamB_effective_offload_rate = as.numeric(teamB_stats_df$teamB_effective_offloads / teamB_stats_df$teamB_off_loads)
           ,teamB_oppHalf_tackle_percent = teamB_stats_df$teamB_tackle_opp_half / teamB_stats_df$teamB_play_the_balls
           ,teamB_redzone_tackle_percent = teamB_stats_df$teamB_tackledOpp20 / teamB_stats_df$teamB_play_the_balls
           ,teamB_redzone_conversion = teamB_stats_df$teamB_tries / teamB_stats_df$teamB_tackledOpp20
           ,teamB_linebreak_try_ratio = teamB_stats_df$teamB_tries / teamB_stats_df$teamB_line_breaks
           ,teamB_average_kick_metres = teamB_stats_df$teamB_kick_metres / teamB_stats_df$teamB_kicks
           ,teamB_average_run_metres = teamB_stats_df$teamB_run_metres / teamB_stats_df$teamB_runs
           ,teamB_run_metres_success_rate = teamB_stats_df$teamB_runs_8plus_meters / teamB_stats_df$teamB_runs
           ,teamB_penalty_ratio = teamB_stats_df$teamB_penaltiesAwarded / (teamB_stats_df$teamB_penalties_conceded + teamB_stats_df$teamB_penaltiesAwarded)
           ,teamB_tackle_busts_per_run = teamB_stats_df$teamB_tackle_busts / teamB_stats_df$teamB_runs
           ,teamB_gang_tackle_ratio = teamA_stats_df$teamA_tackles / teamB_stats_df$teamB_runs
           ,teamB_match_result = ifelse(teamB_stats_df$teamB_points > teamA_stats_df$teamA_points, "Win",
                                        ifelse(teamB_stats_df$teamB_points < teamA_stats_df$teamA_points, "Lose",
                                               ifelse(teamB_stats_df$teamB_points == teamA_stats_df$teamA_points,"Draw",NA)))
          )
  
  #### set up array for both team for and against teams

  # Place holder data frames for the home and away team labels
  home_team <- as.data.frame("Home",stringsAsFactors=FALSE)
  names(home_team) <- "Home_Away"
  away_team <- as.data.frame("Away",stringsAsFactors=FALSE)
  names(away_team) <- "Home_Away"  
  
  
  # Bind Columns to create a single statistics data frames
  # Home data bind columns
  home_data <- bind_cols(match_info
            , home_team
            , teamA_stats_df
            , teamA_sc
            , teamB_stats_df
            , teamB_sc
            )
  
  # Remane Col Name Team_A to for and Team_B to against
  names(home_data) <- gsub("teamA", "for", names(home_data), fixed=TRUE)
  names(home_data) <- gsub("teamB", "against", names(home_data), fixed=TRUE)
  
  # Away data bind columns
  away_data <- bind_cols(match_info
            , away_team
            , teamB_stats_df
            , teamB_sc
            , teamA_stats_df
            , teamA_sc
            )
  
  # Remane Col Name Team_A to for and Team_B to against
  names(away_data) <- gsub("teamB", "for", names(away_data), fixed=TRUE)
  names(away_data) <- gsub("teamA", "against", names(away_data), fixed=TRUE)
  
  # If Else statement to bind rows into the master data frame
  if(i == 1){
    stats_df <- bind_rows(home_data,away_data)
  } else{
    stats_df <- bind_rows(stats_df, home_data,away_data)
  }
  
  
  
  }
  
  return(stats_df)
}


# season_2010_datamatrix <- create.statsmatrix(season_2010)
season_2011_datamatrix <- create.statsmatrix(season_2011)
season_2012_datamatrix <- create.statsmatrix(season_2012)
season_2013_datamatrix <- create.statsmatrix(season_2013)
season_2014_datamatrix <- create.statsmatrix(season_2014)
season_2015_datamatrix <- create.statsmatrix(season_2015)
season_2016_datamatrix <- create.statsmatrix(season_2016)
season_2017_datamatrix <- create.statsmatrix(season_2017)
season_2018_datamatrix <- create.statsmatrix(season_2018)
season_2019_datamatrix <- create.statsmatrix(season_2019)


# Row binds to combin into a all data matrix
season_all_datamatrix <- bind_rows(season_2011_datamatrix
                                    ,season_2012_datamatrix
                                    ,season_2013_datamatrix
                                    ,season_2014_datamatrix
                                    ,season_2015_datamatrix
                                    ,season_2016_datamatrix
                                    ,season_2017_datamatrix
                                    ,season_2018_datamatrix
                                    ,season_2019_datamatrix)

# dim(season_2010_datamatrix)
dim(season_2011_datamatrix)
dim(season_2012_datamatrix) 
dim(season_2013_datamatrix) 
dim(season_2014_datamatrix) 
dim(season_2015_datamatrix) 
dim(season_2016_datamatrix) 
dim(season_2017_datamatrix)
dim(season_2018_datamatrix)
dim(season_2019_datamatrix)
dim(season_all_matchmatrix)


write.csv(season_all_datamatrix,file="../season_all_datamatrix.csv")


# check class
sapply(season_2018_datamatrix,class)
sapply(season_2018_datamatrix_old,class)

class(season_2018_datamatrix$for_match_result)
class(season_2018_datamatrix_old$for_match_result)



# To check that data is all equal to previous function
season_2018_datamatrix$against_territory_time<- NULL
season_2018_datamatrix$for_territory_time<- NULL

dim(season_2018_datamatrix_old)
dim(season_2018_datamatrix)
head(season_2018_datamatrix)

# If this check is true then correctly built

all_equal(season_2018_datamatrix_old,season_2018_datamatrix, convert = TRUE, ignore_col_order = TRUE)









