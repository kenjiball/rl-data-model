# Create a all time file which prepares data for the NRL ML Model
# Version 2: function-season-data-v2
# Major Overhaul: Improve function by using tidyverse packages

# Call packages
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

# Set working directory
setwd(wd)
getwd()


match_vector <- season_2018
i <- 1    

  # Load a json file for a single game and build to data frame
  jsonFileName <- match_vector[i]
  jsonMatchData <- fromJSON(paste("matchdata_",jsonFileName, ".json",sep = ""))
  
  # Unload the json format into a data frame
  # Get match ID and match information
  
  match_id <- data.frame(jsonMatchData$match_id)
  names(match_id) <- c("match_id") 
  
  match_id %>% mutate(season = substr(match_id, 4, 7),
                    round = substr(match_id, 8, 9),
                    match = substr(match_id, 10, 11)
                    )   
    
  ##### Get stats for Team A
  teamA_stats <- jsonlite:::simplify(jsonMatchData$team_A$stats, flatten = TRUE)
  
  # Remove columns which are not required: rm_cols
  rm_cols <- c("momentum", "period", "completion_rate", "possession_time", "territory_time")
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
  rm_cols <- c("momentum", "period", "completion_rate", "possession_time", "territory_time")
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
                         mutate( teamA_goal_made = as.numeric(strsplit(as.character(teamA_stats_df$teamA_goal_rate), " from ")[[1]][1]),
                                teamA_goal_attempted = as.numeric(strsplit(as.character(teamA_stats_df$teamA_goal_rate), " from ")[[1]][2]),
                                teamB_goal_made = as.numeric(strsplit(as.character(teamB_stats_df$teamB_goal_rate), " from ")[[1]][1]),
                                teamB_goal_attempted = as.numeric(strsplit(as.character(teamB_stats_df$teamB_goal_rate), " from ")[[1]][2])
                                )
  
  
  teamA_stats_df$teamA_goal_rate <- NULL
  teamB_stats_df$teamB_goal_rate <- NULL 
  

  
  
  
  
  
  
  
  
  
  
  # Create additional metrics to be used on the data.frame
  
  teamA_tackle_success_rate <- as.numeric(1 - teamA_stats_df$teamA_missed_tackles / teamA_stats_df$teamA_tackles)
  teamA_completion_rate <- as.numeric(teamA_stats_df$teamA_complete_sets / teamA_stats_df$teamA_total_sets)
  teamA_effective_offload_rate <- as.numeric(teamA_stats_df$teamA_effective_offloads / teamA_stats_df$teamA_off_loads)
  teamA_oppHalf_tackle_percent <- teamA_stats_df$teamA_tackle_opp_half / teamA_stats_df$teamA_play_the_balls
  teamA_redzone_tackle_percent <- teamA_stats_df$teamA_tackledOpp20 / teamA_stats_df$teamA_play_the_balls
  teamA_redzone_conversion <- teamA_stats_df$teamA_tries / teamA_stats_df$teamA_tackledOpp20
  teamA_linebreaK_try_ratio <- teamA_stats_df$teamA_tries / teamA_stats_df$teamA_line_breaks
  teamA_average_kick_metres <- teamA_stats_df$teamA_kick_metres / teamA_stats_df$teamA_kicks
  teamA_average_run_metres <- teamA_stats_df$teamA_run_metres / teamA_stats_df$teamA_runs
  teamA_run_metres_success_rate <- teamA_stats_df$teamA_runs_8plus_meters / teamA_stats_df$teamA_runs
  teamA_penalty_ratio <- teamA_stats_df$teamA_penaltiesAwarded / (teamA_stats_df$teamA_penalties_conceded + teamA_stats_df$teamA_penaltiesAwarded)
  teamA_tackle_busts_per_run <- teamA_stats_df$teamA_tackle_busts / teamA_stats_df$teamA_runs
  teamA_gang_tackle_ratio <- teamB_stats_df$teamB_tackles / teamA_stats_df$teamA_runs
  teamA_match_result <- ifelse(teamA_stats_df$teamA_points > teamB_stats_df$teamB_points, "Win",
                               ifelse(teamA_stats_df$teamA_points < teamB_stats_df$teamB_points, "Lose",
                                      ifelse(teamA_stats_df$teamA_points < teamB_stats_df$teamB_points,"Draw",NA)))
  
  teamA_additional_stats <- data.frame(teamA_tackle_success_rate,
                                       teamA_completion_rate,
                                       teamA_effective_offload_rate,
                                       teamA_oppHalf_tackle_percent,
                                       teamA_redzone_tackle_percent,
                                       teamA_redzone_conversion,
                                       teamA_linebreaK_try_ratio,
                                       teamA_average_kick_metres,
                                       teamA_average_run_metres,
                                       teamA_run_metres_success_rate,
                                       teamA_penalty_ratio,
                                       teamA_tackle_busts_per_run,
                                       teamA_gang_tackle_ratio,
                                       teamA_match_result)
  
  teamB_tackle_success_rate <- as.numeric(1 - teamB_stats_df$teamB_missed_tackles / teamB_stats_df$teamB_tackles)
  teamB_completion_rate <- as.numeric(teamB_stats_df$teamB_complete_sets / teamB_stats_df$teamB_total_sets)
  teamB_effective_offload_rate <- as.numeric(teamB_stats_df$teamB_effective_offloads / teamB_stats_df$teamB_off_loads)
  teamB_oppHalf_tackle_percent <- teamB_stats_df$teamB_tackle_opp_half / teamB_stats_df$teamB_play_the_balls
  teamB_redzone_tackle_percent <- teamB_stats_df$teamB_tackledOpp20 / teamB_stats_df$teamB_play_the_balls
  teamB_redzone_conversion <- teamB_stats_df$teamB_tries / teamB_stats_df$teamB_tackledOpp20
  teamB_linebreaK_try_ratio <- teamB_stats_df$teamB_tries / teamB_stats_df$teamB_line_breaks
  teamB_average_kick_metres <- teamB_stats_df$teamB_kick_metres / teamB_stats_df$teamB_kicks
  teamB_average_run_metres <- teamB_stats_df$teamB_run_metres / teamB_stats_df$teamB_runs
  teamB_run_metres_success_rate <- teamB_stats_df$teamB_runs_8plus_meters / teamB_stats_df$teamB_runs
  teamB_penalty_ratio <- teamB_stats_df$teamB_penaltiesAwarded / (teamB_stats_df$teamB_penalties_conceded + teamB_stats_df$teamB_penaltiesAwarded)
  teamB_tackle_busts_per_run <- teamB_stats_df$teamB_tackle_busts / teamB_stats_df$teamB_runs
  teamB_gang_tackle_ratio <- teamA_stats_df$teamA_tackles / teamB_stats_df$teamB_runs
  teamB_match_result <- ifelse(teamB_stats_df$teamB_points > teamA_stats_df$teamA_points, "Win",
                               ifelse(teamB_stats_df$teamB_points < teamA_stats_df$teamA_points, "Lose",
                                      ifelse(teamB_stats_df$teamB_points < teamA_stats_df$teamA_points,"Draw",NA)))
  
  teamB_additional_stats <- data.frame(teamB_tackle_success_rate,
                                       teamB_completion_rate,
                                       teamB_effective_offload_rate,
                                       teamB_oppHalf_tackle_percent,
                                       teamB_redzone_tackle_percent,
                                       teamB_redzone_conversion,
                                       teamB_linebreaK_try_ratio,
                                       teamB_average_kick_metres,
                                       teamB_average_run_metres,
                                       teamB_run_metres_success_rate,
                                       teamB_penalty_ratio,
                                       teamB_tackle_busts_per_run,
                                       teamB_gang_tackle_ratio,
                                       teamB_match_result)
  
  # set up array for both team for and against
  for_stats_df_A <- teamA_stats_df
  names(for_stats_df_A) <- gsub("teamA", "for", names(for_stats_df_A), fixed=TRUE)
  for_additional_stats_A <- teamA_additional_stats
  names(for_additional_stats_A) <- gsub("teamA", "for", names(for_additional_stats_A), fixed=TRUE)
  for_sc_df_A <- teamA_sc
  names(for_sc_df_A) <- gsub("teamA", "for", names(for_sc_df_A), fixed=TRUE)
  against_stats_df_A <- teamB_stats_df
  names(against_stats_df_A) <- gsub("teamB", "against", names(against_stats_df_A), fixed=TRUE)
  against_additional_stats_A <- teamB_additional_stats
  names(against_additional_stats_A) <- gsub("teamB", "against", names(against_additional_stats_A), fixed=TRUE)
  against_sc_df_A <- teamB_sc
  names(against_sc_df_A) <- gsub("teamB", "against", names(against_sc_df_A), fixed=TRUE)
  
  home_team <- as.data.frame("Home",stringsAsFactors=FALSE)
  names(home_team) <- "Home_Away"
  
  for_stats_df_B <- teamB_stats_df
  names(for_stats_df_B) <- gsub("teamB", "for", names(for_stats_df_B), fixed=TRUE)
  for_additional_stats_B <- teamB_additional_stats
  names(for_additional_stats_B) <- gsub("teamB", "for", names(for_additional_stats_B), fixed=TRUE)
  for_sc_df_B <- teamB_sc
  names(for_sc_df_B) <- gsub("teamB", "for", names(for_sc_df_B), fixed=TRUE)
  against_stats_df_B <- teamA_stats_df
  names(against_stats_df_B) <- gsub("teamA", "against", names(against_stats_df_B), fixed=TRUE)
  against_additional_stats_B <- teamA_additional_stats
  names(against_additional_stats_B) <- gsub("teamA", "against", names(against_additional_stats_B), fixed=TRUE)
  against_sc_df_B <- teamA_sc
  names(against_sc_df_B) <- gsub("teamA", "against", names(against_sc_df_B), fixed=TRUE)
  
  away_team <- as.data.frame("Away",stringsAsFactors=FALSE)
  names(away_team) <- "Home_Away"
  
  
  
  # Combine all data sets data frames
  stats_df <- rbind(stats_df, 
                    cbind(as.data.frame(match_info,stringsAsFactors=FALSE)
                          ,as.data.frame(home_team,stringsAsFactors=FALSE)
                          ,as.data.frame(for_stats_df_A,stringsAsFactors=FALSE)
                          ,as.data.frame(for_additional_stats_A,stringsAsFactors=FALSE)
                          ,as.data.frame(for_sc_df_A,stringsAsFactors=FALSE)
                          ,as.data.frame(against_stats_df_A,stringsAsFactors=FALSE)
                          ,as.data.frame(against_additional_stats_A,stringsAsFactors=FALSE)
                          ,as.data.frame(against_sc_df_A,stringsAsFactors=FALSE)
                    ),
                    cbind(as.data.frame(match_info,stringsAsFactors=FALSE)
                          ,as.data.frame(away_team,stringsAsFactors=FALSE)
                          ,as.data.frame(for_stats_df_B,stringsAsFactors=FALSE)
                          ,as.data.frame(for_additional_stats_B,stringsAsFactors=FALSE)
                          ,as.data.frame(for_sc_df_B,stringsAsFactors=FALSE)
                          ,as.data.frame(against_stats_df_B,stringsAsFactors=FALSE)
                          ,as.data.frame(against_additional_stats_B,stringsAsFactors=FALSE)
                          ,as.data.frame(against_sc_df_B,stringsAsFactors=FALSE)
                    ))
  






