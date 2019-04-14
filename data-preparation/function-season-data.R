# Create a all time file which prepares data for the NRL ML Model

# Call packages
library(jsonlite)

##### Import local variables from file #####

# Set working directory
setwd(wd)
getwd()


# Function to run season data into a single file
create.statsmatrix <-function(match_vector){
  #match_vector <- season_2018
# Set up an empty data frame     
  stats_df <- data.frame(
    match_id=character(),
    season=numeric(),
    round=numeric(),
    match=numeric(),
    Home_Away=character(),
    for_name=character(),
    for_errors=numeric(),
    for_inCompleteSets=numeric(),
    for_kicks=numeric(),
    for_penaltiesAwarded=numeric(),
    for_points=numeric(),
    for_runs=numeric(),
    for_tackledOpp20=numeric(),
    for_tackles=numeric(),
    for_territory=numeric(),
    for_tries=numeric(),
    for_attacking_kicks=numeric(),
    for_complete_sets=numeric(),
    for_drop_outs=numeric(),
    for_dummy_half_runs=numeric(),
    for_effective_offloads=numeric(),
    for_field_goal_attempts=numeric(),
    for_field_goal_misses=numeric(),
    for_field_goals=numeric(),
    for_forced_drop_outs=numeric(),
    for_general_play_pass=numeric(),
    for_goal_percentage=numeric(),
    for_in_goal_escapes=numeric(),
    for_interchanges_off=numeric(),
    for_interchanges_on=numeric(),
    for_kick_metres=numeric(),
    for_kicks_4020=numeric(),
    for_kicks_dead=numeric(),
    for_last_touch_try_assists=numeric(),
    for_line_break_assists=numeric(),
    for_line_break_causes=numeric(),
    for_line_breaks=numeric(),
    for_line_engagements=numeric(),
    for_long_kicks=numeric(),
    for_missed_tackles=numeric(),
    for_off_loads=numeric(),
    for_one_pass_runs=numeric(),
    for_penalties_conceded=numeric(),
    for_play_the_balls=numeric(),
    for_possession_last10=numeric(),
    for_possession_percentage=numeric(),
    for_run_metres=numeric(),
    for_runs_7less_meters=numeric(),
    for_runs_8plus_meters=numeric(),
    for_send_offs=numeric(),
    for_sin_bins=numeric(),
    for_tackle_busts=numeric(),
    for_tackle_opp_half=numeric(),
    for_tackles_one_on_one=numeric(),
    for_total_sets=numeric(),
    for_try_assists=numeric(),
    for_try_causes=numeric(),
    for_try_contributions=numeric(),
    for_try_involvements=numeric(),
    for_twenty_metre_restarts=numeric(),
    for_weighted_kicks=numeric(),
    for_win_prediction_percentage=numeric(),
    for_goal_made=numeric(),
    for_goal_attempted=numeric(),
    for_tackle_success_rate=numeric(),
    for_completion_rate=numeric(),
    for_effective_offload_rate=numeric(),
    for_oppHalf_tackle_percent=numeric(),
    for_redzone_tackle_percent=numeric(),
    for_redzone_conversion=numeric(),
    for_linebreaK_try_ratio=numeric(),
    for_average_kick_metres=numeric(),
    for_average_run_metres=numeric(),
    for_run_metres_success_rate=numeric(),
    for_penalty_ratio=numeric(),
    for_tackle_busts_per_run=numeric(),
    for_gang_tackle_ratio=numeric(),
    for_match_result=character(),
    for_Possession_Last_5_Minutes=numeric(),
    for_Time_in_Opposition_Half=numeric(),
    for_Tackled_in_Opposition_20m=numeric(),
    for_Play_the_balls=numeric(),
    against_name=character(),
    against_errors=numeric(),
    against_inCompleteSets=numeric(),
    against_kicks=numeric(),
    against_penaltiesAwarded=numeric(),
    against_points=numeric(),
    against_runs=numeric(),
    against_tackledOpp20=numeric(),
    against_tackles=numeric(),
    against_territory=numeric(),
    against_tries=numeric(),
    against_attacking_kicks=numeric(),
    against_complete_sets=numeric(),
    against_drop_outs=numeric(),
    against_dummy_half_runs=numeric(),
    against_effective_offloads=numeric(),
    against_field_goal_attempts=numeric(),
    against_field_goal_misses=numeric(),
    against_field_goals=numeric(),
    against_forced_drop_outs=numeric(),
    against_general_play_pass=numeric(),
    against_goal_percentage=numeric(),
    against_in_goal_escapes=numeric(),
    against_interchanges_off=numeric(),
    against_interchanges_on=numeric(),
    against_kick_metres=numeric(),
    against_kicks_4020=numeric(),
    against_kicks_dead=numeric(),
    against_last_touch_try_assists=numeric(),
    against_line_break_assists=numeric(),
    against_line_break_causes=numeric(),
    against_line_breaks=numeric(),
    against_line_engagements=numeric(),
    against_long_kicks=numeric(),
    against_missed_tackles=numeric(),
    against_off_loads=numeric(),
    against_one_pass_runs=numeric(),
    against_penalties_conceded=numeric(),
    against_play_the_balls=numeric(),
    against_possession_last10=numeric(),
    against_possession_percentage=numeric(),
    against_run_metres=numeric(),
    against_runs_7less_meters=numeric(),
    against_runs_8plus_meters=numeric(),
    against_send_offs=numeric(),
    against_sin_bins=numeric(),
    against_tackle_busts=numeric(),
    against_tackle_opp_half=numeric(),
    against_tackles_one_on_one=numeric(),
    against_total_sets=numeric(),
    against_try_assists=numeric(),
    against_try_causes=numeric(),
    against_try_contributions=numeric(),
    against_try_involvements=numeric(),
    against_twenty_metre_restarts=numeric(),
    against_weighted_kicks=numeric(),
    against_win_prediction_percentage=numeric(),
    against_goal_made=numeric(),
    against_goal_attempted=numeric(),
    against_tackle_success_rate=numeric(),
    against_completion_rate=numeric(),
    against_effective_offload_rate=numeric(),
    against_oppHalf_tackle_percent=numeric(),
    against_redzone_tackle_percent=numeric(),
    against_redzone_conversion=numeric(),
    against_linebreaK_try_ratio=numeric(),
    against_average_kick_metres=numeric(),
    against_average_run_metres=numeric(),
    against_run_metres_success_rate=numeric(),
    against_penalty_ratio=numeric(),
    against_tackle_busts_per_run=numeric(),
    against_gang_tackle_ratio=numeric(),
    against_match_result=character(),
    against_Possession_Last_5_Minutes=numeric(),
    against_Time_in_Opposition_Half=numeric(),
    against_Tackled_in_Opposition_20m=numeric(),
    against_Play_the_balls=numeric(),
    stringsAsFactors=FALSE)
  
  
for(i in 1:length(match_vector)){  
  #i <- 1    
# Load a json file for a single game and build to data frame
jsonFileName <- match_vector[i]
jsonMatchData <- fromJSON(paste("matchdata_",jsonFileName, ".json",sep = ""))

# Unload the json format into a data frame
# Get match ID and match information
match_id <- as.character(jsonMatchData$match_id)
match_info <- cbind(as.data.frame(match_id, stringsAsFactors=FALSE),
                    as.data.frame(substr(match_id, 4, 7), stringsAsFactors=FALSE),
                    as.data.frame(substr(match_id, 8, 9), stringsAsFactors=FALSE),
                    as.data.frame(substr(match_id, 10, 11), stringsAsFactors=FALSE))
names(match_info) <- c("match_id","season",	"round",	"match")


# Get stats for Team A
teamA_stats <- jsonlite:::simplify(jsonMatchData$team_A$stats, flatten = TRUE)
teamA_stats$momentum <- NULL
teamA_stats$period <- NULL
teamA_stats$completion_rate <- NULL
teamA_stats$possession_time <- NULL
teamA_stats$territory_time <- NULL
teamA_stats_df <- as.data.frame(teamA_stats)
name <- as.character(jsonMatchData$team_A$name)
teamA_stats_df <- cbind(as.data.frame(name,stringsAsFactors=FALSE), as.data.frame(teamA_stats_df,stringsAsFactors=FALSE))
colnames(teamA_stats_df) <- paste("teamA", colnames(teamA_stats_df), sep = "_")


# Get stats for Team B
teamB_stats <- jsonlite:::simplify(jsonMatchData$team_B$stats, flatten = TRUE)
teamB_stats$momentum <- NULL
teamB_stats$period <- NULL
teamB_stats$completion_rate <- NULL
teamB_stats$possession_time <- NULL
teamB_stats$territory_time <- NULL
teamB_stats_df <- as.data.frame(teamB_stats)
name <- as.character(jsonMatchData$team_B$name)
teamB_stats_df <- cbind(as.data.frame(name,stringsAsFactors=FALSE), as.data.frame(teamB_stats_df,stringsAsFactors=FALSE))
colnames(teamB_stats_df) <- paste("teamB", colnames(teamB_stats_df), sep = "_")


# Get Stat Comparisons
stats_comparison <- rbind(jsonMatchData$stat_comparisons$stat_name
,jsonMatchData$stat_comparisons$team_A_value
,jsonMatchData$stat_comparisons$team_B_value)

# Pull individual stats from the df stats_comparison and create a single row
teamA_sc_1 <- as.numeric(stats_comparison[2,1])
teamA_sc_2 <- as.numeric(stats_comparison[2,2])
teamA_sc_3 <- as.numeric(stats_comparison[2,3])
teamA_sc_4 <- as.numeric(stats_comparison[2,4])
teamA_sc <- as.data.frame(cbind(teamA_sc_1,teamA_sc_2,teamA_sc_3,teamA_sc_4))
names(teamA_sc) <- paste("teamA",stats_comparison[1,],sep="_")
names(teamA_sc) <- gsub(" ", "_", names(teamA_sc), fixed=TRUE)

teamB_sc_1 <- as.numeric(stats_comparison[3,1])
teamB_sc_2 <- as.numeric(stats_comparison[3,2])
teamB_sc_3 <- as.numeric(stats_comparison[3,3])
teamB_sc_4 <- as.numeric(stats_comparison[3,4])
teamB_sc <- as.data.frame(cbind(teamB_sc_1,teamB_sc_2,teamB_sc_3,teamB_sc_4))
names(teamB_sc) <- paste("teamB",stats_comparison[1,],sep="_")
names(teamB_sc) <- gsub(" ", "_", names(teamB_sc), fixed=TRUE)
  
# Fix Goal Rate and Completion Rate Fields
teamA_stats_df$teamA_goal_made <- as.numeric(strsplit(as.character(teamA_stats_df$teamA_goal_rate), " from ")[[1]][1])
teamA_stats_df$teamA_goal_attempted <- as.numeric(strsplit(as.character(teamA_stats_df$teamA_goal_rate), " from ")[[1]][2])
teamB_stats_df$teamB_goal_made <- as.numeric(strsplit(as.character(teamB_stats_df$teamB_goal_rate), " from ")[[1]][1])
teamB_stats_df$teamB_goal_attempted <- as.numeric(strsplit(as.character(teamB_stats_df$teamB_goal_rate), " from ")[[1]][2])
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
  
  }

  return(stats_df)
}




season_2010_datamatrix <- create.statsmatrix(season_2010)
season_2011_datamatrix <- create.statsmatrix(season_2011)
season_2012_datamatrix <- create.statsmatrix(season_2012)
season_2013_datamatrix <- create.statsmatrix(season_2013)
season_2014_datamatrix <- create.statsmatrix(season_2014)
season_2015_datamatrix <- create.statsmatrix(season_2015)
season_2016_datamatrix <- create.statsmatrix(season_2016)
season_2017_datamatrix <- create.statsmatrix(season_2017)
season_2018_datamatrix <- create.statsmatrix(season_2018)

dim(season_2010_datamatrix)
dim(season_2011_datamatrix)
dim(season_2012_datamatrix) 
dim(season_2013_datamatrix) 
dim(season_2014_datamatrix) 
dim(season_2015_datamatrix) 
dim(season_2016_datamatrix) 
dim(season_2017_datamatrix) 
dim(season_2018_datamatrix)

season_all_datamatrix <- rbind(season_2011_datamatrix
                                    ,season_2012_datamatrix
                                    ,season_2013_datamatrix
                                    ,season_2014_datamatrix
                                    ,season_2015_datamatrix
                                    ,season_2016_datamatrix
                                    ,season_2017_datamatrix
                                    ,season_2018_datamatrix
                                    
                                    )


write.csv(season_all_datamatrix,file="../season_all_datamatrix.csv")



sapply(jsonMatchData$scoring_summary,class)
stats_df




