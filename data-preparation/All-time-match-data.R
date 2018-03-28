# Create a all time file which prepares data for the NRL ML Model
library(jsonlite)

getwd()
#setwd("C:/Users/kenji/Desktop/NRL/Fox_Sports_Data")
setwd("C:/Users/ballk/Documents/Fox_Sports_Data/Fox_Sports_Data")

# Function to run season data into a single file
create.statsmatrix <-function(match_vector){
  #match_vector <- season_2017
# Set up an empty data frame     
  stats_df <- data.frame(match_id=character(),teamA_name=character(),teamA_errors=numeric(),teamA_inCompleteSets=numeric(),
                         teamA_kicks=numeric(),teamA_penaltiesAwarded=numeric(),teamA_points=numeric(),teamA_runs=numeric(),
                         teamA_tackledOpp20=numeric(),teamA_tackles=numeric(),teamA_territory=numeric(),teamA_tries=numeric(),
                         teamA_attacking_kicks=numeric(),teamA_complete_sets=numeric(),teamA_drop_outs=numeric(),teamA_dummy_half_runs=numeric(),
                         teamA_effective_offloads=numeric(),teamA_field_goal_attempts=numeric(),teamA_field_goal_misses=numeric(),
                         teamA_field_goals=numeric(),teamA_forced_drop_outs=numeric(),teamA_general_play_pass=numeric(),
                         teamA_goal_percentage=numeric(),teamA_in_goal_escapes=numeric(),teamA_interchanges_off=numeric(),
                         teamA_interchanges_on=numeric(),teamA_kick_metres=numeric(),teamA_kicks_4020=numeric(),teamA_kicks_dead=numeric(),
                         teamA_last_touch_try_assists=numeric(),teamA_line_break_assists=numeric(),teamA_line_break_causes=numeric(),
                         teamA_line_breaks=numeric(),teamA_line_engagements=numeric(),teamA_long_kicks=numeric(),teamA_missed_tackles=numeric(),
                         teamA_off_loads=numeric(),teamA_one_pass_runs=numeric(),teamA_penalties_conceded=numeric(),teamA_play_the_balls=numeric(),
                         teamA_possession_last10=numeric(),teamA_possession_percentage=numeric(),teamA_run_metres=numeric(),teamA_runs_7less_meters=numeric(),
                         teamA_runs_8plus_meters=numeric(),teamA_send_offs=numeric(),teamA_sin_bins=numeric(),teamA_tackle_busts=numeric(),
                         teamA_tackle_opp_half=numeric(),teamA_tackles_one_on_one=numeric(),teamA_total_sets=numeric(),teamA_try_assists=numeric(),
                         teamA_try_causes=numeric(),teamA_try_contributions=numeric(),teamA_try_involvements=numeric(),teamA_twenty_metre_restarts=numeric(),
                         teamA_weighted_kicks=numeric(),teamA_win_prediction_percentage=numeric(),teamA_goal_made=numeric(),teamA_goal_missed=numeric(),
                         teamA_Possession_Last_5_Minutes=numeric(),teamA_Time_in_Opposition_Half=numeric(),teamA_Tackled_in_Opposition_20m=numeric(),
                         teamA_Play_the_balls=numeric(),teamB_name=character(),teamB_errors=numeric(),teamB_inCompleteSets=numeric(),teamB_kicks=numeric(),
                         teamB_penaltiesAwarded=numeric(),teamB_points=numeric(),teamB_runs=numeric(),teamB_tackledOpp20=numeric(),
                         teamB_tackles=numeric(),teamB_territory=numeric(),teamB_tries=numeric(),teamB_attacking_kicks=numeric(),teamB_complete_sets=numeric(),
                         teamB_drop_outs=numeric(),teamB_dummy_half_runs=numeric(),teamB_effective_offloads=numeric(),teamB_field_goal_attempts=numeric(),
                         teamB_field_goal_misses=numeric(),teamB_field_goals=numeric(),teamB_forced_drop_outs=numeric(),teamB_general_play_pass=numeric(),
                         teamB_goal_percentage=numeric(),teamB_in_goal_escapes=numeric(),teamB_interchanges_off=numeric(),teamB_interchanges_on=numeric(),
                         teamB_kick_metres=numeric(),teamB_kicks_4020=numeric(),teamB_kicks_dead=numeric(),teamB_last_touch_try_assists=numeric(),
                         teamB_line_break_assists=numeric(),teamB_line_break_causes=numeric(),teamB_line_breaks=numeric(),teamB_line_engagements=numeric(),
                         teamB_long_kicks=numeric(),teamB_missed_tackles=numeric(),teamB_off_loads=numeric(),teamB_one_pass_runs=numeric(),
                         teamB_penalties_conceded=numeric(),teamB_play_the_balls=numeric(),teamB_possession_last10=numeric(),teamB_possession_percentage=numeric(),
                         teamB_run_metres=numeric(),teamB_runs_7less_meters=numeric(),teamB_runs_8plus_meters=numeric(),teamB_send_offs=numeric(),
                         teamB_sin_bins=numeric(),teamB_tackle_busts=numeric(),teamB_tackle_opp_half=numeric(),teamB_tackles_one_on_one=numeric(),
                         teamB_total_sets=numeric(),teamB_try_assists=numeric(),teamB_try_causes=numeric(),teamB_try_contributions=numeric(),
                         teamB_try_involvements=numeric(),teamB_twenty_metre_restarts=numeric(),teamB_weighted_kicks=numeric(),
                         teamB_win_prediction_percentage=numeric(),teamB_goal_made=numeric(),teamB_goal_missed=numeric(),teamB_Possession_Last_5_Minutes=numeric(),
                         teamB_Time_in_Opposition_Half=numeric(),teamB_Tackled_in_Opposition_20m=numeric(),teamB_Play_the_balls=numeric(),
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
teamA_stats_df <- as.data.frame(teamA_stats)
name <- as.character(jsonMatchData$team_A$name)
teamA_stats_df <- cbind(as.data.frame(name,stringsAsFactors=FALSE), as.data.frame(teamA_stats_df,stringsAsFactors=FALSE))
colnames(teamA_stats_df) <- paste("teamA", colnames(teamA_stats_df), sep = "_")


# Get stats for Team B
teamB_stats <- jsonlite:::simplify(jsonMatchData$team_B$stats, flatten = TRUE)
teamB_stats$momentum <- NULL
teamB_stats$period <- NULL
teamB_stats$completion_rate <- NULL
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

teamA_additional_stats <- cbind(as.data.frame(teamA_tackle_success_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamA_completion_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamA_effective_offload_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamA_oppHalf_tackle_percent, stringsAsFactors=FALSE),
                    as.data.frame(teamA_redzone_tackle_percent, stringsAsFactors=FALSE),
                    as.data.frame(teamA_redzone_conversion, stringsAsFactors=FALSE),
                    as.data.frame(teamA_linebreaK_try_ratio, stringsAsFactors=FALSE),
                    as.data.frame(teamA_average_kick_metres, stringsAsFactors=FALSE),
                    as.data.frame(teamA_average_run_metres, stringsAsFactors=FALSE),
                    as.data.frame(teamA_run_metres_success_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamA_penalty_ratio, stringsAsFactors=FALSE))

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

teamB_additional_stats <- cbind(as.data.frame(teamB_tackle_success_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamB_completion_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamB_effective_offload_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamB_oppHalf_tackle_percent, stringsAsFactors=FALSE),
                    as.data.frame(teamB_redzone_tackle_percent, stringsAsFactors=FALSE),
                    as.data.frame(teamB_redzone_conversion, stringsAsFactors=FALSE),
                    as.data.frame(teamB_linebreaK_try_ratio, stringsAsFactors=FALSE),
                    as.data.frame(teamB_average_kick_metres, stringsAsFactors=FALSE),
                    as.data.frame(teamB_average_run_metres, stringsAsFactors=FALSE),
                    as.data.frame(teamB_run_metres_success_rate, stringsAsFactors=FALSE),
                    as.data.frame(teamB_penalty_ratio, stringsAsFactors=FALSE))




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
names(against_additional_stats_A) <- gsub("teamB", "for", names(against_additional_stats_A), fixed=TRUE)
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
names(against_additional_stats_B) <- gsub("teamA", "for", names(against_additional_stats_B), fixed=TRUE)
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




season_2017_datamatrix <- create.statsmatrix(season_2017)



# Test build some rolling average attributes for season

# Account withdrawal features
cat("calculating withdrawal profile features...\n")

acc_trans_withdrawals_7days <- acc_trans_withdrawals[transaction_date >= observation_date - last_7_days]
acc_trans_withdrawals_14days <- acc_trans_withdrawals[transaction_date >= observation_date - last_14_days]
acc_trans_withdrawals_28days <- acc_trans_withdrawals[transaction_date >= observation_date - last_28_days]
acc_trans_withdrawals_3months <- acc_trans_withdrawals[transaction_date >= observation_date - last_3_months]
acc_trans_withdrawals_6months <- acc_trans_withdrawals[transaction_date >= observation_date - last_6_months]
acc_trans_withdrawals_12months <- acc_trans_withdrawals[transaction_date >= observation_date - last_12_months]

datasets <- list(acc_trans_withdrawals
                 , acc_trans_withdrawals_7days
                 , acc_trans_withdrawals_14days
                 , acc_trans_withdrawals_28days
                 , acc_trans_withdrawals_3months
                 , acc_trans_withdrawals_6months
                 , acc_trans_withdrawals_12months)
periods <- c("", "_last_7_days", "_last_14_days", "_last_28_days",
             "_last_3_months", "_last_6_months", "_last_12_months")
results <- foreach(i=1:length(periods)) %dopar% {
  result <- aggregate_withdrawal_features(datasets[[i]])
  # make names like withdrawal_last_7_days ...
  colnames(result)[-1] <- paste0(colnames(result)[-1], periods[[i]])
  result
}





season_2017_datamatrix$for_tackles_avg_last3 <- sapply(1:nrow(season_2017_datamatrix), FUN = last3)


season_2017_datamatrix$for_name




write.csv(season_2017_datamatrix,file="../season_2017_datamatrix3.csv")

sapply(jsonMatchData$scoring_summary,class)
stats_df




