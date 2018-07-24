# function-scoring-data.R

# N.B. Ladder Position appears to be the current ladder positions i.e. when the data was run.
#      As opposed to the ladder position before the game was played.
#      Probably need to fix this by creating a dynamic ladder calculator



# Call packages
library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)

##### Import local variables from file #####

# Set working directory
setwd(wd)
getwd()


# Function to run season data into a single file
create.matchmatrix <-function(match_vector){
  
  # match_vector <- "20161501" #FOR TESTING PURPOSES ONLY
 
  # Start for loop here
  for(i in 1:length(match_vector)){  
  # i <- 1    #FOR TESTING PURPOSES ONLY
  
  # Load a json file for a single game and build to data frame
  jsonFileName <- match_vector[i]
  jsonMatchData <- fromJSON(paste("scoringdata_",jsonFileName, ".json",sep = ""))

  # Unload the json format into a data frame
  # Get match ID and match information
  match_id <- data.frame(jsonMatchData$scoring_summary$match_id)
  names(match_id) <- c("match_id") 
  
  match_info_df <- match_id %>% 
                mutate(season = substr(match_id, 4, 7),
                       round = substr(match_id, 8, 9),
                       match = substr(match_id, 10, 11)
                       )
  
  match_info <- data.frame(match_info_df
               ,as.data.frame(jsonMatchData[[2]][[2]][c("id", "name", "city")])
               ,jsonMatchData[[2]][c("match_start_date", "extra_time")]
               , stringsAsFactors=FALSE)
  
  teamA_df <- as.data.frame(
              jsonMatchData[[2]][["team_A"]]
              [c("id", "name", "code", "score", "halftime_score", "eighty_minutes_score", "competition_table_position")]
              )

  
  teamB_df <- as.data.frame(
              jsonMatchData[[2]][["team_B"]]
              [c("id", "name", "code", "score", "halftime_score", "eighty_minutes_score", "competition_table_position")]
              )
                           
  names(match_info) <- c("match_id","season",	"round",	"match", "venue_id", "venue_name", "venue_city", "match_start_date","extra_time")
  names(teamA_df) <- c("teamA_team_id","teamA_team_name","teamA_team_code","teamA_score","teamA_halftime_score","teamA_eighty_min_score","teamA_ladder_position")
  names(teamB_df) <- c("teamB_team_id","teamB_team_name","teamB_team_code","teamB_score","teamB_halftime_score","teamB_eighty_min_score","teamB_ladder_position")
  
  #### Mutate to create new features from the events data frame
  # Unnest events data frame
  events_df <- data.frame(jsonMatchData$events$code,
             jsonMatchData$events$team,
             jsonMatchData$events$player,
             jsonMatchData$events[4:10]
             )
  names(events_df) <- c("code", "team.id", "team.name", "team.code", "team.short_name", "player.id",  "player.full_name",
                       "player.short_name", "period", "time", "name", "display_time","team_A_score", "team_B_score", "sequence_no")
  
  # Mutate and summarize data
  events_summary_df <- events_df %>% 
          mutate( lead_teamA = team_A_score - team_B_score,
                  lead_teamB = team_B_score - team_A_score,
                  team = if_else(team.id == teamA_df$teamA_team_id, "teamA",if_else(team.id == teamB_df$teamB_team_id, "teamB",NULL))
                  ) %>%
          arrange(time) %>%
          summarize(

                    first_try_time = min(if_else(code == "TRY", time, NULL),na.rm = TRUE ),
                    first_try_mins = min(if_else(code == "TRY", display_time, NULL),na.rm = TRUE ),
                    first_try_player_id = first(na.omit(if_else(code == "TRY", player.id, NULL)) ),
                    first_try_scorer = first(na.omit(if_else(code == "TRY", player.full_name, NULL)) ),
                    first_try_team = first(na.omit(if_else(code == "TRY", team, NULL)) ),
                    first_try_teamA = if_else(first_try_team == "teamA",TRUE,FALSE),
                    first_try_teamB = if_else(first_try_team == "teamB",TRUE,FALSE),
                    
                    first_score_time = min(if_else(team_A_score > 0 | team_B_score  > 0 , time, NULL),na.rm = TRUE),
                    first_score_mins = min(if_else(team_A_score > 0 | team_B_score  > 0 , display_time, NULL),na.rm = TRUE),
                    first_score_type = first(na.omit(if_else(team_A_score > 0 | team_B_score  > 0 , code, NULL))),
                    first_score_player_id = first(na.omit(if_else(team_A_score > 0 | team_B_score  > 0 , player.id, NULL))),
                    first_score_player = first(na.omit(if_else(team_A_score > 0 | team_B_score  > 0 , player.full_name, NULL))),
                    first_score_team = first(na.omit(if_else(team_A_score > 0 | team_B_score  > 0 , team, NULL))),
                    first_score_teamA = if_else(first_score_team == "teamA",TRUE,FALSE),
                    first_score_teamB = if_else(first_score_team == "teamB",TRUE,FALSE),
                    
                    last_try_time = max(if_else(code == "TRY", time, NULL),na.rm = TRUE ),
                    last_try_mins = max(if_else(code == "TRY", display_time, NULL),na.rm = TRUE ),
                    last_try_player_id = last(na.omit(if_else(code == "TRY", player.id, NULL)) ),
                    last_try_scorer = last(na.omit(if_else(code == "TRY", player.full_name, NULL)) ),
                    last_try_team = last(na.omit(if_else(code == "TRY", team, NULL)) ),                      
                    last_try_teamA = if_else(last_try_team == "teamA",TRUE,FALSE),
                    last_try_teamB = if_else(last_try_team == "teamB",TRUE,FALSE),                    
                    
                    largest_lead_teamA = max(lead_teamA),
                    largest_lead_teamB = max(lead_teamB),
                    led_by_12_teamA = if_else(largest_lead_teamA >= 12, TRUE, FALSE),
                    led_by_12_teamB = if_else(largest_lead_teamB >= 12, TRUE, FALSE),
                    
                    conversions_made_teamA = sum(na.omit(if_else(code == "CONOK" & team == "teamA" , 1, 0))),
                    conversions_missed_teamA = sum(na.omit(if_else(code == "CONMS" & team == "teamA" , 1, 0))),
                    conversions_attempted_teamA = sum(na.omit(if_else(code %in% c("CONOK","CONMS") & team == "teamA" , 1, 0))),
                    penalty_goal_made_teamA = sum(na.omit(if_else(code == "PGOK" & team == "teamA" , 1, 0))),
                    penalty_goal_missed_teamA = sum(na.omit(if_else(code == "PGMS" & team == "teamA" , 1, 0))),
                    penalty_goal_attempted_teamA = sum(na.omit(if_else(code %in% c("PGMS","PGOK") & team == "teamA" , 1, 0))),
                    
                    conversions_made_teamB = sum(na.omit(if_else(code == "CONOK" & team == "teamB" , 1, 0))),
                    conversions_missed_teamB = sum(na.omit(if_else(code == "CONMS" & team == "teamB" , 1, 0))),
                    conversions_attempted_teamB = sum(na.omit(if_else(code %in% c("CONOK","CONMS") & team == "teamB" , 1, 0))),
                    penalty_goal_made_teamB = sum(na.omit(if_else(code == "PGOK" & team == "teamB" , 1, 0))),
                    penalty_goal_missed_teamB = sum(na.omit(if_else(code == "PGMS" & team == "teamB" , 1, 0))),
                    penalty_goal_attempted_teamB = sum(na.omit(if_else(code %in% c("PGMS","PGOK") & team == "teamB" , 1, 0)))


                    )
  

  #### Define function to count consecutive 
  sum_consecutive <- function(x) {x[x == 1]=sequence(with(rle(x), lengths[values == 1]));x}
  
  # group by team data
  consecutive_tries_df <- events_df %>% 
          mutate(team = if_else(team.id == teamA_df$teamA_team_id, "teamA",if_else(team.id == teamB_df$teamB_team_id, "teamB",NULL))) %>%
          arrange(time) %>%
          group_by(team) %>%
          filter(code == "TRY") %>%
          mutate(consecutive_tries =sum_consecutive(na.omit(if_else(code == "TRY", 1, 0)))) %>%
          summarize( max_consecutive_tries = max(consecutive_tries) ) 
  
  # separate consective try data frames by team
  # logical condition to check for when a team does not score a try
  if(!"teamA" %in% consecutive_tries_df$team){
      consecutive_tries_teamA <- data.frame(as.numeric(0)) 
      } else {
        consecutive_tries_teamA <- consecutive_tries_df %>% filter(team == "teamA") %>% select(max_consecutive_tries)
      }
  names(consecutive_tries_teamA) <- "max_consecutive_tries_teamA"
  
  if(!"teamB" %in% consecutive_tries_df$team){
    consecutive_tries_teamB <- data.frame(as.numeric(0)) 
  } else {
    consecutive_tries_teamB <- consecutive_tries_df %>% filter(team == "teamB") %>% select(max_consecutive_tries)
  }
  names(consecutive_tries_teamB) <- "max_consecutive_tries_teamB"
  
  
  #####Data frame to add home and away fields
  home_team <- data.frame("Home")
  names(home_team) <- "Home_Away"
  
  away_team <- data.frame("Away")
  names(away_team) <- "Home_Away"
  
  
  # Combine all data frames for home data
  home_data <- bind_cols(match_info
                         , home_team
                         , teamA_df
                         , teamB_df
                         , events_summary_df
                         , consecutive_tries_teamA
                         , consecutive_tries_teamB
                         )
  
 
  # Combine all data frames for away data
  away_data <- bind_cols(match_info
                         , away_team
                         , teamB_df
                         , teamA_df
                         , events_summary_df
                         , consecutive_tries_teamB
                         , consecutive_tries_teamA
                         )
  
  # Substite names in home data drame
  names(home_data) <- gsub("teamA", "for", names(home_data), fixed=TRUE)
  names(home_data) <- gsub("teamB", "against", names(home_data), fixed=TRUE)
  
  # Substite names in home data drame
  names(away_data) <- gsub("teamB", "for", names(away_data), fixed=TRUE)
  names(away_data) <- gsub("teamA", "against", names(away_data), fixed=TRUE)

  
  # Coerse any factors into character before the bind rows
  home_data <- home_data %>% mutate_if(sapply(home_data,is.factor), as.character)
  away_data <- away_data %>% mutate_if(sapply(away_data,is.factor), as.character)


  # mutate to include match result
  # Fix date formats using lubridate
  home_data <- home_data %>% mutate(
                for_match_result = if_else(for_score > against_score, "Win",
                                   if_else(for_score < against_score, "Lose",
                                   if_else(for_score == against_score ,"Draw",NULL))),
            against_match_result = if_else(against_score > for_score, "Win",
                                   if_else(against_score < for_score, "Lose",
                                   if_else(against_score == for_score ,"Draw",NULL))),
                      start_date = floor_date(with_tz(ymd_hms(match_start_date), tzone = "Australia/Sydney"),"day"),
                  start_datetime = with_tz(ymd_hms(match_start_date), tzone = "Australia/Sydney")
            )
  
  away_data <- away_data %>% mutate(
                for_match_result = if_else(for_score > against_score, "Win",
                                   if_else(for_score < against_score, "Lose",
                                   if_else(for_score == against_score ,"Draw",NULL))),
            against_match_result = if_else(against_score > for_score, "Win",
                                   if_else(against_score < for_score, "Lose",
                                   if_else(against_score == for_score ,"Draw",NULL))),
                      start_date = floor_date(with_tz(ymd_hms(match_start_date), tzone = "Australia/Sydney"),"day"),
                  start_datetime = with_tz(ymd_hms(match_start_date), tzone = "Australia/Sydney")
              )
              
  if(i == 1){
    return_df <- bind_rows(home_data,away_data)
  } else{
    return_df <- bind_rows(return_df, home_data,away_data)
  }
  

  }
  
  return(return_df)
}



#season_2010_matchmatrix <- create.matchmatrix(season_2010)
season_2011_matchmatrix <- create.matchmatrix(season_2011)
season_2012_matchmatrix <- create.matchmatrix(season_2012)
season_2013_matchmatrix <- create.matchmatrix(season_2013)
season_2014_matchmatrix <- create.matchmatrix(season_2014)
season_2015_matchmatrix <- create.matchmatrix(season_2015)
season_2016_matchmatrix <- create.matchmatrix(season_2016)
season_2017_matchmatrix <- create.matchmatrix(season_2017)
season_2018_matchmatrix <- create.matchmatrix(season_2018)

#dim(season_2010_matchmatrix)
dim(season_2011_matchmatrix)
dim(season_2012_matchmatrix)
dim(season_2013_matchmatrix)
dim(season_2014_matchmatrix)
dim(season_2015_matchmatrix)
dim(season_2016_matchmatrix)
dim(season_2017_matchmatrix)
dim(season_2018_matchmatrix)

season_all_matchmatrix <- bind_rows(season_2011_matchmatrix
                               ,season_2012_matchmatrix
                               ,season_2013_matchmatrix
                               ,season_2014_matchmatrix
                               ,season_2015_matchmatrix
                               ,season_2016_matchmatrix
                               ,season_2017_matchmatrix
                               ,season_2018_matchmatrix )


write.csv(season_all_matchmatrix,file="../season_all_matchmatrix.csv")

write.csv(season_2018_matchmatrix,file="../season_2018_matchmatrix.csv")

head(season_2018_matchmatrix,20)
head(season_2018_matchmatrix $match_start_date)

