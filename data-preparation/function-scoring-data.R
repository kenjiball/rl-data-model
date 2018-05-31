# function-scoring-data.R

# N.B. Ladder Position appears to be the current ladder positions i.e. when the data was run.
#      As opposed to the ladder position before the game was played.
#      Probably need to fix this by creating a dynamic ladder calculator



# Call packages
library(jsonlite)

##### Import local variables from file #####

# Set working directory
setwd(wd)
getwd()


# Function to run season data into a single file
create.matchmatrix <-function(match_vector){
  
  #match_vector <- season_2018 #FOR TESTING PURPOSES ONLY
  # Set up an empty data frame  
  return_df <- data.frame(
    match_id=character(),
    season=numeric(),
    round=numeric(),
    match=numeric(),
    venue_id=numeric(),
    venue_name=character(),
    venue_city=character(),
    match_start_date=as.Date(character()),
    extra_time=logical(),
    Home_Away=character(),
    for_team_id=numeric(),
    for_team_name=character(),
    for_team_code=character(),
    for_score=numeric(),
    for_halftime_score=numeric(),
    for_eighty_min_score=numeric(),
    for_ladder_position=numeric(),
    against_team_id=numeric(),
    against_team_name=character(),
    against_team_code=character(),
    against_score=numeric(),
    against_halftime_score=numeric(),
    against_eighty_min_score=numeric(),
    against_ladder_position=numeric(),
    stringsAsFactors=FALSE)
  
  # Start for loop here
  for(i in 1:length(match_vector)){  
  #i <- 1    #FOR TESTING PURPOSES ONLY
  
  # Load a json file for a single game and build to data frame
  jsonFileName <- match_vector[i]
  jsonMatchData <- fromJSON(paste("scoringdata_",jsonFileName, ".json",sep = ""))
  
  
  # Unload the json format into a data frame
  # Get match ID and match information
  match_id <- as.character(jsonMatchData$scoring_summary$match_id)
  match_info <- data.frame(match_id
                           ,substr(match_id, 4, 7)
                           ,substr(match_id, 8, 9)
                           ,substr(match_id, 10, 11)
                           ,jsonMatchData$scoring_summary$venue$id
                           ,jsonMatchData$scoring_summary$venue$name
                           ,jsonMatchData$scoring_summary$venue$city
                           ,jsonMatchData$scoring_summary$match_start_date
                           ,jsonMatchData$scoring_summary$extra_time
                           , stringsAsFactors=FALSE)
                           
  teamA_df <- data.frame(  jsonMatchData$scoring_summary$team_A$id
                           ,jsonMatchData$scoring_summary$team_A$name
                           ,jsonMatchData$scoring_summary$team_A$code
                           ,jsonMatchData$scoring_summary$team_A$score
                           ,jsonMatchData$scoring_summary$team_A$halftime_score
                           ,jsonMatchData$scoring_summary$team_A$eighty_minutes_score
                           ,jsonMatchData$scoring_summary$team_A$competition_table_position
                           , stringsAsFactors=FALSE)
  
  teamB_df <- data.frame(  jsonMatchData$scoring_summary$team_B$id
                           ,jsonMatchData$scoring_summary$team_B$name
                           ,jsonMatchData$scoring_summary$team_B$code
                           ,jsonMatchData$scoring_summary$team_B$score
                           ,jsonMatchData$scoring_summary$team_B$halftime_score
                           ,jsonMatchData$scoring_summary$team_B$eighty_minutes_score
                           ,jsonMatchData$scoring_summary$team_B$competition_table_position
                           , stringsAsFactors=FALSE)
                           
  names(match_info) <- c("match_id","season",	"round",	"match", "venue_id", "venue_name", "venue_city", "match_start_date","extra_time")
  names(teamA_df) <- c("teamA_team_id","teamA_team_name","teamA_team_code","teamA_score","teamA_halftime_score","teamA_eighty_min_score","teamA_ladder_position")
  names(teamB_df) <- c("teamB_team_id","teamB_team_name","teamB_team_code","teamB_score","teamB_halftime_score","teamB_eighty_min_score","teamB_ladder_position")

  head(match_info)
  head(teamA_df)
  head(teamB_df)
  
  # Creating part A of final data frame - return_df
  for_df_A <- teamA_df
  names(for_df_A) <- gsub("teamA", "for", names(for_df_A), fixed=TRUE)
  against_df_A <- teamB_df
  names(against_df_A) <- gsub("teamB", "against", names(against_df_A), fixed=TRUE)
  
  # Creating part B of final data frame - return_df
  for_df_B <- teamB_df
  names(for_df_B) <- gsub("teamB", "for", names(for_df_B), fixed=TRUE)
  against_df_B <- teamA_df
  names(against_df_B) <- gsub("teamA", "against", names(against_df_B), fixed=TRUE)
  
  # Data frame to add home and away fields
  home_team <- data.frame("Home")
  names(home_team) <- "Home_Away"
  
  away_team <- data.frame("Away")
  names(away_team) <- "Home_Away"
  
  
  # Combine all data sets data frames
  return_df <- rbind( return_df,
                     data.frame(match_info,home_team,for_df_A,against_df_A)
                     ,data.frame(match_info,away_team,for_df_B,against_df_B)
                     ) 
  head(return_df)
  
  
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

season_all_matchmatrix <- rbind(season_2011_matchmatrix
                               ,season_2012_matchmatrix
                               ,season_2013_matchmatrix
                               ,season_2014_matchmatrix
                               ,season_2015_matchmatrix
                               ,season_2016_matchmatrix
                               ,season_2017_matchmatrix
                               ,season_2018_matchmatrix )


write.csv(season_all_matchmatrix,file="../season_all_matchmatrix.csv")


head(season_2018_matchmatrix,20)
