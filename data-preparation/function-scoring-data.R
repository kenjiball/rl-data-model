# function-scoring-data.R




# Call packages
library(jsonlite)

##### Import local variables from file #####

# Set working directory
setwd(wd)
getwd()


# Function to run season data into a single file
create.scoringmatrix <-function(match_vector){
  
  match_vector <- season_2018 #FOR TESTING PURPOSES ONLY
  # Set up an empty data frame  
  
  # Start for loop here
  i <- 1    #FOR TESTING PURPOSES ONLY
  
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
                           ,jsonMatchData$scoring_summary$team_A$id
                           ,jsonMatchData$scoring_summary$team_A$name
                           ,jsonMatchData$scoring_summary$team_A$code
                           ,jsonMatchData$scoring_summary$team_A$score
                           ,jsonMatchData$scoring_summary$team_A$halftime_score
                           ,jsonMatchData$scoring_summary$team_A$eighty_minutes_score
                           ,jsonMatchData$scoring_summary$team_A$competition_table_position
                           
                           ,jsonMatchData$scoring_summary$team_B$id
                           ,jsonMatchData$scoring_summary$team_B$name
                           ,jsonMatchData$scoring_summary$team_B$code
                           ,jsonMatchData$scoring_summary$team_B$score
                           ,jsonMatchData$scoring_summary$team_B$halftime_score
                           ,jsonMatchData$scoring_summary$team_B$eighty_minutes_score
                           ,jsonMatchData$scoring_summary$team_B$competition_table_position
                           
                           
                           , stringsAsFactors=FALSE)
  names(match_info) <- 
    c("match_id","season",	"round",	"match", "venue_id", "venue_name", "venue_city", "match_start_date","extra_time"
  ,"teamA_id","teamA_name","teamA_code","teamA_score","teamA_halftime_score","teamA_eighty_min_score","teamA_ladder_position"
  ,"teamB_id","teamB_name","teamB_code","teamB_score","teamB_halftime_score","teamB_eighty_min_score","teamB_ladder_position")

  head(match_info)
  
  
  
}