# Get data using the Stats API and Key

# Call packages
library(curl)
library(jsonlite)


##### Import local variables from file #####
wd <- "/Users/ballk/OneDrive - Tabcorp/Documents/rl-data-model/data"

setwd(wd)
getwd()


# Function to Read Player Data Json from API and save to file 
grabData <- function(gameid,api_url,api_key,fileprefix){
  # Create API call from gameid
  API_str <- paste(api_url
               ,gameid
               ,api_key
               , sep = "", collapse = NULL)
  # Create fie name from gameid
  file_name <- paste(fileprefix,gameid,".json", sep = "", collapse = NULL)
  # Call API to get data for a game
  jsonData <- fromJSON(API_str)
  jsonDataPretty <- toJSON(jsonData, pretty = TRUE)
  # Write to file
  write(jsonDataPretty, file_name)
}


# Single game 
grabData(20180101,api_url,api_player_key,"playerdata_")
grabData(20180101,api_url,api_match_key,"matchdata_")
grabData(20180101,api_url,api_scoring_key,"scoringdata_")


# Vector of games
                 
sapply(season_2018, grabData,api_url,api_player_key,"playerdata_")
sapply(season_2018, grabData,api_url,api_match_key,"matchdata_")
sapply(season_2018, grabData,api_url,api_scoring_key,"scoringdata_")

