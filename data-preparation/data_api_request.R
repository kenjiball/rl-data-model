# Get data using the Stats API and Key

# Call packages
source("./load-packages.R")

##### Import local variables from file #####
wd_data <- paste0(wd, "/data")

setwd(wd_data)
getwd()


# Function to Read Player Data Json from API and save to file 
grabData <- function(gameid,api_url,api_key,fileprefix, wd_in){
  # Create API call from gameid
  API_str <- paste(api_url
               ,gameid
               ,api_key
               , sep = "", collapse = NULL)
  # Set year from gameid
  year <- substr(gameid,1,4)
  # Create fie name from gameid
  file_name <- paste(fileprefix,"_",gameid,".json", sep = "", collapse = NULL)
  file_path <- paste0(wd_in, "/data/fox_",fileprefix,"/",year,"/")
  # Call API to get data for a game
  jsonData <- fromJSON(API_str)
  jsonDataPretty <- toJSON(jsonData, pretty = TRUE)
  # Write to file
  write(jsonDataPretty, paste0(file_path,file_name))
}


# Single game 
grabData(20170101,api_url,api_player_key,"playerdata",wd)
grabData(20170101,api_url,api_match_key,"matchdata",wd)
grabData(20170101,api_url,api_scoring_key,"scoringdata",wd)

# Round of games
sapply(20191401:20191408, grabData,api_url,api_player_key,"playerdata",wd)
sapply(20191401:20191408, grabData,api_url,api_match_key,"matchdata",wd)
sapply(20191401:20191408, grabData,api_url,api_scoring_key,"scoringdata",wd)

# Vector of games
                 
sapply(season_2016, grabData,api_url,api_player_key,"playerdata",wd)
sapply(season_2016, grabData,api_url,api_match_key,"matchdata",wd)
sapply(season_2016, grabData,api_url,api_scoring_key,"scoringdata",wd)

