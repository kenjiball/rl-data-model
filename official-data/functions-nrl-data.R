# Data functions
# functions-nrl-data.R
# To load functions run the command below from the local directory:
#  source(functions-nrl-data.R)
# This script contains the functions used to get and manipulate data


##### get_nrl_id_info
# this function allows the user to get or update a list of categorical variables used to get NRL data
# Can be called to get team, round, comp or season info.

get_nrl_id_info <- function(type = c("team","round","comp","season"), draw_url_in){
  
  # Call api for the json file and convert to list
  nrl_id_info_json <- fromJSON(draw_url_in)
  
  # Build data frame for id's and names
  
  # NRL Teams (nrl_team_id_df)
  if(type == "team"){
    output_df  <- nrl_id_info_json[["filterTeams"]] %>% select(value, name)
    names(output_df) <- c("team_id", "team_name")
    output_df <- arrange(output_df, team_id)
  }
  
  # NRL Rounds (nrl_round_id_df)
  if(type == "round"){
    output_df <- nrl_id_info_json[["filterRounds"]] %>% select(value, name)
    names(output_df) <- c("round_id", "round_name")
    output_df <- arrange(output_df, round_id)
  }
  
  # NRL Comps (nrl_comp_id_df)
  if(type == "comp"){
    output_df <- nrl_id_info_json[["filterCompetitions"]] %>% select(value, name)
    names(output_df) <- c("comp_id", "comp_name")
    output_df <- arrange(output_df, comp_id)
  }
  
  # NRL Season (nrl_season_id_df)
  if(type == "season"){
    output_df <- nrl_id_info_json[["filterSeasons"]] %>% select(value, name)
    names(output_df) <- c("season_id", "season_name")
    output_df <- arrange(output_df, season_id)
  }
  
  return(output_df)
  
}


##### Get NRL Draw by comp, year, team
# Function: get_nrl_draw

get_nrl_draw <- function(competition_id, season_id, lookup_id, type = c("round","team"), draw_url_in){
  
  # Set up the url_string to get draw information by comp, season, team
  draw_url_string <- paste0(draw_url_in,
                            "?competition=", competition_id,
                            "&season=", season_id,
                            "&", type, "=", lookup_id)
  # Get Json team file and convert to List
  nrl_draw_json <- fromJSON(draw_url_string)
  
  # Simply json list into dataframe
  nrl_draw_df <- jsonlite:::simplify(nrl_draw_json$drawGroups$matches, flatten = TRUE)
  
  return(nrl_draw_df)
  
}

##### Get NRL match data
# Function: get_nrl_match_data

get_nrl_match_data <- function(match_url, web_url_in, write_file = FALSE, file_dir = "" ){
  
  # Set up the url_string to get draw information by comp, season, team
  draw_url_string <- paste0(web_url_in, match_url, "data")
  # Get Json team file and convert to List
  nrl_match_json <- fromJSON(draw_url_string)
  
  if(write_file == TRUE){
    # 2: Make pretty Json
    to_file <- toJSON(nrl_match_json, pretty = TRUE)
    # 3: write to file in data sub directory
    write(to_file, paste0(file_dir, nrl_match_json$matchId,".json") )
  } else{
    return(nrl_match_json) 
  }
  
}

##### Write and load NRL match data
# Function: write_load_nrl_match_data
#Example: match_data_2018 <- write_load_nrl_match_data(2018, 25:30, wd_data, load_full_year = TRUE, update_files = TRUE)
#
# year:           The year of the season. Used to save and load files from the correct directory
# matches:        A numerical vector of the match numbers e.g. 1:8 will load the first 8 games of the season
# wd_data_f:      The working directory for the data files
# load_full_year: A logical to load the full year of data or just the games defined by matches
# update_files:   A logical that when TRUE to load and save fiels from the match_urls to the wd_data_f. 
#                 This is left false unless we need to update or write new files to the data directory.
#

write_load_nrl_match_data <- function(year, match_str, wd_data_f, load_full_year = TRUE, update_files = FALSE){
  
  # STEP 1: Load match urls from file
  match_urls <- read_csv(paste0(wd_data_f, "/nrl_url/", "match_url_",year,".csv"), col_names = "url")
  
  # STEP 2: Using match urls, data is saved to file
  # Apply across the match urls to get the data and write to file
  if(update_files == TRUE){
    sapply(match_urls$url[match_str],
         function(i)get_nrl_match_data(i, web_url, write_file = TRUE, file_dir = paste0(wd_data_f,"/nrl_match_data/",year,"/") ))
  }
  
  # STEP 3: Data is loaded from file
  # Load data from file into a single list
  files_list <- list.files(paste0(wd_data_f,"/nrl_match_data/",year), pattern="*.json", full.names=TRUE)
  
  if(load_full_year == FALSE){
    files_list <- files_list[match_str]
  }
  
  match_data_new <- lapply(files_list, function(i)fromJSON(i))
  
  return(match_data_new)
  
}

### Extract match data
# Function: extract_nrl_match_data
# Gets player level data from a list of matches
extract_nrl_match_data <- function(input_list,match_num){
  
  # replace missing data in hisotrical files.
  replace_null <- function(value, replace){
    if(is.null(value)){
      output <- replace
    } else{
      output <- value
    }
  }
  
  
  # Get match level data
  match_data   <- data.frame(replace_null(input_list[[match_num]]$matchId, 'NA'),
                             replace_null(input_list[[match_num]]$competition$competitionId, 'NA'),
                             replace_null(input_list[[match_num]]$roundNumber, 'NA'),
                             replace_null(input_list[[match_num]]$roundTitle, 'NA'),
                             replace_null(input_list[[match_num]]$homeTeam$teamId, 0),
                             replace_null(input_list[[match_num]]$homeTeam$name, 'NA'),
                             replace_null(input_list[[match_num]]$homeTeam$nickName, 'NA'),
                             replace_null(input_list[[match_num]]$homeTeam$theme$key, 'NA'),
                             replace_null(input_list[[match_num]]$awayTeam$teamId, 0),
                             replace_null(input_list[[match_num]]$awayTeam$name, 'NA'),
                             replace_null(input_list[[match_num]]$awayTeam$nickName, 'NA'),
                             replace_null(input_list[[match_num]]$awayTeam$theme$key, 'NA'),
                             replace_null(input_list[[match_num]]$venue, 'NA'),
                             replace_null(input_list[[match_num]]$startTime, 'NA'),
                             replace_null(input_list[[match_num]]$hashTag, 'NA'),
                             replace_null(input_list[[match_num]]$weather, 'NA'),
                             replace_null(input_list[[match_num]]$groundConditions, 'NA'),
                             replace_null(input_list[[match_num]]$attendance, 0),
                             replace_null(input_list[[match_num]]$homeTeam$score, 0),
                             replace_null(input_list[[match_num]]$awayTeam$score, 0),
                             replace_null(input_list[[match_num]]$homeTeam$scoring$halfTimeScore, 0),
                             replace_null(input_list[[match_num]]$awayTeam$scoring$halfTimeScore, 0),
                             replace_null(input_list[[match_num]]$homeTeam$captainPlayerId, 0),
                             replace_null(input_list[[match_num]]$awayTeam$captainPlayerId, 0))
  # Set Names
  names(match_data) <- c("matchId","competitionId","roundNumber","roundTitle","homeId","homeTeam","homeNickName","homeKey","awayId","awayTeam",
                         "awayNickName","awayKey","venue","startTime","hashTag","weather","groundConditions","attendance","homeScore","awayScore",
                         "homeHTScore","awayHTScore","homeCaptainId","awayCaptainId")
  
  # Set Data Types
  num_char_cols <- c("matchId")
  numeric_cols <- c("competitionId", "roundNumber", "homeId", "awayId", "attendance","homeScore","awayScore",
                    "homeHTScore","awayHTScore","homeCaptainId","awayCaptainId")
  character_cols <- c("roundTitle", "homeTeam", "homeNickName", "homeKey", "awayTeam", "awayNickName", "awayKey", "venue",
                      "hashTag", "weather", "groundConditions")
  date_cols <- c("startTime")

  # Mutate at cols to set correct data type
  match_data <- match_data %>% mutate_at(num_char_cols, funs(as.numeric(as.character(.))))
  match_data <- match_data %>% mutate_at(numeric_cols, funs(as.numeric(.)))
  match_data <- match_data %>% mutate_at(character_cols, funs(as.character(.)))
  match_data <- match_data %>% mutate_at(date_cols, funs(as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OSZ")))
  
  # Add some basic features
  match_data <- match_data %>% mutate(season = substr(matchId,1,4) )
  
  return(match_data)
  
}

### Extract match data
# Function: extract_nrl_player_data
extract_nrl_player_data <- function(input_list,match_num){
  
  # Get player level data
  # Home Player Data
  home_meta_data            <- input_list[[match_num]][["homeTeam"]][["players"]]
  home_meta_data$matchId    <- input_list[[match_num]][["matchId"]]
  home_meta_data$homeAway   <- "Home"
  home_meta_data$teamId     <- input_list[[match_num]][["homeTeam"]][["teamId"]]
  home_meta_data$name       <- input_list[[match_num]][["homeTeam"]][["name"]]
  home_meta_data$nickName   <- input_list[[match_num]][["homeTeam"]][["nickName"]]
  home_stats                <- input_list[[match_num]][["stats"]][["players"]][["homeTeam"]]
  # home_df
  home_df <- inner_join(home_meta_data, home_stats, by = "playerId")
  
  # Home Player Data
  away_meta_data            <- input_list[[match_num]][["awayTeam"]][["players"]]
  away_meta_data$matchId    <- input_list[[match_num]][["matchId"]]
  away_meta_data$homeAway   <- "Away"
  away_meta_data$teamId     <- input_list[[match_num]][["awayTeam"]][["teamId"]]
  away_meta_data$name       <- input_list[[match_num]][["awayTeam"]][["name"]]
  away_meta_data$nickName   <- input_list[[match_num]][["awayTeam"]][["nickName"]]
  away_stats                <- input_list[[match_num]][["stats"]][["players"]][["awayTeam"]]
  # home_df
  away_df <- inner_join(away_meta_data, away_stats, by = "playerId")
  
  player_df <- bind_rows(home_df, away_df)
  
  # add some basic features
  player_df <- player_df %>% 
    mutate(positionGroups = case_when(position %in% c("Fullback","Winger","Centre","Five-Eighth","Halfback") ~ "Backs",
                                       position %in% c("2nd Row","Hooker","Lock","Prop") ~ "Forwards",
                                       position %in% c("Interchange") ~ "Interchange",
                                       TRUE ~ "NA" )
          )
  
  # Set Data Types
  numeric_cols <- c("playerId","number","matchId","teamId","allRunMetres","allRuns","bombKicks","crossFieldKicks","conversions",
                    "conversionAttempts", "dummyHalfRuns", "dummyHalfRunMetres", "dummyPasses", "errors", "fantasyPointsTotal" , 
                      "fieldGoals", "forcedDropOutKicks", "fortyTwentyKicks", "goals", "goalConversionRate" ,
                      "grubberKicks", "handlingErrors", "hitUps", "hitUpRunMetres", "ineffectiveTackles" ,
                      "intercepts", "kicks", "kicksDead", "kicksDefused", "kickMetres" ,
                      "kickReturnMetres", "lineBreakAssists", "lineBreaks", "lineEngagedRuns", "minutesPlayed" ,
                      "missedTackles", "offloads", "oneOnOneLost", "oneOnOneSteal", "onReport" ,
                      "passesToRunRatio", "passes", "playTheBallTotal", "playTheBallAverageSpeed", "penalties" ,
                      "points", "penaltyGoals", "postContactMetres", "receipts", "sendOffs" ,
                      "sinBins", "stintOne", "tackleBreaks", "tackleEfficiency", "tacklesMade" ,
                      "tries", "tryAssists") #, "stintTwo"
  
  character_cols <- c("firstName","lastName","position","headImage","bodyImage","url","homeAway","name","nickName","positionGroups")
  logical_cols <- c("isOnField")

  # Mutate at cols to set correct data type
  player_df <- player_df %>% mutate_at(numeric_cols, funs(as.numeric(.)))
  player_df <- player_df %>% mutate_at(character_cols, funs(as.character(.)))
  player_df <- player_df %>% mutate_at(logical_cols, funs(as.logical(.)))
  
  # Add some basic features
  player_df <- player_df %>% mutate(season = substr(matchId,1,4), 
                                    roundNumber = as.numeric(substr(matchId,8,9)) )
  
  return(player_df)
  
}

### Function to upload data files to google drive

upload_to_drive <- function(upload_data, upload_path){
  
  # Define file name
  file_name <- paste0("./",deparse(substitute(upload_data)),".csv")
  
  # temporarily write file to directory
  write_csv(upload_data, file_name)
  
  # Upload file to drive
  drive_upload( media = file_name, path  = as_id(upload_path), type = "spreadsheet")
  
  # Remove csv file from directory
  file.remove(file_name)
  
  print(paste0("Successfully uploaded file: ",deparse(substitute(upload_data))))
  
}

### Function to download data files to google drive

download_from_drive <- function(download_data){
  
  # Define file name
  file_name <- paste0("./",download_data,".csv")
  
  # temporarily download file to directory
  drive_download(download_data, type = "csv")
  
  # Upload file to drive
  outputdf <- read_csv(file_name)
  
  # Remove csv file from directory
  file.remove(file_name)
  
  print(paste0("Successfully uploaded file: ",file_name))
  
  return(outputdf)
  
}




