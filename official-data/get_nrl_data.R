# Get data using the Stats API and Key

# Call packages
source("./load-packages.R")

# Read masking file
source("./masking_file.R")

##### Import local variables from file #####
setwd(wd)
getwd()

# Load functions
source("./official-data/functions-nrl-data.R")


# get reference dfs.
nrl_team_id_df   <- get_nrl_id_info("team", draw_url)
nrl_round_id_df  <- get_nrl_id_info("round", draw_url)
nrl_comp_id_df   <- get_nrl_id_info("comp", draw_url)
nrl_season_id_df <- get_nrl_id_info("season", draw_url)

# Save to file
write_csv(nrl_team_id_df, paste0(wd_data, "/nrl_tables/", "nrl_team_id_2019.csv"))
write_csv(nrl_round_id_df, paste0(wd_data, "/nrl_tables/", "nrl_round_id_2019.csv"))
write_csv(nrl_comp_id_df, paste0(wd_data, "/nrl_tables/", "nrl_comp_id_2019.csv"))
write_csv(nrl_season_id_df, paste0(wd_data, "/nrl_tables/", "nrl_season_id_2019.csv"))


# Getting nrl draw as a list
# match and player data only beginfs from 2013 season
draw_2018_round <- lapply(1:29 , function(i)get_nrl_draw(111,2018,i, "round", draw_url))
draw_2019_round <- lapply(1:25  , function(i)get_nrl_draw(111,2019,i, "round", draw_url))

# write to file
test <- toJSON( draw_2018_round, pretty = TRUE)
write(test, paste0(wd_data, "/nrl_draw/", "nrl_draw_2018.json"))
rm(test, draw_2018_round)


#### Load URLs
# 2013 - 2017: Rounds 1:30
# 2018 - 2019: Rounds 1:29

# set rounds
rounds <- 1:17
# define number of mataches for current season
matches <- 1:length(match_urls_2019)

# Load draw files from json
draw_2019_round <- fromJSON(paste0(wd_data, "/nrl_draw/", "nrl_draw_2019.json"))
match_url_lookup_2019 <- unlist(lapply( rounds, function(i) unlist(draw_2019_round[[i]]$matchCentreUrl)))
# Remove NAs
match_urls_2019 <- match_url_lookup_2019[!is.na(match_url_lookup_2019)]
# write to file
write_csv(as.data.frame(match_urls_2019), paste0(wd_data, "/nrl_url/", "match_url_2019.csv"), col_names = FALSE)

##### New function to write or load data
# OLD WAY: match_data_2019 <- sapply(match_urls_2019, function(i)get_nrl_match_data(i, web_url))
# Typical season has 201 matches.
match_data_2014 <- write_load_nrl_match_data(2014, 1:201, wd_data, load_full_year = TRUE, update_files = FALSE)
match_data_2019 <- write_load_nrl_match_data(2019, 1:128, wd_data, load_full_year = TRUE, update_files = FALSE)


# STEP 4: Data is extracted from the match data list.
# Run apply to extract player and team data
match_table_2018_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2018,i)))
player_2018_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2018,i)))

# Need to fix this in the extract_nrl_player_data function
player_2016_df_new$stintTwo <- as.numeric(player_2016_df_new$stintTwo)


match_table_df <- bind_rows(match_table_2013_df, match_table_2014_df, match_table_2015_df, match_table_2016_df,
                            match_table_2017_df, match_table_2018_df, match_table_2019_df)

player_df <- bind_rows(player_2013_df, player_2014_df, player_2015_df, player_2016_df,
                            player_2017_df, player_2018_df, player_2019_df)



player_df %>% 
  group_by(season, roundNumber) %>% 
  summarise( n = n()/34)


### Extract current season data and upload to drive
# Load data to Googledrive
# use googledrive package to upload file to drive
upload_to_drive(match_table_df, gdrive_path_id)
# Download data from Google drive
transitionDataset <- download_from_drive("transitionDataset")

#ggimage, googledrive, rsvg

drive_files <- drive_find(n_max = 100)


# More data not currently extracted
match_data_2019[[25]]$stats$groups$stats

match_data_2019[[25]]$timeline

match_data_2019[[22]]$officials

match_data_2019[[22]]$homeTeam$coaches

match_data_2019[[22]]$homeTeam$captainPlayerId

# discipline filed for sin bins


# Team score distribution
match_table_df %>%
  ggplot(aes(homeScore)) +
  geom_histogram(binwidth = 2)
  

# Errors per try plot
match_features_df %>% 
  inner_join(player_df, by = "matchId") %>% 
  mutate(team_url = if_else(homeAway == "Home", home_team_logo_url,away_team_logo_url) ) %>% 
  select(season, matchId, name, homeAway, team_url, errors, tries, missedTackles, ) %>%
  group_by(season, name, team_url) %>% 
  filter(season >= 2016) %>% 
  summarise( count = n(),
             games = n_distinct(matchId),
             errors = sum(errors, na.rm = TRUE),
             tries  = sum(tries, na.rm = TRUE),
             missed_tackles_per_game = errors/games,
             errors_per_tries = errors/tries
             ) %>% 
  ungroup() %>% 
  ggplot(aes(x = season, y= errors_per_tries)) +
  geom_image(aes(image=team_url), size=.035)





# Kick return metres vs hit up metres


match_table_df_2 %>% 
  inner_join(player_df, by = "matchId") %>%
  filter(allRuns > 3 ) %>% 
  select( year, firstName, lastName, playerId, position, positionGroups, allRuns, allRunMetres, 
          kickReturnMetres, hitUps, hitUpRunMetres, dummyHalfRuns, dummyHalfRunMetres, postContactMetres ) %>%
  mutate( allRunMetresPerRun = allRunMetres/allRuns ) %>% 
  ggplot(aes(x=allRunMetresPerRun))+
    geom_histogram(binwidth = 1) + 
    facet_grid(positionGroups ~.) +
    xlim(0,20)

summary(player_df$hitUpRunMetres)

# Effective tackle rate
player_df %>% 
  select( playerId, position, positionGroups, tacklesMade, missedTackles, ineffectiveTackles, tackleEfficiency ) %>%
  group_by(playerId) %>% 
  summarise( tacklesMade = sum(tacklesMade, na.rm = TRUE),
             missedTackles = sum(missedTackles, na.rm = TRUE),
             ineffectiveTackles = sum(ineffectiveTackles, na.rm = TRUE),
             tackleEfficiency = tacklesMade / (tacklesMade + missedTackles + ineffectiveTackles)
             ) %>% 
  ungroup() %>% 
  filter(tacklesMade > 20) %>% 
  ggplot(aes(tackleEfficiency)) +
  geom_histogram(binwidth = 0.02)










