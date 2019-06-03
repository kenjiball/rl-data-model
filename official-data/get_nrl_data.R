# Get data using the Stats API and Key

# Call packages
source("./load-packages.R")

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


# Getting nrl draw as a list
# draw_2019_team <- lapply(1:16 , function(i)get_nrl_draw(111,2019,i, "team"), draw_url)
draw_2010_round <- lapply(1:30 , function(i)get_nrl_draw(111,2010,i, "round", draw_url))
draw_2011_round <- lapply(1:30 , function(i)get_nrl_draw(111,2011,i, "round", draw_url))
draw_2012_round <- lapply(1:30 , function(i)get_nrl_draw(111,2012,i, "round", draw_url))

# match and player data only beginfs from 2013 season
draw_2013_round <- lapply(1:30 , function(i)get_nrl_draw(111,2013,i, "round", draw_url))
draw_2014_round <- lapply(1:30 , function(i)get_nrl_draw(111,2014,i, "round", draw_url))
draw_2015_round <- lapply(1:30 , function(i)get_nrl_draw(111,2015,i, "round", draw_url))
draw_2016_round <- lapply(1:30 , function(i)get_nrl_draw(111,2016,i, "round", draw_url))
draw_2017_round <- lapply(1:30 , function(i)get_nrl_draw(111,2017,i, "round", draw_url))
draw_2018_round <- lapply(1:29 , function(i)get_nrl_draw(111,2018,i, "round", draw_url))

draw_2019_round <- lapply(1:25  , function(i)get_nrl_draw(111,2019,i, "round", draw_url))

# set rounds
rounds <- 1:10

# get Match URLs to get match data
match_url_lookup_2013 <- lapply( 1:30, function(i) unlist(draw_2013_round[[i]]$matchCentreUrl))
match_url_lookup_2014 <- lapply( 1:30, function(i) unlist(draw_2014_round[[i]]$matchCentreUrl))
match_url_lookup_2015 <- lapply( 1:30, function(i) unlist(draw_2015_round[[i]]$matchCentreUrl))
match_url_lookup_2016 <- lapply( 1:30, function(i) unlist(draw_2016_round[[i]]$matchCentreUrl))
match_url_lookup_2017 <- lapply( 1:30, function(i) unlist(draw_2017_round[[i]]$matchCentreUrl))
match_url_lookup_2018 <- lapply( 1:29, function(i) unlist(draw_2018_round[[i]]$matchCentreUrl))

match_url_lookup_2019 <- lapply( rounds , function(i) unlist(draw_2019_round[[i]]$matchCentreUrl))


### get match data into list
# create a list of urls to apply over
match_urls_2013 <- unlist(match_url_lookup_2013[1:30])[!is.na(unlist(match_url_lookup_2013[1:30]))]
match_urls_2014 <- unlist(match_url_lookup_2014[1:30])[!is.na(unlist(match_url_lookup_2014[1:30]))]
match_urls_2015 <- unlist(match_url_lookup_2015[1:30])[!is.na(unlist(match_url_lookup_2015[1:30]))]
match_urls_2016 <- unlist(match_url_lookup_2016[1:30])[!is.na(unlist(match_url_lookup_2016[1:30]))]
match_urls_2017 <- unlist(match_url_lookup_2017[1:30])[!is.na(unlist(match_url_lookup_2017[1:30]))]
match_urls_2018 <- unlist(match_url_lookup_2018[1:29])[!is.na(unlist(match_url_lookup_2018[1:29]))]

match_urls_2019 <- unlist(match_url_lookup_2019[rounds])[!is.na(unlist(match_url_lookup_2019[rounds]))]

# get nrl match data
match_data_2013 <- lapply(match_urls_2013, function(i)get_nrl_match_data(i, web_url))
match_data_2014 <- lapply(match_urls_2014, function(i)get_nrl_match_data(i, web_url))
match_data_2015 <- lapply(match_urls_2015, function(i)get_nrl_match_data(i, web_url))
match_data_2016 <- lapply(match_urls_2016, function(i)get_nrl_match_data(i, web_url))
match_data_2017 <- lapply(match_urls_2017, function(i)get_nrl_match_data(i, web_url))
match_data_2018 <- lapply(match_urls_2018, function(i)get_nrl_match_data(i, web_url))

match_data_2019 <- lapply(match_urls_2019, function(i)get_nrl_match_data(i, web_url))

#### Extract Match data
# define number of mataches for current season
matches <- 1:length(match_urls_2019)

match_table_2010_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2010,i)))

match_table_2013_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2013,i)))
match_table_2014_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2014,i)))
match_table_2015_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2015,i)))
match_table_2016_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2016,i)))
match_table_2017_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2017,i)))
match_table_2018_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2018,i)))

match_table_2019_df <- list_df2df(lapply(matches, function(i)extract_nrl_match_data(match_data_2019,i)))

# TO DO: Fix issue of factor to numeric conversion in the function
match_table_2019_df$matchId <- as.numeric(as.character(match_table_2019_df$matchId))
match_table_2019_df$startTime <- as.POSIXct(match_table_2019_df$startTime)

match_table_df <- bind_rows(match_table_2013_df, match_table_2014_df, match_table_2015_df, match_table_2016_df,
                            match_table_2017_df, match_table_2018_df, match_table_2019_df)

#### Extract Player data
player_2013_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2013,i)))
player_2014_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2014,i)))
player_2015_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2015,i)))
player_2016_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2016,i)))
player_2017_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2017,i)))
player_2018_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2018,i)))

player_2019_df <- bind_rows(lapply(matches, function(i)extract_nrl_player_data(match_data_2019,i)))

# TO DO: Fix issue of factor to numeric conversion in the function
player_2019_df$matchId <- as.numeric(player_2019_df$matchId)

player_df <- bind_rows(player_2013_df, player_2014_df, player_2015_df, player_2016_df,
                            player_2017_df, player_2018_df, player_2019_df)


# Extract current season data and upload to drive


# Load data to Googledrive
# use googledrive package to upload file to drive
upload_to_drive(transitionDataset, gdrive_path_id)
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





### play the ball speed by team and position
mu <- player_df %>%
  filter(playTheBallAverageSpeed > 0 & playTheBallAverageSpeed < 6) %>% 
  mutate( total_playTheBall_time = playTheBallTotal*playTheBallAverageSpeed) %>% 
  group_by(name, position) %>% 
  summarise( grp.mean=mean(playTheBallAverageSpeed),
             total.mean = sum(total_playTheBall_time)/sum(playTheBallTotal)  )

player_df %>%
  filter(playTheBallAverageSpeed > 0 & playTheBallAverageSpeed < 6) %>% 
  ggplot(aes(playTheBallAverageSpeed , color = name)) +
  geom_density() +
  geom_vline(data=mu, aes(xintercept=total.mean, color=name), linetype="dashed") +
  facet_grid(position~., scales = "free") + 
  scale_color_manual(values=c(rainbow(16)))
  
### play the ball speed by team and season
mu <- match_table_df_2 %>% 
  inner_join(player_df, by = "matchId") %>%
  filter(playTheBallAverageSpeed > 0 & playTheBallAverageSpeed < 6) %>% 
  mutate( total_playTheBall_time = playTheBallTotal*playTheBallAverageSpeed,
          oppositionTeam = if_else(name == homeTeam, awayTeam, homeTeam),
          oppositionTeam_is_souths  = if_else(oppositionTeam == 'South Sydney Rabbitohs', TRUE, FALSE)
          ) %>% 
  group_by(oppositionTeam_is_souths, year) %>% 
  summarise( grp.mean=mean(playTheBallAverageSpeed),
             total.mean = sum(total_playTheBall_time)/sum(playTheBallTotal)  )

match_table_df_2 %>% 
  inner_join(player_df, by = "matchId") %>%
  filter(playTheBallAverageSpeed > 0 & playTheBallAverageSpeed < 6) %>% 
  mutate( total_playTheBall_time = playTheBallTotal*playTheBallAverageSpeed,
          oppositionTeam = if_else(name == homeTeam, awayTeam, homeTeam),
          oppositionTeam_is_souths  = if_else(oppositionTeam == 'South Sydney Rabbitohs', TRUE, FALSE)
  ) %>% 
  ggplot(aes(playTheBallAverageSpeed , color = oppositionTeam_is_souths)) +
  geom_density() +
  geom_vline(data=mu, aes(xintercept=total.mean, color= oppositionTeam_is_souths), linetype="dashed") +
  facet_grid(year~., scales = "free") #+ 
  #scale_color_manual(values=c(rainbow(2)))



player_df %>% 
  group_by(firstName, lastName) %>% 
  summarise( total_tries = sum(tries, na.rm = TRUE)) %>% 
  arrange(desc(total_tries))

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
             errors = sum(missedTackles, na.rm = TRUE),
             tries  = sum(tries, na.rm = TRUE),
             missed_tackles_per_game = errors/games,
             errors_per_tries = errors/tries
             ) %>% 
  ungroup() %>% 
  ggplot(aes(x = season, y= missed_tackles_per_game)) +
  geom_image(aes(image=team_url), size=.035)



names(player_df)
names(match_table_df)

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










