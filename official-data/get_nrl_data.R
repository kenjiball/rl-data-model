# Get data using the Stats API and Key

# Call packages
library(curl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(qdapTools)
library(ggplot2)
library(ggthemes)

##### Import local variables from file #####
wd <- "/Users/ballk/OneDrive - Tabcorp/Documents/rl-data-model/data"

setwd(wd)
getwd()

# Load functions
source(functions-nrl-data.R)


# get reference dfs.
nrl_team_id_df   <- get_nrl_id_info("team")
nrl_round_id_df  <- get_nrl_id_info("round")
nrl_comp_id_df   <- get_nrl_id_info("comp")
nrl_season_id_df <- get_nrl_id_info("season")


# Getting nrl draw as a list
# draw_2019_team <- lapply(1:16 , function(i)get_nrl_draw(111,2019,i, "team"))
draw_2010_round <- lapply(1:30 , function(i)get_nrl_draw(111,2010,i, "round"))
draw_2011_round <- lapply(1:30 , function(i)get_nrl_draw(111,2011,i, "round"))
draw_2012_round <- lapply(1:30 , function(i)get_nrl_draw(111,2012,i, "round"))

# match and player data only beginfs from 2013 season
draw_2013_round <- lapply(1:30 , function(i)get_nrl_draw(111,2013,i, "round"))
draw_2014_round <- lapply(1:30 , function(i)get_nrl_draw(111,2014,i, "round"))
draw_2015_round <- lapply(1:30 , function(i)get_nrl_draw(111,2015,i, "round"))
draw_2016_round <- lapply(1:30 , function(i)get_nrl_draw(111,2016,i, "round"))
draw_2017_round <- lapply(1:30 , function(i)get_nrl_draw(111,2017,i, "round"))
draw_2018_round <- lapply(1:29 , function(i)get_nrl_draw(111,2018,i, "round"))

draw_2019_round <- lapply(1:4  , function(i)get_nrl_draw(111,2019,i, "round"))


# get Match URLs to get match data
match_url_lookup_2013 <- lapply( 1:30, function(i) unlist(draw_2013_round[[i]]$matchCentreUrl))
match_url_lookup_2014 <- lapply( 1:30, function(i) unlist(draw_2014_round[[i]]$matchCentreUrl))
match_url_lookup_2015 <- lapply( 1:30, function(i) unlist(draw_2015_round[[i]]$matchCentreUrl))
match_url_lookup_2016 <- lapply( 1:30, function(i) unlist(draw_2016_round[[i]]$matchCentreUrl))
match_url_lookup_2017 <- lapply( 1:30, function(i) unlist(draw_2017_round[[i]]$matchCentreUrl))
match_url_lookup_2018 <- lapply( 1:29, function(i) unlist(draw_2018_round[[i]]$matchCentreUrl))

match_url_lookup_2019 <- lapply( 1:4 , function(i) unlist(draw_2019_round[[i]]$matchCentreUrl))


### get match data into list
# create a list of urls to apply over
match_urls_2013 <- unlist(match_url_lookup_2013[1:30])[!is.na(unlist(match_url_lookup_2013[1:30]))]
match_urls_2014 <- unlist(match_url_lookup_2014[1:30])[!is.na(unlist(match_url_lookup_2014[1:30]))]
match_urls_2015 <- unlist(match_url_lookup_2015[1:30])[!is.na(unlist(match_url_lookup_2015[1:30]))]
match_urls_2016 <- unlist(match_url_lookup_2016[1:30])[!is.na(unlist(match_url_lookup_2016[1:30]))]
match_urls_2017 <- unlist(match_url_lookup_2017[1:30])[!is.na(unlist(match_url_lookup_2017[1:30]))]
match_urls_2018 <- unlist(match_url_lookup_2018[1:29])[!is.na(unlist(match_url_lookup_2018[1:29]))]

match_urls_2019 <- unlist(match_url_lookup_2019[1:4 ])[!is.na(unlist(match_url_lookup_2019[1:4 ]))]

# get nrl match data
match_data_2013 <- lapply(match_urls_2013, function(i)get_nrl_match_data(i))
match_data_2014 <- lapply(match_urls_2014, function(i)get_nrl_match_data(i))
match_data_2015 <- lapply(match_urls_2015, function(i)get_nrl_match_data(i))
match_data_2016 <- lapply(match_urls_2016, function(i)get_nrl_match_data(i))
match_data_2017 <- lapply(match_urls_2017, function(i)get_nrl_match_data(i))
match_data_2018 <- lapply(match_urls_2018, function(i)get_nrl_match_data(i))

match_data_2019 <- lapply(match_urls_2019, function(i)get_nrl_match_data(i))

#### Extract Match data
match_table_2010_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2010,i)))

match_table_2013_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2013,i)))
match_table_2014_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2014,i)))
match_table_2015_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2015,i)))
match_table_2016_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2016,i)))
match_table_2017_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2017,i)))
match_table_2018_df <- list_df2df(lapply(1:201, function(i)extract_nrl_match_data(match_data_2018,i)))

match_table_2019_df <- list_df2df(lapply(1:32, function(i)extract_nrl_match_data(match_data_2019,i)))

match_table_df <- bind_rows(match_table_2013_df, match_table_2014_df, match_table_2015_df, match_table_2016_df,
                            match_table_2017_df, match_table_2018_df, match_table_2019_df)

#### Extract Player data
player_2013_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2013,i)))
player_2014_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2014,i)))
player_2015_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2015,i)))
player_2016_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2016,i)))
player_2017_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2017,i)))
player_2018_df <- bind_rows(lapply(1:201, function(i)extract_nrl_player_data(match_data_2018,i)))

player_2019_df <- bind_rows(lapply(1:32, function(i)extract_nrl_player_data(match_data_2019,i)))

player_df <- bind_rows(player_2013_df, player_2014_df, player_2015_df, player_2016_df,
                            player_2017_df, player_2018_df, player_2019_df)







# More data not currently extracted
match_data_2019[[25]]$timeline

match_data_2019[[22]]$officials

match_data_2019[[22]]$homeTeam$coaches

match_data_2019[[22]]$homeTeam$captainPlayerId





### ggplot
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
  


player_df %>% 
  group_by(firstName, lastName) %>% 
  summarise( total_tries = sum(tries, na.rm = TRUE)) %>% 
  arrange(desc(total_tries))


match_table_df %>%
  ggplot(aes(homeScore)) +
  geom_histogram(binwidth = 2)
  


names(player_df)


# Create some basic features
match_table_df_2 <- match_table_df %>% 
  mutate(
    homeResult = case_when( homeScore > awayScore ~ 'Win',
                            homeScore < awayScore ~ 'Lose',
                            homeScore == awayScore ~ 'Draw',
                            TRUE ~ 'NA')
  )

### build basic logistic regression model

