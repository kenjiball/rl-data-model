### get_player_career_summary
# This script loops through current players and saves thier career summary. 
# Probably run this task more periodically like 4 times a year maybe?

# Get a list of player names by year

player_names_df <- player_df %>% 
  select(playerId ,firstName, lastName, teamId, url, nickName, matchId) %>%
  mutate(year = substr(matchId,1,4), round = substr(matchId,8,9), year_round = paste0(year,round) ) %>% 
  select(-matchId) %>% 
  group_by(playerId) %>% 
  top_n(1, year_round) %>% 
  distinct() %>% 
  filter(!is.na(url) & year == 2019) %>%  # Can only currently do for players who played this season, it grabs the summary tables by mistake
  arrange(playerId)

length(player_names_df$firstName)

# loop through players and save to file
for(i in 301:406 ){
  player_career_df <- get_player_career_summary(player_first_name = player_names_df$firstName[i], 
                                                player_last_name  = player_names_df$lastName[i], 
                                                player_team       = player_names_df$nickName[i], 
                                                web_url, 
                                                player_url        = player_names_df$url[i])
  
  csv_name <- paste0("./data/nrl_career_data/2019/",paste(player_names_df$playerId[i], player_names_df$firstName[i],
                                                          player_names_df$lastName[i], player_names_df$year[i], sep = "-"),".csv")
  
  write_csv(player_career_df,csv_name)                         
}

# Merge files into one
myMergedData <- 
  do.call(bind_rows,
          lapply(paste0("/Users/kenjiball/rl-data-model/data/nrl_career_data/2019/",
                        list.files(path = "/Users/kenjiball/rl-data-model/data/nrl_career_data/2019/")[1:404]), read.csv))


#####
# ISSUE WITH:
# Scott Bolton 84 - some quote on the page - Did not import
# Patrick Kaufusi 88 - New Team
# Matthew Eisenhuth 298 - No Team for 2018? - Did not import


