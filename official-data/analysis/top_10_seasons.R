library(ggthemes)

# Merge files into one
myMergedData <- 
  do.call(bind_rows,
          lapply(paste0(wd_data, "/nrl_career_data/2019/",
                        list.files(path = paste0(wd_data,"/nrl_career_data/2019/"))[1:404]), read.csv))

####### Top 10 Seasons scripts :

# Missed_tackles_pg
test1 <- myMergedData %>% 
  select(firstName, lastName, Team, Year, Played, Tackles.Made , Missed.Tackles) %>% 
  mutate(Missed_tackles_pg = Missed.Tackles/Played) %>% 
  filter(Played >= 5) %>% 
  arrange(desc(Missed_tackles_pg)) %>% 
  top_n(10, Missed_tackles_pg)

# Non_Kick_Return_Metres
test1 <- myMergedData %>% 
  select(firstName, lastName, Team, Year, Played, TotalRunning.Metres, KickReturn.Metres, AverageRunning.Metres ) %>% 
  mutate( Non_Kick_Return_Metres = (TotalRunning.Metres - KickReturn.Metres )/Played) %>% 
  filter(Played >= 10) %>% 
  arrange(desc(Non_Kick_Return_Metres)) %>%
  select( -TotalRunning.Metres, -KickReturn.Metres) %>%
  rename("Run_Metres_pg" = AverageRunning.Metres) %>% 
  select(firstName, lastName, Team, Year, Played, Non_Kick_Return_Metres, Run_Metres_pg) %>% 
  top_n(10, Non_Kick_Return_Metres)

# Forced Dropouts per game
test1 <- myMergedData %>% 
  select(firstName, lastName, Team, Year, Played, ForcedDrop.Outs ) %>% 
  mutate( Forced_dropouts = ForcedDrop.Outs/Played) %>% 
  filter(Played >= 10) %>% 
  arrange(desc(Forced_dropouts)) %>%
  select(firstName, lastName, Team, Year, Played, Forced_dropouts, ForcedDrop.Outs) %>% 
  rename("Total_forced_dropouts" = ForcedDrop.Outs) %>% 
  top_n(10, Forced_dropouts) 

# Tries Assists per game
test1 <- myMergedData %>% 
  select(firstName, lastName, Team, Year, Played, Try.Assists ) %>% 
  mutate( Try_assists_per_game = Try.Assists/Played) %>% 
  filter(Played >= 10) %>% 
  arrange(desc(Try_assists_per_game)) %>%
  select(firstName, lastName, Team, Year, Played, Try.Assists, Try_assists_per_game) %>% 
  top_n(10, Try_assists_per_game) %>% 
  mutate( id = row_number())


### GGPLOT of the top 10 season
# set names for labels
positions <- rev(test1$lastName)

# Build plot 
test1 %>% 
  left_join(team_names_df, by = c("Team" = "nrl_team_short_name_ns")) %>% 
  ggplot(aes(x = lastName , y = Try_assists_per_game, group=factor(id))) +
  geom_bar(position="dodge",stat="identity") + 
  coord_flip() + 
  scale_x_discrete(limits = positions) + 
  geom_text(aes(label=Year), position=position_dodge(width=0.9), vjust=0.1, hjust = 1.6, col = "white") + 
  ggtitle("Top 10 Seasons", "Try Assists per game") + 
  theme_fivethirtyeight() + 
  geom_image(aes(image=team_logo_url), size=0.085)


(14 + 2 + 9 + 6 + 7 + 12 + 15 + 16 + 9 + 10 )





