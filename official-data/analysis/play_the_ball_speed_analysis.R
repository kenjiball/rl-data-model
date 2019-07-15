# Play the ball speed anaklysis

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

