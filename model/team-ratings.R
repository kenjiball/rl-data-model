# Tackle in Opp 20 analysis for offense, defense efficiency ratings

# Build rating data df
# Include basic match data
# Last 5 matches and season rating using 20m offense and defence efficiency

rating_data <- season_2019_datamatrix %>% 
  group_by(for_name) %>% 
  mutate( ratio_20m = for_tackledOpp20/for_runs,
          tries_per_tackledOpp20m = for_tries/for_tackledOpp20,
          net_tries = for_tries - against_tries,
          net_points = for_points - against_points,
            
          for_tackledOpp20_l5 = rollapply(for_tackledOpp20, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          for_tries_l5 = rollapply(for_tries, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          for_tries_per_tackledOpp20m_l5 = for_tries_l5/ for_tackledOpp20_l5,
          
          for_tackledOpp20_2019 = lag(cumsum(for_tackledOpp20), n=1),
          for_tries_2019 =  lag(cumsum(for_tries), n=1),
          for_tries_per_tackledOpp20m_2019 = for_tries_2019/ for_tackledOpp20_2019,

          against_tackledOpp20_l5 = rollapply(against_tackledOpp20, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          against_tries_l5 = rollapply(against_tries, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          against_tries_per_tackledOpp20m_l5 = against_tries_l5/ against_tackledOpp20_l5,

          against_tackledOpp20_2019 = lag(cumsum(against_tackledOpp20), n=1),
          against_tries_2019 =  lag(cumsum(against_tries), n=1),
          against_tries_per_tackledOpp20m_2019 = against_tries_2019/ against_tackledOpp20_2019,
          
          ) %>% 
  select(season, round, for_name, against_name, Home_Away, for_match_result, for_tries, against_tries,
         net_tries, for_points, against_points, net_points,
         for_tries_per_tackledOpp20m_l5, for_tries_per_tackledOpp20m_2019,
         against_tries_per_tackledOpp20m_l5, against_tries_per_tackledOpp20m_2019)

# Build model

model_data <- rating_data %>% 
  filter(as.numeric(round) >= 5) %>% 
  left_join(rating_data, by = c("season" = "season", "round" = "round", "against_name" = "for_name")) %>% 
  mutate(
    net_tries_per_tackledOpp20m_l5 = for_tries_per_tackledOpp20m_l5.x - against_tries_per_tackledOpp20m_l5.y ,
    net_tries_per_tackledOpp20m_2019 = for_tries_per_tackledOpp20m_2019.x - against_tries_per_tackledOpp20m_2019.y ,
    Home_Away = if_else(Home_Away.x == "Home", 1, 0 )
  )




model_glm <- glm(for_tries.x ~ for_tries_per_tackledOpp20m_2019.x + against_tries_per_tackledOpp20m_2019.y  + Home_Away 
                 , data = model_data, family = poisson )
  
summary(model_glm)

model_data %>% 
  ggplot(aes(x = for_tries.x, y = net_tries_per_tackledOpp20m_2019))+
  geom_point()



  
