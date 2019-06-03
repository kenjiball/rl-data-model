# Tackle in Opp 20 analysis for offense, defense efficiency ratings

# Build rating data df
# Include basic match data
# Last 5 matches and season rating using 20m offense and defence efficiency

rating_data <- season_2018_datamatrix %>% 
  group_by(for_name) %>% 
  mutate( ratio_20m = for_tackledOpp20/for_runs,
          tries_per_tackledOpp20m = for_tries/for_tackledOpp20,
          net_tries = for_tries - against_tries,
          net_points = for_points - against_points,
          
          for_post_contact_m_per_run = for_post_contact_metres / for_runs,
          against_post_contact_m_per_run = against_post_contact_metres / against_runs,
            
          for_tackledOpp20_l5 = rollapply(for_tackledOpp20, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          for_tries_l5 = rollapply(for_tries, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          for_tries_per_tackledOpp20m_l5 = for_tries_l5/ for_tackledOpp20_l5,
          
          for_tackledOpp20_season = lag(cumsum(for_tackledOpp20), n=1),
          for_tries_season =  lag(cumsum(for_tries), n=1),
          for_tries_per_tackledOpp20m_season = for_tries_season/ for_tackledOpp20_season,

          against_tackledOpp20_l5 = rollapply(against_tackledOpp20, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          against_tries_l5 = rollapply(against_tries, width=list(-1:-5), sum, na.rm=TRUE, partial = 0, align = "right"),
          against_tries_per_tackledOpp20m_l5 = against_tries_l5/ against_tackledOpp20_l5,

          against_tackledOpp20_season = lag(cumsum(against_tackledOpp20), n=1),
          against_tries_season =  lag(cumsum(against_tries), n=1),
          against_tries_per_tackledOpp20m_season = against_tries_season/ against_tackledOpp20_season,
          
          for_post_contact_metres_season = lag(cumsum(for_post_contact_metres), n=1),
          for_runs_season =  lag(cumsum(for_runs), n=1),
          for_post_contact_m_per_run_season = for_post_contact_metres_season/for_runs_season,

          against_post_contact_metres_season = lag(cumsum(against_post_contact_metres), n=1),
          against_runs_season =  lag(cumsum(against_runs), n=1),
          against_post_contact_m_per_run_season = against_post_contact_metres_season/against_runs_season,
          
          for_missed_tackless_season = lag(cumsum(for_missed_tackles), n=1),
          against_runs_season =  lag(cumsum(against_runs), n=1),
          for_missed_tackles_per_run_season = for_missed_tackless_season/against_runs_season,
          
          against_missed_tackless_season = lag(cumsum(against_missed_tackles), n=1),
          for_runs_season =  lag(cumsum(for_runs), n=1),
          against_missed_tackles_per_run_season = against_missed_tackless_season/for_runs_season,
          
          
          
          
          ) %>% 
  select(season, round, for_name, against_name, Home_Away, for_match_result, for_tries, against_tries,
         net_tries, for_points, against_points, net_points,
         
         for_tries_per_tackledOpp20m_l5,
         for_tries_per_tackledOpp20m_season,
         for_post_contact_m_per_run_season,
         for_missed_tackles_per_run_season,
         
         against_tries_per_tackledOpp20m_l5,
         against_tries_per_tackledOpp20m_season,
         against_post_contact_m_per_run_season,
         against_missed_tackles_per_run_season
         )

# Build model

model_data <- rating_data %>% 
  filter(as.numeric(round) >= 4) %>% 
  left_join(rating_data, by = c("season" = "season", "round" = "round", "against_name" = "for_name")) %>% 
  mutate(
    net_tries_per_tackledOpp20m_l5 = for_tries_per_tackledOpp20m_l5.x - against_tries_per_tackledOpp20m_l5.y ,
    net_tries_per_tackledOpp20m_season = for_tries_per_tackledOpp20m_season.x - against_tries_per_tackledOpp20m_season.y ,
    Home_Away = if_else(Home_Away.x == "Home", 1, 0 ),
    for_match_result.x = if_else(for_match_result.x == "Win", 1, 0)
  )




model_glm <- glm(for_match_result.x ~ Home_Away + 
                   for_tries_per_tackledOpp20m_season.x + against_tries_per_tackledOpp20m_season.y  +
                     for_missed_tackles_per_run_season.y + against_missed_tackles_per_run_season.x

                 , data = model_data
                 #, family = poisson 
                 , family = "binomial"
                 )

# for_tries_per_tackledOpp20m_season.x + against_tries_per_tackledOpp20m_season.y  + 
#   for_post_contact_m_per_run_season.x + against_post_contact_m_per_run_season.y + 
#   for_missed_tackles_per_run_season.y + against_missed_tackles_per_run_season.x 

summary(model_glm)

model_data %>% 
  ggplot(aes(x = for_tries_per_tackledOpp20m_season.x, y = for_match_result.x)) +
  geom_point()



  
