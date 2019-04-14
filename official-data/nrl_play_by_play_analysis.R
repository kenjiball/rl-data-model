# nrl play by play: key Event transitionDataset.csv

library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)


# load data 
transitionDataset <- read_csv("./transitionDataset.csv")

glimpse(transitionDataset)


transitionDataset_2 <- transitionDataset %>% 
  mutate(
    home_away       = substr(state, nchar(state), nchar(state)),
    home_away_prev  = lag(home_away),
    new_set_flag    = coalesce(if_else(home_away != home_away_prev | lag(toTransition) == "try", TRUE, FALSE),TRUE),
    set_start_coord = if_else(new_set_flag == TRUE, xCoord, 0),
    set_id          = cumsum(new_set_flag)
    
  ) %>% 
  group_by(set_id) %>% 
  mutate(
    total_tries_per_set  = sum( toTransition == "try" , na.rm = TRUE)
  ) %>% 
  ungroup()



table(transitionDataset$toTransition)
table(transitionDataset_2$total_tries_per_set)


# exp points

transitionDataset_3 <- transitionDataset_2 %>%
  filter(new_set_flag == TRUE ) %>% 
  select(set_id, set_start_coord, total_tries_per_set) %>%
  mutate( xCoord_bands  = cut(set_start_coord, c(-6:6)*10 )) %>% 
  group_by(xCoord_bands) %>% 
  summarise( count = n(),
             mean_tries   = mean(total_tries_per_set, na.rm = TRUE),
             mean_points  = mean_tries * 4 ) %>% 
  ungroup() %>% 
  mutate(
    freq  = count / sum(count, na.rm = TRUE)
  )
  
transitionDataset_3 %>% 
  ggplot( aes( x = xCoord_bands, y = mean_tries ))+
  geom_point()






