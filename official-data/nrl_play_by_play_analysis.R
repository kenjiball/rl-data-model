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
    end_set_flag    = if_else(lead(new_set_flag) == TRUE, TRUE, FALSE),
    set_start_coord = if_else(new_set_flag == TRUE, xCoord, 0),
    set_end_coord   = if_else(end_set_flag == TRUE, xCoord, 0),
    set_id          = cumsum(new_set_flag)
    
  ) %>% 
  group_by(set_id) %>% 
  mutate(
    total_tries_per_set  = sum( toTransition == "try" , na.rm = TRUE),
    metres_gained        = abs(sum(set_end_coord, na.rm = TRUE) - sum(set_start_coord, na.rm = TRUE)),
    tackles_per_set      = sum(toTransition == "tackled", na.rm = TRUE),
    penalties_per_set    = sum(toTransition == "penalty", na.rm = TRUE)
  ) %>% 
  ungroup()



table(transitionDataset$toTransition)
table(transitionDataset_2$total_tries_per_set)
table(transitionDataset_2$tackles_per_set)
table(transitionDataset_2$penalties_per_set)

# metres gained distribution
transitionDataset_2 %>% 
  filter(penalties_per_set == 1 & metres_gained > 0) %>% 
  select(set_id, metres_gained, total_tries_per_set, penalties_per_set) %>% 
  distinct() %>% 
  ggplot( aes(x = metres_gained) ) +
  #facet_grid(total_tries_per_set~. , scales = "free" ) +
  geom_histogram(binwidth = 5)


transitionDataset_2 %>%
  select(set_id, metres_gained, total_tries_per_set, penalties_per_set) %>% 
  distinct() %>% 
  group_by(total_tries_per_set, penalties_per_set) %>% 
  summarise( count = n()
           )



# exp points modelling

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
    freq  = count / sum(count, na.rm = TRUE),
    xCoord = as.numeric(xCoord_bands),
    xCoord_mid = c(-5.5:5.5, NA)*10
  )


# ggplot
# linear model with bands
transitionDataset_3 %>% 
  filter(!is.na(xCoord_bands) & xCoord_bands != "(50,60]") %>% 
  ggplot( aes( x = xCoord_mid, y = mean_tries )) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE)

transitionDataset_4 <- transitionDataset_3 %>% 
  filter(!is.na(xCoord_bands) & xCoord_bands != "(50,60]")

linear_exp_tries_model <- lm(mean_tries~xCoord_mid, data = transitionDataset_4)
summary(linear_exp_tries_model)

# linear model
transitionDataset_2 %>%
  filter(new_set_flag == TRUE & set_start_coord >= -60 & set_start_coord <= 50 ) %>% 
  select(set_id, set_start_coord, total_tries_per_set) %>% 
  ggplot( aes( x = set_start_coord, y = total_tries_per_set )) +
  geom_point() + 
  geom_smooth(method = "lm", 
              se = FALSE)












