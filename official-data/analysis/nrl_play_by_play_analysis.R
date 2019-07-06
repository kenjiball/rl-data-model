# nrl play by play: key Event transitionDataset.csv
source("./load-packages.R")

# load data 
transitionDataset <- read.csv("./transitionDataset.csv")

glimpse(transitionDataset)


transitionDataset_2 <- transitionDataset %>% 
  mutate(
    state           = as.character(state),
    toTransition    = as.character(toTransition),
    fromTransition  = lag(toTransition, n = 1),
    fromXCoord      = lag(xCoord, n = 1),
    home_away       = substr(state, nchar(state), nchar(state)),
    home_away_prev  = lag(home_away),
    score_diff      = score - oppScore,
    total_points    = score + oppScore,
    points_added    = total_points - lag(total_points, n = 1),
    new_set_flag    = coalesce(if_else(home_away != home_away_prev | lag(toTransition) == "try", TRUE, FALSE),TRUE),
    end_set_flag    = if_else(lead(new_set_flag) == TRUE, TRUE, FALSE),
    set_start_coord = if_else(new_set_flag == TRUE, xCoord, 0),
    set_end_coord   = if_else(end_set_flag == TRUE, xCoord, 0),
    set_id          = cumsum(new_set_flag),
    xCoord_bands    = cut(set_start_coord, c(-6:6)*10 ),
    is_penalty_goal = if_else(toTransition == "penalty" & lead(points_added,n=1) == 2, TRUE, FALSE),
    penalty_metres  = if_else(toTransition == "penalty" & is_penalty_goal == FALSE, abs(xCoord - lead(xCoord, n=2)) , 0)
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
  filter(penalties_per_set == 0 & metres_gained > 0) %>% 
  select(set_id, metres_gained, total_tries_per_set, penalties_per_set) %>% 
  distinct() %>% 
  ggplot( aes(x = metres_gained) ) +
  #facet_grid(total_tries_per_set~. , scales = "free" ) +
  geom_histogram(binwidth = 5)


#### Expected points modelling

transitionDataset_3 <- transitionDataset_2 %>%
  filter(new_set_flag == TRUE ) %>% 
  select(set_id, set_start_coord, total_tries_per_set) %>%
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


# Plot results and build linear model
# linear model with bands
transitionDataset_3 %>% 
  filter(!is.na(xCoord_bands) & xCoord_bands != "(50,60]") %>% 
  ggplot( aes( x = xCoord_mid, y = mean_tries )) +
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  ggtitle("Means Tries by Set Starting Position")

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


### Expected Points from a penalty

# Simple analysis of Tries per penalty set
transitionDataset_2 %>%
  select(set_id, metres_gained, total_tries_per_set, penalties_per_set) %>% 
  distinct() %>% 
  group_by( penalties_per_set) %>% 
  summarise( count = n(),
             tries = sum(total_tries_per_set),
             tries_per_set = tries/count
  )


# Using our linear model
tidy_model <- tidy(linear_exp_tries_model)
# using y = mx + b, we want to find the exp_tries (y) for penalties.
# First we have convert penalties into metres gained
#average_metres_gained_by_penalty 
transitionDataset_2 %>%
  select(set_id, fromTransition, toTransition, fromXCoord, xCoord, xCoord_bands, is_penalty_goal,penalty_metres,
         score_diff,
         metres_gained, total_tries_per_set, penalties_per_set) %>% 
  filter(toTransition == "penalty" ) %>% 
  mutate( score_diff_band = case_when(score_diff <= -8 ~ "<=-8",
                                 score_diff > -8 & score_diff < 0 ~ ">-8<0",
                                 score_diff == 0 ~ "=0",
                                 score_diff > 0 & score_diff < 8 ~ ">0<=8",
                                 score_diff >= 8 ~ ">=8",
                                 )
          
          ) %>% 
  ggplot( aes(penalty_metres)) +
  geom_histogram()+
    facet_grid(score_diff_band~is_penalty_goal)

# ggplot( aes( x = xCoord_mid, y = mean_tries )) +
#   geom_point() + 
#   geom_smooth(method = lm, se = FALSE) +
#   ggtitle("Means Tries by Set Starting Position")



