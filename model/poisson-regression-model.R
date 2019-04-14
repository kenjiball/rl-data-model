# First attempt at Logistic Regression using NRL season-data matrix
# Example run taken from the following blog article:
# https://datascienceplus.com/perform-logistic-regression-in-r/
# https://gist.github.com/mick001/ac92e7c017aecff216fd

# Load any required packages
library(ggplot2)
library(Amelia)
library(rminer)
library(pscl)
library(neuralnet)
library(caret)
library(dplyr)
library(pROC)
library(Rcpp)
library(tidyr)

# define the data set to be used
nrl_data <- season_all_datamatrix
nrl_data <- nrl_data %>% filter(round <= 26 & season <= 2017 ) #%>% select(season) %>% table()
  
# Quick plot to observe if there is any missing data
missmap_data <- nrl_data %>% 
                filter(Home_Away == "Home")%>% 
                select(for_tries
                       # Retained Features
                       , for_completion_rate, for_average_run_metres, for_tackle_busts_per_run, for_kick_metres
                       , for_possession_percentage, for_play_the_balls, for_tackledOpp20
                )

missmap(missmap_data, main = "Missing values vs observed")

# Define a training set and the hold out set
# holdout set MUST be ORDERED and by Round
# nrl_holdout <- holdout(nrl_data$match_id, ratio = 4/5, internalsplit = FALSE, mode = "random",  seed = 2)

round_factors <- levels(as.factor(nrl_data$round))
ratio <- 3/4
nrl_train_ind <- floor(length(round_factors)*ratio)
nrl_train <- subset(nrl_data, as.numeric(round) <= nrl_train_ind )
nrl_test <- subset(nrl_data, as.numeric(round) > nrl_train_ind )

table(nrl_train$round)
table(nrl_test$round)
dim(nrl_train)
dim(nrl_test)


# Run the glm Model 
# Initally using the binomial logit fit for the match result

# model 1 training set

nrl_train_trim_m1 <- nrl_train %>% 
  filter(Home_Away == "Home")%>% 
  select(for_tries
         # Retained Features
         , for_completion_rate, for_average_run_metres, for_tackle_busts_per_run, for_kick_metres
         , for_possession_percentage, for_play_the_balls, for_tackledOpp20

  )

# model 2 training set

nrl_train_trim_m2 <- nrl_train %>% 
                   filter(Home_Away == "Home")%>% 
                   select(for_tries
                          # Retained Features
                          , for_completion_rate, for_average_run_metres, for_tackle_busts_per_run, for_kick_metres
                          , for_possession_percentage, for_play_the_balls, for_tackledOpp20
                          # Testing Features
                          , for_territory
                   )

# set set holout data
nrl_test_trim <-    nrl_test %>% 
                    filter(Home_Away == "Home")%>% 
                    select(for_tries
                           # Retained Features
                           , for_completion_rate, for_average_run_metres, for_tackle_busts_per_run, for_kick_metres
                           , for_possession_percentage, for_play_the_balls, for_tackledOpp20
                           # Testing Features
                           , for_territory
                    )


#all_equal(nrl_test_trim_m1,nrl_test_trim2, convert = TRUE, ignore_col_order = TRUE)

contrasts(as.factor(nrl_train_trim_m1$for_tries))
table(as.factor(nrl_train_trim_m1$for_tries))

# Run Logit model m1 and m2

model_m1 <- glm(for_tries ~ . , data=nrl_train_trim_m1 , family=poisson())
model_m2 <- glm(for_tries ~ . , data=nrl_train_trim_m2 , family=poisson())

# See summary of model to determine Deterministic (significant) Variables
# Remember that in the logit model the response variable is log odds: 
# ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + … + z*xn
# McFadden R2 index can be used to assess the model fit
# McFadden's pseudo R-squared ranging from 0.2 to 0.4 indicates very good model fit

summary(model_m1)
summary(model_m2)
anova(model_m2, model_m1, test="Chisq")
pR2(model_m1)
pR2(model_m2)

# Test the accuracy of the model using the test hold out cell
nrl_test_trim$predicted_tries_m1 <-  predict(model_m1,nrl_test_trim,type='response')
nrl_test_trim$predicted_tries_m2 <-  predict(model_m2,nrl_test_trim,type='response')

cor(nrl_test_trim$for_tries,nrl_test_trim$predicted_tries_m1)
cor(nrl_test_trim$for_tries,nrl_test_trim$predicted_tries_m2)

# chart/ print results and predictions 
nrl_test_trim_plot <- nrl_test_trim %>%
                      select(for_tries, predicted_tries_m1, predicted_tries_m2) %>%
                      gather(model, predicted_tries, -for_tries)



ggplot(nrl_test_trim_plot, aes(x = as.factor(for_tries), y = predicted_tries))+
  geom_boxplot(aes(col = model)) +
  geom_rug() +
  #facet_grid(model ~ .)+
  geom_abline(slope=1, intercept = min(nrl_test_trim_plot$for_tries) -1 ) +
  expand_limits(x = 0, y = 0)


# Build a ROC curve to graph model performance
ROC_m1 <-roc(nrl_test_trim$for_tries,nrl_test_trim$predicted_tries_m1, levels = c(3,4))
ROC_m2 <-roc(nrl_test_trim$for_tries,nrl_test_trim$predicted_tries_m2, levels = c(3,4))

plot.new()
plot.roc(ROC_m1, col="blue")
plot.new()
plot.roc(ROC_m2, col="blue")
auc(ROC_m1)
auc(ROC_m2)


