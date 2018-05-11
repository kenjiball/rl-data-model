# First attempt at Logistic Regression using NRL season-data matrix
# Example run taken from the following blog article:
# https://datascienceplus.com/perform-logistic-regression-in-r/
# https://gist.github.com/mick001/ac92e7c017aecff216fd

# Load any required packages

library(Amelia)
library(rminer)
library(pscl)

# define the data set to be used
nrl_data <- season_2018_datamatrix

# Quick plot to observe if there is any missing data
missmap(nrl_data, main = "Missing values vs observed")

# Define a training set and the hold out set
nrl_holdout <- holdout(nrl_data$match_id, ratio = 4/5, internalsplit = FALSE, mode = "random",  seed = 1)

# nrl_holdout$tr is the training set indexes
length(nrl_holdout$tr)
nrl_tr <- nrl_data[nrl_holdout$tr,]
# nrl_holdout$tr is the model test set indexes
length(nrl_holdout$ts)
nrl_ts <- nrl_data[nrl_holdout$ts,]

# Run the glm Model 
# Initally using the binomial logit fit for the match result

# Remove other factors and character field for testing to run the model
choose.columns <- c("for_match_result",
                    "against_complete_sets",
                    "against_drop_outs",
                    "against_errors",
                    "against_off_loads",
                    "against_run_metres",
                    "against_runs",
                    "against_tackle_busts",
                    "against_tackle_busts_per_run",
                    "against_weighted_kicks",
                    "for_complete_sets",
                    "for_completion_rate",
                    "for_errors",
                    "for_forced_drop_outs",
                    "for_line_breaks",
                    "for_missed_tackles",
                    "for_possession_percentage",
                    "for_redzone_tackle_percent",
                    "for_run_metres",
                    "for_tackle_busts_per_run")

nrl_tr_trim <- nrl_tr[choose.columns]
nrl_ts_trim <- nrl_ts[choose.columns]

contrasts(nrl_tr_trim$for_match_result)

model <- glm(for_match_result ~.,family=binomial(link='logit'),data=nrl_tr_trim)

# See summary of model to determine Deterministic (significant) Variables
# Remember that in the logit model the response variable is log odds: 
# ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + … + z*xn
# McFadden R2 index can be used to assess the model fit
# McFadden's pseudo R-squared ranging from 0.2 to 0.4 indicates very good model fit

summary(model)
anova(model, test="Chisq")
pR2(model)

# Test the accuracy of the model using the test hold out cell

fitted.results <- predict(model,nrl_ts_trim,type='response')
fitted.results2 <- ifelse(fitted.results > 0.5,"Lose","Win")
misClasificError <- mean(fitted.results2 != nrl_ts_trim$for_match_result)
print(paste('Accuracy',1-misClasificError))

see_results <- data.frame(nrl_ts$for_name, nrl_ts$against_name,
                          nrl_ts$for_points, nrl_ts$against_points,
                          fitted.results, fitted.results2, nrl_ts_trim)

write.csv(see_results,file="../model_test_results.csv")


dim(nrl_data)

head(nrl_data[,1:7])


