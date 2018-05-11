# First attempt at Logistic Regression using NRL season-data matrix
# Example run taken from the following blog article:
# https://datascienceplus.com/perform-logistic-regression-in-r/
# https://gist.github.com/mick001/ac92e7c017aecff216fd

# Load any required packages

library(Amelia)
library(rminer)

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
                    

#c(7:83,85:161)
nrl_tr_trim <- nrl_tr[choose.columns]
contrasts(nrl_tr_trim$for_match_result)

model <- glm(for_match_result ~.,family=binomial(link='logit'),data=nrl_tr_trim)

# See summary of model to determine Deterministic (significant) Variables
# Remember that in the logit model the response variable is log odds: 
# ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + … + z*xn

summary(model)







dim(nrl_data)

head(nrl_data[,1:7])


