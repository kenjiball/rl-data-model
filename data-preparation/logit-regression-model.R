# First attempt at Logistic Regression using NRL season-data matrix
# Example run taken from the following blog article:
# https://datascienceplus.com/perform-logistic-regression-in-r/
# https://gist.github.com/mick001/ac92e7c017aecff216fd

# Load any required packages

library(Amelia)
library(rminer)
library(pscl)
library(neuralnet)

# define the data set to be used
nrl_data <- season_2018_datamatrix

# Quick plot to observe if there is any missing data
missmap(nrl_data, main = "Missing values vs observed")

# Define a training set and the hold out set
# holdout set MUST be ORDERED and by Round
# nrl_holdout <- holdout(nrl_data$match_id, ratio = 4/5, internalsplit = FALSE, mode = "random",  seed = 2)

round_factors <- levels(as.factor(nrl_data$round))
ratio <- 3/4
nrl_train_ind <- floor(length(round_factors)*ratio)
nrl_train <- subset(nrl_data, as.numeric(round) <= nrl_train_ind )
nrl_test <- subset(nrl_data, as.numeric(round) > nrl_train_ind )

levels(as.factor(nrl_train$round))
levels(as.factor(nrl_test$round))
dim(nrl_train)
dim(nrl_test)

# nrl_holdout$tr is the training set indexes
# nrl_tr <- nrl_data[nrl_holdout$tr,]
# nrl_holdout$tr is the model test set indexes
# nrl_ts <- nrl_data[nrl_holdout$ts,]

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

nrl_train_trim <- nrl_train[choose.columns]
nrl_test_trim <- nrl_test[choose.columns]

dim(nrl_train_trim)
dim(nrl_test_trim)

contrasts(nrl_train_trim$for_match_result)

# Run Logit model

model <- glm(for_match_result ~.,family=binomial(link='logit'),data=nrl_train_trim)

# See summary of model to determine Deterministic (significant) Variables
# Remember that in the logit model the response variable is log odds: 
# ln(odds) = ln(p/(1-p)) = a*x1 + b*x2 + … + z*xn
# McFadden R2 index can be used to assess the model fit
# McFadden's pseudo R-squared ranging from 0.2 to 0.4 indicates very good model fit

summary(model)
anova(model, test="Chisq")
pR2(model)

# Test the accuracy of the model using the test hold out cell

fitted.results <- predict(model,nrl_test_trim,type='response')
fitted.results2 <- ifelse(fitted.results > 0.5,"Lose","Win")
misClasificError <- mean(fitted.results2 != nrl_test_trim$for_match_result)
print(paste('Accuracy',1-misClasificError))

see_results <- data.frame(nrl_test$for_name, nrl_test$against_name,
                          nrl_test$for_points, nrl_test$against_points,
                          fitted.results, fitted.results2, nrl_test_trim)

write.csv(see_results,file="../model_test_results.csv")

# Run Neural Network Model

# Replace Win/Loss in for_match_result
nrl_train_trim_s <- nrl_train_trim
nrl_train_trim_s$for_match_result <- sapply(nrl_train_trim_s$for_match_result,function(x) {x <- gsub("Win",1,x)})
nrl_train_trim_s$for_match_result <- sapply(nrl_train_trim_s$for_match_result,function(x) {x <- gsub("Lose",0,x)})
nrl_train_trim_s$for_match_result <- as.numeric(nrl_train_trim_s$for_match_result)

nrl_test_trim_s <- nrl_test_trim
nrl_test_trim_s$for_match_result <- sapply(nrl_test_trim_s$for_match_result,function(x) {x <- gsub("Win",1,x)})
nrl_test_trim_s$for_match_result <- sapply(nrl_test_trim_s$for_match_result,function(x) {x <- gsub("Lose",0,x)})
nrl_test_trim_s$for_match_result <- as.numeric(nrl_test_trim_s$for_match_result)


# Normalise data so each variable is scaled from 0 to 1
max_value <- as.numeric(sapply(nrl_train_trim_s, max))
min_value <- as.numeric(sapply(nrl_train_trim_s, min))
scale_value <- max_value - min_value
nrl_train_trim_norm <- as.data.frame(scale(nrl_train_trim_s, center = min_value, scale = scale_value))

max_value <- as.numeric(sapply(nrl_test_trim_s, max))
min_value <- as.numeric(sapply(nrl_test_trim_s, min))
scale_value <- max_value - min_value
nrl_test_trim_norm <- as.data.frame(scale(nrl_test_trim_s, center = min_value, scale = scale_value))


head(nrl_train_trim_norm)

# Set up the formula to define the prediction and the input variables.

all_vars <- colnames(nrl_train_trim_norm)
predictor_var <- "for_match_result"
input_vars <- paste(all_vars[!all_vars%in%predictor_var], collapse = "+")
formula_vars <-  as.formula(paste(paste(predictor_var,"~"),input_vars, collapse = "+"))


# Create Neural Network Model
neural_model <- neuralnet(formula = formula_vars, hidden = 4, linear.output = T, data = nrl_train_trim_norm)

plot(neural_model)

# Predit result using neural network and test data
predict_games <- compute(neural_model, nrl_test_trim_norm[,2:20])

# Measure the accuracy of the Predictions
predict_games_result <- predict_games$net.result*(max(nrl_test_trim_norm$for_match_result)-min(nrl_test_trim_norm$for_match_result))+min(nrl_test_trim_norm$for_match_result)
actual_games_result <- nrl_test_trim_norm$for_match_result * (max(nrl_test_trim_norm$for_match_result)-min(nrl_test_trim_norm$for_match_result))+min(nrl_test_trim_norm$for_match_result)

MSE <- sum((predict_games_result - actual_games_result)^2)/nrow(nrl_test_trim_norm)
MSE

plot_data <- data.frame(as.data.frame(predict_games_result),as.data.frame(actual_games_result))
names(plot_data) <- c("prediction","actual")
plot(plot_data$prediction, plot_data$actual)


dim(nrl_data)

head(nrl_data[,1:7])


