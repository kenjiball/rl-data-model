# First attempt at Logistic Regression using NRL season-data matrix
# Example run taken from the following blog article:
# https://datascienceplus.com/perform-logistic-regression-in-r/
# https://gist.github.com/mick001/ac92e7c017aecff216fd

# Load any required packages

library(Amelia)
library(rminer)
library(pscl)
library(neuralnet)
library(caret)
library(dplyr)
library(ROCR)
library(pROC)
library(ggplot2)

# define the data set to be used
nrl_data <- season_all_datamatrix

# Quick plot to observe if there is any missing data
missmap(nrl_data, main = "Missing values vs observed")

# Define a training set and the hold out set
# holdout set MUST be ORDERED and by Round
# nrl_holdout <- holdout(nrl_data$match_id, ratio = 4/5, internalsplit = FALSE, mode = "random",  seed = 2)

round_factors <- levels(as.factor(nrl_data$round))
ratio <- 3/5
nrl_train_ind <- floor(length(round_factors)*ratio)
nrl_train <- subset(nrl_data, as.numeric(round) <= nrl_train_ind )
nrl_test <- subset(nrl_data, as.numeric(round) > nrl_train_ind )

table(nrl_train$round)
table(nrl_test$round)
dim(nrl_train)
dim(nrl_test)


# Run the glm Model 
# Initally using the binomial logit fit for the match result

# Choose columns to run the model
choose.columns <- c("for_match_result",
                    "Home_Away",
                    
                    # Retained Features
                    "for_run_metres",
                    "for_complete_sets",
                    "for_inCompleteSets",
                    "for_forced_drop_outs",
                    "for_redzone_tackle_percent",
                    "for_weighted_kicks",
                    "for_oppHalf_tackle_percent",
                    
                    "against_run_metres",
                    "against_complete_sets",
                    "against_inCompleteSets",
                    "against_forced_drop_outs",
                    "against_redzone_tackle_percent",
                    "against_weighted_kicks",
                    "against_oppHalf_tackle_percent",
                    
                    # Testing Features
                    
                    "for_gang_tackle_ratio",
                    "against_gang_tackle_ratio"
                    
                    
                    
                    )

nrl_train_trim <- nrl_train[choose.columns] #nrl_train %>% select(c(-match_id:-match,-for_name,-against_name))  
nrl_test_trim <- nrl_test[choose.columns] #nrl_test %>% select(c(-match_id:-match,-for_name,-against_name))

# Filter for only the home team data to avoid duplication in the model
nrl_train_trim <- nrl_train_trim %>% filter(Home_Away == "Home") %>% select(-Home_Away)
nrl_test_trim <- nrl_test_trim %>% filter(Home_Away == "Home") %>% select(-Home_Away)

dim(nrl_train_trim)
dim(nrl_test_trim)

contrasts(as.factor(nrl_train_trim$for_match_result))

# Run Logit model

model <- glm(as.factor(for_match_result) ~., data=nrl_train_trim , family="binomial"(link='logit'))

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
fitted.results2 <- ifelse(fitted.results > 0.5,"Win","Lose")
misClasificError <- mean(fitted.results2 != nrl_test_trim$for_match_result)
print(paste('Accuracy',1-misClasificError))

# chart/ print results and predictions 
test_vs_model <- data.frame(nrl_test_trim$for_match_result,fitted.results)
names(test_vs_model) <- c("actual","model")
test_vs_model <- test_vs_model %>% mutate(actual_num = if_else(actual == "Win",1,0) )

ggplot(test_vs_model, aes(x = model, y = actual_num))+
    geom_point() +
    stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)





# Build a ROC curve to graph model performance
ROC <-roc(nrl_test_trim$for_match_result,fitted.results)
plot(ROC, col="blue")
auc(ROC)

# Principal Component Analysis
pr.train <- prcomp(x = nrl_train_trim[-1] , scale = TRUE, center = TRUE )
summary(pr.train)

biplot(pr.train)

pr.var <- pr.train$sdev^2
pve <- pr.var/sum(pr.var)

plot(pve, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b")


train_results <- nrl_train_trim[1] %>% mutate(result = if_else(for_match_result == "Win",1,0) ) %>% select(result)

plot(pr.train$x[, c(1, 2)], col = (train_results$result + 1) ,  xlab = "PC1", ylab = "PC2")
plot(pr.train$x[, c(1, 3)], col = (train_results$result + 1) ,  xlab = "PC1", ylab = "PC3")


nComp = 10
mu = colMeans(nrl_train_trim[-1])
  
pr.train.hat = pr.train$x[,1:nComp] %*% t(pr.train$rotation[,1:nComp])
pr.train.hat.s = scale(pr.train.hat, center = -mu, scale = FALSE)

pr.train.hat.s[1,]
nrl_train_trim[1,-1]
mu

