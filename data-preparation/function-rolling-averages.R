
# Test build some rolling average attributes for season

# Account withdrawal features
cat("calculating withdrawal profile features...\n")

acc_trans_withdrawals_7days <- acc_trans_withdrawals[transaction_date >= observation_date - last_7_days]
acc_trans_withdrawals_14days <- acc_trans_withdrawals[transaction_date >= observation_date - last_14_days]
acc_trans_withdrawals_28days <- acc_trans_withdrawals[transaction_date >= observation_date - last_28_days]
acc_trans_withdrawals_3months <- acc_trans_withdrawals[transaction_date >= observation_date - last_3_months]
acc_trans_withdrawals_6months <- acc_trans_withdrawals[transaction_date >= observation_date - last_6_months]
acc_trans_withdrawals_12months <- acc_trans_withdrawals[transaction_date >= observation_date - last_12_months]

datasets <- list(acc_trans_withdrawals
                 , acc_trans_withdrawals_7days
                 , acc_trans_withdrawals_14days
                 , acc_trans_withdrawals_28days
                 , acc_trans_withdrawals_3months
                 , acc_trans_withdrawals_6months
                 , acc_trans_withdrawals_12months)
periods <- c("", "_last_7_days", "_last_14_days", "_last_28_days",
             "_last_3_months", "_last_6_months", "_last_12_months")
results <- foreach(i=1:length(periods)) %dopar% {
  result <- aggregate_withdrawal_features(datasets[[i]])
  # make names like withdrawal_last_7_days ...
  colnames(result)[-1] <- paste0(colnames(result)[-1], periods[[i]])
  result
}
