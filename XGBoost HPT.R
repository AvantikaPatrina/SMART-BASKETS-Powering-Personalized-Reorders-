library(xgboost)
library(data.table)

param_grid <- expand.grid(
  max_depth = c(4, 6, 8),
  eta = c(0.1, 0.3),
  subsample = c(0.7, 1),
  colsample_bytree = c(0.7, 1)
)
  
best_auc <- 0
best_params <- list()

# Convert your train_data to DMatrix
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, ..features]), label = train_data$reordered)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, ..features]))
actual_labels <- test_data$reordered

for (i in 1:nrow(param_grid)) {
  params <- list(
    booster = "gbtree",
    objective = "binary:logistic",
    eval_metric = "auc",
    max_depth = param_grid$max_depth[i],
    eta = param_grid$eta[i],
    subsample = param_grid$subsample[i],
    colsample_bytree = param_grid$colsample_bytree[i]
  )
  
  model <- xgb.train(
    params = params,
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  # Predict on test
  preds <- predict(model, dtest)
  
  # Calculate AUC
  auc_score <- pROC::auc(actual_labels, preds)
  
  # Update best model
  if (auc_score > best_auc) {
    best_auc <- auc_score
    best_params <- params
  }
}

cat("Best AUC:", best_auc, "\n")
cat("Best Parameters:\n")
print(best_params)


final_model <- xgb.train(
  params = best_params,
  data = dtrain,
  nrounds = 100,
  verbose = 1
)


# Predict on Train
train_preds_prob <- predict(final_model, dtrain)
train_preds_label <- ifelse(train_preds_prob > 0.5, 1, 0)

# Predict on Test
test_preds_prob <- predict(final_model, dtest)
test_preds_label <- ifelse(test_preds_prob > 0.5, 1, 0)

library(caret)

# Train Confusion Matrix
cat("\nFinal XGBoost - Train Confusion Matrix:\n")
print(confusionMatrix(factor(train_preds_label), factor(train_data$reordered)))

# Test Confusion Matrix
cat("\nFinal XGBoost - Test Confusion Matrix:\n")
print(confusionMatrix(factor(test_preds_label), factor(test_data$reordered)))

library(pROC)

roc_final <- roc(test_data$reordered, test_preds_prob)
plot(roc_final, col = "darkblue", main = "Final XGBoost ROC Curve")
auc(roc_final)
