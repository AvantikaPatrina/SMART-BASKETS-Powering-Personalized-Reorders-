# Columns to remove
cols_to_remove <- c("last_reordered", "total_orders", "preferred_day","preferred_hour", "avg_days_between_orders","up_order_count", "time_since_last_purchase")

# Drop columns from final_train_df
final_train_df <- final_train_df[, !..cols_to_remove, with = FALSE]


set.seed(123)  # any number to ensure consistent splits

# Total number of rows
n <- nrow(final_train_df)

# Randomly sample 80% row indices for training
train_indices <- sample(seq_len(n), size = 0.8 * n)

train_data <- final_train_df[train_indices]
test_data  <- final_train_df[-train_indices]

library(data.table)
install.packages("xgboost")
library(xgboost)
# Install if not already
if (!require(caret)) install.packages("caret", dependencies = TRUE)

# Load the package
library(caret)


setDT(train_data)
setDT(test_data)

features <- c(
  "order_hour_of_day","order_number", "order_dow", "days_since_prior_order",
  "reorder_ratio", "prod_reorder_rate",
  "product_popularity_log", "avg_cart_position","add_to_cart_order"
)

# Prepare matrices
dtrain <- xgb.DMatrix(data = as.matrix(train_data[, ..features]), label = train_data$reordered)
dtest <- xgb.DMatrix(data = as.matrix(test_data[, ..features]))


################# Logistic Regression Model #############

# Logistic Regression
logistic_model <- glm(reordered ~ ., data = train_data[, c(features, "reordered"), with = FALSE], family = "binomial")

# Predict probabilities
train_logistic_pred_probs <- predict(logistic_model, newdata = train_data[, ..features], type = "response")
test_logistic_pred_probs <- predict(logistic_model, newdata = test_data[, ..features], type = "response")

# Convert to class labels
train_logistic_labels <- ifelse(train_logistic_pred_probs > 0.5, 1, 0)
test_logistic_labels  <- ifelse(test_logistic_pred_probs > 0.5, 1, 0)

# Confusion matrices
cat("\nLogistic Regression - Train Confusion Matrix:\n")
print(confusionMatrix(factor(train_logistic_labels), factor(train_data$reordered)))

cat("\nLogistic Regression - Test Confusion Matrix:\n")
print(confusionMatrix(factor(test_logistic_labels), factor(test_data$reordered)))


########################Random Forest Model #######################

# Install and load randomForest if not already
if (!require(randomForest)) install.packages("randomForest")
library(randomForest)

# Train Random Forest
rf_model <- randomForest(as.factor(reordered) ~ ., data = train_data[, c(features, "reordered"), with = FALSE], ntree = 100)

# Predict
train_rf_pred <- predict(rf_model, newdata = train_data[, ..features])
test_rf_pred  <- predict(rf_model, newdata = test_data[, ..features])

# Confusion matrices
cat("\nRandom Forest - Train Confusion Matrix:\n")
print(confusionMatrix(train_rf_pred, factor(train_data$reordered)))

cat("\nRandom Forest - Test Confusion Matrix:\n")
print(confusionMatrix(test_rf_pred, factor(test_data$reordered)))



#######################################Train XGBoost ##########################
params <- list(
  objective = "binary:logistic",
  eval_metric = "logloss"
)

model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 100,
  verbose = 1
)

# Predict on train
train_pred <- predict(model, dtrain)

# Predict on test
test_pred <- predict(model, dtest)
test_pred


# Predict probabilities
pred_probs <- predict(model, dtest)
pred_probs


# Convert probabilities to binary (0 or 1) using a threshold (e.g., 0.5)
pred_labels <- ifelse(pred_probs > 0.5, 1, 0)

# Actual reordered values
actual_labels <- test_data$reordered

# Install and load caret if not already
if (!require(caret)) install.packages("caret")
library(caret)

confusionMatrix(factor(pred_labels), factor(actual_labels))


# Convert probabilities to labels
train_xgb_labels <- ifelse(train_pred > 0.5, 1, 0)
test_xgb_labels  <- ifelse(test_pred > 0.5, 1, 0)

# Confusion matrices
cat("\nXGBoost - Train Confusion Matrix:\n")
print(confusionMatrix(factor(train_xgb_labels), factor(train_data$reordered)))
cat("\nXGBoost - Test Confusion Matrix:\n")
print(confusionMatrix(factor(test_xgb_labels), factor(test_data$reordered)))



###############ROC ##############
if (!require(pROC)) install.packages("pROC")
library(pROC)

# For randomForest, get predicted probabilities
test_rf_pred_probs <- predict(rf_model, newdata = test_data[, ..features], type = "prob")[, "1"]
# Create ROC objects
roc_logistic <- roc(test_data$reordered, test_logistic_pred_probs)
roc_rf       <- roc(test_data$reordered, test_rf_pred_probs)
roc_xgb      <- roc(test_data$reordered, test_pred)
# Plot Logistic Regression ROC first
plot(roc_logistic, col = "blue", main = "ROC Curve Comparison", lwd = 2)

# Add Random Forest ROC
lines(roc_rf, col = "green", lwd = 2)

# Add XGBoost ROC
lines(roc_xgb, col = "red", lwd = 2)

# Add a legend
legend("bottomright",
       legend = c("Logistic Regression", "Random Forest", "XGBoost"),
       col = c("blue", "green", "red"),
       lwd = 2)



cat("Logistic Regression AUC:", auc(roc_logistic), "\n")
cat("Random Forest AUC:", auc(roc_rf), "\n")
cat("XGBoost AUC:", auc(roc_xgb), "\n")


