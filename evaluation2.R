# Load required packages
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)
library(pROC)

# Part 1: Cross-Validation on Training Data
set.seed(123)
train_control <- trainControl(
  method = "cv", 
  number = 10, 
  classProbs = TRUE, 
  summaryFunction = twoClassSummary, 
  savePredictions = "final", 
  verboseIter = TRUE
)

tune_grid <- expand.grid(cp = 0.01)

set.seed(123)
dt_model_cv <- train(
  x = train_data_final[, predictors_top], 
  y = train_data_final$apo_general,
  method = "rpart",
  trControl = train_control,
  tuneGrid = tune_grid,
  weights = train_data_final$sample_weight,
  metric = "ROC"
)

# Print cross-validation results
print("Cross-Validation Results (Training Data, 10-Fold CV):")
print(dt_model_cv)

# Extract cross-validated metrics (default threshold = 0.5)
cv_metrics <- dt_model_cv$results
cat("Cross-Validated Metrics (Default Threshold = 0.5):\n")
cat("ROC:", cv_metrics$ROC, "\n")
cat("Sensitivity:", cv_metrics$Sens, "\n")
cat("Specificity:", cv_metrics$Spec, "\n")

# Compute cross-validated metrics with threshold = 0.6
cv_predictions <- dt_model_cv$pred
cv_predictions <- cv_predictions %>%
  filter(cp == 0.01)  # Select predictions for cp = 0.01
cv_predictions_adjusted <- factor(ifelse(cv_predictions$APO > 0.6, "APO", "NonAPO"), 
                                  levels = c("NonAPO", "APO"))
cv_conf_matrix <- confusionMatrix(cv_predictions_adjusted, 
                                  cv_predictions$obs, 
                                  positive = "APO",
                                  mode = "everything")
print("Cross-Validated Confusion Matrix and Metrics (Threshold = 0.6):")
print(cv_conf_matrix)

cv_accuracy <- cv_conf_matrix$overall["Accuracy"]
cv_precision <- cv_conf_matrix$byClass["Pos Pred Value"]
cv_recall <- cv_conf_matrix$byClass["Sensitivity"]
cv_specificity <- cv_conf_matrix$byClass["Specificity"]
cv_f1_score <- cv_conf_matrix$byClass["F1"]

cat("Cross-Validated Metrics (Threshold = 0.6):\n")
cat("Accuracy:", cv_accuracy, "\n")
cat("Precision:", cv_precision, "\n")
cat("Recall (Sensitivity):", cv_recall, "\n")
cat("Specificity:", cv_specificity, "\n")
cat("F1-Score:", cv_f1_score, "\n")

# Generate ROC Curve for Cross-Validation
roc_obj_cv <- roc(cv_predictions$obs, cv_predictions$APO, levels = c("NonAPO", "APO"), direction = "<")

# Calculate AUC for Cross-Validation
auc_value_cv <- auc(roc_obj_cv)

# Save ROC Curve Plot for Cross-Validation to PNG
png("roc_curve_cv.png", width = 800, height = 600, res = 100)
plot(roc_obj_cv, 
     main = paste("ROC Curve for 10-Fold Cross-Validation\nAUC =", round(auc_value_cv, 3)),
     col = "blue", 
     lwd = 2, 
     print.auc = FALSE, 
     legacy.axes = TRUE)
legend("bottomright", 
       legend = paste("AUC =", round(auc_value_cv, 3)), 
       col = "blue", 
       lwd = 2)
dev.off()

# Print AUC for Cross-Validation
cat("AUC for Cross-Validation (10-Fold):", auc_value_cv, "\n")

# Part 2: Test Set Evaluation
prob_predictions <- predict(dt_model_weighted, test_data_final, type = "prob")
threshold <- 0.6
predictions_adjusted <- factor(ifelse(prob_predictions[, "APO"] > threshold, "APO", "NonAPO"), 
                               levels = c("NonAPO", "APO"))

test_data_final <- test_data_final %>%
  mutate(apo_general = factor(apo_general, levels = c("NonAPO", "APO")))

conf_matrix_weighted <- confusionMatrix(predictions_adjusted, 
                                        test_data_final$apo_general, 
                                        positive = "APO",
                                        mode = "everything")
print("Test Set Confusion Matrix and Performance Metrics (Unweighted, Threshold = 0.6):")
print(conf_matrix_weighted)

accuracy <- conf_matrix_weighted$overall["Accuracy"]
precision <- conf_matrix_weighted$byClass["Pos Pred Value"]
recall <- conf_matrix_weighted$byClass["Sensitivity"]
specificity <- conf_matrix_weighted$byClass["Specificity"]
f1_score <- conf_matrix_weighted$byClass["F1"]

cat("Test Set Metrics (Threshold = 0.6):\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("F1-Score:", f1_score, "\n")

# Generate ROC Curve for Test Set
roc_obj_test <- roc(test_data_final$apo_general, prob_predictions[, "APO"], levels = c("NonAPO", "APO"), direction = "<")

# Calculate AUC for Test Set
auc_value_test <- auc(roc_obj_test)

# Save ROC Curve Plot for Test Set to PNG
png("roc_curve_test.png", width = 800, height = 600, res = 100)
plot(roc_obj_test, 
     main = paste("ROC Curve for Test Set\nAUC =", round(auc_value_test, 3)),
     col = "red", 
     lwd = 2, 
     print.auc = FALSE, 
     legacy.axes = TRUE)
legend("bottomright", 
       legend = paste("AUC =", round(auc_value_test, 3)), 
       col = "red", 
       lwd = 2)
dev.off()

# Print AUC for Test Set
cat("AUC for Test Set:", auc_value_test, "\n")