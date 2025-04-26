# Prepare Test Data to Match Training Data
test_data_final <- test_data[, c(predictors, "apo_general"), drop = FALSE]

# Make Predictions on Test Data (Probabilities and Class)
prob_predictions <- predict(dt_model_simplified, test_data_final, type = "prob")
class_predictions <- predict(dt_model_simplified, test_data_final, type = "class")

# Evaluate Model Performance on Test Set (Confusion Matrix)
conf_matrix_final <- confusionMatrix(class_predictions, test_data_final$apo_general, positive = "APO")
print("Confusion Matrix and Performance Metrics (Final Model):")
print(conf_matrix_final)

# Extract Key Metrics
accuracy <- conf_matrix_final$overall["Accuracy"]
precision <- conf_matrix_final$byClass["Pos Pred Value"]
recall <- conf_matrix_final$byClass["Sensitivity"]
specificity <- conf_matrix_final$byClass["Specificity"]
f1_score <- conf_matrix_final$byClass["F1"]

# Print Metrics
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")
cat("Specificity:", specificity, "\n")
cat("F1-Score:", f1_score, "\n")

# Generate ROC Curve
roc_obj <- roc(test_data_final$apo_general, prob_predictions[, "APO"], levels = c("NonAPO", "APO"), direction = "<")

# Calculate AUC
auc_value <- auc(roc_obj)

# Save ROC Curve Plot to PNG
png("roc_curve_model1.png", width = 800, height = 600, res = 100)
plot(roc_obj, 
     main = paste("ROC Curve for Model 1 (Unweighted)\nAUC =", round(auc_value, 3)),
     col = "blue", 
     lwd = 2, 
     print.auc = FALSE, 
     legacy.axes = TRUE)
legend("bottomright", 
       legend = paste("AUC =", round(auc_value, 3)), 
       col = "blue", 
       lwd = 2)
dev.off()

# Print AUC
cat("AUC for Model 1 (Unweighted):", auc_value, "\n")