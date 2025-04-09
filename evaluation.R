# Prepare Test Data to Match Training Data
if (all(predictors %in% colnames(train_data_final))) {
  # upSample was used, no dummy variables
  test_data_final <- test_data[, c(predictors, "apo_general"), drop = FALSE]
} else {
  # SMOTE was used, contains dummy variables
  test_X <- test_data[, predictors, drop = FALSE]
  # Ensure categorical variables are factors
  test_X <- test_X %>%
    mutate(
      place_of_residence = as.factor(place_of_residence),
      wealth_index = as.factor(wealth_index),
      highest_education_level = as.factor(highest_education_level),
      age_at_first_birth_cat = as.factor(age_at_first_birth_cat),
      contraceptive_category = as.factor(contraceptive_category)
    )
  test_X <- model.matrix(~ . - 1, data = test_X)  # Convert to dummy variables
  # Ensure test_X has the same columns as train_data_final (excluding apo_general)
  missing_cols <- setdiff(colnames(train_data_final)[colnames(train_data_final) != "apo_general"], colnames(test_X))
  if (length(missing_cols) > 0) {
    # Add missing columns with zeros
    for (col in missing_cols) {
      test_X <- cbind(test_X, matrix(0, nrow = nrow(test_X), ncol = 1, dimnames = list(NULL, col)))
    }
  }
  # Reorder columns to match train_data_final
  test_X <- test_X[, colnames(train_data_final)[colnames(train_data_final) != "apo_general"]]
  test_data_final <- as.data.frame(test_X)
  test_data_final$apo_general <- test_data$apo_general
}

# Make Predictions on Test Data
predictions_final <- predict(dt_model_final, test_data_final, type = "class")

# Evaluate Model Performance on Test Set
conf_matrix_final <- confusionMatrix(predictions_final, test_data_final$apo_general, positive = "APO")
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

# Extract Feature Importance
importance <- dt_model_final$variable.importance
if (!is.null(importance)) {
  importance_df <- data.frame(Variable = names(importance), Importance = importance)
  importance_df <- importance_df[order(-importance_df$Importance), ]
  print("Feature Importance:")
  print(importance_df)
  
  # Visualize Feature Importance
  ggplot(importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(title = "Feature Importance in Final Decision Tree Model", x = "Predictor", y = "Importance") +
    theme_minimal()
} else {
  print("No feature importance available (tree may be too simple).")
}

# Perform 10-Fold Cross-Validation on Full Dataset with Selected Predictors
# Prepare full dataset for cross-validation
if (all(predictors %in% colnames(train_data_final))) {
  # upSample was used, no dummy variables
  df_final <- df[, c(predictors, "apo_general"), drop = FALSE]
} else {
  # SMOTE was used, contains dummy variables
  df_X <- df[, predictors, drop = FALSE]
  df_X <- df_X %>%
    mutate(
      place_of_residence = as.factor(place_of_residence),
      wealth_index = as.factor(wealth_index),
      highest_education_level = as.factor(highest_education_level),
      age_at_first_birth_cat = as.factor(age_at_first_birth_cat),
      contraceptive_category = as.factor(contraceptive_category)
    )
  df_X <- model.matrix(~ . - 1, data = df_X)
  df_final <- as.data.frame(df_X)
  df_final$apo_general <- df$apo_general
}

set.seed(123)
cv_model <- train(formula, 
                  data = df_final, 
                  method = "rpart", 
                  trControl = cv_control, 
                  tuneGrid = tune_grid)

# Print Cross-Validation Results
print("10-Fold Cross-Validation Results:")
print(cv_model)