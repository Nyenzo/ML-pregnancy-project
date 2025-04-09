# Check if train_data_smote contains dummy variables (from SMOTE) or original predictors (from upSample)
if (all(predictors %in% colnames(train_data_smote))) {
  # upSample was used, no dummy variables
  train_data_final <- train_data_smote
} else {
  # SMOTE was used, contains dummy variables
  train_data_final <- train_data_smote
}

# Feature Selection with Recursive Feature Elimination (RFE)
set.seed(123)
rfe_control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
# Use all columns except the target (apo_general)
rfe_predictors <- setdiff(colnames(train_data_final), "apo_general")
rfe_result <- rfe(train_data_final[, rfe_predictors], 
                  train_data_final$apo_general, 
                  sizes = c(5, 10, 15), 
                  rfeControl = rfe_control)

# Selected Predictors
print("Selected Predictors from RFE:")
print(predictors(rfe_result))

# Define Formula with Selected Predictors
selected_predictors <- predictors(rfe_result)
formula <- as.formula(paste("apo_general ~", paste(selected_predictors, collapse = "+")))

# Hyperparameter Tuning with caret
set.seed(123)
cv_control <- trainControl(method = "cv", number = 10)
tune_grid <- expand.grid(cp = seq(0.001, 0.1, by = 0.001))
dt_tuned <- train(formula, 
                  data = train_data_final, 
                  method = "rpart", 
                  trControl = cv_control, 
                  tuneGrid = tune_grid)

# Best Model Parameters
print("Best Model Parameters from Tuning:")
print(dt_tuned$bestTune)

# Build Final Decision Tree with Selected Predictors and Tuned Parameters
dt_model_final <- rpart(formula, 
                        data = train_data_final, 
                        method = "class", 
                        control = rpart.control(cp = dt_tuned$bestTune$cp))

# Visualize the Final Decision Tree
rpart.plot(dt_model_final, main = "Final Decision Tree for Predicting Adverse Pregnancy Outcomes")