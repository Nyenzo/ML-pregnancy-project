# Since we're using caret::upSample, train_data_smote contains the original predictors (not dummy variables)
train_data_final <- train_data_smote

# Define Formula with All Predictors
formula <- as.formula(paste("apo_general ~", paste(predictors, collapse = "+")))

# Build a Simpler Decision Tree with a Higher cp Value for Interpretability
dt_model_simplified <- rpart(formula, 
                             data = train_data_final, 
                             method = "class", 
                             control = rpart.control(cp = 0.01))  # Increased cp to 0.01 for a simpler tree

# Visualize the Simplified Decision Tree
rpart.plot(dt_model_simplified, 
           main = "Simplified Decision Tree for Predicting Adverse Pregnancy Outcomes",
           extra = 104,  # Display the number of observations and predicted class
           fallen.leaves = TRUE,  # Align terminal nodes at the bottom
           cex = 0.8)  # Adjust text size for readability