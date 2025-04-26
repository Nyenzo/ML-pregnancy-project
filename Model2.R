# Load required packages for modeling
library(rpart)
library(rpart.plot)

# Define Formula with Top Predictors
formula <- as.formula(paste("apo_general ~", paste(predictors_top, collapse = "+")))

# Build a pruned decision tree with a higher cp value
dt_model_weighted_pruned <- rpart(formula, 
                                  data = train_data_final, 
                                  method = "class", 
                                  control = rpart.control(cp = 0.01),  # Increase cp to prune more
                                  weights = train_data_final$sample_weight)

# Save the pruned plot to a PNG file
png("decision_tree_pruned.png", width = 1200, height = 800, res = 100)

# Plot the pruned decision tree
rpart.plot(dt_model_weighted_pruned, 
           main = "Pruned Weighted Decision Tree for Predicting Adverse Pregnancy Outcomes",
           extra = 104, 
           fallen.leaves = FALSE, 
           tweak = 1.2, 
           box.palette = "Blues", 
           shadow.col = "gray", 
           cex = 0.8, 
           round = 0, 
           type = 4, 
           clip.right.labs = FALSE, 
           under = TRUE)

# Close the plotting device
dev.off()