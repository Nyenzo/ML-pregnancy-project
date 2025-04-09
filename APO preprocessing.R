# Load Libraries
library(smotefamily)
library(rpart)
library(rpart.plot)
library(tidyr)
library(dplyr)
library(caret)

# Load Data
df <- read.csv("data_safi.csv")
print("Dataset dimensions:")
dim(df)
print("Column names:")
names(df)

# Define Outcome Columns
outcome_cols <- paste0("pregnancy_outcome_reclassified_", sprintf("%02d", 1:10))

# Define apo_general
df$apo_general <- apply(df[, outcome_cols], 1, 
                        function(x) ifelse(any(x %in% 2:4, na.rm = TRUE), 1, 0))
df$apo_general <- as.factor(df$apo_general)
print("General APO distribution (full data):")
table(df$apo_general)

# Select Predictors
predictors_general <- c("age_of_respondent_at_first_birth", "wealth_index", 
                        "smokes_nothing", "noone_ever_abused_the_respondent_when_pregnant",
                        "anything_to_delay_or_avoid_pregnancy", "current_contraceptive_method",
                        "total_pregnancies")
df_general <- df[, c("apo_general", predictors_general)]

# Impute -1s
print("Checking -1 counts:")
sapply(df_general, function(x) sum(x == -1, na.rm = TRUE))
for (col in predictors_general) {
  if (any(df_general[[col]] == -1, na.rm = TRUE)) {
    median_val <- median(df_general[[col]][df_general[[col]] != -1], na.rm = TRUE)
    df_general[[col]][df_general[[col]] == -1] <- median_val
  }
}

# Ensure Numeric Predictors
df_general[, predictors_general] <- lapply(df_general[, predictors_general], as.numeric)
df_general$apo_general <- as.factor(df_general$apo_general)
print("Predictor types:")
sapply(df_general[, predictors_general], class)
print("Target levels:")
levels(df_general$apo_general)

# Split Data
set.seed(123)
trainIndex <- sample(1:nrow(df_general), 0.7 * nrow(df_general))
train_data <- df_general[trainIndex, ]
test_data <- df_general[-trainIndex, ]
print("Pre-SMOTE train dimensions:")
dim(train_data)
print("Pre-SMOTE train APO distribution:")
table(train_data$apo_general)
print("Pre-SMOTE train predictor summary:")
summary(train_data[, predictors_general])

# Apply SMOTE with Debugging
# Before SMOTE
train_data_numeric <- train_data
train_data_numeric$apo_general <- as.numeric(train_data$apo_general) - 1  # 0,1 instead of 1,2
smote_train <- SMOTE(X = train_data_numeric[, predictors_general], 
                     target = train_data_numeric$apo_general, K = 5, dup_size = 0.7)
df_balanced_train <- smote_train$data
colnames(df_balanced_train)[ncol(df_balanced_train)] <- "apo_general"
df_balanced_train$apo_general <- factor(df_balanced_train$apo_general, 
                                        levels = c(0, 1), labels = c("0", "1"))
print("Post-SMOTE train dimensions:")
dim(df_balanced_train)
print("Post-SMOTE train APO distribution:")
table(df_balanced_train$apo_general)
write.csv(df_balanced_train, "kdhs_balanced_apo_general.csv", row.names = FALSE)