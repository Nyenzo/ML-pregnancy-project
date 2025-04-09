library(smotefamily)
library(haven)

# Load data (adjust path)
df <- read.csv("friday_work.csv")

# Define outcome
df$outcome <- ifelse(df$pregnancy_outcome_reclassified_01 == 1, 0, 1, NA)
df$outcome <- as.factor(df$outcome)

# Select predictors
predictors <- c("age", "total_children_ever_born", "highest_education_level", 
                "preceeding_birth_interval_01", "any_abuse_during_pregnancy")
df_subset <- df[, c("outcome", predictors)]

# Handle missing data
df_subset$age[is.na(df_subset$age)] <- median(df_subset$age, na.rm = TRUE)
df_subset$highest_education_level[is.na(df_subset$highest_education_level)] <- 1  # Mode = Primary
df_subset$preceeding_birth_interval_01[is.na(df_subset$preceeding_birth_interval_01)] <- median(df_subset$preceeding_birth_interval_01, na.rm = TRUE)
df_subset$any_abuse_during_pregnancy[is.na(df_subset$any_abuse_during_pregnancy)] <- 0
df_subset <- df_subset[!is.na(df_subset$outcome), ]

# Apply SMOTE
smote_result <- SMOTE(X = df_subset[, predictors], 
                      target = df_subset$outcome, 
                      K = 5, 
                      dup_size = 2)
df_balanced <- smote_result$data
colnames(df_balanced)[ncol(df_balanced)] <- "outcome"
df_balanced$outcome <- as.factor(df_balanced$outcome)

# Check balance
table(df_balanced$outcome)
