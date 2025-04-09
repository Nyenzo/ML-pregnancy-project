# Define outcome (APO = 1, Live = 0)
df$outcome <- ifelse(df$pregnancy_outcome_reclassified_01 == 1, 0, 1, NA)
df$outcome <- as.factor(df$outcome)

# Check initial distribution
print("Initial outcome distribution:")
table(df$outcome, useNA = "ifany")

# Select predictors
predictors <- c("age", "total_children_ever_born", "highest_education_level", 
                "preceeding_birth_interval_01", "any_abuse_during_pregnancy")
df_subset <- df[, c("outcome", predictors)]

# Verify subset
print("Subset dimensions:")
dim(df_subset)
print("Subset head:")
head(df_subset)

