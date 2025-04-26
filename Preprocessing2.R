# Load required packages for preprocessing
library(dplyr)
library(caret)

# Step 1: Calculate the missing columns in df_2
# 1. age_at_first_birth_cat: Categorize age at first birth
df_2 <- df_2 %>%
  mutate(age_at_first_birth_cat = case_when(
    age_at_first_birth < 18 ~ "<18",
    age_at_first_birth >= 18 & age_at_first_birth <= 24 ~ "18-24",
    age_at_first_birth > 24 ~ "25+",
    TRUE ~ NA_character_
  )) %>%
  mutate(age_at_first_birth_cat = as.factor(age_at_first_birth_cat))

# 2. avg_size_of_child: Average size of child at birth across births
size_cols <- paste0("size_of_child_", 1:6)
df_2 <- df_2 %>%
  mutate(across(all_of(size_cols), as.numeric)) %>%  # Ensure numeric for averaging
  mutate(avg_size_of_child = rowMeans(select(., all_of(size_cols)), na.rm = TRUE))

# 3. contraceptive_category: Categorize contraceptive use
df_2 <- df_2 %>%
  mutate(contraceptive_category = case_when(
    current_contraceptive_method == 0 ~ "none",
    current_contraceptive_method %in% c(1:3, 5:11) ~ "modern",
    current_contraceptive_method %in% c(4, 12:13) ~ "traditional",
    TRUE ~ "none"
  )) %>%
  mutate(contraceptive_category = as.factor(contraceptive_category))

# Debug: Verify the new columns
print("Column names in df_2 after adding calculated columns:")
print(colnames(df_2))
print("Sample of calculated columns:")
print(head(df_2[, c("age_at_first_birth_cat", "avg_size_of_child", "contraceptive_category")]))

# Step 2: Split df_2 into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(df_2), 0.7 * nrow(df_2))
train_data <- df_2[train_indices, ]
test_data <- df_2[-train_indices, ]

# Ensure apo_general is a factor with consistent levels in both train_data and test_data
train_data <- train_data %>%
  mutate(apo_general = factor(apo_general, levels = c("NonAPO", "APO")))

test_data <- test_data %>%
  mutate(apo_general = factor(apo_general, levels = c("NonAPO", "APO")))

# Debug: Check levels of apo_general
print("Levels of apo_general in train_data:")
print(levels(train_data$apo_general))
print("Levels of apo_general in test_data:")
print(levels(test_data$apo_general))

# Step 3: Define predictors
predictors <- c("avg_timing_first_antenatal_check", "avg_num_antenatal_visits", 
                "prop_cesarean", "place_of_residence", "wealth_index", 
                "highest_education_level", "avg_tetanus_during_pregnancy", 
                "avg_succeeding_birth_interval", "prop_short_succeeding_interval", 
                "avg_tetanus_before_birth", "age_at_first_birth_cat", 
                "avg_size_of_child", "total_pregnancies", "contraceptive_category")

# Debug: Check which predictors are missing in train_data
missing_predictors <- predictors[!predictors %in% colnames(train_data)]
if (length(missing_predictors) > 0) {
  print("The following predictors are missing in train_data:")
  print(missing_predictors)
} else {
  print("All predictors are present in train_data.")
}

# Step 4: Subset and preprocess train_data
train_data_subset <- train_data[, c(predictors, "apo_general", "caseid", "sample_weight"), drop = FALSE]

# Impute missing values (median for numeric, mode for categorical)
numeric_vars <- predictors[sapply(train_data_subset[, predictors], is.numeric)]
categorical_vars <- predictors[sapply(train_data_subset[, predictors], is.factor)]

# Median imputation for numeric variables
for (var in numeric_vars) {
  train_data_subset[[var]][is.na(train_data_subset[[var]])] <- median(train_data_subset[[var]], na.rm = TRUE)
}

# Mode imputation for categorical variables
get_mode <- function(v) {
  uniq_v <- unique(v[!is.na(v)])
  uniq_v[which.max(tabulate(match(v, uniq_v)))]
}
for (var in categorical_vars) {
  train_data_subset[[var]][is.na(train_data_subset[[var]])] <- get_mode(train_data_subset[[var]])
}

# Ensure categorical variables are factors
train_data_subset <- train_data_subset %>%
  mutate(across(all_of(categorical_vars), as.factor),
         apo_general = factor(apo_general, levels = c("NonAPO", "APO")))

# Step 5: Class Balancing with caret::upSample
# Include caseid in the predictors temporarily to retain it after up-sampling
predictors_with_caseid <- c(predictors, "caseid")
set.seed(123)
train_data_balanced <- upSample(x = train_data_subset[, predictors_with_caseid], 
                                y = train_data_subset$apo_general, 
                                yname = "apo_general")

# Add sample_weight back to the balanced dataset
train_data_balanced <- train_data_balanced %>%
  left_join(select(train_data_subset, caseid, sample_weight), by = "caseid") %>%
  mutate(sample_weight = ifelse(is.na(sample_weight), 1, sample_weight))

# Ensure apo_general in train_data_balanced has the correct levels
train_data_balanced <- train_data_balanced %>%
  mutate(apo_general = factor(apo_general, levels = c("NonAPO", "APO")))

print("Class Distribution After Balancing:")
print(table(train_data_balanced$apo_general))

# Step 6: Feature Selection (Use Top 8 Features)
predictors_top <- c("total_pregnancies", "prop_short_succeeding_interval", "avg_succeeding_birth_interval", 
                    "avg_num_antenatal_visits", "age_at_first_birth_cat", "wealth_index", 
                    "avg_tetanus_during_pregnancy", "highest_education_level")

# Debug: Check if predictors_top are in train_data_balanced
missing_predictors_top <- predictors_top[!predictors_top %in% colnames(train_data_balanced)]
if (length(missing_predictors_top) > 0) {
  print("The following predictors_top are missing in train_data_balanced:")
  print(missing_predictors_top)
} else {
  print("All predictors_top are present in train_data_balanced.")
}

train_data_final <- train_data_balanced[, c(predictors_top, "apo_general", "sample_weight"), drop = FALSE]
test_data_final <- test_data[, c(predictors_top, "apo_general", "sample_weight"), drop = FALSE]

# Verify the preprocessed data
print("Dimensions of train_data_final:")
print(dim(train_data_final))
print("Dimensions of test_data_final:")
print(dim(test_data_final))
print("Summary of sample weights in train_data_final:")
print(summary(train_data_final$sample_weight))