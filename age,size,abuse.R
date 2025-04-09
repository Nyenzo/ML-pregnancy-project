# Load Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(haven)

# Load Data
df <- read.csv("data_safi.csv")

# Define Outcome
df$apo_general <- factor(df$apo_general, levels = c("NonAPO", "APO"))

# Convert cesarean_birth_ columns to numeric using zap_labels()
df <- df %>%
  mutate(
    across(starts_with("cesarean_birth_"), ~ haven::zap_labels(.))
  )

# Recode timing_first_antenatal_check_ columns: values > 9 months to NA
df <- df %>%
  mutate(
    across(starts_with("timing_first_antenatal_check_"), ~ ifelse(. > 9, NA, .))
  )

# Recode num_antenatal_visits_ columns: values > 12 visits to NA
df <- df %>%
  mutate(
    across(starts_with("num_antenatal_visits_"), ~ ifelse(. > 12, NA, .))
  )

# Recode tetanus_during_pregnancy_ columns: values > 5 to NA
df <- df %>%
  mutate(
    across(starts_with("tetanus_during_pregnancy_"), ~ ifelse(. > 5, NA, .))
  )

# Recode tetanus_before_birth_ columns: values > 5 to NA
df <- df %>%
  mutate(
    across(starts_with("tetanus_before_birth_"), ~ ifelse(. > 5, NA, .))
  )

# Recode succeeding_birth_interval_ columns: values > 120 months or < 0 to NA
df <- df %>%
  mutate(
    across(starts_with("succeeding_birth_interval_"), ~ ifelse(. > 120 | . < 0, NA, .))
  )

# Recode age_at_first_birth: values < 10 or > 50 to NA (assuming realistic range)
df <- df %>%
  mutate(
    age_at_first_birth = ifelse(age_at_first_birth < 10 | age_at_first_birth > 50, NA, age_at_first_birth)
  )

# Categorize age_at_first_birth into three groups: <19, 19-35, >35
df <- df %>%
  mutate(
    age_at_first_birth_cat = case_when(
      age_at_first_birth < 19 ~ "Below 19",
      age_at_first_birth >= 19 & age_at_first_birth <= 35 ~ "19-35",
      age_at_first_birth > 35 ~ "Above 35",
      TRUE ~ NA_character_
    ),
    age_at_first_birth_cat = factor(age_at_first_birth_cat, levels = c("Below 19", "19-35", "Above 35"))
  )

# Recode abused_during_pregnancy_ columns: assuming binary (0 = No, 1 = Yes), compute proportion of pregnancies with abuse
if ("abused_during_pregnancy" %in% colnames(df)) {
  df$abused_during_pregnancy <- factor(df$abused_during_pregnancy, levels = c(0, 1), labels = c("No", "Yes"))
} else {
  df <- df %>%
    mutate(
      across(starts_with("abused_during_pregnancy_"), ~ ifelse(. %in% c(0, 1), ., NA)),
      prop_abused_during_pregnancy = rowMeans(select(., starts_with("abused_during_pregnancy_")) == 1, na.rm = TRUE)
    )
}

# Recode size_of_child_ columns: values outside 1-5 to NA, then compute average
df <- df %>%
  mutate(
    across(starts_with("size_of_child_"), ~ ifelse(. %in% 1:5, ., NA))
  )

# Compute avg_size_of_child, ensuring NA for rows with all NA values
df <- df %>%
  mutate(
    avg_size_of_child = rowMeans(select(., starts_with("size_of_child_")), na.rm = TRUE),
    avg_size_of_child = ifelse(rowSums(is.na(select(., starts_with("size_of_child_")))) == length(select(., starts_with("size_of_child_"))), NA, avg_size_of_child)
  )

# Ensure wealth_index is a factor
df$wealth_index <- factor(df$wealth_index, levels = c(1, 2, 3, 4, 5), 
                          labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# Ensure highest_education_level is a factor
df$highest_education_level <- factor(df$highest_education_level, 
                                     levels = c(0, 1, 2, 3), 
                                     labels = c("No Education", "Primary", "Secondary", "Higher"))

# Descriptive Statistics for avg_size_of_child (re-run after fixing)
print("Summary statistics for avg_size_of_child (after fixing):")
print(summary(df$avg_size_of_child))

# Mean of avg_size_of_child by apo_general (re-run after fixing)
print("Mean of avg_size_of_child by apo_general (after fixing):")
numerical_means_size <- df %>%
  group_by(apo_general) %>%
  summarise(mean_size = mean(avg_size_of_child, na.rm = TRUE))
print(numerical_means_size)

# Visualization: Box plot of avg_size_of_child by apo_general (re-run after fixing)
ggplot(df, aes(x = apo_general, y = avg_size_of_child, fill = apo_general)) +
  geom_boxplot() +
  labs(title = "Average Size of Child by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Average Size of Child (1 = Very Small, 5 = Very Large)") +
  coord_cartesian(ylim = c(1, 5)) +
  theme_minimal()