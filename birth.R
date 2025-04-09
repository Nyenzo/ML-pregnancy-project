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

# Recode succeeding_birth_interval_ columns: values > 120 months or < 0 to NA
df <- df %>%
  mutate(
    across(starts_with("succeeding_birth_interval_"), ~ ifelse(. > 120 | . < 0, NA, .))
  )

# Compute aggregated variables
df <- df %>%
  mutate(
    avg_succeeding_birth_interval = rowMeans(select(., starts_with("succeeding_birth_interval_")), na.rm = TRUE),
    prop_short_succeeding_interval = rowMeans(select(., starts_with("succeeding_birth_interval_")) < 24, na.rm = TRUE),
    avg_timing_first_antenatal_check = rowMeans(select(., starts_with("timing_first_antenatal_check_")), na.rm = TRUE),
    avg_num_antenatal_visits = rowMeans(select(., starts_with("num_antenatal_visits_")), na.rm = TRUE),
    avg_tetanus_before_birth = rowMeans(select(., starts_with("tetanus_before_birth_")), na.rm = TRUE),
    avg_tetanus_during_pregnancy = rowMeans(select(., starts_with("tetanus_during_pregnancy_")), na.rm = TRUE),
    prop_cesarean = rowMeans(select(., starts_with("cesarean_birth_")) == 1, na.rm = TRUE)
  )

# Ensure wealth_index is a factor
df$wealth_index <- factor(df$wealth_index, levels = c(1, 2, 3, 4, 5), 
                          labels = c("Poorest", "Poorer", "Middle", "Richer", "Richest"))

# Ensure highest_education_level is a factor
df$highest_education_level <- factor(df$highest_education_level, 
                                     levels = c(0, 1, 2, 3), 
                                     labels = c("No Education", "Primary", "Secondary", "Higher"))

# Descriptive Statistics for avg_succeeding_birth_interval
print("Summary statistics for avg_succeeding_birth_interval (after recoding):")
print(summary(df$avg_succeeding_birth_interval))

# Mean of avg_succeeding_birth_interval by apo_general
print("Mean of avg_succeeding_birth_interval by apo_general (after recoding):")
numerical_means <- df %>%
  group_by(apo_general) %>%
  summarise(mean_interval = mean(avg_succeeding_birth_interval, na.rm = TRUE))
print(numerical_means)

# Improved Visualization: Box plot of avg_succeeding_birth_interval by apo_general
ggplot(df, aes(x = apo_general, y = avg_succeeding_birth_interval, fill = apo_general)) +
  geom_boxplot() +
  labs(title = "Average Succeeding Birth Interval by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Average Succeeding Birth Interval (Months)") +
  theme_minimal()

# Descriptive Statistics for prop_short_succeeding_interval (recompute for consistency)
print("Summary statistics for prop_short_succeeding_interval (after recoding):")
print(summary(df$prop_short_succeeding_interval))

# Mean of prop_short_succeeding_interval by apo_general
print("Mean of prop_short_succeeding_interval by apo_general (after recoding):")
numerical_means <- df %>%
  group_by(apo_general) %>%
  summarise(mean_prop_short = mean(prop_short_succeeding_interval, na.rm = TRUE))
print(numerical_means)

# Visualization: Box plot of prop_short_succeeding_interval by apo_general
ggplot(df, aes(x = apo_general, y = prop_short_succeeding_interval, fill = apo_general)) +
  geom_boxplot() +
  labs(title = "Proportion of Short Succeeding Birth Intervals by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Proportion of Birth Intervals < 24 Months") +
  theme_minimal()