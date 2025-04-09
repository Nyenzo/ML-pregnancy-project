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

# Descriptive Statistics for wealth_index
# Frequency table for wealth_index
print("Frequency table for wealth_index:")
print(table(df$wealth_index))
print("Proportions for wealth_index:")
print(prop.table(table(df$wealth_index)))

# Cross-tabulation of wealth_index by apo_general
print("Cross-tabulation of wealth_index by apo_general:")
print(table(df$wealth_index, df$apo_general))
print("Proportions of wealth_index by apo_general:")
print(prop.table(table(df$wealth_index, df$apo_general), margin = 1))

# Visualization: Bar plot of wealth_index by apo_general
ggplot(df, aes(x = wealth_index, fill = apo_general)) +
  geom_bar(position = "fill") +
  labs(title = "Wealth Index by Pregnancy Outcome", 
       x = "Wealth Index", y = "Proportion") +
  theme_minimal()