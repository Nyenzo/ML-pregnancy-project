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

# Descriptive Statistics for avg_num_antenatal_visits
print("Summary statistics for avg_num_antenatal_visits (after recoding):")
print(summary(df$avg_num_antenatal_visits))

# Mean of avg_num_antenatal_visits by apo_general
print("Mean of avg_num_antenatal_visits by apo_general (after recoding):")
numerical_means <- df %>%
  group_by(apo_general) %>%
  summarise(mean_visits = mean(avg_num_antenatal_visits, na.rm = TRUE))
print(numerical_means)

# Improved Visualization: Box plot with adjusted y-axis
ggplot(df, aes(x = apo_general, y = avg_num_antenatal_visits, fill = apo_general)) +
  geom_boxplot() +
  labs(title = "Average Number of Antenatal Visits by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Average Number of Antenatal Visits") +
  coord_cartesian(ylim = c(0, 10)) +  # Zoom in on 0 to 10 visits
  theme_minimal()