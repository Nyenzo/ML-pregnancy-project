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

# Inspect max values of timing_first_antenatal_check_ columns before recoding
print("Max values of timing_first_antenatal_check_ columns before recoding:")
timing_cols <- select(df, starts_with("timing_first_antenatal_check_"))
print(sapply(timing_cols, max, na.rm = TRUE))

# Recode timing_first_antenatal_check_ columns: values > 9 months to NA
df <- df %>%
  mutate(
    across(starts_with("timing_first_antenatal_check_"), ~ ifelse(. > 9, NA, .))
  )

# Inspect max values after recoding
print("Max values of timing_first_antenatal_check_ columns after recoding:")
timing_cols <- select(df, starts_with("timing_first_antenatal_check_"))
print(sapply(timing_cols, max, na.rm = TRUE))

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

# Descriptive Statistics for avg_timing_first_antenatal_check
print("Summary statistics for avg_timing_first_antenatal_check (after recoding):")
print(summary(df$avg_timing_first_antenatal_check))

# Mean of avg_timing_first_antenatal_check by apo_general
print("Mean of avg_timing_first_antenatal_check by apo_general (after recoding):")
numerical_means <- df %>%
  group_by(apo_general) %>%
  summarise(mean_timing = mean(avg_timing_first_antenatal_check, na.rm = TRUE))
print(numerical_means)

# Improved Visualization: Box plot with adjusted y-axis
ggplot(df, aes(x = apo_general, y = avg_timing_first_antenatal_check, fill = apo_general)) +
  geom_boxplot() +
  labs(title = "Average Timing of First Antenatal Check by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Average Timing of First Antenatal Check (Months)") +
  coord_cartesian(ylim = c(0, 10)) +
  theme_minimal()