# Set seed for reproducibility
set.seed(123)

# Number of respondents
n <- 1000

# Create empty dataframe
df <- data.frame(caseid = paste("KE", sprintf("%05d", 1:n), sep = ""))

# 1. age (v012): Respondent's age, 15-49
df$age <- round(runif(n, 15, 49))
# Add a few outliers (e.g., <15 or >49 as errors)
df$age[sample(n, 10)] <- c(12, 10, 55, 60, 13, 11, 56, 9, 62, 14)

# 2. region: 8 regions in Kenya
regions <- c("Coast", "North Eastern", "Eastern", "Central", "Rift Valley", "Western", "Nyanza", "Nairobi")
df$region <- sample(regions, n, replace = TRUE, prob = c(0.12, 0.08, 0.15, 0.14, 0.20, 0.13, 0.13, 0.05))

# 3. place_of_residence (v025): Urban (1) or Rural (2)
df$place_of_residence <- sample(c(1, 2), n, replace = TRUE, prob = c(0.3, 0.7))  # More rural

# 4. highest_education_level (v106): 0 = None, 1 = Primary, 2 = Secondary, 3 = Higher
df$highest_education_level <- sample(0:3, n, replace = TRUE, prob = c(0.2, 0.5, 0.25, 0.05))

# 5-14. duration_of_live_pregnancies_months_01 to _10: Months for up to 10 live births
live_births <- rpois(n, 2.5)  # Average 2.5 births
live_births[live_births > 10] <- 10  # Cap at 10
for (i in 1:10) {
  col_name <- paste0("duration_of_live_pregnancies_months_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(1:n <= live_births, sample(7:10, n, replace = TRUE), NA)
  # Add outliers (e.g., <6 months)
  if (i <= 5) df[[col_name]][sample(which(!is.na(df[[col_name]])), 5)] <- c(4, 5, 3, 2, 6)
}

# 15-16. awfacte, awfactw: All-women factors (weights, simplified as 1-2)
df$awfacte <- runif(n, 1, 2)
df$awfactw <- runif(n, 1, 2)

# 17-21. birth_order_1 to _5: Birth order for live births
for (i in 1:5) {
  col_name <- paste0("birth_order_", i)
  df[[col_name]] <- ifelse(live_births >= i, i, NA)
}

# 22-26. pregnancy_order_1 to _5: Pregnancy order (includes non-live)
total_pregs <- rpois(n, 3)  # Average 3 pregnancies
total_pregs[total_pregs > 5] <- 5
for (i in 1:5) {
  col_name <- paste0("pregnancy_order_", i)
  df[[col_name]] <- ifelse(total_pregs >= i, i, NA)
}

# 27-30. preceeding_birth_interval_01 to _04: Months between births
for (i in 1:4) {
  col_name <- paste0("preceeding_birth_interval_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(live_births > i, sample(12:60, n, replace = TRUE), NA)
  # Add outliers (e.g., <9 or >120)
  if (i <= 2) df[[col_name]][sample(which(!is.na(df[[col_name]])), 3)] <- c(6, 150, 180)
}

# 31-34. succeeding_birth_interval_01 to _04: Months to next birth
for (i in 1:4) {
  col_name <- paste0("succeeding_birth_interval_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(live_births > i + 1, sample(12:60, n, replace = TRUE), NA)
  # Outliers
  if (i <= 2) df[[col_name]][sample(which(!is.na(df[[col_name]])), 2)] <- c(8, 140)
}

# 35-38. other_pregnancies_btween_preg_01 to _04: Non-live between live births (0/1)
for (i in 1:4) {
  col_name <- paste0("other_pregnancies_btween_preg_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(total_pregs > live_births & live_births > i, sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2)), NA)
}

# 39-46. months_since_pregnancy_outcome_01 to _08: Months since last 8 outcomes
for (i in 1:8) {
  col_name <- paste0("months_since_pregnancy_outcome_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(total_pregs >= i, sample(1:60, n, replace = TRUE), NA)
  # Outlier: Very old pregnancies
  if (i <= 2) df[[col_name]][sample(which(!is.na(df[[col_name]])), 2)] <- c(120, 150)
}

# 47. anything_to_delay_or_avoid_pregnancy (v312): 0 = No, 1 = Yes
df$anything_to_delay_or_avoid_pregnancy <- sample(0:1, n, replace = TRUE, prob = c(0.6, 0.4))

# 48. respondent_occupation (v717): 0 = Not working, 1 = Agriculture, 2 = Other
df$respondent_occupation <- sample(0:2, n, replace = TRUE, prob = c(0.3, 0.4, 0.3))

# 49-50. respondent_circumcised, genital_area_swen_closed: 0 = No, 1 = Yes
df$respondent_circumcised <- sample(0:1, n, replace = TRUE, prob = c(0.95, 0.05))
df$genital_area_swen_closed <- sample(0:1, n, replace = TRUE, prob = c(0.98, 0.02))

# 51-52. current_contraceptive_method (v312), year_of_using_the_contraceptive
df$current_contraceptive_method <- sample(c(0, 1, 3, 5), n, replace = TRUE, prob = c(0.6, 0.2, 0.15, 0.05))  # 0 = None, 1 = Pill, 3 = IUD, 5 = Condom
df$year_of_using_the_contraceptive <- ifelse(df$current_contraceptive_method > 0, sample(2015:2022, n, replace = TRUE), NA)

# 53-54. pregnancy_loss, ever_terminated_pregnancy (v228): 0 = No, 1 = Yes
df$pregnancy_loss <- sample(0:1, n, replace = TRUE, prob = c(0.75, 0.25))
df$ever_terminated_pregnancy <- sample(0:1, n, replace = TRUE, prob = c(0.85, 0.15))

# 55-56. month_when_pregnancy_was_terminated, other_pregnancy_terminated_before_the_last_one
df$month_when_pregnancy_was_terminated <- ifelse(df$ever_terminated_pregnancy == 1, sample(1:12, n, replace = TRUE), NA)
df$other_pregnancy_terminated_before_the_last_one <- ifelse(df$ever_terminated_pregnancy == 1, sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)), NA)

# 57-58. age_of_respondent_at_first_birth (v212), total_children_ever_born (v201)
df$age_of_respondent_at_first_birth <- round(runif(n, 14, 30))
df$age_of_respondent_at_first_birth[sample(n, 5)] <- c(12, 13, 11, 10, 31)
df$total_children_ever_born <- live_births

# 59-62. timing_of_first_antenatal_visit (m13), no_of_antenatal_visits_during_pregnancy (m14), delivery_by_cs (m17), size_of_child_at_birth (m18)
df$timing_of_first_antenatal_visit <- sample(c(1:9, NA), n, replace = TRUE, prob = c(rep(0.1, 9), 0.1))
df$no_of_antenatal_visits_during_pregnancy <- sample(c(0:8, NA), n, replace = TRUE, prob = c(0.05, 0.1, 0.15, 0.2, 0.2, 0.15, 0.1, 0.03, 0.01, 0.01))
df$delivery_by_cs <- sample(0:1, n, replace = TRUE, prob = c(0.85, 0.15))
df$size_of_child_at_birth <- sample(c(1:5, NA), n, replace = TRUE, prob = c(0.1, 0.25, 0.3, 0.2, 0.05, 0.1))

# 63. birth_weight_in_kg_3decimals (m19): kg, some NA
df$birth_weight_in_kg_3decimals <- ifelse(is.na(df$size_of_child_at_birth), NA, round(runif(n, 1.5, 4.5), 3))
df$birth_weight_in_kg_3decimals[sample(which(!is.na(df$birth_weight_in_kg_3decimals)), 10)] <- c(0.5, 6.0, 0.8, 5.5, 1.0, 0.4, 6.5, 0.9, 5.8, 1.2)

# 64-66. during_pregnancy_blood_pressure_taken (m42c), foods, breastfeeding: 0 = No, 1 = Yes
df$during_pregnancy_blood_pressure_taken <- sample(0:1, n, replace = TRUE, prob = c(0.3, 0.7))
df$during_pregnancy_foods <- sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6))
df$during_pregnancy_breastfeeding <- sample(0:1, n, replace = TRUE, prob = c(0.2, 0.8))

# 67-76. duration_of_all_pregnancies_months_01 to _10: Includes non-live, 0-10 months
for (i in 1:10) {
  col_name <- paste0("duration_of_all_pregnancies_months_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(total_pregs >= i, sample(0:10, n, replace = TRUE, prob = c(0.1, 0.05, 0.05, 0.05, 0.05, 0.05, 0.1, 0.15, 0.2, 0.15, 0.1)), NA)
}

# 77-83. pregnancy_outcome_reclassified_01 to _07: 1 = Live, 2 = Stillbirth, 3 = Miscarriage
for (i in 1:7) {
  col_name <- paste0("pregnancy_outcome_reclassified_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(total_pregs >= i, sample(1:3, n, replace = TRUE, prob = c(0.8, 0.1, 0.1)), NA)
}

# 84-99. d118a to d118y: Violence indicators (0 = No, 1 = Yes)
violence_vars <- c("d118a", "d118b", "d118c", "d118d", "d118f", "d118g", "d118j", "d118k", "d118l", 
                   "d118o", "d118p", "d118q", "d118v", "d118w", "d118x", "d118y")
for (var in violence_vars) {
  df[[var]] <- sample(0:1, n, replace = TRUE, prob = c(0.9, 0.1))  # 10% prevalence
}

# 100-103. live_birth_count, total_pregnancy_count, non_live_birth_count, has_count_discrepancy
df$live_birth_count <- live_births
df$total_pregnancy_count <- total_pregs
df$non_live_birth_count <- df$total_pregnancy_count - df$live_birth_count
df$has_count_discrepancy <- ifelse(df$non_live_birth_count < 0, 1, 0)  # Should be rare

# 104-115. Pregnancy timing and interval risk flags
df$has_early_loss <- ifelse(rowSums(df[, grepl("duration_of_all_pregnancies", names(df))] <= 3, na.rm = TRUE) > 0, 1, 0)
df$has_mid_term_loss <- ifelse(rowSums(df[, grepl("duration_of_all_pregnancies", names(df))] %in% 4:6, na.rm = TRUE) > 0, 1, 0)
df$has_near_term <- ifelse(rowSums(df[, grepl("duration_of_all_pregnancies", names(df))] %in% 7:8, na.rm = TRUE) > 0, 1, 0)
df$has_full_term <- ifelse(rowSums(df[, grepl("duration_of_all_pregnancies", names(df))] >= 9, na.rm = TRUE) > 0, 1, 0)

df$shortest_preceding <- apply(df[, grepl("preceeding_birth_interval", names(df))], 1, min, na.rm = TRUE)
df$longest_preceding <- apply(df[, grepl("preceeding_birth_interval", names(df))], 1, max, na.rm = TRUE)
df$mean_preceding <- apply(df[, grepl("preceeding_birth_interval", names(df))], 1, mean, na.rm = TRUE)

df$shortest_succeeding <- apply(df[, grepl("succeeding_birth_interval", names(df))], 1, min, na.rm = TRUE)
df$longest_succeeding <- apply(df[, grepl("succeeding_birth_interval", names(df))], 1, max, na.rm = TRUE)
df$mean_succeeding <- apply(df[, grepl("succeeding_birth_interval", names(df))], 1, mean, na.rm = TRUE)

# Replace Infs from NA-only rows
df$shortest_preceding[is.infinite(df$shortest_preceding)] <- NA
df$longest_preceding[is.infinite(df$longest_preceding)] <- NA
df$mean_preceding[is.infinite(df$mean_preceding)] <- NA
df$shortest_succeeding[is.infinite(df$shortest_succeeding)] <- NA
df$longest_succeeding[is.infinite(df$longest_succeeding)] <- NA
df$mean_succeeding[is.infinite(df$mean_succeeding)] <- NA

# 116-120. Risk flags
df$has_high_risk_preceding <- ifelse(df$shortest_preceding < 18, 1, 0)
df$has_high_risk_succeeding <- ifelse(df$shortest_succeeding < 18, 1, 0)
df$has_moderate_risk_preceding <- ifelse(df$shortest_preceding %in% 18:24, 1, 0)
df$has_moderate_risk_succeeding <- ifelse(df$shortest_succeeding %in% 18:24, 1, 0)
df$high_risk_interval_count <- rowSums(df[, c("has_high_risk_preceding", "has_high_risk_succeeding")], na.rm = TRUE)

# 121-124. Scores and completeness
df$birth_interval_risk_score <- df$high_risk_interval_count * 2 + rowSums(df[, c("has_moderate_risk_preceding", "has_moderate_risk_succeeding")], na.rm = TRUE)
df$prenatal_care_complete <- ifelse(!is.na(df$no_of_antenatal_visits_during_pregnancy) & df$no_of_antenatal_visits_during_pregnancy >= 4, 1, 0)
df$birth_data_complete <- ifelse(!is.na(df$birth_weight_in_kg_3decimals) & !is.na(df$size_of_child_at_birth), 1, 0)
df$data_quality_score <- df$prenatal_care_complete + df$birth_data_complete

# 125-129. Additional pregnancy outcomes and months
for (i in 8:10) {
  col_name <- paste0("pregnancy_outcome_reclassified_", sprintf("%02d", i))
  df[[col_name]] <- ifelse(total_pregs >= i, sample(1:3, n, replace = TRUE, prob = c(0.8, 0.1, 0.1)), NA)
}
df$months_since_pregnancy_outcome_08.y <- df$months_since_pregnancy_outcome_08

# 130-132. v233.y, v234.y, v463z: Placeholder (binary)
df$v233.y <- sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3))  # Smoking-related?
df$v234.y <- sample(0:1, n, replace = TRUE, prob = c(0.8, 0.2))  # Alcohol-related?
df$v463z <- sample(0:1, n, replace = TRUE, prob = c(0.9, 0.1))  # Tobacco use

# 133-135. Abuse during pregnancy
df$any_abuse_during_pregnancy <- ifelse(rowSums(df[, violence_vars] == 1, na.rm = TRUE) > 0, 1, 0)
df$abuse_types_count <- rowSums(df[, violence_vars], na.rm = TRUE)
df$abuse_data_quality <- ifelse(rowSums(is.na(df[, violence_vars])) == 0, 1, 0)

# Save to CSV for testing
write.csv(df, "data_safi.csv", row.names = FALSE)

# View summary and first few rows
dim(df)  # Should be 1000 rows, 135 columns
head(df)
