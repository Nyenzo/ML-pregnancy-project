# Summary statistics for total_pregnancies
print("Summary statistics for total_pregnancies:")
print(summary(df$total_pregnancies))

# Mean of total_pregnancies by apo_general
print("Mean of total_pregnancies by apo_general:")
numerical_means_pregnancies <- df %>%
  group_by(apo_general) %>%
  summarise(mean_pregnancies = mean(total_pregnancies, na.rm = TRUE))
print(numerical_means_pregnancies)

# Visualization: Box plot of total_pregnancies by apo_general
ggplot(df, aes(x = apo_general, y = total_pregnancies, fill = apo_general)) +
  geom_boxplot() +
  labs(title = "Total Pregnancies by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Total Number of Pregnancies") +
  theme_minimal()
# Recode current_contraceptive_method based on DHS numeric coding
df <- df %>%
  mutate(contraceptive_category = case_when(
    current_contraceptive_method == 0 ~ "None",
    current_contraceptive_method %in% c(1, 2, 3, 5, 6, 7, 11, 14, 16, 17, 18) ~ "Modern",
    current_contraceptive_method %in% c(8, 9, 10, 13) ~ "Traditional",
    TRUE ~ "Other"
  ),
  contraceptive_category = factor(contraceptive_category, 
                                  levels = c("None", "Modern", "Traditional", "Other")))

# Proportion of contraceptive_category by apo_general
print("Proportion of contraceptive_category by apo_general (after recoding):")
contraceptive_props <- df %>%
  group_by(apo_general, contraceptive_category) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(apo_general) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  select(apo_general, contraceptive_category, proportion)
print(contraceptive_props)

# Visualization: Bar plot of contraceptive_category by apo_general
ggplot(df, aes(x = apo_general, fill = contraceptive_category)) +
  geom_bar(position = "fill") +
  labs(title = "Contraceptive Use by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Proportion", fill = "Contraceptive Category") +
  theme_minimal()

# Proportion of respondent_circumcised by apo_general
print("Proportion of respondent_circumcised by apo_general:")
circumcision_props <- df %>%
  group_by(apo_general, respondent_circumcised) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(apo_general) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  select(apo_general, respondent_circumcised, proportion)
print(circumcision_props)

# Visualization: Bar plot of respondent_circumcised by apo_general
ggplot(df, aes(x = apo_general, fill = respondent_circumcised)) +
  geom_bar(position = "fill") +
  labs(title = "Female Circumcision by Pregnancy Outcome", 
       x = "Pregnancy Outcome", y = "Proportion", fill = "Circumcised") +
  theme_minimal()