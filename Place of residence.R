# Frequency table for place_of_residence
print("Frequency table for place_of_residence:")
print(table(df$place_of_residence))
print("Proportions for place_of_residence:")
print(prop.table(table(df$place_of_residence)))

# Cross-tabulation of place_of_residence by apo_general
print("Cross-tabulation of place_of_residence by apo_general:")
print(table(df$place_of_residence, df$apo_general))
print("Proportions of place_of_residence by apo_general:")
print(prop.table(table(df$place_of_residence, df$apo_general), margin = 1))

# Bar plot of place_of_residence by apo_general
ggplot(df, aes(x = place_of_residence, fill = apo_general)) +
  geom_bar(position = "fill") +
  labs(title = "Place of Residence by Pregnancy Outcome", 
       x = "Place of Residence", y = "Proportion") +
  theme_minimal()