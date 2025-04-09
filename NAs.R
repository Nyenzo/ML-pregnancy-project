# Check for NAs
print("Missing values in subset:")
colSums(is.na(df_subset))

# Impute or remove NAs
df_subset$age[is.na(df_subset$age)] <- median(df_subset$age, na.rm = TRUE)
df_subset$total_children_ever_born[is.na(df_subset$total_children_ever_born)] <- 0  # 0 if never pregnant
df_subset$highest_education_level[is.na(df_subset$highest_education_level)] <- 1  # Mode = Primary
df_subset$preceeding_birth_interval_01[is.na(df_subset$preceeding_birth_interval_01)] <- median(df_subset$preceeding_birth_interval_01, na.rm = TRUE)
df_subset$any_abuse_during_pregnancy[is.na(df_subset$any_abuse_during_pregnancy)] <- 0

# Remove rows with NA in outcome
df_subset <- df_subset[!is.na(df_subset$outcome), ]

# Confirm no NAs remain
print("Missing values after cleaning:")
colSums(is.na(df_subset))