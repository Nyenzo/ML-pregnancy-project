🍼 Predicting Adverse Pregnancy Outcomes (APOs) Using KDHS Data
This project builds a machine learning model to predict whether a respondent has ever experienced an Adverse Pregnancy Outcome (APO) using Kenya Demographic and Health Survey (KDHS) data. APOs include miscarriages, stillbirths, and other non-live birth outcomes.

📂 Dataset
Source: Cleaned data from KDHS survey (data_safi.csv)

Target Variable: apo_general

0 = No APO (only live births)

1 = At least one APO (includes outcomes coded as 2, 3, or 4 in pregnancy_outcome_reclassified)

🧠 Objective
To train a decision tree model that can identify women at risk of experiencing an APO using interpretable demographic and health-related predictors.

🔍 Predictors Used
age_of_respondent_at_first_birth

wealth_index

smokes_nothing

noone_ever_abused_the_respondent_when_pregnant

anything_to_delay_or_avoid_pregnancy

current_contraceptive_method

total_pregnancies

ever_terminated_pregnancy

succeeding_birth_interval_02

timing_of_first_antenatal_visit

no_of_antenatal_visits_during_pregnancy

delivery_by_cs

size_of_child_at_birth

place_of_residence

🔧 Modeling Approach
Model: Decision Tree (rpart in R)

Resampling: 10-fold Cross-Validation

Class Imbalance Handling: caret::upSample used to balance APO and non-APO cases in training data

Tuning Parameter: Complexity Parameter (cp), tuned using a grid from 0.001 to 0.1

📊 Model Performance
✅ Final Confusion Matrix
Predicted Non-APO	Predicted APO
Actual Non-APO	4244	108
Actual APO	1572	1070
🔍 Key Metrics
Metric	Value
Accuracy	0.7598
Sensitivity (Recall)	0.9083
Specificity	0.7297
Precision (PPV)	0.4050
Negative Predictive	0.9752
Balanced Accuracy	0.8190
Kappa	0.4266
🔬 Tuning Results
The best model had a complexity parameter (cp) of 0.001 with the highest accuracy of 87.4% during cross-validation.

🚀 Future Directions
Apply SMOTE or other resampling techniques for improved minority class learning

Explore ensemble models (Random Forest, XGBoost)

Perform feature importance analysis

Use SHAP values or LIME for interpretability

🛠️ Tools & Libraries
Language: R

Key Packages: rpart, rpart.plot, caret, dplyr, ROSE, DMwR, ggplot2
