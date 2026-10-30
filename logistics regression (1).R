# ============================================
# TASK 2: IMPROVED LOGISTIC REGRESSION MODEL
# ============================================

install.packages("ROCR")

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(caret)
library(pROC)
library(ROCR)

# Read the data
file_path <- "C:\\Users\\ndlov\\Downloads\\loan_book.csv"
df <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE)

# ============================================
# 1. DATA PREPARATION
# ============================================

cat("========== DATA PREPARATION ==========\n")

# Convert numeric columns
numeric_cols <- c("age", "annual_income", "employment_length_years", 
                  "num_open_accounts", "num_delinquencies_2yr", 
                  "total_revolving_balance", "credit_utilisation_pct",
                  "months_since_oldest_account", "num_hard_inquiries_6mo",
                  "loan_amount", "interest_rate", "dti_ratio",
                  "months_since_last_delinquency", "pct_accounts_curRent",
                  "months_at_curRent_address")

for(col in numeric_cols) {
  df[[col]] <- as.numeric(df[[col]])
}

# Create clean target variable
df$default_flag <- as.numeric(df$default_flag)

# Split data into train and test
set.seed(42)
train_df <- df %>% filter(set == "train")
test_df <- df %>% filter(set == "test")

cat(sprintf("Training set: %d rows\n", nrow(train_df)))
cat(sprintf("Test set: %d rows\n", nrow(test_df)))
cat(sprintf("Overall default rate: %.2f%%\n", mean(df$default_flag, na.rm = TRUE) * 100))
cat(sprintf("Train default rate: %.2f%%\n", mean(train_df$default_flag, na.rm = TRUE) * 100))
cat(sprintf("Test default rate: %.2f%%\n", mean(test_df$default_flag, na.rm = TRUE) * 100))

# ============================================
# 2. BASELINE MODEL (Simple Logistic Regression)
# ============================================

cat("\n========== BASELINE MODEL ==========\n")

# Simple model with raw features
baseline_features <- c("age", "annual_income", "employment_length_years",
                       "num_open_accounts", "num_delinquencies_2yr",
                       "credit_utilisation_pct", "loan_amount", 
                       "interest_rate", "dti_ratio")

# Handle missing values - simple imputation with median
train_baseline <- train_df[, c(baseline_features, "default_flag")]
for(col in baseline_features) {
  median_val <- median(train_baseline[[col]], na.rm = TRUE)
  train_baseline[[col]][is.na(train_baseline[[col]])] <- median_val
}

# Train baseline model
baseline_model <- glm(default_flag ~ ., 
                      data = train_baseline, 
                      family = binomial(link = "logit"))

# Make predictions on test set
test_baseline <- test_df[, baseline_features]
for(col in baseline_features) {
  median_val <- median(train_df[[col]], na.rm = TRUE)
  test_baseline[[col]][is.na(test_baseline[[col]])] <- median_val
}

baseline_pred <- predict(baseline_model, newdata = test_baseline, type = "response")

# Calculate baseline AUC
baseline_roc <- roc(test_df$default_flag, baseline_pred)
baseline_auc <- auc(baseline_roc)

cat(sprintf("Baseline Model AUC: %.4f\n", baseline_auc))
cat("Baseline Model Summary:\n")
print(summary(baseline_model)$coefficients[1:10, ])

# ============================================
# 3. FEATURE ENGINEERING BASED ON EDA INSIGHTS
# ============================================

cat("\n========== FEATURE ENGINEERING ==========\n")
cat("Based on EDA findings:\n")
cat("1. Age has non-linear relationship with default (binning needed)\n")
cat("2. Income is strongly predictive (IV = 0.38)\n")
cat("3. Missing data patterns are highly informative\n")
cat("4. Credit utilization has threshold at ~68%\n")
cat("5. DTI ratio has monotonic relationship\n")
cat("6. Interest rate correlates positively with default\n")
cat("7. Prior delinquencies are strong predictors\n")

# Create enhanced features
create_enhanced_features <- function(df, is_training = TRUE, train_stats = NULL) {
  
  # Make a copy to avoid modifying original
  df_enhanced <- df
  
  # ---- 1. AGE TRANSFORMATIONS (non-linear) ----
  # From EDA: Default rate decreases non-linearly with age
  # Age bins (based on binning analysis)
  df_enhanced$age_bin <- cut(df_enhanced$age, 
                             breaks = c(0, 25, 35, 45, 55, 100),
                             labels = c("18-25", "26-35", "36-45", "46-55", "55+"))
  
  # Age squared (to capture U-shape if any)
  df_enhanced$age_squared <- (df_enhanced$age)^2 / 100
  
  # Age risk score (based on observed default rates)
  df_enhanced$age_risk <- ifelse(df_enhanced$age < 25, 3,
                                 ifelse(df_enhanced$age < 35, 2,
                                        ifelse(df_enhanced$age < 45, 1, 0)))
  
  # ---- 2. INCOME TRANSFORMATIONS (Strongest IV = 0.38) ----
  # Log income (credit models often use log income)
  df_enhanced$log_income <- log(df_enhanced$annual_income + 1)
  
  # Income bins (based on WoE analysis)
  df_enhanced$income_bin <- cut(df_enhanced$annual_income,
                                breaks = c(0, 25000, 50000, 75000, 100000, 150000, Inf),
                                labels = c("Very Low", "Low", "Medium", "Medium-High", "High", "Very High"))
  
  # Income risk score (based on default rates from EDA)
  df_enhanced$income_risk <- ifelse(df_enhanced$annual_income < 25000, 4,
                                    ifelse(df_enhanced$annual_income < 50000, 3,
                                           ifelse(df_enhanced$annual_income < 75000, 2,
                                                  ifelse(df_enhanced$annual_income < 100000, 1, 0))))
  
  # ---- 3. MISSING DATA PATTERNS (Highly predictive) ----
  # From EDA: Pattern 100 (only missing delinquency) = 7.69% default (SAFE)
  # Pattern 001/011 (missing income) = 28-29% default (RISKY)
  df_enhanced$miss_delinquency <- ifelse(is.na(df_enhanced$months_since_last_delinquency), 1, 0)
  df_enhanced$miss_employment <- ifelse(is.na(df_enhanced$employment_length_years), 1, 0)
  df_enhanced$miss_income <- ifelse(is.na(df_enhanced$annual_income), 1, 0)
  
  # Create missing pattern flags
  df_enhanced$clean_borrower <- ifelse(df_enhanced$miss_delinquency == 1 & 
                                         df_enhanced$num_delinquencies_2yr == 0, 1, 0)
  
  df_enhanced$hidden_income <- ifelse(df_enhanced$miss_income == 1 & 
                                        df_enhanced$miss_delinquency == 0, 1, 0)
  
  df_enhanced$hidden_employment <- ifelse(df_enhanced$miss_employment == 1 & 
                                            df_enhanced$miss_delinquency == 0, 1, 0)
  
  # Missing risk score (based on pattern analysis)
  df_enhanced$missing_risk <- 0
  df_enhanced$missing_risk[df_enhanced$miss_delinquency == 0 & 
                             df_enhanced$miss_employment == 1 & 
                             df_enhanced$miss_income == 1] <- 4  # Highest risk
  df_enhanced$missing_risk[df_enhanced$miss_income == 1 & 
                             df_enhanced$miss_delinquency == 0] <- 3
  df_enhanced$missing_risk[df_enhanced$miss_employment == 1 & 
                             df_enhanced$miss_delinquency == 0] <- 2
  df_enhanced$missing_risk[df_enhanced$miss_delinquency == 0] <- 1
  df_enhanced$missing_risk[df_enhanced$clean_borrower == 1] <- 0
  
  # ---- 4. CREDIT UTILIZATION (Threshold at 68%) ----
  df_enhanced$util_high <- ifelse(df_enhanced$credit_utilisation_pct > 68, 1, 0)
  df_enhanced$util_very_high <- ifelse(df_enhanced$credit_utilisation_pct > 80, 1, 0)
  df_enhanced$util_squared <- (df_enhanced$credit_utilisation_pct)^2 / 100
  
  # ---- 5. DTI RATIO (Monotonic relationship) ----
  df_enhanced$dti_high <- ifelse(df_enhanced$dti_ratio > 0.25, 1, 0)
  df_enhanced$dti_squared <- (df_enhanced$dti_ratio)^2
  
  # ---- 6. LOAN-TO-INCOME RATIO (New feature) ----
  df_enhanced$loan_to_income <- df_enhanced$loan_amount / (df_enhanced$annual_income + 1)
  df_enhanced$loan_to_income_capped <- pmin(df_enhanced$loan_to_income, 2)  # Cap at 200%
  
  # ---- 7. INTERACTION TERMS (Based on EDA) ----
  # Age × Income interaction
  df_enhanced$age_income_interaction <- df_enhanced$age_risk * df_enhanced$income_risk
  
  # Delinquency × Income interaction
  df_enhanced$delinq_income <- df_enhanced$num_delinquencies_2yr * (df_enhanced$annual_income < 50000)
  
  # ---- 8. BINNED VERSIONS OF OTHER VARIABLES ----
  # Prior delinquencies (categorical)
  df_enhanced$delinq_bin <- cut(df_enhanced$num_delinquencies_2yr,
                                breaks = c(-1, 0, 1, 2, Inf),
                                labels = c("0", "1", "2", "3+"))
  
  # Interest rate bins
  df_enhanced$rate_bin <- cut(df_enhanced$interest_rate,
                              breaks = quantile(df_enhanced$interest_rate, 
                                                probs = c(0, 0.25, 0.5, 0.75, 1), 
                                                na.rm = TRUE),
                              labels = c("Low", "Medium", "High", "Very High"))
  
  # ---- 9. IMRPUTE MISSING VALUES FOR MODELING ----
  # For numeric features, impute with median (by default status if training)
  numeric_enhanced <- c("age", "age_squared", "log_income", "credit_utilisation_pct",
                        "util_squared", "dti_ratio", "dti_squared", "loan_to_income_capped",
                        "num_delinquencies_2yr", "interest_rate", "num_hard_inquiries_6mo",
                        "months_since_oldest_account", "pct_accounts_curRent")
  
  if(is_training) {
    # Store imputation values
    impute_values <- list()
    for(col in numeric_enhanced) {
      # Impute by default status for better accuracy
      default_median <- median(df_enhanced[[col]][df_enhanced$default_flag == 1], na.rm = TRUE)
      non_default_median <- median(df_enhanced[[col]][df_enhanced$default_flag == 0], na.rm = TRUE)
      
      impute_values[[paste0(col, "_default")]] <- default_median
      impute_values[[paste0(col, "_non_default")]] <- non_default_median
      
      # Apply imputation
      df_enhanced[[col]][is.na(df_enhanced[[col]]) & df_enhanced$default_flag == 1] <- default_median
      df_enhanced[[col]][is.na(df_enhanced[[col]]) & df_enhanced$default_flag == 0] <- non_default_median
      
      # For any remaining NAs, use overall median
      overall_median <- median(df_enhanced[[col]], na.rm = TRUE)
      df_enhanced[[col]][is.na(df_enhanced[[col]])] <- overall_median
      impute_values[[col]] <- overall_median
    }
    result <- list(data = df_enhanced, impute_values = impute_values)
  } else {
    # Use stored imputation values
    for(col in numeric_enhanced) {
      default_median <- train_stats[[paste0(col, "_default")]]
      non_default_median <- train_stats[[paste0(col, "_non_default")]]
      
      df_enhanced[[col]][is.na(df_enhanced[[col]])] <- train_stats[[col]]
    }
    result <- list(data = df_enhanced, impute_values = NULL)
  }
  
  return(result)
}

# Apply feature engineering
train_enhanced <- create_enhanced_features(train_df, is_training = TRUE)
test_enhanced <- create_enhanced_features(test_df, is_training = FALSE, 
                                          train_stats = train_enhanced$impute_values)

train_enhanced_df <- train_enhanced$data
test_enhanced_df <- test_enhanced$data

cat("\nFeatures created successfully!\n")
cat(sprintf("Training set now has %d features\n", ncol(train_enhanced_df)))

# ============================================
# 4. IMPROVED MODEL WITH FEATURE ENGINEERING
# ============================================

cat("\n========== IMPROVED MODEL TRAINING ==========\n")

# Select features for improved model (based on EDA insights)
improved_features <- c(
  # Demographics
  "age_bin", "age_squared", "log_income", "income_bin",
  
  # Missing data patterns (highly predictive)
  "clean_borrower", "hidden_income", "hidden_employment", "missing_risk",
  
  # Credit characteristics
  "num_delinquencies_2yr", "delinq_bin",
  "credit_utilisation_pct", "util_high", "util_squared",
  "dti_ratio", "dti_high", "dti_squared",
  "interest_rate", "rate_bin",
  "num_hard_inquiries_6mo",
  "pct_accounts_curRent",
  "months_since_oldest_account",
  
  # New engineered features
  "loan_to_income_capped",
  "age_income_interaction",
  "delinq_income"
)

# Create model matrix for categorical variables
# Convert factors to dummy variables
train_model_df <- train_enhanced_df[, c(improved_features, "default_flag")]

# Convert character/factor columns to factors for modeling
factor_cols <- c("age_bin", "income_bin", "delinq_bin", "rate_bin")
for(col in factor_cols) {
  train_model_df[[col]] <- as.factor(train_model_df[[col]])
}

# Handle any remaining NA values
train_model_df <- train_model_df %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

# Train improved logistic regression
improved_model <- glm(default_flag ~ ., 
                      data = train_model_df, 
                      family = binomial(link = "logit"))

# Prepare test data
test_model_df <- test_enhanced_df[, c(improved_features, "default_flag")]

for(col in factor_cols) {
  test_model_df[[col]] <- as.factor(test_model_df[[col]])
}

# Align factor levels with training data
for(col in factor_cols) {
  test_model_df[[col]] <- factor(test_model_df[[col]], 
                                 levels = levels(train_model_df[[col]]))
}

# Handle NAs in test set
test_model_df <- test_model_df %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

# Make predictions
improved_pred <- predict(improved_model, newdata = test_model_df, type = "response")

# Calculate improved AUC
improved_roc <- roc(test_model_df$default_flag, improved_pred)
improved_auc <- auc(improved_roc)

cat(sprintf("\nImproved Model AUC: %.4f\n", improved_auc))
cat(sprintf("Improvement over baseline: +%.4f (%.1f%%)\n", 
            improved_auc - baseline_auc, 
            (improved_auc - baseline_auc) / baseline_auc * 100))

# ============================================
# 5. MODEL COMPARISON & ANALYSIS
# ============================================

cat("\n========== MODEL COMPARISON ==========\n")

# Create comparison dataframe
comparison <- data.frame(
  Model = c("Baseline Logistic", "Improved Logistic", "LightGBM (Reference)"),
  AUC = c(baseline_auc, improved_auc, 0.82),
  Improvement = c("-", paste0("+", round((improved_auc - baseline_auc) * 100, 1), "%"), "Reference")
)

print(comparison)

# Feature importance analysis
cat("\n========== TOP FEATURES IN IMPROVED MODEL ==========\n")

# Get coefficients
coef_summary <- summary(improved_model)$coefficients
coef_df <- data.frame(
  Feature = rownames(coef_summary),
  Coefficient = coef_summary[, 1],
  P_value = coef_summary[, 4]
)

# Sort by absolute coefficient (excluding intercept)
coef_df <- coef_df[coef_df$Feature != "(Intercept)", ]
coef_df$Abs_Coefficient <- abs(coef_df$Coefficient)
coef_df <- coef_df[order(-coef_df$Abs_Coefficient), ]

cat("\nTop 10 Most Important Features:\n")
print(head(coef_df, 10))

# ============================================
# 6. MODEL EVALUATION METRICS
# ============================================

cat("\n========== DETAILED MODEL EVALUATION ==========\n")

# Find optimal threshold for improved model
optimal_threshold <- coords(improved_roc, "best", ret = "threshold")$threshold

# Calculate predictions at optimal threshold
improved_pred_class <- ifelse(improved_pred > optimal_threshold, 1, 0)

# Confusion matrix
confusion <- confusionMatrix(as.factor(improved_pred_class), 
                             as.factor(test_model_df$default_flag),
                             positive = "1")

cat("\nConfusion Matrix (at optimal threshold):\n")
print(confusion$table)

# Calculate additional metrics
tn <- confusion$table[1,1]
fp <- confusion$table[1,2]
fn <- confusion$table[2,1]
tp <- confusion$table[2,2]

precision <- tp / (tp + fp)
recall <- tp / (tp + fn)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nPerformance Metrics:\n")
cat(sprintf("  Accuracy: %.4f\n", confusion$overall["Accuracy"]))
cat(sprintf("  Precision: %.4f\n", precision))
cat(sprintf("  Recall/Sensitivity: %.4f\n", recall))
cat(sprintf("  Specificity: %.4f\n", tn / (tn + fp)))
cat(sprintf("  F1 Score: %.4f\n", f1_score))
cat(sprintf("  AUC: %.4f\n", improved_auc))
cat(sprintf("  Gini Coefficient: %.4f\n", 2 * improved_auc - 1))

# ============================================
# 7. PREDICTION DISTRIBUTION ANALYSIS
# ============================================

cat("\n========== PREDICTION DISTRIBUTION ==========\n")

# Analyze predictions by risk score segments
test_enhanced_df$predicted_prob <- improved_pred
test_enhanced_df$predicted_class <- improved_pred_class

# Create risk buckets
test_enhanced_df$risk_bucket <- cut(test_enhanced_df$predicted_prob,
                                    breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 1),
                                    labels = c("0-10%", "10-20%", "20-30%", "30-40%", "40-50%", "50%+"))

risk_distribution <- test_enhanced_df %>%
  group_by(risk_bucket) %>%
  summarise(
    Count = n(),
    Actual_Default_Rate = mean(default_flag) * 100,
    Avg_Predicted = mean(predicted_prob) * 100
  )

print(risk_distribution)

# ============================================
# 8. SAVE MODEL RESULTS
# ============================================

cat("\n========== SAVING RESULTS ==========\n")

# Save models
saveRDS(baseline_model, "baseline_model.rds")
saveRDS(improved_model, "improved_model.rds")
saveRDS(optimal_threshold, "optimal_threshold.rds")

# Save predictions
results <- data.frame(
  actual = test_model_df$default_flag,
  baseline_pred = baseline_pred,
  improved_pred = improved_pred,
  improved_class = improved_pred_class
)

write.csv(results, "model_predictions.csv", row.names = FALSE)

# Create summary report
report <- data.frame(
  Metric = c("Baseline AUC", "Improved AUC", "Absolute Improvement", 
             "Relative Improvement", "Optimal Threshold", "Gini Coefficient",
             "Precision", "Recall", "F1 Score", "Accuracy"),
  Value = c(round(baseline_auc, 4), round(improved_auc, 4),
            round(improved_auc - baseline_auc, 4),
            paste0(round((improved_auc - baseline_auc) / baseline_auc * 100, 1), "%"),
            round(optimal_threshold, 4), round(2 * improved_auc - 1, 4),
            round(precision, 4), round(recall, 4), round(f1_score, 4),
            round(confusion$overall["Accuracy"], 4))
)

write.csv(report, "model_performance_report.csv", row.names = FALSE)

cat("\nResults saved to:\n")
cat("  - baseline_model.rds\n")
cat("  - improved_model.rds\n")
cat("  - optimal_threshold.rds\n")
cat("  - model_predictions.csv\n")
cat("  - model_performance_report.csv\n")

# ============================================
# 9. VISUALIZATION OF RESULTS
# ============================================

cat("\n========== GENERATING VISUALIZATIONS ==========\n")

# ROC Curves
png("roc_curves.png", width = 800, height = 600)
plot(baseline_roc, col = "red", lwd = 2, 
     main = "ROC Curves Comparison",
     xlab = "False Positive Rate (1 - Specificity)",
     ylab = "True Positive Rate (Sensitivity)")
lines(improved_roc, col = "blue", lwd = 2)
legend("bottomright", 
       legend = c(paste("Baseline (AUC =", round(baseline_auc, 3), ")"),
                  paste("Improved (AUC =", round(improved_auc, 3), ")"),
                  "LightGBM Reference (AUC = 0.82)"),
       col = c("red", "blue", "green"), lwd = 2, lty = 1)
abline(a = 0, b = 1, lty = 2, col = "gray")
dev.off()

# Prediction distribution
png("prediction_distribution.png", width = 800, height = 600)
ggplot(test_enhanced_df, aes(x = predicted_prob, fill = as.factor(default_flag))) +
  geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
  labs(title = "Predicted Probability Distribution by Actual Default",
       x = "Predicted Probability of Default",
       y = "Count",
       fill = "Actual Default") +
  theme_minimal() +
  scale_fill_manual(values = c("blue", "red"), labels = c("No Default", "Default"))
dev.off()

# Feature importance plot
png("feature_importance.png", width = 1000, height = 600)
ggplot(head(coef_df, 15), aes(x = reorder(Feature, Abs_Coefficient), 
                              y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 15 Features by Coefficient Magnitude",
       x = "Feature",
       y = "Coefficient Value") +
  theme_minimal() +
  theme(legend.position = "none")
dev.off()

cat("\nVisualizations saved:\n")
cat("  - roc_curves.png\n")
cat("  - prediction_distribution.png\n")
cat("  - feature_importance.png\n")

# ============================================
# 10. SUMMARY OF IMPROVEMENTS
# ============================================

cat("\n========== SUMMARY OF IMPROVEMENTS ==========\n")
cat("\nKEY FEATURE ENGINEERING IMPROVEMENTS:\n")
cat("1. Age transformations: Binning, squared term, and risk scoring\n")
cat("2. Income transformations: Log transform, binning, and risk scoring\n")
cat("3. Missing data patterns: Created 5 new features capturing missingness signals\n")
cat("4. Credit utilization: Added threshold flags and squared term\n")
cat("5. DTI ratio: Added threshold and squared term\n")
cat("6. Loan-to-income ratio: New engineered ratio feature\n")
cat("7. Interaction terms: Age × Income and Delinquency × Income\n")
cat("8. Binned versions of key features for non-linear relationships\n")
cat("9. Smart imputation using default status\n")

cat("\nPERFORMANCE IMPROVEMENT:\n")
cat(sprintf("  Baseline AUC: %.4f\n", baseline_auc))
cat(sprintf("  Improved AUC: %.4f\n", improved_auc))
cat(sprintf("  Absolute Gain: +%.4f\n", improved_auc - baseline_auc))
cat(sprintf("  Relative Gain: +%.1f%%\n", (improved_auc - baseline_auc) / baseline_auc * 100))
cat(sprintf("  Distance to LightGBM ceiling: %.4f\n", 0.82 - improved_auc))

cat("\n✅ TASK 2 COMPLETED SUCCESSFULLY!\n")

