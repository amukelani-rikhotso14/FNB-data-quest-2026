

# Load required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)
library(MASS)

# Fix the select conflict - use dplyr explicitly
select <- dplyr::select

# Set seed for reproducibility
set.seed(42)

# ============================================
# 1. DATA LOADING AND PREPARATION
# ============================================

cat("========================================\n")
cat("STEP 1: LOADING AND PREPARING DATA\n")
cat("========================================\n\n")

# Read the data
file_path <- "C:\\Users\\ndlov\\Downloads\\loan_book.csv"
df <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE)

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

# Create clean loan purpose
df$loan_purpose_clean <- tolower(df$loan_purpose)
df$loan_purpose_clean <- gsub("_", " ", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("debt consolidation", "debt_consolidation", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("home improvement", "home_improvement", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("major purchase", "major_purchase", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("small business", "small_business", df$loan_purpose_clean)

# Create age groups (from EDA)
df$age_group <- cut(df$age, 
                    breaks = c(0, 25, 35, 45, 55, 100), 
                    labels = c("18-25", "26-35", "36-45", "46-55", "55+"))

# Create DTI bins (from EDA binning analysis)
df$dti_binned <- cut(df$dti_ratio, 
                     breaks = c(0, 0.1, 0.15, 0.2, 0.25, 0.3, 1),
                     labels = c("0-0.1", "0.1-0.15", "0.15-0.2", 
                                "0.2-0.25", "0.25-0.3", "0.3+"))

# Create interest rate bins
df$interest_rate_binned <- cut(df$interest_rate,
                               breaks = c(0, 10, 15, 20, 100),
                               labels = c("Low", "Medium", "High", "Very High"))

# Convert target to numeric
df$default_flag <- as.numeric(df$default_flag)

# Split data into train and test
train_df <- df[df$set == "train", ]
test_df <- df[df$set == "test", ]

cat("Training set size:", nrow(train_df), "\n")
cat("Test set size:", nrow(test_df), "\n")
cat("Baseline default rate (train):", 
    round(mean(train_df$default_flag, na.rm = TRUE) * 100, 2), "%\n\n")


# ============================================
# 2. FEATURE ENGINEERING FUNCTION
# ============================================

cat("========================================\n")
cat("STEP 2: FEATURE ENGINEERING\n")
cat("========================================\n\n")

engineer_features <- function(data) {
  
  df_eng <- data
  
  # 1. AGE FEATURES (from binning analysis - risk drops sharply after 25)
  df_eng$age_young <- ifelse(df_eng$age < 25, 1, 0)
  df_eng$age_middle <- ifelse(df_eng$age >= 35 & df_eng$age < 45, 1, 0)
  df_eng$age_senior <- ifelse(df_eng$age >= 55, 1, 0)
  df_eng$age_squared <- (df_eng$age - 38)^2 / 100
  
  # 2. INCOME FEATURES (strongest IV from EDA)
  df_eng$income_low <- ifelse(df_eng$annual_income < 25000, 1, 0)
  df_eng$income_medium <- ifelse(df_eng$annual_income >= 50000 & df_eng$annual_income < 75000, 1, 0)
  df_eng$income_high <- ifelse(df_eng$annual_income >= 100000, 1, 0)
  df_eng$log_income <- log(df_eng$annual_income + 1)
  
  # 3. DTI RATIO FEATURES (sharp increase after 0.25 from binning)
  df_eng$dti_high <- ifelse(df_eng$dti_ratio > 0.25, 1, 0)
  df_eng$dti_very_high <- ifelse(df_eng$dti_ratio > 0.35, 1, 0)
  
  # 4. CREDIT UTILIZATION FEATURES (sharp increase above 60%)
  df_eng$util_high <- ifelse(df_eng$credit_utilisation_pct > 60, 1, 0)
  df_eng$util_very_high <- ifelse(df_eng$credit_utilisation_pct > 80, 1, 0)
  df_eng$util_squared <- (df_eng$credit_utilisation_pct / 100)^2
  
  # 5. DELINQUENCY FEATURES (strong correlation from EDA)
  df_eng$has_delinquency <- ifelse(df_eng$num_delinquencies_2yr > 0, 1, 0)
  df_eng$multiple_delinquencies <- ifelse(df_eng$num_delinquencies_2yr > 1, 1, 0)
  df_eng$delinquency_count <- pmin(df_eng$num_delinquencies_2yr, 5)
  
  # 6. MISSING DATA FEATURES (key discovery from EDA)
  df_eng$no_delinquency_record <- ifelse(is.na(df_eng$months_since_last_delinquency), 1, 0)
  df_eng$missing_income <- ifelse(is.na(df_eng$annual_income), 1, 0)
  df_eng$missing_employment <- ifelse(is.na(df_eng$employment_length_years), 1, 0)
  
  # 7. INTERACTION: Missing income + known credit (28.8% default rate segment)
  df_eng$hidden_income_risk <- ifelse(
    is.na(df_eng$annual_income) & !is.na(df_eng$months_since_last_delinquency), 
    1, 0
  )
  
  # 8. EMPLOYMENT FEATURES
  df_eng$employment_short <- ifelse(df_eng$employment_length_years < 2, 1, 0)
  df_eng$employment_long <- ifelse(df_eng$employment_length_years > 10, 1, 0)
  df_eng$log_employment <- log(df_eng$employment_length_years + 1)
  
  # 9. CREDIT HISTORY LENGTH
  df_eng$credit_history_short <- ifelse(df_eng$months_since_oldest_account < 100, 1, 0)
  df_eng$credit_history_long <- ifelse(df_eng$months_since_oldest_account > 200, 1, 0)
  df_eng$log_credit_history <- log(df_eng$months_since_oldest_account + 1)
  
  # 10. INQUIRIES
  df_eng$inquiries_high <- ifelse(df_eng$num_hard_inquiries_6mo > 2, 1, 0)
  df_eng$inquiries_very_high <- ifelse(df_eng$num_hard_inquiries_6mo > 4, 1, 0)
  
  # 11. LOAN TO INCOME RATIO
  df_eng$loan_to_income <- df_eng$loan_amount / (df_eng$annual_income + 1)
  df_eng$loan_to_income_high <- ifelse(df_eng$loan_to_income > 0.5, 1, 0)
  
  # 12. INTEREST RATE FEATURES (correlation 0.232 with default)
  df_eng$interest_rate_high <- ifelse(df_eng$interest_rate > 15, 1, 0)
  df_eng$interest_rate_very_high <- ifelse(df_eng$interest_rate > 20, 1, 0)
  
  # 13. HOME OWNERSHIP
  df_eng$is_renter <- ifelse(df_eng$home_Ownship == "Rent", 1, 0)
  df_eng$is_owner <- ifelse(df_eng$home_Ownship == "Own", 1, 0)
  
  # 14. LOAN PURPOSE INDICATORS
  df_eng$purpose_medical <- ifelse(df_eng$loan_purpose_clean == "medical", 1, 0)
  df_eng$purpose_education <- ifelse(df_eng$loan_purpose_clean == "education", 1, 0)
  df_eng$purpose_small_business <- ifelse(df_eng$loan_purpose_clean == "small_business", 1, 0)
  
  # 15. AGE × INCOME INTERACTIONS (from EDA cross-tabulation)
  df_eng$young_low_income <- ifelse(df_eng$age < 30 & df_eng$annual_income < 30000, 1, 0)
  df_eng$senior_high_income <- ifelse(df_eng$age > 55 & df_eng$annual_income > 100000, 1, 0)
  
  return(df_eng)
}

# Apply feature engineering
train_eng <- engineer_features(train_df)
test_eng <- engineer_features(test_df)

cat("Features engineered successfully!\n")
cat("Total features in engineered dataset:", ncol(train_eng), "\n\n")


# ============================================
# 3. WEIGHT OF EVIDENCE (WOE) ENCODING
# ============================================

cat("========================================\n")
cat("STEP 3: WEIGHT OF EVIDENCE ENCODING\n")
cat("========================================\n\n")

# Function to calculate WoE mapping
calculate_woe_mapping <- function(data, feature, target, min_samples = 30) {
  df_temp <- data[!is.na(data[[feature]]) & !is.na(data[[target]]), ]
  
  total_events <- sum(df_temp[[target]] == 1, na.rm = TRUE)
  total_non_events <- sum(df_temp[[target]] == 0, na.rm = TRUE)
  
  unique_vals <- unique(df_temp[[feature]])
  woe_list <- list()
  
  for(val in unique_vals) {
    subset_data <- df_temp[df_temp[[feature]] == val, ]
    count <- nrow(subset_data)
    if(count >= min_samples) {
      events <- sum(subset_data[[target]] == 1, na.rm = TRUE)
      non_events <- sum(subset_data[[target]] == 0, na.rm = TRUE)
      
      pct_events <- events / total_events
      pct_non_events <- non_events / total_non_events
      
      woe <- log(pct_non_events / pct_events)
      if(is.infinite(woe)) woe <- 0
      
      woe_list[[length(woe_list) + 1]] <- data.frame(
        category = val,
        WoE = woe,
        stringsAsFactors = FALSE
      )
    }
  }
  
  woe_map <- do.call(rbind, woe_list)
  names(woe_map)[1] <- feature
  return(woe_map)
}

# Apply WoE mapping function
apply_woe <- function(data, woe_map, feature_name, new_col_name) {
  data[[new_col_name]] <- NA_real_
  for(i in 1:nrow(woe_map)) {
    data[[new_col_name]][data[[feature_name]] == woe_map[[feature_name]][i]] <- woe_map$WoE[i]
  }
  # Fill remaining with 0 (overall WoE)
  data[[new_col_name]][is.na(data[[new_col_name]])] <- 0
  return(data)
}

# Calculate WoE mappings on training data
cat("Calculating WoE mappings...\n")
age_woe_map <- calculate_woe_mapping(train_df, "age_group", "default_flag")
dti_woe_map <- calculate_woe_mapping(train_df, "dti_binned", "default_flag")
interest_woe_map <- calculate_woe_mapping(train_df, "interest_rate_binned", "default_flag")

cat("Age WoE mapping complete\n")
cat("DTI WoE mapping complete\n")
cat("Interest WoE mapping complete\n")

# Apply WoE to train and test
train_eng <- apply_woe(train_eng, age_woe_map, "age_group", "age_woe")
train_eng <- apply_woe(train_eng, dti_woe_map, "dti_binned", "dti_woe")
train_eng <- apply_woe(train_eng, interest_woe_map, "interest_rate_binned", "interest_woe")

test_eng <- apply_woe(test_eng, age_woe_map, "age_group", "age_woe")
test_eng <- apply_woe(test_eng, dti_woe_map, "dti_binned", "dti_woe")
test_eng <- apply_woe(test_eng, interest_woe_map, "interest_rate_binned", "interest_woe")

cat("WoE encoding complete!\n\n")


# ============================================
# 4. BASELINE MODEL (RAW FEATURES)
# ============================================

cat("========================================\n")
cat("STEP 4: TRAINING MODELS\n")
cat("========================================\n\n")

# Function to prepare data for modeling (handle NAs)
prepare_model_data <- function(data, feature_list) {
  model_data <- data[, c(feature_list, "default_flag")]
  for(col in feature_list) {
    if(is.numeric(model_data[[col]])) {
      model_data[[col]][is.na(model_data[[col]])] <- median(model_data[[col]], na.rm = TRUE)
    } else {
      model_data[[col]][is.na(model_data[[col]])] <- 0
    }
  }
  return(model_data)
}

# Function to evaluate model
evaluate_model <- function(model, test_data, model_name) {
  pred_prob <- predict(model, newdata = test_data, type = "response")
  pred_class <- ifelse(pred_prob > 0.5, 1, 0)
  
  # Confusion matrix
  cm <- table(Predicted = pred_class, Actual = test_data$default_flag)
  
  # Calculate metrics
  accuracy <- sum(diag(cm)) / sum(cm)
  precision <- ifelse(sum(pred_class == 1) > 0, cm[2,2] / sum(pred_class == 1), 0)
  recall <- ifelse(sum(test_data$default_flag == 1) > 0, cm[2,2] / sum(test_data$default_flag == 1), 0)
  f1 <- ifelse((precision + recall) > 0, 2 * precision * recall / (precision + recall), 0)
  
  # AUC and Gini
  roc_obj <- roc(test_data$default_flag, pred_prob, quiet = TRUE)
  auc_value <- auc(roc_obj)
  gini <- 2 * auc_value - 1
  
  results <- data.frame(
    Model = model_name,
    AUC = round(as.numeric(auc_value), 4),
    Gini = round(gini, 4),
    Accuracy = round(accuracy, 4),
    Precision = round(precision, 4),
    Recall = round(recall, 4),
    F1 = round(f1, 4)
  )
  
  return(results)
}

# BASELINE: Raw numeric features
baseline_vars <- c("age", "annual_income", "employment_length_years", 
                   "num_open_accounts", "num_delinquencies_2yr",
                   "credit_utilisation_pct", "loan_amount", 
                   "interest_rate", "dti_ratio")

train_baseline <- prepare_model_data(train_eng, baseline_vars)
test_baseline <- prepare_model_data(test_eng, baseline_vars)

baseline_model <- glm(default_flag ~ ., data = train_baseline, family = binomial)
cat("Baseline model trained\n")

# MODEL 1: Risk indicators only
risk_vars <- c("age_young", "age_senior", "income_low", "income_high",
               "dti_high", "util_high", "has_delinquency", 
               "interest_rate_high", "is_renter")

train_model1 <- prepare_model_data(train_eng, risk_vars)
test_model1 <- prepare_model_data(test_eng, risk_vars)

model1 <- glm(default_flag ~ ., data = train_model1, family = binomial)
cat("Model 1 (Risk Indicators) trained\n")

# MODEL 2: Add missing data indicators
missing_vars <- c(risk_vars, "no_delinquency_record", "missing_income", 
                  "missing_employment", "hidden_income_risk")

train_model2 <- prepare_model_data(train_eng, missing_vars)
test_model2 <- prepare_model_data(test_eng, missing_vars)

model2 <- glm(default_flag ~ ., data = train_model2, family = binomial)
cat("Model 2 (+ Missing Data) trained\n")

# MODEL 3: Full engineered features
full_features <- c("age_young", "age_middle", "age_senior", "age_squared",
                   "income_low", "income_medium", "income_high", "log_income",
                   "dti_high", "dti_very_high", "util_high", "util_very_high",
                   "has_delinquency", "multiple_delinquencies", "delinquency_count",
                   "no_delinquency_record", "missing_income", "missing_employment", 
                   "hidden_income_risk",
                   "employment_short", "employment_long", "log_employment",
                   "credit_history_short", "credit_history_long", "log_credit_history",
                   "inquiries_high", "inquiries_very_high",
                   "loan_to_income_high", "interest_rate_high", "interest_rate_very_high",
                   "is_renter", "is_owner",
                   "purpose_medical", "purpose_education", "purpose_small_business",
                   "young_low_income", "senior_high_income")

train_model3 <- prepare_model_data(train_eng, full_features)
test_model3 <- prepare_model_data(test_eng, full_features)

model3 <- glm(default_flag ~ ., data = train_model3, family = binomial)
cat("Model 3 (Full Engineered) trained\n")

# MODEL 4: Add WoE features
woe_features <- c(full_features, "age_woe", "dti_woe", "interest_woe")

train_model4 <- prepare_model_data(train_eng, woe_features)
test_model4 <- prepare_model_data(test_eng, woe_features)

model4 <- glm(default_flag ~ ., data = train_model4, family = binomial)
cat("Model 4 (+ WoE Encoding) trained\n\n")


# ============================================
# 5. EVALUATE ALL MODELS
# ============================================

cat("========================================\n")
cat("STEP 5: MODEL EVALUATION\n")
cat("========================================\n\n")

# Evaluate all models
results_baseline <- evaluate_model(baseline_model, test_baseline, "Baseline (Raw)")
results_model1 <- evaluate_model(model1, test_model1, "Risk Indicators")
results_model2 <- evaluate_model(model2, test_model2, "+ Missing Data")
results_model3 <- evaluate_model(model3, test_model3, "Full Engineered")
results_model4 <- evaluate_model(model4, test_model4, "+ WoE Encoding")

# Combine results
all_results <- rbind(results_baseline, results_model1, results_model2, 
                     results_model3, results_model4)

print(all_results)

# Calculate improvement over baseline
baseline_auc <- results_baseline$AUC
cat("\n--- Improvement Summary ---\n")
cat(sprintf("Baseline AUC: %.4f\n", baseline_auc))
cat(sprintf("Risk Indicators AUC: %.4f (Improvement: +%.4f)\n", 
            results_model1$AUC, results_model1$AUC - baseline_auc))
cat(sprintf("+ Missing Data AUC: %.4f (Improvement: +%.4f)\n", 
            results_model2$AUC, results_model2$AUC - baseline_auc))
cat(sprintf("Full Engineered AUC: %.4f (Improvement: +%.4f)\n", 
            results_model3$AUC, results_model3$AUC - baseline_auc))
cat(sprintf("+ WoE Encoding AUC: %.4f (Improvement: +%.4f)\n", 
            results_model4$AUC, results_model4$AUC - baseline_auc))


# ============================================
# STEP 6: SIMPLIFIED FEATURE SELECTION
# ============================================

cat("\n========================================\n")
cat("STEP 6: FEATURE SELECTION (STEPWISE)\n")
cat("========================================\n\n")

# Method 1: Manual forward selection (more reliable than stepAIC)
# This finds the best features one by one

cat("Method 1: Forward Selection (finding best features)...\n")

# Get all feature names
all_features <- woe_features
current_features <- c()
remaining_features <- all_features
best_auc <- 0
best_features <- c()

# Try up to 20 features (or fewer if no improvement)
max_features <- min(20, length(all_features))

for(i in 1:max_features) {
  best_candidate <- NULL
  best_candidate_auc <- 0
  
  # Try each remaining feature
  for(feat in remaining_features) {
    test_features <- c(current_features, feat)
    
    # Prepare data
    temp_data <- train_model4[, c(test_features, "default_flag")]
    for(col in test_features) {
      temp_data[[col]][is.na(temp_data[[col]])] <- 0
    }
    
    # Train model
    temp_model <- suppressWarnings(glm(default_flag ~ ., data = temp_data, family = binomial))
    
    # Quick evaluation on a subset (10000 rows for speed)
    eval_data <- test_model4[sample(1:nrow(test_model4), min(10000, nrow(test_model4))), 
                             c(test_features, "default_flag")]
    for(col in test_features) {
      eval_data[[col]][is.na(eval_data[[col]])] <- 0
    }
    
    pred <- predict(temp_model, newdata = eval_data, type = "response")
    roc_obj <- roc(eval_data$default_flag, pred, quiet = TRUE)
    auc_val <- auc(roc_obj)
    
    if(auc_val > best_candidate_auc) {
      best_candidate_auc <- auc_val
      best_candidate <- feat
    }
  }
  
  # Check if this feature improves the model
  if(best_candidate_auc > best_auc + 0.001) {  # Only add if improves by >0.001
    current_features <- c(current_features, best_candidate)
    remaining_features <- remaining_features[remaining_features != best_candidate]
    best_auc <- best_candidate_auc
    cat(sprintf("  Added %s (AUC: %.4f)\n", best_candidate, best_auc))
  } else {
    cat(sprintf("  Stopping - no further improvement (AUC: %.4f)\n", best_auc))
    break
  }
}

cat(sprintf("\nForward selection complete! Selected %d features\n", length(current_features)))
cat("Top 10 selected features:\n")
print(head(current_features, 10))

# Method 2: Feature importance by coefficient magnitude (simpler)
cat("\nMethod 2: Feature Importance by Coefficient Magnitude\n")
cat("----------------------------------------------------\n")

# Train a regularized model (ridge regression) to get stable coefficients
# We'll use a simple approach: remove correlated features first

# Calculate correlation matrix for features
feature_cors <- cor(train_model4[, woe_features], use = "complete.obs")

# Find highly correlated features (>0.8) to remove
high_cor <- which(abs(feature_cors) > 0.8 & upper.tri(feature_cors), arr.ind = TRUE)
if(nrow(high_cor) > 0) {
  cat("Removing highly correlated features:\n")
  to_remove <- c()
  for(i in 1:nrow(high_cor)) {
    feat1 <- rownames(feature_cors)[high_cor[i, 1]]
    feat2 <- colnames(feature_cors)[high_cor[i, 2]]
    cat(sprintf("  %s correlated with %s (%.3f)\n", feat1, feat2, feature_cors[high_cor[i, 1], high_cor[i, 2]]))
    to_remove <- c(to_remove, feat2)  # Remove the second feature
  }
  
  # Train model without highly correlated features
  reduced_features <- setdiff(woe_features, unique(to_remove))
} else {
  reduced_features <- woe_features
}

# Train final model with reduced features
train_reduced <- train_model4[, c(reduced_features, "default_flag")]
for(col in reduced_features) {
  train_reduced[[col]][is.na(train_reduced[[col]])] <- 0
}

final_model <- glm(default_flag ~ ., data = train_reduced, family = binomial)

# Get feature importance
feature_importance <- data.frame(
  Feature = names(coef(final_model))[-1],
  Coefficient = coef(final_model)[-1],
  Odds_Ratio = exp(coef(final_model)[-1]),
  Abs_Coefficient = abs(coef(final_model)[-1])
) %>%
  arrange(desc(Abs_Coefficient))

cat("\nTop 10 Most Important Features (by absolute coefficient):\n")
print(head(feature_importance, 10))

# Evaluate this model on test set
test_reduced <- test_model4[, c(reduced_features, "default_flag")]
for(col in reduced_features) {
  test_reduced[[col]][is.na(test_reduced[[col]])] <- 0
}

final_predictions <- predict(final_model, newdata = test_reduced, type = "response")
final_roc <- roc(test_reduced$default_flag, final_predictions, quiet = TRUE)
final_auc <- auc(final_roc)
final_gini <- 2 * final_auc - 1

cat("\nFinal Model Performance (with reduced features):\n")
cat(sprintf("  Number of features: %d\n", length(reduced_features)))
cat(sprintf("  AUC: %.4f\n", final_auc))
cat(sprintf("  Gini: %.4f\n", final_gini))

# Compare with full model
cat("\nComparison:\n")
cat(sprintf("  Full model AUC: %.4f\n", results_model4$AUC))
cat(sprintf("  Reduced model AUC: %.4f\n", final_auc))
cat(sprintf("  Difference: %.4f\n", final_auc - results_model4$AUC))

# Create final results summary
final_results <- rbind(all_results, data.frame(
  Model = "Reduced Features (Stepwise)",
  AUC = round(as.numeric(final_auc), 4),
  Gini = round(final_gini, 4),
  Accuracy = NA,
  Precision = NA,
  Recall = NA,
  F1 = NA
))

print(final_results)

# Visualize top features
importance_plot <- ggplot(feature_importance[1:10, ], 
                          aes(x = reorder(Feature, Abs_Coefficient), 
                              y = Coefficient, fill = Coefficient > 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 10 Most Important Features",
       subtitle = "By absolute coefficient magnitude",
       x = "Feature", y = "Coefficient") +
  theme_minimal() +
  theme(legend.position = "none")

print(importance_plot)

