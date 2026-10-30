# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)

# Read the data
file_path <- "C:\\Users\\ndlov\\Downloads\\loan_book.csv"
df <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE)

# Display basic info
cat("==================== BASIC DATASET INFO ====================\n")
cat("Total number of applicants:", nrow(df), "\n")
cat("Number of columns:", ncol(df), "\n\n")

# Age demographics
cat("==================== AGE DEMOGRAPHICS ====================\n")
# Convert age to numeric
df$age <- as.numeric(df$age)
cat("Age Statistics:\n")
cat("  Mean:", round(mean(df$age, na.rm = TRUE), 2), "years\n")
cat("  Median:", median(df$age, na.rm = TRUE), "years\n")
cat("  Min:", min(df$age, na.rm = TRUE), "years\n")
cat("  Max:", max(df$age, na.rm = TRUE), "years\n")
cat("  Standard Deviation:", round(sd(df$age, na.rm = TRUE), 2), "years\n\n")

# Age distribution by groups
df$age_group <- cut(df$age, 
                    breaks = c(0, 25, 35, 45, 55, 100), 
                    labels = c("18-25", "26-35", "36-45", "46-55", "55+"))
age_dist <- table(df$age_group, useNA = "ifany")
cat("Age Group Distribution:\n")
print(age_dist)
cat("\nAge Group Percentages:\n")
print(round(prop.table(age_dist) * 100, 2))
cat("\n")

# Annual Income demographics
cat("==================== INCOME DEMOGRAPHICS ====================\n")
df$annual_income <- as.numeric(df$annual_income)
cat("Annual Income Statistics (USD):\n")
cat("  Mean:", round(mean(df$annual_income, na.rm = TRUE), 2), "\n")
cat("  Median:", median(df$annual_income, na.rm = TRUE), "\n")
cat("  Min:", min(df$annual_income, na.rm = TRUE), "\n")
cat("  Max:", max(df$annual_income, na.rm = TRUE), "\n")
cat("  Standard Deviation:", round(sd(df$annual_income, na.rm = TRUE), 2), "\n\n")

# Income brackets
df$income_bracket <- cut(df$annual_income, 
                         breaks = c(0, 25000, 50000, 75000, 100000, 150000, Inf),
                         labels = c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-150k", "150k+"))
income_dist <- table(df$income_bracket, useNA = "ifany")
cat("Income Bracket Distribution:\n")
print(income_dist)
cat("\nIncome Bracket Percentages:\n")
print(round(prop.table(income_dist) * 100, 2))
cat("\n")

# Home Ownership
cat("==================== HOME OWNERSHIP ====================\n")
home_dist <- table(df$home_Ownship, useNA = "ifany")
cat("Home Ownership Distribution:\n")
print(home_dist)
cat("\nHome Ownership Percentages:\n")
print(round(prop.table(home_dist) * 100, 2))
cat("\n")

# Region Demographics
cat("==================== REGION DEMOGRAPHICS ====================\n")
region_dist <- table(df$region, useNA = "ifany")
cat("Region Distribution:\n")
print(region_dist)
cat("\nRegion Percentages:\n")
print(round(prop.table(region_dist) * 100, 2))
cat("\n")

# Employment Length
cat("==================== EMPLOYMENT LENGTH ====================\n")
df$employment_length_years <- as.numeric(df$employment_length_years)
cat("Employment Length Statistics (years):\n")
cat("  Mean:", round(mean(df$employment_length_years, na.rm = TRUE), 2), "\n")
cat("  Median:", median(df$employment_length_years, na.rm = TRUE), "\n")
cat("  Min:", min(df$employment_length_years, na.rm = TRUE), "\n")
cat("  Max:", max(df$employment_length_years, na.rm = TRUE), "\n")
cat("  Standard Deviation:", round(sd(df$employment_length_years, na.rm = TRUE), 2), "\n\n")

# Employment length groups
df$emp_group <- cut(df$employment_length_years, 
                    breaks = c(0, 2, 5, 10, 20, Inf),
                    labels = c("<2 years", "2-5 years", "5-10 years", "10-20 years", "20+ years"))
emp_dist <- table(df$emp_group, useNA = "ifany")
cat("Employment Length Distribution:\n")
print(emp_dist)
cat("\n")

# Loan Purpose
cat("==================== LOAN PURPOSE ====================\n")
# Clean loan_purpose by standardizing
df$loan_purpose_clean <- tolower(df$loan_purpose)
df$loan_purpose_clean <- gsub("_", " ", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("debt consolidation", "debt_consolidation", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("home improvement", "home_improvement", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("major purchase", "major_purchase", df$loan_purpose_clean)
df$loan_purpose_clean <- gsub("small business", "small_business", df$loan_purpose_clean)

purpose_dist <- table(df$loan_purpose_clean, useNA = "ifany")
cat("Loan Purpose Distribution:\n")
print(purpose_dist)
cat("\nLoan Purpose Percentages:\n")
print(round(prop.table(purpose_dist) * 100, 2))
cat("\n")

# Employment by Region
cat("==================== EMPLOYMENT BY REGION ====================\n")
emp_by_region <- df %>%
  group_by(region) %>%
  summarise(
    Count = n(),
    Avg_Employment_Years = round(mean(employment_length_years, na.rm = TRUE), 2),
    Median_Employment_Years = median(employment_length_years, na.rm = TRUE)
  )
print(emp_by_region)
cat("\n")

# Income by Region
cat("==================== INCOME BY REGION ====================\n")
income_by_region <- df %>%
  group_by(region) %>%
  summarise(
    Count = n(),
    Avg_Income = round(mean(annual_income, na.rm = TRUE), 2),
    Median_Income = median(annual_income, na.rm = TRUE),
    Min_Income = min(annual_income, na.rm = TRUE),
    Max_Income = max(annual_income, na.rm = TRUE)
  )
print(income_by_region)
cat("\n")

# Phone Verification
cat("==================== PHONE VERIFICATION ====================\n")
phone_dist <- table(df$phone_verified, useNA = "ifany")
cat("Phone Verification Distribution:\n")
print(phone_dist)
cat("\nPhone Verification Percentages:\n")
print(round(prop.table(phone_dist) * 100, 2))
cat("\n")

# Train/Test Split
cat("==================== DATA SPLIT (TRAIN/TEST) ====================\n")
split_dist <- table(df$set, useNA = "ifany")
cat("Dataset Split Distribution:\n")
print(split_dist)
cat("\nDataset Split Percentages:\n")
print(round(prop.table(split_dist) * 100, 2))
cat("\n")

# Loan Amount Demographics
cat("==================== LOAN AMOUNT DEMOGRAPHICS ====================\n")
df$loan_amount <- as.numeric(df$loan_amount)
cat("Loan Amount Statistics (USD):\n")
cat("  Mean:", round(mean(df$loan_amount, na.rm = TRUE), 2), "\n")
cat("  Median:", median(df$loan_amount, na.rm = TRUE), "\n")
cat("  Min:", min(df$loan_amount, na.rm = TRUE), "\n")
cat("  Max:", max(df$loan_amount, na.rm = TRUE), "\n")
cat("  Standard Deviation:", round(sd(df$loan_amount, na.rm = TRUE), 2), "\n\n")

# Loan amount brackets
df$loan_bracket <- cut(df$loan_amount, 
                       breaks = c(0, 5000, 10000, 15000, 20000, 30000, Inf),
                       labels = c("<5k", "5k-10k", "10k-15k", "15k-20k", "20k-30k", "30k+"))
loan_dist <- table(df$loan_bracket, useNA = "ifany")
cat("Loan Amount Distribution:\n")
print(loan_dist)
cat("\nLoan Amount Percentages:\n")
print(round(prop.table(loan_dist) * 100, 2))
cat("\n")

# Email Domain Types
cat("==================== EMAIL DOMAIN TYPES ====================\n")
email_dist <- table(df$email_domain_type, useNA = "ifany")
cat("Email Domain Distribution:\n")
print(email_dist)
cat("\nEmail Domain Percentages:\n")
print(round(prop.table(email_dist) * 100, 2))
cat("\n")

# Application Day of Week
cat("==================== APPLICATION DAY OF WEEK ====================\n")
dow_dist <- table(df$application_dow, useNA = "ifany")
cat("Application Day Distribution:\n")
print(dow_dist)
cat("\nApplication Day Percentages:\n")
print(round(prop.table(dow_dist) * 100, 2))
cat("\n")

# Cross-tabulation: Default Rate by Demographics
cat("==================== DEFAULT RATE BY DEMOGRAPHICS ====================\n")

# Default rate by age group
default_by_age <- df %>%
  filter(!is.na(age_group), !is.na(default_flag)) %>%
  group_by(age_group) %>%
  summarise(
    Total = n(),
    Defaults = sum(default_flag == 1, na.rm = TRUE),
    Default_Rate = round(mean(default_flag == 1, na.rm = TRUE) * 100, 2)
  ) %>%
  arrange(desc(Default_Rate))
cat("Default Rate by Age Group:\n")
print(default_by_age)
cat("\n")

# Default rate by income bracket
default_by_income <- df %>%
  filter(!is.na(income_bracket), !is.na(default_flag)) %>%
  group_by(income_bracket) %>%
  summarise(
    Total = n(),
    Defaults = sum(default_flag == 1, na.rm = TRUE),
    Default_Rate = round(mean(default_flag == 1, na.rm = TRUE) * 100, 2)
  ) %>%
  arrange(desc(Default_Rate))
cat("Default Rate by Income Bracket:\n")
print(default_by_income)
cat("\n")

# Default rate by home ownership
default_by_home <- df %>%
  filter(!is.na(home_Ownship), !is.na(default_flag)) %>%
  group_by(home_Ownship) %>%
  summarise(
    Total = n(),
    Defaults = sum(default_flag == 1, na.rm = TRUE),
    Default_Rate = round(mean(default_flag == 1, na.rm = TRUE) * 100, 2)
  ) %>%
  arrange(desc(Default_Rate))
cat("Default Rate by Home Ownership:\n")
print(default_by_home)
cat("\n")

# Default rate by region
default_by_region <- df %>%
  filter(!is.na(region), !is.na(default_flag)) %>%
  group_by(region) %>%
  summarise(
    Total = n(),
    Defaults = sum(default_flag == 1, na.rm = TRUE),
    Default_Rate = round(mean(default_flag == 1, na.rm = TRUE) * 100, 2)
  ) %>%
  arrange(desc(Default_Rate))
cat("Default Rate by Region:\n")
print(default_by_region)
cat("\n")

# Default rate by loan purpose
default_by_purpose <- df %>%
  filter(!is.na(loan_purpose_clean), !is.na(default_flag)) %>%
  group_by(loan_purpose_clean) %>%
  summarise(
    Total = n(),
    Defaults = sum(default_flag == 1, na.rm = TRUE),
    Default_Rate = round(mean(default_flag == 1, na.rm = TRUE) * 100, 2)
  ) %>%
  arrange(desc(Default_Rate))
cat("Default Rate by Loan Purpose:\n")
print(default_by_purpose)
cat("\n")

# Summary of Missing Values
cat("==================== MISSING VALUES SUMMARY ====================\n")
missing_summary <- data.frame(
  Column = names(df),
  Missing_Count = sapply(df, function(x) sum(is.na(x) | x == "" | x == "NA")),
  Missing_Percentage = round(sapply(df, function(x) sum(is.na(x) | x == "" | x == "NA") / nrow(df) * 100), 2)
)
missing_summary <- missing_summary[missing_summary$Missing_Count > 0, ]
missing_summary <- missing_summary[order(-missing_summary$Missing_Count), ]
print(missing_summary)

cat("\n==================== ANALYSIS COMPLETE ====================\n")






# ============================================
# ADVANCED MISSING DATA ANALYSIS FOR CREDIT MODELING
# ============================================

# 1. Create missing indicators
cat("==================== CREATING MISSING INDICATORS ====================\n")
df$miss_delinquency <- ifelse(is.na(df$months_since_last_delinquency) | 
                                df$months_since_last_delinquency == "", 1, 0)
df$miss_employment <- ifelse(is.na(df$employment_length_years) | 
                               df$employment_length_years == "", 1, 0)
df$miss_income <- ifelse(is.na(df$annual_income) | 
                           df$annual_income == "", 1, 0)
df$miss_open_accounts <- ifelse(is.na(df$num_open_accounts) | 
                                  df$num_open_accounts == "", 1, 0)

# 2. Default rate by missing delinquency information
cat("==================== MISSING DELINQUENCY INFO VS DEFAULT ====================\n")
delinquency_missing_analysis <- df %>%
  filter(!is.na(default_flag)) %>%
  group_by(miss_delinquency) %>%
  summarise(
    Count = n(),
    Defaults = sum(default_flag == 1),
    Default_Rate = round(mean(default_flag == 1) * 100, 2),
    Avg_Age = round(mean(age, na.rm = TRUE), 1),
    Avg_Income = round(mean(annual_income, na.rm = TRUE), 0)
  )
print(delinquency_missing_analysis)
cat("\nInterpretation: If default rate is HIGHER among missing, it suggests lenders avoid reporting for risky borrowers\n")

# 3. Default rate by missing employment info
cat("==================== MISSING EMPLOYMENT INFO VS DEFAULT ====================\n")
employment_missing_analysis <- df %>%
  filter(!is.na(default_flag)) %>%
  group_by(miss_employment) %>%
  summarise(
    Count = n(),
    Defaults = sum(default_flag == 1),
    Default_Rate = round(mean(default_flag == 1) * 100, 2),
    Avg_Age = round(mean(age, na.rm = TRUE), 1),
    Avg_Loan = round(mean(loan_amount, na.rm = TRUE), 0)
  )
print(employment_missing_analysis)
cat("\n")

# 4. Default rate by missing income info
cat("==================== MISSING INCOME INFO VS DEFAULT ====================\n")
income_missing_analysis <- df %>%
  filter(!is.na(default_flag)) %>%
  group_by(miss_income) %>%
  summarise(
    Count = n(),
    Defaults = sum(default_flag == 1),
    Default_Rate = round(mean(default_flag == 1) * 100, 2),
    Avg_Age = round(mean(age, na.rm = TRUE), 1),
    Avg_Loan = round(mean(loan_amount, na.rm = TRUE), 0)
  )
print(income_missing_analysis)
cat("\n")

# 5. Cross-analysis: Missing patterns and default rates
cat("==================== COMBINED MISSING PATTERNS ====================\n")
df$missing_pattern <- paste(
  ifelse(df$miss_delinquency == 1, "D", "-"),
  ifelse(df$miss_employment == 1, "E", "-"),
  ifelse(df$miss_income == 1, "I", "-"),
  sep = ""
)

pattern_analysis <- df %>%
  filter(!is.na(default_flag), missing_pattern != "---") %>%
  group_by(missing_pattern) %>%
  summarise(
    Count = n(),
    Defaults = sum(default_flag == 1),
    Default_Rate = round(mean(default_flag == 1) * 100, 2),
    Pct_of_Total = round(n() / nrow(filter(df, !is.na(default_flag))) * 100, 2)
  ) %>%
  arrange(desc(Default_Rate))

print(pattern_analysis)
cat("\n")

# 6. For those with missing delinquency: What's their credit profile?
cat("==================== CREDIT PROFILE OF THOSE WITH MISSING DELINQUENCY ====================\n")
credit_profile <- df %>%
  filter(miss_delinquency == 1, !is.na(default_flag)) %>%
  summarise(
    Count = n(),
    Default_Rate = round(mean(default_flag == 1) * 100, 2),
    Avg_Credit_Util = round(mean(credit_utilisation_pct, na.rm = TRUE), 1),
    Avg_Num_Accounts = round(mean(num_open_accounts, na.rm = TRUE), 1),
    Avg_Inquiries = round(mean(num_hard_inquiries_6mo, na.rm = TRUE), 1),
    Pct_No_Delinquencies_2yr = round(sum(num_delinquencies_2yr == 0, na.rm = TRUE) / n() * 100, 1)
  )
print(credit_profile)

# Compare with those who HAVE delinquency info
credit_profile_known <- df %>%
  filter(miss_delinquency == 0, !is.na(default_flag)) %>%
  summarise(
    Count = n(),
    Default_Rate = round(mean(default_flag == 1) * 100, 2),
    Avg_Credit_Util = round(mean(credit_utilisation_pct, na.rm = TRUE), 1),
    Avg_Num_Accounts = round(mean(num_open_accounts, na.rm = TRUE), 1),
    Avg_Inquiries = round(mean(num_hard_inquiries_6mo, na.rm = TRUE), 1),
    Pct_No_Delinquencies_2yr = round(sum(num_delinquencies_2yr == 0, na.rm = TRUE) / n() * 100, 1)
  )

cat("\nCOMPARISON - Known vs Missing Delinquency Info:\n")
cat("Metric                          | Known Delinquency | Missing Delinquency\n")
cat("--------------------------------|-------------------|-------------------\n")
cat(sprintf("Default Rate %%                 | %15.2f | %17.2f\n", 
            credit_profile_known$Default_Rate, credit_profile$Default_Rate))
cat(sprintf("Credit Utilization %%           | %15.1f | %17.1f\n", 
            credit_profile_known$Avg_Credit_Util, credit_profile$Avg_Credit_Util))
cat(sprintf("Num Open Accounts              | %15.1f | %17.1f\n", 
            credit_profile_known$Avg_Num_Accounts, credit_profile$Avg_Num_Accounts))
cat(sprintf("Hard Inquiries (6mo)           | %15.1f | %17.1f\n", 
            credit_profile_known$Avg_Inquiries, credit_profile$Avg_Inquiries))
cat(sprintf("Pct with 0 Delinquencies (2yr) | %15.1f | %17.1f\n", 
            credit_profile_known$Pct_No_Delinquencies_2yr, credit_profile$Pct_No_Delinquencies_2yr))

# 7. Does missing data correlate with other risk factors?
cat("\n==================== MISSING DATA CORRELATIONS ====================\n")
correlations <- df %>%
  summarise(
    cor_delinq_default = cor(miss_delinquency, default_flag, use = "complete.obs"),
    cor_emp_default = cor(miss_employment, default_flag, use = "complete.obs"),
    cor_income_default = cor(miss_income, default_flag, use = "complete.obs"),
    cor_delinq_age = cor(miss_delinquency, age, use = "complete.obs"),
    cor_emp_age = cor(miss_employment, age, use = "complete.obs")
  )
print(round(correlations, 3))
cat("\nPositive correlation = missing values associated with higher defaults\n")

# 8. Age distribution of missing data
cat("==================== MISSING DATA BY AGE GROUP ====================\n")
age_missing <- df %>%
  filter(!is.na(age_group)) %>%
  group_by(age_group) %>%
  summarise(
    Total = n(),
    Pct_Missing_Delinquency = round(mean(miss_delinquency) * 100, 1),
    Pct_Missing_Employment = round(mean(miss_employment) * 100, 1),
    Pct_Missing_Income = round(mean(miss_income) * 100, 1)
  )
print(age_missing)
cat("\n")

# 9. Loan purpose and missing data
cat("==================== MISSING DATA BY LOAN PURPOSE ====================\n")
purpose_missing <- df %>%
  filter(!is.na(loan_purpose_clean), loan_purpose_clean != "0") %>%
  group_by(loan_purpose_clean) %>%
  summarise(
    Total = n(),
    Pct_Missing_Delinquency = round(mean(miss_delinquency) * 100, 1),
    Default_Rate = round(mean(default_flag == 1, na.rm = TRUE) * 100, 1)
  ) %>%
  arrange(desc(Pct_Missing_Delinquency))
print(purpose_missing)

# 10. RECOMMENDATIONS FOR MODELING
cat("\n==================== MODELING RECOMMENDATIONS ====================\n")
cat("Based on the analysis:\n\n")

# Check if missing delinquency predicts default
delinq_missing_default <- delinquency_missing_analysis$Default_Rate[
  delinquency_missing_analysis$miss_delinquency == 1] > 
  delinquency_missing_analysis$Default_Rate[
    delinquency_missing_analysis$miss_delinquency == 0]

if(delinq_missing_default) {
  cat("✓ MISSING DELINQUENCY INFO is predictive - Keep as a feature (e.g., 'no_delinquency_record')\n")
  cat("  → Suggests these may be 'no prior credit' or 'thin file' borrowers\n")
} else {
  cat("✗ Missing delinquency info not strongly predictive\n")
}

# Check if missing employment predicts default
emp_missing_default <- employment_missing_analysis$Default_Rate[
  employment_missing_analysis$miss_employment == 1] > 
  employment_missing_analysis$Default_Rate[
    employment_missing_analysis$miss_employment == 0]

if(emp_missing_default) {
  cat("✓ MISSING EMPLOYMENT INFO is predictive - Create flag 'employment_not_reported'\n")
  cat("  → Unemployed or self-employed applicants may be higher risk\n")
}

# Check if missing income predicts default
inc_missing_default <- income_missing_analysis$Default_Rate[
  income_missing_analysis$miss_income == 1] > 
  income_missing_analysis$Default_Rate[
    income_missing_analysis$miss_income == 0]

if(inc_missing_default) {
  cat("✓ MISSING INCOME INFO is predictive - Create flag 'income_not_disclosed'\n")
  cat("  → Income not disclosed often correlates with payment difficulties\n")
}

cat("\nFINAL RECOMMENDATIONS:\n")
cat("1. DO NOT simply drop or impute missing values - they are informative\n")
cat("2. Create binary flags for each missing data pattern\n")
cat("3. For numeric imputation, consider imputing BY DEFAULT STATUS separately\n")
cat("4. Test interaction terms: missing_delinquency × loan_purpose\n")
cat("5. 'months_since_last_delinquency' missing (50%) may indicate 'no credit history' - valuable signal!\n")

cat("\n==================== ANALYSIS COMPLETE ====================\n")

