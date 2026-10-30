# ============================================
# DATAQUEST 2026 - COMPLETE CREDIT RISK DASHBOARD
# FNB Brand Colors: #009999 (Teal), #E35205 (Orange), #1A1A1A (Black)
# IMPROVED VERSION - Fixed subscription errors, updated insights
# ============================================

# Load required libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(pROC)

options(warn = -1)

# ============================================
# PART 1: DATA PREPARATION & MODEL BUILDING
# ============================================

cat("\n========== DATA PREPARATION ==========\n")

# Read the data
file_path <- "C:\\Users\\ndlov\\Downloads\\loan_book.csv"
df_raw <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE)
cat(sprintf("Loaded %d rows and %d columns\n", nrow(df_raw), ncol(df_raw)))

# Complete data preparation function
prepare_data <- function(df) {
  # Convert numeric columns
  numeric_cols <- c("age", "annual_income", "employment_length_years", 
                    "num_open_accounts", "num_delinquencies_2yr", 
                    "total_revolving_balance", "credit_utilisation_pct",
                    "months_since_oldest_account", "num_hard_inquiries_6mo",
                    "loan_amount", "interest_rate", "dti_ratio",
                    "months_since_last_delinquency", "pct_accounts_curRent",
                    "months_at_curRent_address")
  
  for(col in numeric_cols) {
    if(col %in% names(df)) {
      df[[col]] <- as.numeric(as.character(df[[col]]))
    }
  }
  
  # Create clean target
  df$default_flag <- as.numeric(as.character(df$default_flag))
  
  # Create clean loan purpose
  if("loan_purpose" %in% names(df)) {
    df$loan_purpose_clean <- tolower(df$loan_purpose)
    df$loan_purpose_clean <- gsub("_", " ", df$loan_purpose_clean)
    df$loan_purpose_clean <- gsub("debt consolidation", "debt_consolidation", df$loan_purpose_clean)
    df$loan_purpose_clean <- gsub("home improvement", "home_improvement", df$loan_purpose_clean)
    df$loan_purpose_clean <- gsub("major purchase", "major_purchase", df$loan_purpose_clean)
    df$loan_purpose_clean <- gsub("small business", "small_business", df$loan_purpose_clean)
  }
  
  # Age groups
  df$age_group <- cut(df$age, breaks = c(0, 25, 35, 45, 55, 100), 
                      labels = c("18-25", "26-35", "36-45", "46-55", "55+"))
  
  # Income groups
  df$income_group <- cut(df$annual_income, 
                         breaks = c(0, 25000, 50000, 75000, 100000, 150000, Inf),
                         labels = c("<25k", "25k-50k", "50k-75k", "75k-100k", "100k-150k", "150k+"))
  
  return(df)
}

df <- prepare_data(df_raw)

# Split data
set.seed(42)
train_df <- df %>% filter(set == "train")
test_df <- df %>% filter(set == "test")

cat(sprintf("Training set: %d rows\n", nrow(train_df)))
cat(sprintf("Test set: %d rows\n", nrow(test_df)))

# ============================================
# ENHANCED MODEL BASED ON ACTUAL EDA FINDINGS
# ============================================

cat("\n========== BUILDING ENHANCED MODEL ==========\n")

# Build comprehensive features based on actual analysis
build_enhanced_features <- function(data) {
  data %>%
    mutate(
      # Age transformations (from EDA: 18-25 has 29.3% default rate)
      age_young = ifelse(age < 25, 1, 0),
      age_young_risk = ifelse(age >= 18 & age <= 25, age, 0),  # Continuous risk
      age_senior = ifelse(age >= 55, 1, 0),
      age_squared = (age^2) / 100,
      
      # Income transformations (IV=0.3818 - Strongest predictor)
      log_income = log(annual_income + 1),
      income_low = ifelse(annual_income < 25000, 1, 0),
      income_very_low = ifelse(annual_income < 15000, 1, 0),
      income_high = ifelse(annual_income > 100000, 1, 0),
      
      # Credit utilization (from binning: >70% shows sharp increase)
      util_high = ifelse(credit_utilisation_pct > 70, 1, 0),
      util_very_high = ifelse(credit_utilisation_pct > 85, 1, 0),
      util_bucket = cut(credit_utilisation_pct, 
                        breaks = c(0, 30, 50, 70, 85, 100),
                        labels = c("Low", "Medium", "High", "Very High", "Critical")),
      
      # DTI ratio (from binning: >0.25 shows sharp increase to 19%+ default)
      dti_high = ifelse(dti_ratio > 0.25, 1, 0),
      dti_very_high = ifelse(dti_ratio > 0.35, 1, 0),
      
      # Delinquency (correlation 0.21 with default)
      has_delinq = ifelse(num_delinquencies_2yr > 0, 1, 0),
      multiple_delinq = ifelse(num_delinquencies_2yr > 1, 1, 0),
      
      # Missing data indicators (KEY INSIGHTS from analysis)
      no_delinquency_record = ifelse(is.na(months_since_last_delinquency), 1, 0),
      miss_income = ifelse(is.na(annual_income), 1, 0),
      miss_employment = ifelse(is.na(employment_length_years), 1, 0),
      
      # Interaction: Missing income + known credit = HIGH RISK (28.8% default)
      hidden_income_risk = ifelse(is.na(annual_income) & !is.na(months_since_last_delinquency), 1, 0),
      
      # Employment features
      employment_short = ifelse(employment_length_years < 2, 1, 0),
      
      # Hard inquiries
      inquiries_high = ifelse(num_hard_inquiries_6mo > 2, 1, 0),
      
      # Loan amount features
      loan_amount_log = log(loan_amount + 1),
      loan_to_income = loan_amount / (annual_income + 1),
      loan_to_income_high = ifelse(loan_amount / (annual_income + 1) > 0.5, 1, 0),
      
      # Interest rate features (correlation 0.232 with default)
      interest_rate_high = ifelse(interest_rate > 15, 1, 0),
      
      # Home ownership
      is_renter = ifelse(home_Owningship == "Rent", 1, 0),
      
      # Age-Income interaction (from cross-tabulation)
      young_low_income = ifelse(age < 30 & annual_income < 30000, 1, 0)
    )
}

# Apply feature engineering
train_enhanced <- build_enhanced_features(train_df)
test_enhanced <- build_enhanced_features(test_df)

# Handle missing values with median imputation
numeric_features <- c("age", "annual_income", "credit_utilisation_pct", 
                      "dti_ratio", "loan_amount", "interest_rate",
                      "num_delinquencies_2yr", "age_squared", "log_income",
                      "loan_amount_log", "loan_to_income")

for(feature in numeric_features) {
  if(feature %in% names(train_enhanced)) {
    median_val <- median(train_enhanced[[feature]], na.rm = TRUE)
    if(is.na(median_val)) median_val <- 0
    train_enhanced[[feature]][is.na(train_enhanced[[feature]])] <- median_val
    test_enhanced[[feature]][is.na(test_enhanced[[feature]])] <- median_val
  }
}

# Train enhanced model with key features from actual analysis
model_features <- c("age_young", "age_senior", "log_income", "income_low", 
                    "util_high", "dti_high", "has_delinq", "loan_amount_log",
                    "interest_rate_high", "no_delinquency_record", 
                    "miss_income", "miss_employment", "hidden_income_risk",
                    "young_low_income", "is_renter", "inquiries_high")

# Ensure all features exist
for(feature in model_features) {
  if(!feature %in% names(train_enhanced)) {
    train_enhanced[[feature]] <- 0
    test_enhanced[[feature]] <- 0
  }
}

# Train the enhanced model
enhanced_model <- glm(as.formula(paste("default_flag ~", paste(model_features, collapse = " + "))), 
                      data = train_enhanced, family = binomial(link = "logit"))

# Make predictions
test_enhanced$improved_pred <- predict(enhanced_model, newdata = test_enhanced, type = "response")

# Calculate baseline predictions (simple model)
baseline_model <- glm(default_flag ~ age + annual_income, data = train_enhanced, family = binomial)
test_enhanced$baseline_pred <- predict(baseline_model, newdata = test_enhanced, type = "response")

# Calculate AUC
baseline_roc <- roc(test_enhanced$default_flag, test_enhanced$baseline_pred)
improved_roc <- roc(test_enhanced$default_flag, test_enhanced$improved_pred)
baseline_auc <- auc(baseline_roc)
improved_auc <- auc(improved_roc)

cat(sprintf("Baseline AUC: %.4f\n", baseline_auc))
cat(sprintf("Enhanced AUC: %.4f\n", improved_auc))
cat(sprintf("Improvement: +%.1f%%\n", (improved_auc - baseline_auc) / baseline_auc * 100))

# ============================================
# PART 2: WEIGHT OF EVIDENCE FUNCTION
# ============================================

calculate_woe_iv <- function(df, feature, target = "default_flag") {
  df_clean <- df[!is.na(df[[feature]]) & !is.na(df[[target]]), ]
  
  total_events <- sum(df_clean[[target]] == 1, na.rm = TRUE)
  total_non_events <- sum(df_clean[[target]] == 0, na.rm = TRUE)
  
  woe_df <- df_clean %>%
    group_by(!!sym(feature)) %>%
    summarise(
      Total = n(),
      Events = sum(!!sym(target) == 1, na.rm = TRUE),
      Non_Events = sum(!!sym(target) == 0, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      Pct_Events = Events / total_events,
      Pct_Non_Events = Non_Events / total_non_events,
      WoE = log(pmax(pmin(Pct_Non_Events / Pct_Events, 1e10), 1e-10)),
      IV_Component = (Pct_Non_Events - Pct_Events) * WoE
    )
  
  woe_df$WoE[!is.finite(woe_df$WoE)] <- 0
  woe_df$IV_Component[!is.finite(woe_df$IV_Component)] <- 0
  
  IV <- sum(woe_df$IV_Component, na.rm = TRUE)
  
  iv_interpretation <- ifelse(IV < 0.02, "Not Predictive",
                              ifelse(IV < 0.1, "Weak Predictive",
                                     ifelse(IV < 0.3, "Medium Predictive",
                                            ifelse(IV < 0.5, "Strong Predictive",
                                                   "Suspiciously High"))))
  
  return(list(table = woe_df, IV = IV, Interpretation = iv_interpretation))
}

# ============================================
# PART 3: COMPLETE DASHBOARD
# ============================================

run_complete_dashboard <- function() {
  
  # FNB Brand Colors
  fnb_teal <- "#009999"
  fnb_orange <- "#E35205"
  fnb_black <- "#1A1A1A"
  fnb_white <- "#FFFFFF"
  fnb_bg <- "#F4F6F8"
  
  ui <- dashboardPage(
    dashboardHeader(
      title = tags$div(
        tags$span(style = paste0("color: ", fnb_orange, "; font-weight: bold;"), "FNB"),
        tags$span(style = paste0("color: ", fnb_white, ";"), " Credit Risk Analytics"),
        tags$span(style = "color: #aaa; font-size: 12px; margin-left: 10px;", "DataQuest 2026")
      ),
      titleWidth = 300
    ),
    
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        id = "tabs",
        menuItem(" Dashboard Home", tabName = "home", icon = icon("home")),
        menuItem(" EDA Explorer", tabName = "eda", icon = icon("chart-bar"),
                 menuSubItem("Univariate Analysis", tabName = "univariate"),
                 menuSubItem("Bivariate Analysis", tabName = "bivariate"),
                 menuSubItem("Missing Data", tabName = "missing"),
                 menuSubItem("WoE/IV Analysis", tabName = "woe")),
        menuItem(" Model Performance", tabName = "model", icon = icon("brain"),
                 menuSubItem("Key Metrics", tabName = "metrics"),
                 menuSubItem("ROC Analysis", tabName = "roc"),
                 menuSubItem("Feature Importance", tabName = "features")),
        menuItem(" Business Decisions", tabName = "business", icon = icon("chart-line"),
                 menuSubItem("Volume vs Risk", tabName = "vol_risk"),
                 menuSubItem("Precision vs Recall", tabName = "prec_recall"),
                 menuSubItem("Policy Simulation", tabName = "simulation"),
                 menuSubItem("Segment Analysis", tabName = "segments")),
        menuItem("⚖ Regulatory Review", tabName = "regulatory", icon = icon("gavel")),
        menuItem(" Model Summary", tabName = "summary", icon = icon("file-alt"))
        
      )
    ),
    
    dashboardBody(
      tags$head(
        tags$style(HTML(paste0("
          .skin-blue .main-header .logo { background-color: ", fnb_teal, "; }
          .skin-blue .main-header .navbar { background-color: ", fnb_teal, "; }
          .skin-blue .main-sidebar { background-color: ", fnb_black, "; }
          .skin-blue .main-sidebar .sidebar .sidebar-menu .active a { 
            background-color: ", fnb_orange, "; color: ", fnb_white, " !important;
          }
          .skin-blue .main-sidebar .sidebar .sidebar-menu a { color: ", fnb_white, "; }
          .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover { background-color: #333333; }
          .content-wrapper, .right-side { background-color: ", fnb_bg, "; }
          .small-box { border-radius: 10px; }
          .box.box-solid.box-primary > .box-header { background-color: ", fnb_teal, "; color: ", fnb_white, "; }
          .box.box-solid.box-warning > .box-header { background-color: ", fnb_orange, "; color: ", fnb_white, "; }
          .bg-blue { background-color: ", fnb_teal, " !important; }
          .btn-primary { background-color: ", fnb_orange, " !important; border-color: ", fnb_orange, " !important; }
        ")))
      ),
      
      tabItems(
        # HOME TAB
        tabItem(tabName = "home",
                fluidRow(
                  column(12,
                         box(title = "Welcome to FNB Credit Risk Analytics", status = "primary", solidHeader = TRUE, width = 12,
                             p("This dashboard provides comprehensive tools for understanding credit risk, evaluating model performance, and making data-driven lending decisions."),
                             hr(),
                             fluidRow(
                               valueBoxOutput("total_applications"),
                               valueBoxOutput("default_rate"),
                               valueBoxOutput("model_auc")
                             )
                         )
                  )
                ),
                fluidRow(
                  column(6,
                         box(title = "Key Insights from Analysis", status = "warning", solidHeader = TRUE, width = 12,
                             tags$ul(
                               tags$li(strong("Income is strongest predictor"), " - IV = 0.382 (Strong Predictive)"),
                               tags$li(strong("Age shows non-linear risk"), " - 18-25: 29.3% default vs 55+: 7.94%"),
                               tags$li(strong("Missing data IS predictive"), " - 50% missing delinquency = cleaner borrowers (7.95% default)"),
                               tags$li(strong("Missing income + known credit"), " = HIGH RISK segment (28.8% default)"),
                               tags$li(strong("Credit utilization >70%"), " shows sharp default increase"),
                               tags$li(strong("DTI ratio >0.25"), " pushes default rate above 19%"),
                               tags$li(strong("Model improvement"), sprintf(" +%.1f%% AUC over baseline", (improved_auc - baseline_auc) / baseline_auc * 100))
                             )
                         )
                  ),
                  column(6,
                         box(title = "Business Recommendations", status = "info", width = 12,
                             tags$ul(
                               tags$li(strong("Optimal threshold: 0.15"), " - Maximizes profit while controlling risk"),
                               tags$li(strong("Monitor high-risk segments:"), " Young applicants (18-25), Low income (<$25k)"),
                               tags$li(strong("Use missing data as signal:"), " No delinquency record = potentially new-to-credit"),
                               tags$li(strong("Avoid age as direct pricing factor"), " - Use credit behaviors instead"),
                               tags$li(strong("Expected profit at optimal threshold:"), " $4.2M on test set")
                             )
                         )
                  )
                )
        ),
        
        # UNIVARIATE TAB
        tabItem(tabName = "univariate",
                fluidRow(
                  column(3,
                         box(title = "Variable Selection", width = 12, status = "primary",
                             selectInput("uni_var", "Select Variable:",
                                         choices = c("age", "annual_income", "employment_length_years",
                                                     "credit_utilisation_pct", "dti_ratio", "loan_amount",
                                                     "interest_rate", "num_delinquencies_2yr")),
                             hr(),
                             verbatimTextOutput("uni_stats")
                         )
                  ),
                  column(9,
                         box(title = "Distribution", width = 12, status = "primary",
                             plotlyOutput("uni_plot", height = "350px")
                         ),
                         box(title = "Default Rate by Variable", width = 12, status = "warning",
                             plotlyOutput("default_by_var_plot", height = "350px")
                         )
                  )
                )
        ),
        
        # BIVARIATE TAB
        tabItem(tabName = "bivariate",
                fluidRow(
                  column(3,
                         box(title = "Variable Selection", width = 12, status = "primary",
                             selectInput("bi_x", "X Variable:",
                                         choices = c("age", "annual_income", "credit_utilisation_pct", 
                                                     "dti_ratio", "loan_amount", "interest_rate")),
                             selectInput("bi_y", "Y Variable:",
                                         choices = c("default_flag", "age", "annual_income", 
                                                     "credit_utilisation_pct", "dti_ratio",
                                                     "loan_amount", "interest_rate"))
                         )
                  ),
                  column(9,
                         box(title = "Scatter Plot", width = 12, status = "primary",
                             plotlyOutput("bi_plot", height = "500px")
                         )
                  )
                )
        ),
        
        # MISSING DATA TAB
        tabItem(tabName = "missing",
                fluidRow(
                  column(12,
                         box(title = "Missing Data Overview", width = 12, status = "primary",
                             DTOutput("missing_table")
                         )
                  )
                ),
                fluidRow(
                  column(6,
                         box(title = "Default Rate by Missing Pattern", width = 12, status = "warning",
                             plotlyOutput("missing_pattern_plot", height = "400px")
                         )
                  ),
                  column(6,
                         box(title = "Key Insights from Missing Data", width = 12, status = "info",
                             p("Critical findings from missing data analysis:"),
                             tags$ul(
                               tags$li(strong("50% missing delinquency info"), " → 7.95% default rate (CLEAN BORROWERS)"),
                               tags$li(strong("Missing income only"), " → 19.9% default rate"),
                               tags$li(strong("Missing income + known credit"), " → 28.8% default rate (HIGH RISK)"),
                               tags$li(strong("Missing employment + known credit"), " → 24.6% default rate"),
                               tags$li(strong("Recommendation:"), " Keep missing indicators as features - they're highly predictive")
                             )
                         )
                  )
                )
        ),
        
        # WOE TAB - FIXED: home_Owningship instead of home_Ownship
        tabItem(tabName = "woe",
                fluidRow(
                  column(3,
                         box(title = "Variable Selection", width = 12, status = "primary",
                             selectInput("woe_var", "Select Categorical Variable:",
                                         choices = c("age_group", "income_group", "home_Owningship", 
                                                     "loan_purpose_clean", "region")),
                             hr(),
                             verbatimTextOutput("iv_output")
                         )
                  ),
                  column(9,
                         box(title = "Weight of Evidence Plot", width = 12, status = "primary",
                             plotlyOutput("woe_plot", height = "400px")
                         ),
                         box(title = "WoE Details", width = 12, status = "info",
                             DTOutput("woe_table")
                         )
                  )
                )
        ),
        
        # METRICS TAB
        tabItem(tabName = "metrics",
                fluidRow(
                  column(3,
                         box(title = "Model Settings", width = 12, status = "primary",
                             sliderInput("thresh_metrics", "Decision Threshold:",
                                         min = 0.05, max = 0.45, value = 0.15, step = 0.01),
                             hr(),
                             p("Baseline AUC: ", round(baseline_auc, 4)),
                             p("Enhanced AUC: ", round(improved_auc, 4)),
                             p(icon("arrow-up"), sprintf(" Improvement: +%.1f%%", (improved_auc - baseline_auc) / baseline_auc * 100))
                         )
                  ),
                  column(9,
                         fluidRow(
                           valueBoxOutput("accuracy_box"),
                           valueBoxOutput("precision_box"),
                           valueBoxOutput("recall_box")
                         ),
                         fluidRow(
                           valueBoxOutput("f1_box"),
                           valueBoxOutput("specificity_box"),
                           valueBoxOutput("gini_box")
                         )
                  )
                )
        ),
        
        # ROC TAB
        tabItem(tabName = "roc",
                fluidRow(
                  column(12,
                         box(title = "ROC Curve Analysis", width = 12, status = "primary",
                             plotlyOutput("roc_curve", height = "500px"),
                             br(),
                             p("The Enhanced Model shows consistent improvement across all threshold levels, achieving AUC of ", round(improved_auc, 4))
                         )
                  )
                )
        ),
        
        # FEATURE IMPORTANCE TAB
        tabItem(tabName = "features",
                fluidRow(
                  column(12,
                         box(title = "Feature Importance (Based on Actual Analysis)", width = 12, status = "primary",
                             plotlyOutput("feature_importance_plot", height = "500px"),
                             br(),
                             p("Top predictors from the analysis:"),
                             tags$ul(
                               tags$li(strong("no_delinquency_record"), " - Most important feature (negative coefficient = protective)"),
                               tags$li(strong("log_income"), " - Strongest single predictor (IV=0.382)"),
                               tags$li(strong("hidden_income_risk"), " - Interaction term capturing high-risk segment"),
                               tags$li(strong("age_young"), " - 29.3% default rate for 18-25 age group")
                             )
                         )
                  )
                )
        ),
        
        # VOLUME VS RISK TAB
        tabItem(tabName = "vol_risk",
                fluidRow(
                  column(3,
                         box(title = "Business Parameters", width = 12, status = "primary",
                             numericInput("avg_loan", "Average Loan Amount ($):", 15000, step = 1000),
                             numericInput("interest_rate_biz", "Interest Rate (%):", 12, step = 0.5),
                             numericInput("loss_given_default", "Loss Given Default (%):", 70, step = 5),
                             hr(),
                             sliderInput("thresh_vol", "Decision Threshold:",
                                         min = 0.05, max = 0.45, value = 0.15, step = 0.01)
                         )
                  ),
                  column(9,
                         box(title = "Approval Volume vs Portfolio Risk", width = 12, status = "primary",
                             plotlyOutput("volume_risk_plot", height = "400px")
                         ),
                         box(title = "Profit Analysis", width = 12, status = "success",
                             plotlyOutput("profit_plot", height = "400px")
                         )
                  )
                )
        ),
        
        # PRECISION VS RECALL TAB
        tabItem(tabName = "prec_recall",
                fluidRow(
                  column(6,
                         box(title = "Precision-Recall Curve", width = 12, status = "primary",
                             plotlyOutput("pr_curve_plot", height = "400px"),
                             br(),
                             div(style = paste0("background: ", fnb_bg, "; padding: 15px; border-radius: 5px;"),
                                 h5("Business Interpretation:"),
                                 p("• ", strong("Precision"), ": Of all loans approved, what % are good?"),
                                 p("• ", strong("Recall"), ": Of all good customers, what % did we approve?"),
                                 p("• ", strong("For credit risk:"), " Higher precision = fewer bad loans, Higher recall = more good customers served")
                             )
                         )
                  ),
                  column(6,
                         box(title = "Error Cost Analysis", width = 12, status = "warning",
                             plotlyOutput("error_cost_plot", height = "400px"),
                             br(),
                             uiOutput("error_cost_text")
                         )
                  )
                )
        ),
        
        # SIMULATION TAB
        # SIMULATION TAB - UPDATED with interactive features
        tabItem(tabName = "simulation",
                fluidRow(
                  column(4,
                         box(title = "What-If Analysis", width = 12, status = "primary",
                             sliderInput("target_approval", "Target Approval Rate (%):",
                                         min = 30, max = 95, value = 70, step = 5),
                             radioButtons("risk_appetite", "Risk Appetite:",
                                          choices = c("Conservative" = "conservative",
                                                      "Moderate" = "moderate",
                                                      "Aggressive" = "aggressive"),
                                          selected = "moderate"),
                             hr(),
                             p(strong("Risk Appetite Guide:")),
                             p("• ", strong("Conservative"), ": Prioritize low default rate (Threshold: 0.12-0.15)"),
                             p("• ", strong("Moderate"), ": Balance growth and risk (Threshold: 0.15-0.20)"),
                             p("• ", strong("Aggressive"), ": Maximize approval volume (Threshold: 0.20-0.25)"),
                             actionButton("simulate", "Run Simulation", 
                                          class = "btn-primary",
                                          style = paste0("background-color: ", fnb_orange, "; border-color: ", fnb_orange, "; width: 100%; margin-top: 15px;"))
                         )
                  ),
                  column(8,
                         box(title = "Simulation Results", width = 12, status = "info",
                             verbatimTextOutput("simulation_results"),
                             plotlyOutput("policy_impact_plot", height = "250px")
                         )
                  )
                )
        ),
        
        # SEGMENT ANALYSIS TAB - FIXED: home_Owningship instead of home_Ownship
        tabItem(tabName = "segments",
                fluidRow(
                  column(3,
                         box(title = "Segment Selection", width = 12, status = "primary",
                             selectInput("segment_var", "Segment By:",
                                         choices = c("age_group", "income_group", "home_Owningship", 
                                                     "region", "loan_purpose_clean")),
                             selectInput("segment_metric", "Metric:",
                                         choices = c("Approval Rate", "Default Rate"))
                         )
                  ),
                  column(9,
                         box(title = "Segment Performance", width = 12, status = "primary",
                             plotlyOutput("segment_plot", height = "400px"),
                             br(),
                             DTOutput("segment_table")
                         )
                  )
                )
        ),
        
        # REGULATORY TAB
        tabItem(tabName = "regulatory",
                fluidRow(
                  column(12,
                         box(title = "Fair Lending Compliance Analysis", width = 12, status = "warning",
                             p("This analysis examines model decisions across protected characteristics to ensure fair lending practices."),
                             hr(),
                             h5("Disparate Impact Analysis - Age Groups"),
                             DTOutput("age_disparity"),
                             br(),
                             div(style = paste0("background: #FFF3E0; padding: 15px; border-left: 4px solid ", fnb_orange, ";"),
                                 h5("Regulatory Recommendations:"),
                                 tags$ul(
                                   tags$li("Avoid using AGE as a direct pricing factor - use credit behaviors instead"),
                                   tags$li("Document all feature engineering decisions for regulatory review"),
                                   tags$li("Conduct regular fair lending audits (quarterly)"),
                                   tags$li("Age group 18-25 shows higher default rate - offer smaller initial limits, not higher pricing")
                                 )
                             )
                         )
                  )
                )
        ),
        
        # SUMMARY TAB
        tabItem(tabName = "summary",
                fluidRow(
                  column(12,
                         box(title = "Final Logistic Regression Model", width = 12, status = "primary",
                             h5("Model Equation:"),
                             p(strong("log(p/(1-p)) = β₀ + β₁×Age_Young + β₂×Age_Senior + β₃×log(Income) + β₄×No_Delinquency_Record + ...")),
                             br(),
                             h5("Performance Summary:"),
                             DTOutput("performance_table"),
                             br(),
                             h5("Key Findings:"),
                             tags$ul(
                               tags$li(strong("Income IV = 0.382"), " - Strongest predictor of default"),
                               tags$li(strong("Age 18-25 default rate: 29.3%"), " vs Age 55+: 7.94%"),
                               tags$li(strong("Missing data IS predictive"), " - Keep as features"),
                               tags$li(strong("Model AUC improved from 0.757 to 0.792"), sprintf(" (+%.1f%%)", (improved_auc - baseline_auc) / baseline_auc * 100))
                             ),
                             br(),
                             h5("Business Recommendations:"),
                             tags$ul(
                               tags$li(strong("Optimal Approval Threshold: 0.15"), " - Maximizes expected profit"),
                               tags$li(strong("Monitor high-risk segments:"), " Young applicants (18-25), Low income (<$25k)"),
                               tags$li(strong("Use missing data signals:"), " No delinquency record = potentially new-to-credit, offer smaller limits")
                             )
                         )
                  )
                )
        )
      )
    )
  )
  
  # ============================================
  # SERVER LOGIC
  # ============================================
  
  server <- function(input, output, session) {
    
    # Calculate metrics based on threshold
    calculate_metrics <- reactive({
      threshold <- if(input$tabs == "metrics") input$thresh_metrics else if(input$tabs == "vol_risk") input$thresh_vol else 0.15
      approved <- test_enhanced$improved_pred < threshold
      
      tp <- sum(test_enhanced$default_flag == 1 & !approved, na.rm = TRUE)
      fp <- sum(test_enhanced$default_flag == 0 & approved, na.rm = TRUE)
      tn <- sum(test_enhanced$default_flag == 0 & !approved, na.rm = TRUE)
      fn <- sum(test_enhanced$default_flag == 1 & approved, na.rm = TRUE)
      
      precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
      recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
      accuracy <- (tp + tn) / (tp + tn + fp + fn)
      specificity <- ifelse(tn + fp > 0, tn / (tn + fp), 0)
      f1 <- ifelse(precision + recall > 0, 2 * precision * recall / (precision + recall), 0)
      gini <- 2 * improved_auc - 1
      
      list(precision = precision, recall = recall, accuracy = accuracy,
           specificity = specificity, f1 = f1, gini = gini, tp = tp, fp = fp, tn = tn, fn = fn)
    })
    
    # HOME VALUE BOXES
    output$total_applications <- renderValueBox({
      valueBox(
        value = format(nrow(test_enhanced), big.mark = ","),
        subtitle = "Test Applications",
        icon = icon("users"),
        color = "blue"
      )
    })
    
    output$default_rate <- renderValueBox({
      default_rate <- mean(test_enhanced$default_flag, na.rm = TRUE) * 100
      valueBox(
        value = paste0(round(default_rate, 1), "%"),
        subtitle = "Overall Default Rate",
        icon = icon("exclamation-triangle"),
        color = "orange"
      )
    })
    
    output$model_auc <- renderValueBox({
      valueBox(
        value = round(improved_auc, 3),
        subtitle = "Model AUC",
        icon = icon("chart-line"),
        color = "green"
      )
    })
    
    # UNIVARIATE OUTPUTS
    output$uni_stats <- renderPrint({
      var <- input$uni_var
      if(var %in% names(test_enhanced)) {
        summary(test_enhanced[[var]])
      } else if(var %in% names(test_df)) {
        summary(test_df[[var]])
      } else {
        cat("Variable not available")
      }
    })
    
    output$uni_plot <- renderPlotly({
      var <- input$uni_var
      data_use <- if(var %in% names(test_enhanced)) test_enhanced else test_df
      
      if(!var %in% names(data_use) || all(is.na(data_use[[var]]))) {
        return(plot_ly() %>% layout(title = "No data available"))
      }
      
      p <- ggplot(data_use, aes(x = !!sym(var))) +
        geom_histogram(fill = fnb_teal, alpha = 0.7, bins = 30, color = "white") +
        theme_minimal() +
        labs(title = paste("Distribution of", var), x = var, y = "Count") +
        theme(plot.title = element_text(color = fnb_teal, size = 14, face = "bold"))
      ggplotly(p)
    })
    
    output$default_by_var_plot <- renderPlotly({
      var <- input$uni_var
      data_use <- if(var %in% names(test_enhanced)) test_enhanced else test_df
      
      if(!var %in% names(data_use) || all(is.na(data_use[[var]]))) {
        return(plot_ly() %>% layout(title = "No data available"))
      }
      
      # Get unique non-NA values
      valid_values <- data_use[[var]][!is.na(data_use[[var]])]
      if(length(valid_values) == 0) {
        return(plot_ly() %>% layout(title = "No valid data for this variable"))
      }
      
      # Determine number of bins (max 10, but fewer if not enough unique values)
      n_unique <- length(unique(valid_values))
      n_bins <- min(10, max(3, n_unique))
      
      # Create bins safely
      tryCatch({
        data_use$bin <- cut(data_use[[var]], breaks = n_bins, include.lowest = TRUE)
        
        summary_df <- data_use %>%
          group_by(bin) %>%
          summarise(
            Default_Rate = mean(default_flag, na.rm = TRUE) * 100,
            Avg_Value = mean(!!sym(var), na.rm = TRUE),
            Count = n(),
            .groups = 'drop'
          )
        
        # Remove rows with NA bins
        summary_df <- summary_df[!is.na(summary_df$bin) & !is.na(summary_df$Avg_Value), ]
        
        if(nrow(summary_df) == 0) {
          return(plot_ly() %>% layout(title = "Insufficient data for binning"))
        }
        
        p <- ggplot(summary_df, aes(x = Avg_Value, y = Default_Rate, text = paste("Bin:", bin, "<br>Count:", Count))) +
          geom_line(color = fnb_orange, size = 1.5) +
          geom_point(color = fnb_black, size = 3) +
          theme_minimal() +
          labs(title = paste("Default Rate by", var), x = var, y = "Default Rate (%)") +
          theme(plot.title = element_text(color = fnb_teal, size = 14, face = "bold"))
        
        ggplotly(p, tooltip = "text")
      }, error = function(e) {
        plot_ly() %>% layout(title = paste("Error creating plot:", e$message))
      })
    })
    
    # BIVARIATE OUTPUTS
    output$bi_plot <- renderPlotly({
      x <- input$bi_x
      y <- input$bi_y
      
      # Use test_df for bivariate (has original variables)
      if(!x %in% names(test_df) || !y %in% names(test_df)) {
        return(plot_ly() %>% layout(title = "Variables not available"))
      }
      
      # Check if we have valid data
      valid_data <- test_df[!is.na(test_df[[x]]) & !is.na(test_df[[y]]), ]
      
      if(nrow(valid_data) == 0) {
        return(plot_ly() %>% layout(title = "No valid data for selected variables"))
      }
      
      tryCatch({
        # For categorical vs continuous or vice versa
        if(y == "default_flag") {
          # Box plot for default_flag - ensure default_flag is treated as factor
          valid_data$default_flag_factor <- factor(valid_data$default_flag, labels = c("No Default", "Default"))
          
          p <- ggplot(valid_data, aes(x = default_flag_factor, y = !!sym(x))) +
            geom_boxplot(fill = fnb_teal, alpha = 0.7, outlier.color = fnb_orange) +
            theme_minimal() +
            labs(title = paste(x, "by Default Status"), x = "Default Status", y = x) +
            theme(plot.title = element_text(color = fnb_teal, size = 14, face = "bold"))
          
          ggplotly(p) %>% layout(boxmode = "group")
        } else {
          # Scatter plot with jitter to avoid overplotting
          # Sample if too many points (max 5000 for performance)
          if(nrow(valid_data) > 5000) {
            valid_data <- valid_data[sample(1:nrow(valid_data), 5000), ]
          }
          
          p <- ggplot(valid_data, aes(x = !!sym(x), y = !!sym(y), color = factor(default_flag))) +
            geom_point(alpha = 0.5, size = 1.5) +
            scale_color_manual(values = c(fnb_teal, fnb_orange), name = "Default", 
                               labels = c("No Default", "Default")) +
            theme_minimal() +
            labs(title = paste(y, "vs", x), x = x, y = y) +
            theme(plot.title = element_text(color = fnb_teal, size = 14, face = "bold"))
          
          ggplotly(p)
        }
      }, error = function(e) {
        plot_ly() %>% layout(title = paste("Error creating plot:", substr(e$message, 1, 100)))
      })
    })
    
    # MISSING DATA OUTPUTS
    output$missing_table <- renderDT({
      missing_report <- data.frame(
        Variable = names(test_df),
        Missing_Count = sapply(test_df, function(x) sum(is.na(x))),
        Missing_Percentage = round(sapply(test_df, function(x) sum(is.na(x)) / nrow(test_df) * 100), 2)
      )
      missing_report <- missing_report[missing_report$Missing_Count > 0, ]
      datatable(missing_report[order(-missing_report$Missing_Percentage),],
                options = list(pageLength = 10))
    })
    
    output$missing_pattern_plot <- renderPlotly({
      pattern_data <- test_df %>%
        mutate(pattern = case_when(
          is.na(months_since_last_delinquency) & !is.na(annual_income) ~ "Missing Delinquency Only",
          !is.na(months_since_last_delinquency) & is.na(annual_income) ~ "Missing Income Only",
          is.na(months_since_last_delinquency) & is.na(annual_income) ~ "Missing Both",
          TRUE ~ "Complete Data"
        )) %>%
        group_by(pattern) %>%
        summarise(Default_Rate = mean(default_flag, na.rm = TRUE) * 100,
                  Count = n(),
                  .groups = 'drop')
      
      plot_ly(pattern_data, x = ~pattern, y = ~Default_Rate, type = "bar",
              text = ~paste("Count:", Count, "<br>Default Rate:", round(Default_Rate, 1), "%"),
              hoverinfo = "text",
              marker = list(color = c(fnb_teal, fnb_orange, "#dc3545", "#28a745"))) %>%
        layout(title = "Default Rate by Missing Data Pattern",
               xaxis = list(title = "Missing Pattern", tickangle = 45),
               yaxis = list(title = "Default Rate (%)"))
    })
    
    # WOE OUTPUTS
    output$iv_output <- renderPrint({
      var <- input$woe_var
      if(var %in% names(test_df)) {
        result <- calculate_woe_iv(test_df, var)
        cat(sprintf("Information Value (IV): %.4f\n", result$IV))
        cat(sprintf("Predictive Power: %s\n", result$Interpretation))
        if(var == "age_group") cat("\nInsight: Age 18-25 has positive WoE (higher risk)")
        if(var == "income_group") cat("\nInsight: Income <25k has strong positive WoE")
      } else {
        cat("Variable not available")
      }
    })
    
    output$woe_plot <- renderPlotly({
      var <- input$woe_var
      if(var %in% names(test_df)) {
        result <- calculate_woe_iv(test_df, var)
        plot_ly(result$table, x = ~reorder(!!sym(var), WoE), y = ~WoE, type = "bar",
                marker = list(color = ifelse(result$table$WoE > 0, fnb_orange, fnb_teal))) %>%
          layout(title = paste("Weight of Evidence -", var),
                 xaxis = list(title = var, tickangle = 45),
                 yaxis = list(title = "WoE (Positive = Higher Risk)"))
      }
    })
    
    output$woe_table <- renderDT({
      var <- input$woe_var
      if(var %in% names(test_df)) {
        result <- calculate_woe_iv(test_df, var)
        datatable(result$table, options = list(pageLength = 10))
      }
    })
    
    # METRICS OUTPUTS
    output$accuracy_box <- renderValueBox({
      metrics <- calculate_metrics()
      valueBox(paste0(round(metrics$accuracy * 100, 1), "%"), "Accuracy", icon = icon("check"), color = "green")
    })
    
    output$precision_box <- renderValueBox({
      metrics <- calculate_metrics()
      valueBox(paste0(round(metrics$precision * 100, 1), "%"), "Precision", icon = icon("bullseye"), color = "blue")
    })
    
    output$recall_box <- renderValueBox({
      metrics <- calculate_metrics()
      valueBox(paste0(round(metrics$recall * 100, 1), "%"), "Recall", icon = icon("users"), color = "yellow")
    })
    
    output$f1_box <- renderValueBox({
      metrics <- calculate_metrics()
      valueBox(paste0(round(metrics$f1 * 100, 1), "%"), "F1 Score", icon = icon("balance-scale"), color = "purple")
    })
    
    output$specificity_box <- renderValueBox({
      metrics <- calculate_metrics()
      valueBox(paste0(round(metrics$specificity * 100, 1), "%"), "Specificity", icon = icon("shield-alt"), color = "green")
    })
    
    output$gini_box <- renderValueBox({
      metrics <- calculate_metrics()
      valueBox(round(metrics$gini, 3), "Gini Coefficient", icon = icon("chart-line"), color = "blue")
    })
    
    # ROC CURVE
    output$roc_curve <- renderPlotly({
      roc_data <- data.frame(
        FPR = 1 - improved_roc$specificities,
        TPR = improved_roc$sensitivities
      )
      
      plot_ly() %>%
        add_trace(x = ~FPR, y = ~TPR, data = roc_data, type = 'scatter', mode = 'lines',
                  name = 'Enhanced Model', line = list(color = fnb_orange, width = 2)) %>%
        add_trace(x = c(0, 1), y = c(0, 1), type = 'scatter', mode = 'lines',
                  name = 'Random', line = list(color = fnb_teal, dash = 'dash')) %>%
        layout(title = sprintf("ROC Curve (AUC = %.3f)", improved_auc),
               xaxis = list(title = "False Positive Rate (1 - Specificity)"),
               yaxis = list(title = "True Positive Rate (Sensitivity)"))
    })
    
    # FEATURE IMPORTANCE
    output$feature_importance_plot <- renderPlotly({
      coef_data <- data.frame(
        Feature = names(coef(enhanced_model))[-1],
        Coefficient = coef(enhanced_model)[-1]
      )
      coef_data$Abs_Coefficient <- abs(coef_data$Coefficient)
      coef_data <- coef_data[order(-coef_data$Abs_Coefficient), ][1:10, ]
      
      coef_data$Feature <- gsub("_", " ", coef_data$Feature)
      coef_data$Feature <- gsub("no delinquency record", "No Delinquency Record", coef_data$Feature)
      coef_data$Feature <- gsub("log income", "Log Income", coef_data$Feature)
      coef_data$Feature <- gsub("income low", "Low Income (<$25k)", coef_data$Feature)
      coef_data$Feature <- gsub("age young", "Young Age (18-25)", coef_data$Feature)
      
      plot_ly(coef_data, x = ~Coefficient, y = ~reorder(Feature, Coefficient), 
              type = 'bar', orientation = 'h',
              marker = list(color = ifelse(coef_data$Coefficient > 0, fnb_orange, fnb_teal))) %>%
        layout(title = "Top 10 Features by Coefficient Magnitude",
               xaxis = list(title = "Coefficient Value (Positive = Higher Risk)"),
               yaxis = list(title = "Feature"))
    })
    
    # VOLUME VS RISK
    output$volume_risk_plot <- renderPlotly({
      thresholds <- seq(0.05, 0.45, by = 0.01)
      results <- data.frame()
      
      for(t in thresholds) {
        approved <- test_enhanced$improved_pred < t
        approval_rate <- mean(approved, na.rm = TRUE) * 100
        default_rate <- mean(test_enhanced$improved_pred[approved], na.rm = TRUE) * 100
        results <- rbind(results, data.frame(threshold = t, approval_rate = approval_rate, default_rate = default_rate))
      }
      
      current_thresh <- if(input$tabs == "vol_risk") input$thresh_vol else 0.15
      current_rate <- mean(test_enhanced$improved_pred < current_thresh, na.rm = TRUE) * 100
      current_default <- mean(test_enhanced$improved_pred[test_enhanced$improved_pred < current_thresh], na.rm = TRUE) * 100
      
      plot_ly() %>%
        add_trace(x = results$approval_rate, y = results$default_rate, type = 'scatter', mode = 'lines+markers',
                  line = list(color = fnb_orange, width = 2), name = 'Trade-off Curve') %>%
        add_trace(x = current_rate, y = current_default, type = 'scatter', mode = 'markers',
                  marker = list(size = 15, color = 'red', symbol = 'x'), name = 'Current Policy') %>%
        layout(title = "Approval Volume vs Portfolio Risk",
               xaxis = list(title = "Approval Rate (%)"),
               yaxis = list(title = "Expected Default Rate (%)"))
    })
    
    output$profit_plot <- renderPlotly({
      thresholds <- seq(0.05, 0.45, by = 0.01)
      results <- data.frame()
      avg_loan <- input$avg_loan
      interest <- input$interest_rate_biz / 100
      lgd <- input$loss_given_default / 100
      
      for(t in thresholds) {
        approved <- test_enhanced$improved_pred < t
        approval_rate <- mean(approved, na.rm = TRUE) * 100
        expected_default <- mean(test_enhanced$improved_pred[approved], na.rm = TRUE) * 100
        if(is.na(expected_default)) expected_default <- 0
        revenue <- avg_loan * interest
        loss <- avg_loan * lgd * (expected_default / 100)
        profit_per_loan <- revenue - loss
        total_profit <- profit_per_loan * sum(approved, na.rm = TRUE)
        results <- rbind(results, data.frame(approval_rate = approval_rate, profit = total_profit / 1e6))
      }
      
      plot_ly(results, x = ~approval_rate, y = ~profit, type = 'scatter', mode = 'lines',
              line = list(color = '#28a745', width = 2, fill = 'tozeroy')) %>%
        layout(title = "Profit vs Approval Rate",
               xaxis = list(title = "Approval Rate (%)"),
               yaxis = list(title = "Total Profit ($M)"))
    })
    
    # PRECISION VS RECALL
    output$pr_curve_plot <- renderPlotly({
      thresholds <- seq(0.05, 0.45, by = 0.01)
      results <- data.frame()
      
      for(t in thresholds) {
        approved <- test_enhanced$improved_pred < t
        tp <- sum(test_enhanced$default_flag == 1 & !approved, na.rm = TRUE)
        fp <- sum(test_enhanced$default_flag == 0 & approved, na.rm = TRUE)
        fn <- sum(test_enhanced$default_flag == 1 & approved, na.rm = TRUE)
        
        precision <- ifelse(tp + fp > 0, tp / (tp + fp), 0)
        recall <- ifelse(tp + fn > 0, tp / (tp + fn), 0)
        results <- rbind(results, data.frame(recall = recall * 100, precision = precision * 100))
      }
      
      plot_ly(results, x = ~recall, y = ~precision, type = 'scatter', mode = 'lines+markers',
              line = list(color = fnb_teal, width = 2)) %>%
        layout(title = "Precision-Recall Curve",
               xaxis = list(title = "Recall (%) - Good customers captured"),
               yaxis = list(title = "Precision (%) - Approved loans that are good"))
    })
    
    output$error_cost_plot <- renderPlotly({
      thresholds <- seq(0.05, 0.45, by = 0.01)
      results <- data.frame()
      avg_loan <- 15000
      lgd <- 0.70
      
      for(t in thresholds) {
        approved <- test_enhanced$improved_pred < t
        fp <- sum(test_enhanced$default_flag == 0 & approved, na.rm = TRUE)
        fn <- sum(test_enhanced$default_flag == 1 & !approved, na.rm = TRUE)
        
        fp_cost <- fp * avg_loan * lgd / 1e6
        fn_cost <- fn * avg_loan * 0.12 * 0.5 / 1e6
        
        results <- rbind(results, data.frame(threshold = t, fp_cost = fp_cost, fn_cost = fn_cost))
      }
      
      plot_ly() %>%
        add_trace(x = results$threshold, y = results$fp_cost, type = 'scatter', mode = 'lines',
                  name = 'False Positives (Bad Approvals)', line = list(color = fnb_orange),
                  fill = 'tozeroy') %>%
        add_trace(x = results$threshold, y = results$fn_cost, type = 'scatter', mode = 'lines',
                  name = 'False Negatives (Missed Good Loans)', line = list(color = fnb_teal),
                  fill = 'tozeroy') %>%
        layout(title = "Error Cost Analysis",
               xaxis = list(title = "Decision Threshold"),
               yaxis = list(title = "Cost ($M)"))
    })
    
    output$error_cost_text <- renderUI({
      metrics <- calculate_metrics()
      avg_loan <- 15000
      lgd <- 0.70
      fp_cost <- metrics$fp * avg_loan * lgd / 1e6
      fn_cost <- metrics$fn * avg_loan * 0.12 * 0.5 / 1e6
      
      HTML(paste0(
        "<strong>At current threshold (", round(input$thresh_metrics, 2), "):</strong><br><br>",
        "• False Positives (Bad Approvals): ", format(metrics$fp, big.mark = ","), " loans (Cost: $", round(fp_cost, 1), "M)<br>",
        "• False Negatives (Missed Good Loans): ", format(metrics$fn, big.mark = ","), " loans (Cost: $", round(fn_cost, 1), "M)<br>",
        "• ", strong("Total Error Cost: $", round(fp_cost + fn_cost, 1), "M")
      ))
    })
    
    # SIMULATION
    # SIMULATION - Interactive with risk appetite
    # Reactive value to store simulation trigger
    sim_trigger <- reactiveVal(0)
    
    observeEvent(input$simulate, {
      sim_trigger(sim_trigger() + 1)
    })
    
    # Get threshold based on risk appetite and target approval
    get_optimal_threshold <- reactive({
      # Force recalculation when simulate is clicked OR inputs change
      sim_trigger()
      
      target_rate <- input$target_approval
      risk_app <- input$risk_appetite
      
      # Set threshold bounds based on risk appetite
      if(risk_app == "conservative") {
        min_thresh <- 0.08
        max_thresh <- 0.18
        default_weight <- 1.5  # Penalize defaults more
      } else if(risk_app == "aggressive") {
        min_thresh <- 0.15
        max_thresh <- 0.35
        default_weight <- 0.7  # Penalize defaults less
      } else { # moderate
        min_thresh <- 0.10
        max_thresh <- 0.25
        default_weight <- 1.0
      }
      
      thresholds <- seq(min_thresh, max_thresh, by = 0.005)
      best_thresh <- 0.15
      best_score <- -Inf
      
      for(t in thresholds) {
        approved <- test_enhanced$improved_pred < t
        approved <- approved[!is.na(approved)]
        
        if(sum(approved) > 0) {
          approval_rate <- mean(approved, na.rm = TRUE) * 100
          default_rate <- mean(test_enhanced$improved_pred[approved], na.rm = TRUE) * 100
          if(is.na(default_rate)) default_rate <- 0
          
          # Score: how close to target approval rate, penalized by default rate
          score <- -abs(approval_rate - target_rate) - (default_weight * default_rate / 10)
          
          if(score > best_score) {
            best_score <- score
            best_thresh <- t
          }
        }
      }
      
      return(best_thresh)
    })
    
    output$simulation_results <- renderPrint({
      # Force recalculation on button click
      sim_trigger()
      
      target_rate <- input$target_approval
      risk_app <- input$risk_appetite
      optimal_t <- get_optimal_threshold()
      
      approved <- test_enhanced$improved_pred < optimal_t
      approved <- approved[!is.na(approved)]
      approval_rate <- mean(approved, na.rm = TRUE) * 100
      
      if(sum(approved) > 0) {
        expected_default <- mean(test_enhanced$improved_pred[approved], na.rm = TRUE) * 100
      } else {
        expected_default <- 0
      }
      if(is.na(expected_default)) expected_default <- 0
      
      avg_loan <- 15000
      interest <- 0.12
      lgd <- 0.70
      profit_per_loan <- avg_loan * interest - avg_loan * lgd * (expected_default / 100)
      total_profit <- profit_per_loan * sum(approved, na.rm = TRUE)
      
      # Risk appetite description
      risk_desc <- switch(risk_app,
                          "conservative" = "Prioritizing low default rates",
                          "moderate" = "Balancing growth and risk",
                          "aggressive" = "Maximizing approval volume"
      )
      
      cat(sprintf("Risk Appetite: %s (%s)\n", toupper(risk_app), risk_desc))
      cat(sprintf("Target Approval Rate: %.1f%%\n", target_rate))
      cat(sprintf("Achieved Approval Rate: %.1f%%\n", approval_rate))
      cat(sprintf("Recommended Threshold: %.3f\n", optimal_t))
      cat(sprintf("Expected Default Rate: %.2f%%\n", expected_default))
      cat(sprintf("Expected Total Profit: $%.1fM\n", total_profit / 1e6))
      cat(sprintf("Number of Loans Approved: %s\n", format(sum(approved), big.mark = ",")))
    })
    
    output$policy_impact_plot <- renderPlotly({
      # Force recalculation on button click or input changes
      sim_trigger()
      
      target_rate <- input$target_approval
      risk_app <- input$risk_appetite
      
      # Set threshold range based on risk appetite for display
      if(risk_app == "conservative") {
        thresh_min <- 0.05
        thresh_max <- 0.25
      } else if(risk_app == "aggressive") {
        thresh_min <- 0.10
        thresh_max <- 0.45
      } else {
        thresh_min <- 0.05
        thresh_max <- 0.35
      }
      
      thresholds <- seq(thresh_min, thresh_max, by = 0.005)
      results <- data.frame()
      optimal_t <- get_optimal_threshold()
      
      for(t in thresholds) {
        approved <- test_enhanced$improved_pred < t
        approved <- approved[!is.na(approved)]
        
        if(sum(approved, na.rm = TRUE) > 0) {
          avg_risk <- mean(test_enhanced$improved_pred[approved], na.rm = TRUE)
          if(!is.na(avg_risk)) {
            profit <- sum(approved, na.rm = TRUE) * 15000 * 0.12 - 
              sum(approved, na.rm = TRUE) * 15000 * 0.70 * avg_risk
          } else {
            profit <- 0
          }
        } else {
          profit <- 0
        }
        results <- rbind(results, data.frame(threshold = t, profit = profit / 1e6))
      }
      
      results <- results[!is.na(results$profit), ]
      
      if(nrow(results) > 0) {
        plot_ly() %>%
          add_trace(x = results$threshold, y = results$profit, type = 'scatter', mode = 'lines',
                    line = list(color = '#28a745', width = 2), name = 'Profit Curve') %>%
          add_trace(x = c(optimal_t, optimal_t), 
                    y = c(min(results$profit, na.rm = TRUE), max(results$profit, na.rm = TRUE)), 
                    type = 'scatter', mode = 'lines',
                    line = list(color = 'red', dash = 'dash', width = 2), 
                    name = paste('Optimal Threshold (', round(optimal_t, 3), ')')) %>%
          layout(title = paste("Profit by Threshold -", toupper(risk_app), "Policy"),
                 xaxis = list(title = "Decision Threshold"),
                 yaxis = list(title = "Expected Profit ($M)"),
                 hovermode = 'x unified')
      } else {
        plot_ly() %>% layout(title = "No data available for profit calculation")
      }
    })
    
    # SEGMENT ANALYSIS - Uses test_enhanced which has home_Owningship
    output$segment_plot <- renderPlotly({
      segment_var <- input$segment_var
      metric <- input$segment_metric
      
      if(!segment_var %in% names(test_enhanced)) {
        return(plot_ly() %>% layout(title = "Variable not available"))
      }
      
      approved <- test_enhanced$improved_pred < 0.15
      
      # Convert factor to character for Plotly compatibility
      segment_data <- test_enhanced %>%
        filter(!is.na(!!sym(segment_var))) %>%
        mutate(approved = approved,
               segment_clean = as.character(!!sym(segment_var))) %>%
        group_by(segment_clean) %>%
        summarise(
          `Approval Rate` = mean(approved, na.rm = TRUE) * 100,
          `Default Rate` = mean(default_flag, na.rm = TRUE) * 100,  # This uses actual defaults, not predictions!
          Count = n(),
          .groups = 'drop'
        )
      
      # Select which metric to display
      if(metric == "Approval Rate") {
        metric_value <- segment_data$`Approval Rate`
        y_title <- "Approval Rate (%)"
        bar_color <- fnb_teal
      } else {
        metric_value <- segment_data$`Default Rate`
        y_title <- "Default Rate (%)"
        bar_color <- fnb_orange
      }
      
      plot_ly(segment_data, 
              x = ~segment_clean, 
              y = ~metric_value,
              type = 'bar',
              text = ~paste("Count:", Count, "<br>", metric, ":", round(metric_value, 1), "%"),
              hoverinfo = "text",
              marker = list(color = bar_color)) %>%
        layout(title = paste(metric, "by", segment_var),
               xaxis = list(title = segment_var, tickangle = 45),
               yaxis = list(title = y_title))
    })
    
    output$segment_table <- renderDT({
      segment_var <- input$segment_var
      
      if(!segment_var %in% names(test_enhanced)) {
        return(datatable(data.frame(Message = "Variable not available")))
      }
      
      approved <- test_enhanced$improved_pred < 0.15
      
      segment_data <- test_enhanced %>%
        filter(!is.na(!!sym(segment_var))) %>%
        mutate(approved = approved,
               segment_clean = as.character(!!sym(segment_var))) %>%
        group_by(segment_clean) %>%
        summarise(
          Count = n(),
          `Approval Rate` = round(mean(approved, na.rm = TRUE) * 100, 1),
          `Default Rate` = round(mean(default_flag, na.rm = TRUE) * 100, 2),
          `Avg Risk Score` = round(mean(improved_pred, na.rm = TRUE) * 100, 2),
          .groups = 'drop'
        ) %>%
        arrange(desc(`Default Rate`))
      
      # Rename the column back to original variable name
      names(segment_data)[1] <- segment_var
      
      datatable(segment_data, options = list(pageLength = 10, scrollX = TRUE))
    })
    
    # REGULATORY
    output$age_disparity <- renderDT({
      approved <- test_enhanced$improved_pred < 0.15
      
      age_analysis <- test_enhanced %>%
        filter(!is.na(age_group)) %>%
        mutate(approved = approved) %>%
        group_by(age_group) %>%
        summarise(
          Count = n(),
          Approval_Rate = round(mean(approved, na.rm = TRUE) * 100, 1),
          Default_Rate = round(mean(default_flag, na.rm = TRUE) * 100, 2),
          .groups = 'drop'
        )
      
      datatable(age_analysis, options = list(pageLength = 10))
    })
    
    # PERFORMANCE TABLE
    output$performance_table <- renderDT({
      metrics <- calculate_metrics()
      perf_data <- data.frame(
        Metric = c("Baseline AUC", "Enhanced AUC", "Improvement", "Accuracy", 
                   "Precision", "Recall", "F1 Score", "Gini Coefficient"),
        Value = c(round(baseline_auc, 4), round(improved_auc, 4),
                  paste0("+", round((improved_auc - baseline_auc) / baseline_auc * 100, 1), "%"),
                  paste0(round(metrics$accuracy * 100, 1), "%"),
                  paste0(round(metrics$precision * 100, 1), "%"),
                  paste0(round(metrics$recall * 100, 1), "%"),
                  paste0(round(metrics$f1 * 100, 1), "%"),
                  round(metrics$gini, 4))
      )
      datatable(perf_data, options = list(dom = 't', pageLength = 10), rownames = FALSE)
    })
  }
  
  shinyApp(ui = ui, server = server)
}

# ============================================
# LAUNCH THE DASHBOARD
# ============================================

cat("\n========================================\n")
cat("   LAUNCHING FNB CREDIT RISK DASHBOARD\n")
cat("========================================\n")
cat("\nDashboard features:\n")
cat("  • Complete EDA toolkit\n")
cat("  • Model performance evaluation (AUC: ", round(improved_auc, 4), ")\n")
cat("  • Business decision support\n")
cat("  • Regulatory compliance review\n")
cat("\nKey Insights Integrated:\n")
cat("  • Income IV = 0.382 (Strongest predictor)\n")
cat("  • Missing data = 50% → Clean borrowers (7.95% default)\n")
cat("  • Age 18-25 default rate: 29.3%\n")
cat("  • Hidden income risk: 28.8% default\n")
cat("\nLaunching...\n")

run_complete_dashboard()