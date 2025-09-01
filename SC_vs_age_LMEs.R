# ==============================================================================
# Project     : Language Aging 
# Purpose     : Analyze age effects on structural connectivity (SC) measures
#               using linear mixed-effects models.
# Lab         : CBDL Lab, NBRC
# Author      : Partika Jain, Ph.D.
# Date        : 22 April 2023
# Dataset     : Normalised_SC.csv (Structural Connectivity matrix)
#               Demographics.csv (Demographic data)
# Outputs     : SC age-related model results CSV and publication-quality plots.
# Requirements: R packages - tidyverse, gdata, lme4, lmerTest, tibble, MuMIn
# ==============================================================================

# Load required libraries.......................................................
# ..............................................................................
required_packages <- c("tidyverse", "gdata", "lme4", "lmerTest", "tibble", "MuMIn")

installed_packages <- required_packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(required_packages[!installed_packages])
}
lapply(required_packages, library, character.only = TRUE)


# Data .........................................................................
# ..............................................................................
# Input "Demographics.csv" must contain: Participant ID, age, gender and edu_degree
# Input "Normalised_SC.csv" must contain: Normalised structural connectivity of core langauge netork

sc_file <- "Normalised_SC.csv"
demographic_file <- "Demographics.csv"
output_csv_file <- "SC_age_Mixed_model_compare_results_with_FDR.csv"
output_plot_left <- "SC_Age_Left_Hemisphere.png"
output_plot_right <- "SC_Age_Right_Hemisphere.png"
output_plot_inter <- "SC_Age_Inter_Hemispheric.png"


# Load and preprocess data .....................................................
# ..............................................................................
# Load SC data (structural connectivity matrix for 558 right-handed subjects)
sc_data_raw <- read.csv(sc_file)

# Extract SC columns only (skip subject ID or first column if any, adjust as needed)
SC1 <- sc_data_raw[, 2:46]

# Z-score standardize each SC column
SC_z <- scale(SC1) %>% as.data.frame()

# Load demographic variables
demographics <- read.csv(demographic_file)
age <- demographics$age
gender <- as.factor(demographics$gender_text)
Edu <- as.factor(demographics$edu_degree)

# Combine all variables into one dataframe
df <- data.frame(age = age, gender = gender, Edu = Edu, SC_z)


# Initialize results storage ...................................................
# ..............................................................................
results <- data.frame()


# Define modeling and stats extraction function.................................
# ..............................................................................
fit_sc_age_model <- function(sc_col_name, data) {
  # Prepare data for the specific SC measure
  df_model <- data %>%
    select(age, gender, Edu, all_of(sc_col_name)) %>%
    rename(sc_value = all_of(sc_col_name)) %>%
    filter(!is.na(sc_value))
  
  # Model formulas
  formula_full <- as.formula("sc_value ~ 1 + age + (1 | gender) + (1 | Edu)")
  formula_null <- as.formula("sc_value ~ 1 + (1 | gender) + (1 | Edu)")
  
  # Fit full model
  model_full <- tryCatch({
    lmer(formula_full, data = df_model,
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
  }, warning = function(w) {
    message("Warning in full model for ", sc_col_name, ": ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in full model for ", sc_col_name, ": ", conditionMessage(e))
    NULL
  })
  
  # Fit null model
  model_null <- tryCatch({
    lmer(formula_null, data = df_model,
         control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
  }, warning = function(w) {
    message("Warning in null model for ", sc_col_name, ": ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in null model for ", sc_col_name, ": ", conditionMessage(e))
    NULL
  })
  
  # Extract statistics if models fit successfully
  if (!is.null(model_full) && !is.null(model_null)) {
    sum_full <- summary(model_full)
    est_age <- sum_full$coefficients["age", "Estimate"]
    se_age <- sum_full$coefficients["age", "Std. Error"]
    p_age <- sum_full$coefficients["age", "Pr(>|t|)"]
    
    aic_full <- AIC(model_full)
    loglik_full <- logLik(model_full)[1]
    r2_full <- r.squaredGLMM(model_full)
    marginal_r2_full <- ifelse(is.vector(r2_full), r2_full["R2m"], NA)
    conditional_r2_full <- ifelse(is.vector(r2_full), r2_full["R2c"], NA)
    
    aic_null <- AIC(model_null)
    loglik_null <- logLik(model_null)[1]
    r2_null <- r.squaredGLMM(model_null)
    marginal_r2_null <- ifelse(is.vector(r2_null), r2_null["R2m"], NA)
    conditional_r2_null <- ifelse(is.vector(r2_null), r2_null["R2c"], NA)
    
    anova_res <- tryCatch({
      anova(model_null, model_full)
    }, error = function(e) NULL)
    anova_p <- if (!is.null(anova_res)) anova_res$`Pr(>Chisq)`[2] else NA
    
    return(data.frame(
      sc_connection = sc_col_name,
      Estimate_age = est_age,
      Std_Error = se_age,
      p_value_age = p_age,
      AIC_full = aic_full,
      LogLik_full = loglik_full,
      Marginal_R2_full = marginal_r2_full,
      Conditional_R2_full = conditional_r2_full,
      AIC_null = aic_null,
      LogLik_null = loglik_null,
      Marginal_R2_null = marginal_r2_null,
      Conditional_R2_null = conditional_r2_null,
      ANOVA_p_value = anova_p,
      stringsAsFactors = FALSE
    ))
  } else {
    return(data.frame(
      sc_connection = sc_col_name,
      Estimate_age = NA,
      Std_Error = NA,
      p_value_age = NA,
      AIC_full = NA,
      BIC_full = NA,
      LogLik_full = NA,
      Marginal_R2_full = NA,
      Conditional_R2_full = NA,
      AIC_null = NA,
      BIC_null = NA,
      LogLik_null = NA,
      Marginal_R2_null = NA,
      Conditional_R2_null = NA,
      ANOVA_p_value = NA,
      stringsAsFactors = FALSE
    ))
  }
}


# Loop through SC columns and fit models........................................
# ..............................................................................
sc_columns <- names(df)[4:ncol(df)]

for (sc_col in sc_columns) {
  message("Processing SC connection: ", sc_col)
  res <- fit_sc_age_model(sc_col, df)
  results <- rbind(results, res)
}

# Reset row names
row.names(results) <- NULL


# Multiple testing correction using FDR.........................................
# ..............................................................................
results <- results %>%
  mutate(FDR_p_value = p.adjust(p_value_age, method = "fdr")) %>%
  mutate(Significance = case_when(
    FDR_p_value < 0.001 ~ "***",
    FDR_p_value < 0.01  ~ "**",
    FDR_p_value < 0.05  ~ "*",
    TRUE                ~ "ns"
  )) %>%
  mutate(Significance = factor(Significance, levels = c("***", "**", "*", "ns")))

# Save results to CSV
write.csv(results, output_csv_file, row.names = FALSE)


# Create grouped plots by connection type.......................................
# ..............................................................................
# Assuming first 10 = left hemisphere, next 10 = right hemisphere, rest inter-hemispheric
results_left <- results[1:10, ]
results_right <- results[11:20, ]
results_inter <- results[21:nrow(results), ]

plot_sc_connections <- function(data, x_limits = c(-0.022, 0.022)) {
  ggplot(data, aes(x = Estimate_age, y = reorder(sc_connection, Estimate_age), fill = Significance)) +
    geom_bar(stat = "identity", na.rm = TRUE) +
    geom_errorbar(aes(xmin = Estimate_age - Std_Error, xmax = Estimate_age + Std_Error),
                  width = 0.2, color = "darkgrey", na.rm = TRUE) +
    scale_fill_manual(values = c("***" = "deepskyblue", "**" = "skyblue", "*" = "powderblue", "ns" = "lightsteelblue")) +
    theme_minimal() +
    labs(y = "Structural Connectivity Edge", x = "Estimated Age Effect", fill = "Significance") +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12)
    ) +
    coord_cartesian(xlim = x_limits)
}

# Plot for left hemisphere SC edges
plot_left <- plot_sc_connections(results_left)

# Plot for right hemisphere SC edges
plot_right <- plot_sc_connections(results_right)

# Plot for inter-hemispheric SC edges
plot_inter <- plot_sc_connections(results_inter)

# Display plots (prints in RStudio or interactive R)
print(plot_left)
print(plot_right)
print(plot_inter)

# After creating the plots for each group, save them as high resolution PNG
ggsave("SC_vs_Age_Left_Hemispheric.png", plot = plot_left, dpi = 700, width = 8, height = 6, units = "in")
ggsave("SC_vs_Age_Right_Hemispheric.png", plot = plot_right, dpi = 700, width = 8, height = 6, units = "in")
ggsave("SC_vs_Age_Inter_Hemispheric.png", plot = plot_inter, dpi = 700, width = 8, height = 6, units = "in")


# --------------------------
# Save session info for reproducibility
# --------------------------
writeLines(capture.output(sessionInfo()), "session_info_SC_age_analysis.txt")
