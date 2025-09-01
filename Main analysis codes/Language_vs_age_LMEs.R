# ==============================================================================
# Project     : Language Aging
# Purpose     : Analyze age effects on language behavior using linear 
#               mixed-effects models, see Readme file
# Lab         : CBDL Lab,NBRC
# Author      : Partika Jain, Ph.D.
# Date        : January 20, 2021 
# Dataset     : Lang_behaviour_scores.csv (Demographics + Language Task Scores)
# Outputs     : Statistical results table and paper figure
# Requirements: R packages - ggplot2, dplyr, readr, lme4, lmerTest, MuMIn
# ==============================================================================

# Load required libraries.......................................................
# ..............................................................................
packages <- c("here", "readxl", "dplyr", "gamm4", "progress",
              "tidyr", "stringr", "ggplot2")

# Install missing packages
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(packages[!installed_packages])
}

# Load packages
lapply(packages, require, character.only = TRUE)

# Data .........................................................................
# ..............................................................................
# Input CSV must contain: Participant ID, age, gender, edu_degree, and 16 
# langauge 16 task columns.
input_file <- "Lang_behaviour_scores.csv"
output_results_file <- "Lang_behave_age_Mixed_model_results.csv"
output_plot_file <- "Lang_behave_age_horizontal_plot.png"

# Load and preprocess data .....................................................
# ..............................................................................

Data_raw <- read.csv("Lang_behaviour_scores.csv")

# Extract key predictors
age <- Data_raw$age
gender <- as.factor(Data_raw$gender_text)
Edu <- as.factor(Data_raw$edu_degree)

# Select language behavior columns
lts <- Data_raw[, c("VF", "High_phon", "High_sem", "Low_phon", "Low_sem",
                    "RT_high_phon", "RT_high_sem", "RT_low_phon", "RT_low_sem",
                    "ToT","Vocab", "Proverb", "Syn_comp", "Sem_comp", 
                    "RT_Syn_comp","RT_Sem_comp")]

# Standardize scoring direction:
# Higher scores indicate better performance for all variables
lts <- lts %>%
  mutate(
    ToT = -ToT,
    across(starts_with("RT"), ~ -.)
  )

# Handle any -Inf values by replacing with NA to prevent modeling errors
lts <- lts %>%
  mutate(across(everything(), ~ ifelse(is.infinite(.), NA, .)))


# Data Summary (Optional) ......................................................
# ..............................................................................
lts_summary <- lts %>%
  summarise(across(everything(), list(
    mean = ~ mean(., na.rm = TRUE),
    sd = ~ sd(., na.rm = TRUE),
    min = ~ min(., na.rm = TRUE),
    max = ~ max(., na.rm = TRUE),
    missing_percent = ~ sum(is.na(.)) / n() * 100
  ), .names = "{col}_{fn}"))
print(lts_summary)

# Combine Data .................................................................
# ..............................................................................
df <- data.frame(age, gender, Edu, lts)

# Define Model Fitting Function ................................................
# ..............................................................................
fit_mixed_model <- function(outcome_var, data) {
  # Prepare data for modeling
  df_model <- data %>%
    select(age, gender, Edu, all_of(outcome_var)) %>%
    rename(LTS = all_of(outcome_var)) %>%
    filter(!is.na(LTS))
  
  # Replace infinite with NA if any remained
  df_model$LTS[is.infinite(df_model$LTS)] <- NA
  df_model <- df_model %>% filter(!is.na(LTS))
  
  # Standardize outcome variable
  df_model$LTS <- scale(df_model$LTS, center = TRUE, scale = TRUE)
  
  # Model formulas
  formula_full <- as.formula("LTS ~ age + (1 | gender) + (1 | Edu)")
  formula_null <- as.formula("LTS ~ (1 | gender) + (1 | Edu)")
  
  # Fit models with robust error handling
  model_full <- tryCatch({
    lmer(formula_full, data = df_model, REML = FALSE,
         control = lmerControl(optimizer = "Nelder_Mead"))
  }, warning = function(w) {
    message("Warning in fitting full model for ", outcome_var, ": ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in fitting full model for ", outcome_var, ": ", conditionMessage(e))
    NULL
  })
  
  model_null <- tryCatch({
    lmer(formula_null, data = df_model, REML = FALSE,
         control = lmerControl(optimizer = "Nelder_Mead"))
  }, warning = function(w) {
    message("Warning in fitting null model for ", outcome_var, ": ", conditionMessage(w))
    NULL
  }, error = function(e) {
    message("Error in fitting null model for ", outcome_var, ": ", conditionMessage(e))
    NULL
  })
  
  # If models fit successfully, extract statistics
  if (!is.null(model_full) && !is.null(model_null)) {
    summary_full <- summary(model_full)
    est_age <- if ("age" %in% rownames(summary_full$coefficients)) summary_full$coefficients["age", "Estimate"] else NA
    se_age <- if ("age" %in% rownames(summary_full$coefficients)) summary_full$coefficients["age", "Std. Error"] else NA
    p_age <- if ("age" %in% rownames(summary_full$coefficients)) summary_full$coefficients["age", "Pr(>|t|)"] else NA
    
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
      Language_task = outcome_var,
      Estimate_age = est_age,
      Std_Error = se_age,
      p_value_age = p_age,
      AIC_model = aic_full,
      LogLik_model = loglik_full,
      Marginal_R2_model = marginal_r2_full,
      Conditional_R2_model = conditional_r2_full,
      AIC_null = aic_null,
      LogLik_null = loglik_null,
      Marginal_R2_null = marginal_r2_null,
      Conditional_R2_null = conditional_r2_null,
      ANOVA_p_value = anova_p,
      stringsAsFactors = FALSE
    ))
  } else {
    # In case of failure return NA row
    return(data.frame(
      Language_task = outcome_var,
      Estimate_age = NA,
      Std_Error = NA,
      p_value_age = NA,
      AIC_model = NA,
      LogLik_model = NA,
      Marginal_R2_model = NA,
      Conditional_R2_model = NA,
      AIC_null = NA,
      LogLik_null = NA,
      Marginal_R2_null = NA,
      Conditional_R2_null = NA,
      ANOVA_p_value = NA,
      stringsAsFactors = FALSE
    ))
  }
}

results_list <- list()
language_cols <- colnames(lts)  

for (task in language_cols) {
  message("Processing task: ", task)
  result <- fit_mixed_model(task, df)  
  results_list[[task]] <- result
}



# Multiple Testing Correction ..................................................
# ..............................................................................
result <- result %>%
  mutate(FDR_p_value = p.adjust(p_value_age, method = "fdr")) %>%
  mutate(Significance = case_when(
    FDR_p_value < 0.001 ~ "***",
    FDR_p_value < 0.01 ~ "**",
    FDR_p_value < 0.05 ~ "*",
    TRUE ~ "ns"
  )) %>%
  mutate(Significance = factor(Significance, levels = c("***", "**", "*", "ns")))

# Save results table to CSV
write_csv(result, output_results_file)

# Plotting Age Effects .........................................................
# ..............................................................................
lang_behave_horizontal <- ggplot(result, aes(
  y = reorder(Language_task, Estimate_age),
  x = Estimate_age,
  fill = Significance
)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(
    xmin = Estimate_age - Std_Error,
    xmax = Estimate_age + Std_Error
  ), width = 0.2, color = "darkgrey") +
  scale_fill_manual(values = c(
    "***" = "deepskyblue",
    "**" = "skyblue",
    "*" = "powderblue",
    "ns" = "lightsteelblue"
  )) +
  theme_minimal() +
  labs(
    y = "Language Task",
    x = "Estimate of Age Effect (Standardized)",
    fill = "Significance (FDR)"
  ) +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.text.x = element_text(size = 14)
  )

print(lang_behave_horizontal)

# Save publication-quality figure (adjust width/height as needed)
ggsave(output_plot_file, plot = lang_behave_horizontal, dpi = 700, width = 7, height = 6)


# Optional: Demographic Plots...................................................
# ..............................................................................
# Age distribution histogram (basic)
ggplot(Data_raw, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "powderblue", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Age of Participants", y = "Number of Participants") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  ) +
  scale_x_continuous(breaks = seq(20, 90, by = 10))

# Education distribution histogram
ggplot(Data_raw, aes(x = as.factor(edu_degree))) +
  geom_bar(fill = "lightgreen", color = "black", alpha = 0.7) +
  theme_minimal() +
  labs(x = "Education Level", y = "Count") +
  theme(
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
    axis.text.y = element_text(size = 12)
  )

# ---------------------------
# Save Session Info for Reproducibility
# ---------------------------
writeLines(capture.output(sessionInfo()), "session_info.txt")

