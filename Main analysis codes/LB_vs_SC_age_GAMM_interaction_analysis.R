# ==============================================================================
# Project     : Language Aging 
# Purpose     : Analyze SC*age interaction effects on language behaviour
#               using generalized additive mixed-models (GAMMs).
# Lab         : CBDL Lab, NBRC
# Author      : Partika Jain, Ph.D.
# Date        : 15 Feb 2024
# Dataset     : Normalised_SC.csv (Structural Connectivity matrix)
#               Lang_behaviour_scores.csv (Language behaviour + demographic data)
# Outputs     : SC age-related model results CSV and publication-quality plots.
# Requirements: R packages - tidyverse, gdata, lme4, lmerTest, tibble, MuMIn
# ==============================================================================


# Load required libraries.......................................................
# ..............................................................................
required_packages <- c("ggplot2", "dplyr", "readr", "lme4", "lmerTest", 
                       "MuMIn", "mgcv", "tidyr", "ggtext", "gridExtra")

installed_packages <- required_packages %in% rownames(installed.packages())
if (any(!installed_packages)) {
  install.packages(required_packages[!installed_packages])
}
lapply(required_packages, library, character.only = TRUE)


# Data .........................................................................
# ..............................................................................
# Input "Lang_behaviour_scores.csv" must contain: Participant ID, age, gender, 
# edu_degree, and 16 langauge task columns.
# Input "Normalised_SC.csv" must contain: Normalised structural connectivity of core langauge netork

sc_file <- "Normalised_SC.csv"
LB_file <- "Lang_behaviour_scores.csv" 
output_results_file <- "SCxAge_GAMM_results_FDR.csv"
Anova_heatmap <- "SCxAge_interaction_ANOVA_F_heatmap.png"
SC_GAMM_quantile_map <- "SCxAge_significant_quantile_panels.png"


# Load and preprocess data .....................................................
# ..............................................................................
sc <- read.csv("Normalised_SC.csv")
SC <- scale(sc[2:46])
SC <- as.data.frame(SC)
edges_sorted <- colnames(SC)

dem <- read.csv("Lang_behaviour_scores.csv")
age <- dem$age
gender <- as.factor(dem$gender_text)
Edu <- as.factor(dem$edu_degree)
dem <- dem %>% mutate(across(starts_with("RT"), ~ . * -1), ToT = -ToT)  # Negate RTs/ToT if needed

# Language behaviors
lts <- dem[, c("VF", "Vocab", "High_phon", "High_sem", "Low_phon", "Low_sem",
               "RT_high_phon", "RT_high_sem", "RT_low_phon", "RT_low_sem",
               "Proverb", "Syn_comp", "Sem_comp", "RT_Syn_comp",
               "RT_Sem_comp",  "ToT")]
behavs_sorted <- c("RT_low_phon", "RT_high_phon", "RT_low_sem", "RT_high_sem", "Low_phon",
                   "High_phon", "Low_sem", "High_sem", "ToT", "VF", "RT_Syn_comp", "RT_Sem_comp",
                   "Syn_comp", "Sem_comp", "Proverb", "Vocab")

# Main Data frame
df <- data.frame(age, gender, Edu, lts, SC)


# Heatmap Analysis + Visualization..............................................
# ..............................................................................
# All behaviors & SC edges
results <- data.frame()

for (i in seq_along(lts)) {
  
  for (j in seq_along(SC)) {
    
    lang_var <- scale(replace(lts[[i]], is.nan(lts[[i]]) | is.infinite(lts[[i]]), NA))
    sc_edge <- SC[[j]]
    tmp <- data.frame(age = scale(age), gender, Edu, lang_var, sc_edge)
    tmp <- tmp[complete.cases(tmp), ]
    
    if (nrow(tmp) == 0) next
    
    null_mod <- bam(lang_var ~ 1 + s(age) + s(sc_edge) + s(gender, bs = "re") + s(Edu, bs = "re"), data = tmp)
    full_mod <- bam(lang_var ~ 1 + s(age) + s(sc_edge) + ti(sc_edge, age) + s(gender, bs = "re") + s(Edu, bs = "re"), data = tmp)
    
    anova_res <- anova.gam(null_mod, full_mod, test = "F")
    s_full <- summary(full_mod)$s.table
    int_p <- s_full[grep("ti", rownames(s_full)), "p-value"]
    int_edf <- s_full[grep("ti", rownames(s_full)), "edf"]
    
    results <- rbind(results, data.frame(
      Language_Behaviour = names(lts)[i], SC_Edge = names(SC)[j],
      AIC_NULL = AIC(null_mod), AIC_FULL = AIC(full_mod),
      R_squared_FULL = summary(full_mod)$r.sq, R_squared_NULL = summary(null_mod)$r.sq,
      LogLik_NULL = logLik(null_mod), LogLik_FULL = logLik(full_mod),
      interaction_p_full = ifelse(length(int_p)>0, int_p, NA),
      interaction_coef_full = ifelse(length(int_edf)>0, int_edf, NA),
      ANOVA_p = anova_res$`Pr(>F)`, ANOVA_F_value = anova_res$F
    ))
  }
}
results <- results %>%
  group_by(Language_Behaviour) %>%
  mutate(
    interaction_p_full_FDR = p.adjust(interaction_p_full, "fdr"),
    anova_P_FDR = p.adjust(ANOVA_p, "fdr"),
    Interaction_Significance_FULL = case_when(
      interaction_p_full_FDR < 0.001 ~ "***",
      interaction_p_full_FDR < 0.01 ~ "**",
      interaction_p_full_FDR < 0.05 ~ "*",
      TRUE ~ "ns"
    ),
    ANOVA_Significance = case_when(
      anova_P_FDR < 0.001 ~ "***",
      anova_P_FDR < 0.01 ~ "**",
      anova_P_FDR < 0.05 ~ "*",
      TRUE ~ "ns"
    )
  ) %>%
  ungroup()

# Save the results as first  output file
write.csv(results, output_results_file, row.names = FALSE)


# Heatmap plot .................................................................
# ..............................................................................

coef_fdr <- results[,c("Language_Behaviour", "SC_Edge", "interaction_coef_full",
                       "interaction_p_full_FDR", "Interaction_Significance_FULL",
                       "anova_P_FDR", "ANOVA_Significance", "ANOVA_F_value")]
coef_fdr$Language_Behaviour <- factor(coef_fdr$Language_Behaviour, levels = behavs_sorted)
coef_fdr$SC_Edge <- factor(coef_fdr$SC_Edge, levels = edges_sorted)

# Remove non-significant for ANOVA_F_value
coef_fdr$ANOVA_F_value[coef_fdr$ANOVA_Significance=="ns" | coef_fdr$interaction_p_full_FDR>0.05] <- NA
coef_fdr$ANOVA_Significance[coef_fdr$ANOVA_Significance=="ns" | coef_fdr$interaction_p_full_FDR>0.05] <- NA

comprehension_tasks <- c("Vocab", "Proverb", "Syn_comp", "Sem_comp", "RT_Syn_comp", "RT_Sem_comp")

colored_y_labels <- sapply(levels(coef_fdr$Language_Behaviour), function(t) if(t %in% comprehension_tasks) sprintf("<span style='color:darkgreen;'>%s</span>", t) else sprintf("<span style='color:red;'>%s</span>", t))
colored_x_labels <- c(sapply(1:10, function(i) sprintf("<span style='color:blue;'>%d</span>", i)),
                      sapply(11:20, function(i) sprintf("<span style='color:orange;'>%d</span>", i)),
                      sapply(21:45, function(i) sprintf("<span style='color:purple;'>%d</span>", i)))

model_plot <- ggplot(coef_fdr, aes(x=factor(SC_Edge), y=Language_Behaviour, fill=ANOVA_F_value)) +
  geom_tile(colour="white", size=1.5) +
  scale_fill_gradient(low="white", high="steelblue") +
  scale_x_discrete(labels=colored_x_labels) +
  scale_y_discrete(labels=colored_y_labels) +
  theme_minimal() +
  theme(axis.text.x=ggtext::element_markdown(angle=45, hjust=1, size=12),
        axis.text.y=ggtext::element_markdown(size=16),
        axis.title.y=element_text(size=20),
        axis.title.x=element_text(size=20),
        legend.text=element_text(size=10),
        legend.title=element_text(size=12)) +
  labs(fill="F-value", x="Edge - SC", y="Language Tasks") +
  geom_text(aes(label=ANOVA_Significance), size=4)
ggsave(Anova_heatmap, plot=model_plot, dpi=700, width=12, height=6.5)


# Significant Behaviors/SCs: Quantile Curve Panel...............................
# ..............................................................................

SC_edges_by_behavior <- list(
  "Syn_comp" = c("LParsT_LSTG", "LMTG_RMTG", "LMTG_RParsT", "LParsO_RParsT", "LSTG_RSTS", "LSTG_RMTG"),
  "VF" = c("LParsT_RMTG"),
  "RT_high_sem" = c("RSTS_RSTG", "LSTS_RMTG")
)

generate_gamm_plots <- function(panel_filename) {
  plot_list <- list()
  plot_index <- 1
  
  for (behavior in names(SC_edges_by_behavior)) {
    
    for (SC_name in SC_edges_by_behavior[[behavior]]) {
      SC_edge <- SC[[SC_name]]
      lang_behave <- scale(dem[[behavior]])
      
      tmp <- data.frame(age, gender, Edu, lang_behave, SC_edge)
      
      tmp <- tmp[complete.cases(tmp), ]
      colnames(tmp)[colnames(tmp) == "lang_behave"] <- behavior
      
      SC_qs <- quantile(tmp$SC_edge, probs = c(0.25, 0.5, 0.75), na.rm=TRUE)
      SC_qs <- round(SC_qs,2)
      SC_qs <- unique(c(SC_qs, max(SC_qs)))
      
      color_map <- setNames(c("blue", "green", "red")[1:length(SC_qs)], as.character(SC_qs))
      mod <- bam(as.formula(sprintf("%s ~ 1 + s(age) + s(SC_edge) + ti(SC_edge, age) + s(gender, bs = 're') + s(Edu, bs = 're')", behavior)), data=tmp)
      
      pred_data <- expand_grid(
        SC_edge = SC_qs,
        age = sample(tmp$age, size=300, replace=TRUE),
        gender = levels(tmp$gender),
        Edu = levels(tmp$Edu))
      pred <- predict(mod, newdata=pred_data, se.fit=TRUE)
      pred_data <- bind_cols(as_tibble(pred), pred_data)
      pred_data$SC_edge <- factor(pred_data$SC_edge, levels=as.character(SC_qs))
     
       p <- ggplot(pred_data, aes(x=age, y=fit, color=SC_edge)) +
        geom_line(aes(group=SC_edge), size=1.2) +
        labs(title=SC_name, x="Age", y=paste("Predicted", behavior), color="SC\nQuantiles") +
        scale_color_manual(values=color_map, labels=names(color_map)) +
        theme_minimal() +
        theme(plot.title=element_text(size=26, face="bold", hjust=0.5),
              axis.title=element_text(size=26),
              axis.text=element_text(size=19),
              legend.title=element_text(size=17),
              legend.text=element_text(size=15),
              legend.key.size=unit(0.4,"cm"),
              legend.title.align=0.5)
      plot_list[[plot_index]] <- p; plot_index <- plot_index + 1
    }
  }
  plot_panel <- grid.arrange(grobs=plot_list, ncol=3, nrow=3)
  ggsave(filename=panel_filename, plot=plot_panel, width=22, height=15, dpi=700)
}
generate_gamm_plots(SC_GAMM_quantile_map)


