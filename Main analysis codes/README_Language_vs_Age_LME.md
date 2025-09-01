# Language Behaviour vs Age — Linear Mixed‑Effects (LME) Analysis

> Part of the **Language Aging** project (CBDL Lab, NBRC).

This repository contains an R pipeline to quantify the effect of **age** on **16 language behaviour (LB) measures** using **Linear Mixed‑Effects models** with random intercepts for **gender** and **education**.

---

## 📦 What this code does
- Loads language behaviour and demographics.
- Standardizes scoring direction (RT & ToT sign‑reversed so higher = better).
- Fits `lmer` models per LB task:  
  - **Full:** `LB_z ~ age + (1|gender) + (1|Edu)`  
  - **Null:** `LB_z ~ (1|gender) + (1|Edu)`
- Extracts estimates, SEs, p‑values, R² (marginal/conditional), AIC, logLik.
- Compares models via **ANOVA**, applies **FDR** correction across tasks.
- Saves a tidy CSV of results and a publication‑quality horizontal bar plot.

> Implementation follows the analysis described in the manuscript (see Citation).

---

## 🗂️ Inputs
- `Lang_behaviour_scores.csv` — columns: `participant_id`, `age`, `gender_text`, `edu_degree`, 16 behaviour measures (e.g., `VF`, `Vocab`, `High_phon`, `RT_Sem_comp`, `ToT`, …).

## 🧪 Outputs
- `Lang_behave_age_Mixed_model_results.csv` — per‑task statistics (age estimate, SE, p, ANOVA p, FDR, R², AIC, …)
- `Lang_behave_age_horizontal_plot.png` — effect of age across tasks with FDR significance fill.

---

## 🔧 Software & Packages
- R (≥ 4.2)
- CRAN: `here`, `readxl`, `dplyr`, `gamm4`, `progress`, `tidyr`, `stringr`, `ggplot2`, `lme4`, `lmerTest`, `MuMIn`

Install in R:
```r
packages <- c("here","readxl","dplyr","gamm4","progress","tidyr","stringr",
              "ggplot2","lme4","lmerTest","MuMIn")
to_install <- packages[!packages %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(packages, require, character.only = TRUE))
```

---

## ▶️ How to run
1. Place `Lang_behaviour_scores.csv` in the repo root (or edit the path in the script).
2. Run the script:
```r
source("Language_vs_age_LMEs.R")   # or .Rmd / .txt renamed to .R
```
3. Check the generated CSV and PNG in the output directory (or repo root).

---

## 📁 Expected folder layout
```
.
├─ Language_vs_age_LMEs.R
├─ Lang_behaviour_scores.csv
├─ Lang_behave_age_Mixed_model_results.csv
└─ Lang_behave_age_horizontal_plot.png
```

---

## ✅ Reproducibility notes
- Missing values are handled per‑task (rowwise drop for the focal task only).
- All outcomes are z‑scored; RT/ToT are sign‑reversed so higher = better.
- Optimizer: `Nelder_Mead` (can switch to `bobyqa` if needed).

---

## 📚 Citation
If you use this code, please cite the manuscript:

> **Frontal Vulnerability vs. Temporal Resilience within core Language Network: Neuro‑compensatory mechanisms underline differential language aging trajectories**. Jain, Akhter, & Banerjee (2025).

---

## 📄 License
MIT (recommended) — update this section to your preferred license.

---

## 🙋 Troubleshooting
- **Convergence warnings**: try `bobyqa` optimizer and/or scale predictors.
- **Singular fit**: check random‑effects structure or category sparsity in `Edu`.
- **Plots empty**: confirm CSV paths and that FDR‑significant effects exist.
