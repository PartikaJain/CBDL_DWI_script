# Structural Connectivity vs Age — Linear Mixed‑Effects (LME) Analysis

R code to estimate **age effects on core‑language‑network (cLAN) structural connectivity (SC)** edges (45 edges) using **LMEs** with random intercepts for **gender** and **education**.

---

## 📦 What this code does
- Loads cLAN structural connectivity matrix and demographics.
- Z‑scores each SC edge.
- Fits per‑edge LMEs:  
  - **Full:** `SC_z ~ 1 + age + (1|gender) + (1|Edu)`  
  - **Null:** `SC_z ~ 1 + (1|gender) + (1|Edu)`
- Extracts estimates, SEs, p‑values, R², AIC, logLik.
- Likelihood‑ratio **ANOVA** (Full vs Null), **FDR** across 45 edges.
- Saves tidy CSV and three grouped bar plots (Left, Right, Inter‑hemispheric).

> Reflects the SC analysis described in the manuscript (see Citation).

---

## 🗂️ Inputs
- `Normalised_SC.csv` — normalized SC for 558 right‑handed CamCAN participants; SC columns in positions 2:46.
- `Demographics.csv` — columns: `age`, `gender_text`, `edu_degree`.

## 🧪 Outputs
- `SC_age_Mixed_model_compare_results_with_FDR.csv`
- `SC_Age_Left_Hemisphere.png`
- `SC_Age_Right_Hemisphere.png`
- `SC_Age_Inter_Hemispheric.png`

---

## 🔧 Software & Packages
- R (≥ 4.2)
- CRAN: `tidyverse`, `gdata`, `lme4`, `lmerTest`, `tibble`, `MuMIn`, `ggplot2`

Install in R:
```r
pkgs <- c("tidyverse","gdata","lme4","lmerTest","tibble","MuMIn","ggplot2")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))
```

---

## ▶️ How to run
1. Place `Normalised_SC.csv` and `Demographics.csv` alongside the script.
2. Execute:
```r
source("SC_vs_age_LMEs.R")   # or .Rmd / .txt renamed to .R
```
3. Inspect the CSV and PNG figures.

---

## 📁 Expected folder layout
```
.
├─ SC_vs_age_LMEs.R
├─ Normalised_SC.csv
├─ Demographics.csv
├─ SC_age_Mixed_model_compare_results_with_FDR.csv
├─ SC_Age_Left_Hemisphere.png
├─ SC_Age_Right_Hemisphere.png
└─ SC_Age_Inter_Hemispheric.png
```

---

## ✅ Reproducibility notes
- SC features are z‑scored per edge.
- Optimizer: `bobyqa` with `maxfun = 1e5` (tuned for stability).
- FDR is applied to age‑term p‑values across 45 edges.

---

## 📚 Citation
> **Frontal Vulnerability vs. Temporal Resilience within core Language Network: Neuro‑compensatory mechanisms underline differential language aging trajectories**. Jain, Akhter, & Banerjee (2025).

---


## 🙋 Troubleshooting
- **Edge indexing**: ensure SC columns are 2:46 in your matrix.
- **Category explosion in `Edu`**: keep education levels as observed (factor).
- **Hemisphere grouping**: update index slices if your edge ordering differs.
