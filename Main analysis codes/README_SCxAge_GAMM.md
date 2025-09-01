# Language Behaviour ~ SC × Age — GAMM Interaction Analysis (mgcv::bam)

R pipeline to test **SC × Age interactions** on **16 language behaviour (LB) measures** across **45 structural edges** of the core language network (cLAN) using **Generalized Additive Mixed Models** (`mgcv::bam`).

---

## 🧠 Rationale
LMEs showed limitations for highly non‑linear relationships and complex interactions. **GAMMs** flexibly model smooth functions of **age** and **SC** and their **tensor interaction**, with random‑effect smooths for **gender** and **education** — aligning with the manuscript’s analysis.

---

## 📦 What this code does
- Loads normalized SC matrix and LB + demographic data.
- Builds main‑effects vs interaction GAMMs per (LB, SC‑edge) pair:
  - **Main effects (null):** `LB_z ~ 1 + s(age) + s(sc_edge) + s(gender, bs="re") + s(Edu, bs="re")`
  - **Interaction (full):** `LB_z ~ 1 + s(age) + s(sc_edge) + ti(sc_edge, age) + s(gender, bs="re") + s(Edu, bs="re")`
- Compares via `anova.gam(..., test="F")`.
- Extracts **F‑tests**, **edf** for the `ti()` term, and **R²**.
- **FDR** correction across the 45 edges **within each behaviour**.
- Creates:
  - Heatmap of significant interactions (F value for `ti()`; stars for FDR q‑value).
  - Quantile‑stratified panels for selected (LB, SC) pairs.

---

## 🗂️ Inputs
- `Normalised_SC.csv` — normalized cLAN SC (columns become edges).  
- `Lang_behaviour_scores.csv` — demographics + 16 LB measures; RT & ToT are sign‑reversed.

## 🧪 Outputs
- `SCxAge_GAMM_results_FDR.csv` — statistics per (LB, edge), incl. ANOVA F, p, FDR, edf.
- `SCxAge_interaction_ANOVA_F_heatmap.png` — heatmap of significant interactions.
- `SCxAge_significant_quantile_panels.png` — per‑panel GAMM partials for selected effects.

---

## 🔧 Software & Packages
- R (≥ 4.2)
- CRAN: `ggplot2`, `dplyr`, `readr`, `lme4`, `lmerTest`, `MuMIn`, `mgcv`, `tidyr`, `ggtext`, `gridExtra`

Install in R:
```r
pkgs <- c("ggplot2","dplyr","readr","lme4","lmerTest","MuMIn",
          "mgcv","tidyr","ggtext","gridExtra")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install)) install.packages(to_install)
invisible(lapply(pkgs, require, character.only = TRUE))
```

---

## ▶️ How to run
```r
source("LB_vs_SC_age_GAMM_interaction_analysis.R")   # or rename .txt → .R
```
Adjust file paths at the top if your CSVs live elsewhere.

---

## 📁 Expected folder layout
```
.
├─ LB_vs_SC_age_GAMM_interaction_analysis.R
├─ Normalised_SC.csv
├─ Lang_behaviour_scores.csv
├─ SCxAge_GAMM_results_FDR.csv
├─ SCxAge_interaction_ANOVA_F_heatmap.png
└─ SCxAge_significant_quantile_panels.png
```

---

## ✅ Reproducibility notes
- Behaviour outcomes and age are z‑scored; NaN/Inf sanitized.
- Random‑effect smooths for `gender` and `Edu` (`bs="re"`).
- Use `bam()` for speed on large loops (720 models for SC; similarly for FC if extended).
- FDR applied **within behaviour** across 45 edges.

---

## 📚 Citation
> **Frontal Vulnerability vs. Temporal Resilience within core Language Network: Neuro‑compensatory mechanisms underline differential language aging trajectories**. Jain, Akhter, & Banerjee (2025).

---

## 🙋 Troubleshooting
- **`anova.gam` p = NA**: ensure `ti()` term present and models converge.
- **`bs="re"` errors**: factors must be coded; verify `gender`, `Edu` are factors.
- **Empty heatmap**: relax filtering or verify FDR thresholding per behaviour.
