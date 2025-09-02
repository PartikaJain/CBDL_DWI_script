# 🧬 Neuro-compensatory mechanisms underline differential language aging trajectories

This repository hosts code and pipelines developed at the **Cognitive Brain Dynamics Lab (CBDL), National Brain Research Centre (NBRC)** for studying **age-related changes in structural and functional connectivity within the core Language Network (cLAN)** and their impact on language abilities across the lifespan.

---

## 📖 Background

Language abilities follow distinct aging trajectories:

- **Language production (LP)** (e.g., verbal fluency, tip-of-the-tongue, priming tasks) tends to decline with age.  
- **Language comprehension (LC)** (e.g., vocabulary, proverb, syntactic/semantic comprehension) is relatively more stable, and in some cases improves with age.  

Using the **CamCAN cohort** (N = 558, ages 18–88), our analyses revealed a **frontal vulnerability vs temporal resilience** pattern:  

- **Decline** in intra- and inter-hemispheric **fronto-temporal SC** with age.  
- **Preservation/strengthening** of **temporo-temporal SC**, supporting LC stability.  
- Complex **SC × age interactions** shaping divergent LC/LP trajectories.

For detailed results, see the manuscript:  
> Jain, P., Akhter, A., & Banerjee, A. (2025). *Frontal Vulnerability vs. Temporal Resilience within core Language Network: Neuro-compensatory mechanisms underline differential language aging trajectories.*

---

## 📂 Repository Structure

```
Language_aging/
│
├── DWI_preprocessing/      # Shell pipeline for DWI preprocessing & connectome construction
│   └── README_CBDL_DWI_script.md
│
├── main_analysis/Language_vs_Age_LME/    # R scripts: language behaviour ~ age (LMEs)
│   └── README_Language_vs_Age_LME.md
│
├── main_analysis/SC_vs_Age_LME/          # R scripts: SC ~ age (LMEs)
│   └── README_SC_vs_Age_LME.md
│
├── main_analysis/SCxAge_GAMM/            # R scripts: SC × age × behaviour interactions (GAMMs)
│   └── README_SCxAge_GAMM.md
│
├── License                               # Lincense MIT
│       
│
├── README.md                             # Project overview and instructions


Each subfolder contains its own `README.md` file, which outlines **inputs, outputs, and dependencies**.

---

## 🚀 Quick Start

1. **Preprocess DWI data**  
   Run the shell pipeline in [`DWI_preprocessing`](./DWI_preprocessing).  

2. **Run statistical models**  
   Use the R scripts in:  
   - [`Language_vs_Age_LME`](./Language_vs_Age_LME)  
   - [`SC_vs_Age_LME`](./SC_vs_Age_LME)  
   - [`SCxAge_GAMM`](./SCxAge_GAMM)  

3. **Explore results**  
   Processed CSVs, figures, and supplementary materials are in `main_analysis/`.

---

## 📚 Citation

If you use this code or pipeline, please cite:

> **Jain, P., Akhter, A., & Banerjee, A. (2025). Frontal Vulnerability vs. Temporal Resilience within core Language Network: Neuro-compensatory mechanisms underline differential language aging trajectories.**

---

## 📄 License

MIT License © 2025 Partika Jain. See [LICENSE](./LICENSE).

---

## 📬 Contact

**Partika Jain**  
PhD Researcher, CBDL Lab, NBRC, Gurgaon, India  
✉️ partika.mhg.bhu16@gmail.com  
🌐 [GitHub](https://github.com/PartikaJain)
