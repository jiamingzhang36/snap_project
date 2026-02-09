# Michigan SNAP Work Requirement Policy Analysis

This repository contains the code, documentation, and supporting materials for the project **"Assessing the Impacts of SNAP Work Requirements in Michigan"**, conducted as part of research in the Department of Agricultural, Food, and Resource Economics (AFRE) at Michigan State University.

---

## 📌 Overview
The project evaluates the effects of the reinstated ABAWD (Able-Bodied Adults Without Dependents) work requirement policy in Michigan using a county-month panel dataset (2016–2019).  
It aims to quantify how the policy affected SNAP participation, exits, and local labor market outcomes.

The analysis is designed for **policy evaluation**, **causal inference**, and **forecasting** under varying local economic conditions.

---

## 🧮 Methods
1. **Staggered Difference-in-Differences (DiD)**  
   - Implements the Callaway & Sant'Anna (2021) estimator to identify the average treatment effects of the work requirement rollout.  
   - Controls for county and time fixed effects.  
   - Robust to staggered treatment timing.

2. **Heterogeneity Analysis via Double Machine Learning (DML)**  
   - Based on Chernozhukov et al. (2018).  
   - Identifies how policy impacts vary across counties with different baseline characteristics (e.g., unemployment rate, waiver status, demographics).  
   - Uses machine learning (Random Forest, Lasso) for high-dimensional control adjustment.

3. **Forecasting SNAP Exit Risk (Micro-level Simulation)**  
   - Predicts individual-level risk of losing benefits under expanded work requirements.  
   - Incorporates demographic and employment predictors.

---

## 📂 Repository Structure

**Pipeline:** `main.R` sources `R/99_run/run_all.R`. The pipeline is the **paper structure** (old flat 4-step layout is no longer used).

| Step | Status | Outputs |
|------|--------|---------|
| **Step 1 – ABAWD DID** | Done | `outputs/step1_did/` (CS + DDD, event-study, robustness) |
| **Step 2 – DML/BLP** | Done | `outputs/step2_dml/` |
| **Step 3 – 2025 forecast** | Done | `outputs/step3_forecast/` |
| **Step 4 – Individual risk** | Script exists | `R/04_individual_risk.R` |
| **Paper structure** | In place | `config/`, `R/01_build`–`05_forecast_2026`, `R/99_run` (some stubs) |

- **Config:** `config/paths.R`, `config/globals.R` — ROOT, data paths, policy dates, variable names.
- **Build:** `R/01_build/` — 01_clean_snap → `panel_base.rds`; 02_clean_laus → `panel_with_laus.rds`; 03_merge_panel → `panel_analysis.rds` (migration: reads `data_clean/panel_with_G.csv`).
- **ABAWD:** `R/02_abawd/` — event_time, event-study (stub calls `R/01_did_cs.R` for full run), figures → `outputs/figures/abawd_*.png`.
- **Income / EA / Forecast:** `R/03_income/`, `R/04_ea/`, `R/05_forecast_2026/` — stubs; implement as needed.
- **Run:** `R/99_run/run_all.R` — runs 01_build → 02_abawd → 03_income → 04_ea → 05_forecast_2026 in order.

---

## ⚙️ Software Requirements
- **R (≥ 4.2)**  
  Key packages: `did`, `data.table`, `tidyverse`, `fixest`
- **Python (≥ 3.9)**  
  Key packages: `pandas`, `econml`, `DoubleML`, `scikit-learn`

---

## 🚫 Data Availability
The underlying SNAP administrative and labor market data are confidential and cannot be publicly shared.  
Replication can be achieved using the same code structure with simulated or publicly available data (e.g., ACS, LAUS).

---

## 📈 Key Outputs
- County-level estimates of SNAP participation changes  
- Heterogeneous effects by local economic conditions  
- Predicted exit risk profiles for ABAWD recipients  

---

## 📄 Paper and slides (LaTeX)
LaTeX source for the **paper** and **Beamer slides** lives in `paper/`. Compile from project root:

```bash
make -C paper
```

Or from R: `source("R/99_run/compile_paper.R")`.  
Outputs: `paper/paper.pdf`, `paper/slides.pdf`. See `paper/README.md` for details and TinyTeX install.

---

## 👤 Author
**Jiaming Zhang**  
Ph.D. Student, Department of Agricultural, Food, and Resource Economics  
Michigan State University  
📧 Email: zhangjiaming3608@gmail.com

---

## 📄 Citation
If you use or reference this work, please cite as:

> Zhang, J. (2025). *Assessing the Impacts of SNAP Work Requirements in Michigan.*  
> Michigan State University, Department of Agricultural, Food, and Resource Economics.
