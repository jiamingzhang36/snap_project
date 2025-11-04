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
   - Implements the Callaway & Sant’Anna (2021) estimator to identify the average treatment effects of the work requirement rollout.  
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
