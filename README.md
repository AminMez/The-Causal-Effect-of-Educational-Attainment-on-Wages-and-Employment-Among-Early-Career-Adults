# 🎓 The Causal Effect of Educational Attainment on Wages and Employment  
### Evidence from the 2021 Canadian Census

**Author:** Amin Angelo Mezouari  
**Course:** ECON 398 – University of British Columbia  
**Supervisor:** Professor Jonathan Graves  
**Date:** April 2025

---

## 📌 Abstract

This project estimates the **causal impact** of post-secondary education on wages and employment probabilities for early-career adults (aged 25–40) in Canada. Using **2021 Canadian Census data**, we apply a **Two-Stage Least Squares (2SLS)** approach with an **instrumental variable** for “moved for education.” To account for non-linearity in treatment effects, we use a **semi-parametric model** for wages and a **logistic regression** for estimating the **log-odds of employment**. The analysis finds a monotonic increase in wages with higher education, but a more irregular pattern for employment outcomes.

---

## 🧠 Research Question

**What is the causal effect of different levels of post-secondary educational attainment on wages and employment outcomes among early-career Canadians?**

---

## 🧪 Methodology

- **Data:** 2021 Canadian Census (cross-sectional)
- **Instrumental Variable:** Binary indicator for “Moved for education”
- **Estimation Strategy:**
  - **First Stage:** Predict education level using 2SLS with relevant covariates.
  - **Second Stage – Wages:**
    - Applied a **semi-parametric regression** model by binning predicted education levels.
    - Allows the model to **flexibly capture non-linearities** in how education affects log wages.
  - **Second Stage – Employment:**
    - Used **logistic regression** on predicted education levels to estimate the **log-odds of being employed**.
    - Converted coefficients to **predicted employment probabilities**.

- **Controls:** Age group, gender, province, family income, citizenship status

This strategy identifies **Local Average Treatment Effects (LATE)** for individuals induced to pursue more education by relocating.

---

## 📈 Key Results

- **Wages:** Increase **monotonically** with higher education levels; largest gains occur between **Bachelor’s and Master’s**.
- **Employment:** Does **not increase consistently**; advanced degrees show dips in predicted employment rates.
- **Modeling advantages:**  
  - **Semi-parametric approach** effectively captures complex wage dynamics across education levels.  
  - **Logistic model** allows nuanced interpretation of **employment odds** across bins.
- Instrument is **strong** (F-statistic > 700) and results are robust to controls.

---
