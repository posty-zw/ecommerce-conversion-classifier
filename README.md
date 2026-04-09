# 🛒 E-Commerce Purchase Conversion Classifier

> Predicting online purchase intent using logistic regression on 12,330 browsing sessions.

---

## 📌 Overview

Built a binary classifier to predict whether an online shopping session results in a purchase, using the [UCI Online Shoppers Purchasing Intention dataset](https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset). The analysis covers exploratory data analysis, model fitting, evaluation, and hypothesis testing.

**Course:** ALY6015 — Intermediate Analytics | Northeastern University  
**Tools:** R, ggplot2, caret, pROC, dplyr, kableExtra

---

## 📊 Results

| Metric | Training Set | Test Set |
|---|---|---|
| Accuracy | 88.1% | 89.1% |
| Specificity | 97.8% | 98.4% |
| Sensitivity (Recall) | 35.3% | 38.2% |
| Precision | 74.6% | 81.1% |
| **AUC** | — | **90.3%** |

> The model correctly ranks a revenue-generating session above a non-converting one **9 out of 10 times**.

---

## 🔍 Key Findings

- **PageValues**, **ExitRates**, and **ProductRelated** page count were the strongest behavioural predictors of purchase conversion
- High exit rates strongly signal session abandonment and kill conversion odds
- Higher page value engagement reliably predicts purchase intent — each unit increase raises conversion odds by 7.9%
- Chi-square test (χ² = 384.94, df = 9, p < 0.05) confirmed revenue generation is **significantly dependent on month**, validating seasonal shopping patterns

---

## 🗂️ Repository Structure

```
ecommerce-conversion-classifier/
│
├── Sigma.R          # Full analysis script (EDA, modelling, evaluation)
├── report.pdf       # Written analysis with visualisations
└── README.md        # Project overview
```

---

## ⚙️ How to Run

1. Clone the repository
2. Download the dataset from [UCI ML Repository](https://archive.ics.uci.edu/ml/datasets/Online+Shoppers+Purchasing+Intention+Dataset)
3. Open `Sigma.R` in RStudio
4. Install required packages if needed:

```r
install.packages(c("dplyr", "ggplot2", "kableExtra", "knitr", 
                   "corrplot", "tidyr", "psych", "caTools", 
                   "caret", "pROC"))
```

5. Run the script — `file.choose()` will prompt you to select the dataset

---

## ⚠️ Limitations

- **Class imbalance** (84.5% non-converting sessions) biases the model toward the majority class, resulting in lower sensitivity (38.2%)
- Threshold adjustment or resampling techniques (e.g. SMOTE) could improve recall for revenue-generating sessions
- Model uses only 3 final predictors — additional feature engineering may improve performance

---

## 👤 Author

**Akheem Amisi**  
MPS Analytics — Northeastern University  
[LinkedIn](https://linkedin.com/in/akheem-amisi)
