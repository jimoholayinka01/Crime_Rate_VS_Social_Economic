# Crime_Rate_VS_Social_Economic
This repository contains a complete statistical analysis project exploring the relationship between crime rates and various socioeconomic indicators (education level, income, poverty, employment) across 1,000 urban regions. The analysis includes data cleaning, exploratory data analysis, non-parametric testing and visualisation using R.
## 🎯 Objectives
- Explore the distribution of socioeconomic indicators and their relation to crime rates.
- Use non-parametric statistical methods due to non-normality in the dataset.
- Derive meaningful insights to support data-driven urban policy discussion.

## 🧠 Key Findings
- No statistically significant relationships were found between crime rates and any of the socioeconomic factors examined.
- Spearman's Rank Correlation revealed very weak or negligible associations.
- The Kruskal-Wallis H test indicated weak evidence of differences in crime rates across education level groups (p ≈ 0.057), but not strong enough to be considered significant.
- Visualisation tools (boxplots, heatmaps, histograms) supported the statistical outcomes.

## 📊 Dataset
Source: [Kaggle – Crime Rate vs Socioeconomic Factors](https://www.kaggle.com/datasets/adilshamim8/crime-rate-vs-socioeconomic-factors)

- 1,000 observations from urban regions
- Variables:
  - Crime_Rate
  - Education_Level (%)
  - Median_Income (USD)
  - Poverty_Rate (%)
  - Employment_Rate (%)
  - Population_Density
  - Region

## 🛠️ Methodology
1. **Data Cleaning & Preparation**
   - Removed duplicates and missing values
   - Validated data types and variable formats

2. **Exploratory Data Analysis (EDA)**
   - Visualisation using scatter plots, histograms, boxplots
   - Statistical summaries

3. **Hypothesis Testing**
   - Kruskal-Wallis H Test for group comparisons
   - Spearman’s Rank Correlation for monotonic relationships

4. **Interpretation & Insights**
   - Combined statistical outputs with graphical analysis
   - Interpreted p-values, correlation coefficients, and chi-squared statistics

## 📉 Limitations
- Non-normal data distribution required use of non-parametric methods, which may miss subtle relationships.
- Binning continuous variables into categories can obscure patterns.
- External factors such as law enforcement, culture, or historical crime trends were not included.
- Temporal and regional specificity is not captured.

## 🔍 Conclusion
The analysis suggests no strong statistical link between crime rate and the socioeconomic indicators in this dataset. While education level showed borderline significance, broader conclusions require larger or more diverse datasets.

---

**📌 To Reproduce the Analysis:**
1. Clone this repository.
2. Open the `crime_analysis.R` file in RStudio.
3. Ensure the `data/` folder contains the dataset.
4. Run the script step-by-step.

---


