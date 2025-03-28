---
title: "HAVij"
output: github_document
---

# HAVij

**HAVij** is an R package designed to automate association analyses for gene expression and clinicopathological data. It streamlines the process by automatically performing normality tests and selecting the appropriate parametric or nonparametric statistical tests. Additionally, the package provides the option to split your dataset for subset analyses.

## Features

- **Automated Normality Testing:** Uses Shapiro-Wilk tests to determine whether your numeric data is normally distributed.
- **Test Selection:** Automatically chooses the correct statistical test based on data distribution:
  - **Parametric tests:** ANOVA, t-test, Pearson correlation.
  - **Nonparametric tests:** Kruskal-Wallis, Mann-Whitney U, Spearman correlation.
  - **Categorical tests:** Chi-square, Fisher's Exact.
- **Data Splitting:** Optionally split your data based on a specified column to perform separate analyses.
- **CSV Export:** Saves analysis results as CSV files to a user-specified destination.

## Installation

You can install the HAVij package from GitHub using `devtools`:

```r
# Install devtools if not already installed
install.packages("devtools")
devtools::install_github("yourusername/HAVij")
```

## Usage
1. The `HAVijALL` function performs overall association tests. At least one of these objects must be provided: `numeric_columns`, `independent_categorical_columns`, or `dependent_categorical_columns`.

