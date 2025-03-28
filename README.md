
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

```r
   HAVijALL(
  file = inputdata,
  numeric_columns = c(...),
  independent_categorical_columns = c(...),
  dependent_categorical_columns = c(...),
  path = "path/to/destination"
)
```

2. The `HAVijSPLIT` function performs association tests on subsets of your data based on one or more split columns. In addition to at least one of the three objects above, you must also provide the `split_columns` object.

```r
HAVijSPLIT(
  file = inputdata,
  numeric_columns = c(...),
  independent_categorical_columns = c(...),
  dependent_categorical_columns = c(...),
  split_columns = c(...),
  path = "path/to/destination"
)
```
**Parameters:**
- `file`: The input dataset.

- `numeric_columns`: A vector of numeric column indices or names.

- `independent_categorical_columns`: A vector of independent categorical column indices or names.

- `dependent_categorical_columns`: A vector of dependent categorical column indices or names (if applicable).

- `split_columns`: A vector of column indices or names used to split the data.

- `path`: A destination path where the result CSV file will be saved.
