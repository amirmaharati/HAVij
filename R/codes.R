library(usethis)
library(devtools)
library(roxygen2)

# Ensure dependencies are installed
if (!requireNamespace("FSA", quietly = TRUE)) install.packages("FSA")
library(FSA)

#' HAVijALL
#'
#' This function performs tests between numeric and categorical variables, correlation analysis
#' between numeric variables, and tests between categorical variables using your Step 1 code.
#' It saves the result (which mimics your original "HAVij_Result.csv") to the specified path.
#'
#' @param file A data frame containing the input data.
#' @param numeric_columns A numeric vector of column indices for numeric variables.
#' @param independent_categorical_columns A numeric vector of column indices for independent categorical variables.
#' @param dependent_categorical_columns A numeric vector of column indices for dependent categorical variables.
#' @param path A character string of the destination path where the CSV file will be saved.
#'
#' @return A data frame of test results is returned and saved as "HAVij_Result.csv" in the specified path.
#' @export
HAVijALL <- function(file,
                     numeric_columns,
                     independent_categorical_columns,
                     dependent_categorical_columns,
                     path) {

  # Initialize results data frame
  test_results <- data.frame(
    Column = character(),
    Significant = character(),
    Details = character(),
    Test = character(),
    p.value = numeric(),
    stringsAsFactors = FALSE
  )

  ### 1. Numeric vs. Categorical Tests ###
  for (num_col in numeric_columns) {
    for (cat_col in independent_categorical_columns) {
      # Extract numeric and categorical data from provided file
      numeric_data <- file[[num_col]]
      categorical_data <- as.factor(file[[cat_col]])
      group_levels <- levels(categorical_data)  # extract group labels

      # Check normality for each group (only if group has more than 2 values)
      shapiro_results <- tapply(numeric_data, categorical_data, function(x) {
        if (length(x) > 2) shapiro.test(x)$p.value else NA
      })

      # Determine if all groups are normal
      all_normal <- all(!is.na(shapiro_results) & (shapiro_results > 0.05))

      # Initialize test details
      test_name <- ""
      details <- "None"
      p_value <- NA

      # Based on normality, choose the appropriate test
      if (all_normal) {
        if (length(unique(categorical_data)) > 2) {
          # Perform ANOVA for multiple groups
          test <- aov(numeric_data ~ categorical_data)
          p_value <- summary(test)[[1]][["Pr(>F)"]][1]
          test_name <- "ANOVA"
          if (p_value < 0.05) {
            means <- tapply(numeric_data, categorical_data, mean)
            details <- paste(names(means)[order(-means)], collapse = " > ")
          }
        } else {
          # Perform t-test for binary groups
          test <- t.test(numeric_data ~ categorical_data)
          p_value <- test$p.value
          test_name <- "t-test"
          if (p_value < 0.05) {
            means <- tapply(numeric_data, categorical_data, mean)
            details <- paste(group_levels[order(-means)], collapse = " > ")
          }
        }
      } else {
        if (length(unique(categorical_data)) > 2) {
          # Perform Kruskal-Wallis test for multiple groups
          test <- kruskal.test(numeric_data ~ categorical_data)
          p_value <- test$p.value
          test_name <- "Kruskal-Wallis"
          if (p_value < 0.05) {
            medians <- tapply(numeric_data, categorical_data, median)
            details <- paste(names(medians)[order(-medians)], collapse = " > ")
          }
        } else {
          # Perform Mann-Whitney-U test for binary groups
          test <- wilcox.test(numeric_data ~ categorical_data, exact = FALSE)
          p_value <- test$p.value
          test_name <- "Mann-Whitney-U"
          if (p_value < 0.05) {
            medians <- tapply(numeric_data, categorical_data, median)
            details <- paste(group_levels[order(-medians)], collapse = " > ")
          }
        }
      }

      significant <- ifelse(p_value < 0.05, "T", "F")

      test_results <- rbind(test_results,
                            data.frame(
                              Column = paste(colnames(file)[num_col], colnames(file)[cat_col], sep = "_"),
                              Significant = significant,
                              Details = details,
                              Test = test_name,
                              p.value = ifelse(significant == "T", p_value, NA),
                              stringsAsFactors = FALSE
                            ))
    }
  }

  ### 2. Correlation Analysis (Numeric vs Numeric) ###
  for (i in 1:(length(numeric_columns) - 1)) {
    for (j in (i + 1):length(numeric_columns)) {
      var1 <- file[[numeric_columns[i]]]
      var2 <- file[[numeric_columns[j]]]

      # Skip if one variable has constant values
      if (length(unique(var1)) == 1 || length(unique(var2)) == 1) next

      normal1 <- ifelse(length(unique(var1)) > 2, shapiro.test(var1)$p.value > 0.05, FALSE)
      normal2 <- ifelse(length(unique(var2)) > 2, shapiro.test(var2)$p.value > 0.05, FALSE)

      if (normal1 && normal2) {
        test <- cor.test(var1, var2, method = "pearson")
        test_name <- "Pearson"
      } else {
        test <- cor.test(var1, var2, method = "spearman")
        test_name <- "Spearman"
      }

      p_value <- test$p.value
      significant <- ifelse(p_value < 0.05, "T", "F")

      test_results <- rbind(test_results,
                            data.frame(
                              Column = paste(colnames(file)[numeric_columns[i]], colnames(file)[numeric_columns[j]], sep = "_"),
                              Significant = significant,
                              Details = ifelse(significant == "T", test$estimate, NA),
                              Test = test_name,
                              p.value = ifelse(significant == "T", p_value, NA),
                              stringsAsFactors = FALSE
                            ))
    }
  }

  ### 3. Categorical vs. Categorical Tests ###
  # Combine independent and dependent categorical columns
  cat_columns <- unique(c(independent_categorical_columns, dependent_categorical_columns))
  if (length(cat_columns) > 1) {
    # Function to perform tests between two categorical variables
    test_categorical_variables <- function(data, cat_col1, cat_col2) {
      contingency_table <- table(data[[cat_col1]], data[[cat_col2]])
      expected_frequencies <- suppressWarnings(chisq.test(contingency_table)$expected)
      use_fisher <- FALSE
      if (all(dim(contingency_table) == c(2, 2))) {
        if (any(expected_frequencies < 5)) {
          use_fisher <- TRUE
        }
      } else {
        if (sum(expected_frequencies < 5) / length(expected_frequencies) > 0.2) {
          use_fisher <- TRUE
        }
      }
      if (use_fisher) {
        test <- fisher.test(contingency_table, simulate.p.value = TRUE)
        test_name <- "Fisher's Exact Test"
      } else {
        test <- chisq.test(contingency_table)
        test_name <- "Chi-Square Test"
      }
      p_value <- test$p.value
      significant <- ifelse(p_value < 0.05, "T", "F")
      result <- data.frame(
        Column = paste(colnames(file)[cat_col1], colnames(file)[cat_col2], sep = "_"),
        Significant = significant,
        Details = ifelse(significant == "T", "Association found", "No association"),
        Test = test_name,
        p.value = ifelse(significant == "T", p_value, NA),
        stringsAsFactors = FALSE
      )
      return(result)
    }

    for (i in 1:(length(cat_columns) - 1)) {
      for (j in (i + 1):length(cat_columns)) {
        result <- test_categorical_variables(file, cat_columns[i], cat_columns[j])
        test_results <- rbind(test_results, result)
      }
    }
  }

  # Save results as "HAVij_Result.csv" in the specified path
  out_file <- file.path(path, "HAVij_Result.csv")
  write.csv(test_results, out_file, row.names = FALSE)
  message("HAVijALL results saved to: ", out_file)

  return(test_results)
}

#' HAVijSPLIT
#'
#' This function processes the input data for each level of the specified split_columns.
#' For each split subset, it performs tests between numeric and categorical variables (based on normality),
#' correlations between numeric variables, and tests between categorical variables.
#' The results for each subset are saved as CSV files using a naming convention.
#'
#' @param file A data frame containing the input data.
#' @param numeric_columns A numeric vector of column indices for numeric variables.
#' @param independent_categorical_columns A numeric vector of column indices for independent categorical variables.
#' @param dependent_categorical_columns A numeric vector of column indices for dependent categorical variables.
#' @param split_columns A numeric vector of column indices to split the data by.
#' @param path A character string of the destination path where the CSV files will be saved.
#'
#' @return For each split subset, a CSV file is saved with the test results.
#' @export
HAVijSPLIT <- function(file,
                       numeric_columns,
                       independent_categorical_columns,
                       dependent_categorical_columns,
                       split_columns,
                       path) {

  # Nested function to perform tests and correlations on a given subset
  perform_tests_and_correlations <- function(data, numeric_cols, ind_cat_cols, dep_cat_cols, split_col = NULL) {
    sub_results <- data.frame(
      Column = character(),
      Significant = character(),
      Details = character(),
      Test = character(),
      p.value = numeric(),
      stringsAsFactors = FALSE
    )

    ### 1. Numeric vs. Categorical Tests ###
    for (num_col in numeric_cols) {
      for (cat_col in ind_cat_cols) {
        if (!is.null(split_col) && cat_col == split_col) next  # Skip split column
        numeric_data <- data[[num_col]]
        categorical_data <- as.factor(data[[cat_col]])
        if (length(unique(categorical_data)) < 2 || length(unique(numeric_data)) == 1) next

        shapiro_results <- tapply(numeric_data, categorical_data, function(x) {
          if (length(x) >= 3 && length(unique(x)) > 1) {
            shapiro.test(x)$p.value
          } else {
            NA
          }
        })
        all_normal <- all(!is.na(shapiro_results) & (shapiro_results > 0.05))

        test_name <- ""
        details <- "None"
        p_value <- NA

        if (all_normal) {
          if (length(unique(categorical_data)) > 2) {
            test <- aov(numeric_data ~ categorical_data)
            p_value <- summary(test)[[1]][["Pr(>F)"]][1]
            test_name <- "ANOVA"
            if (p_value < 0.05) {
              means <- tapply(numeric_data, categorical_data, mean)
              details <- paste(names(means)[order(-means)], collapse = " > ")
            }
          } else {
            test <- t.test(numeric_data ~ categorical_data)
            p_value <- test$p.value
            test_name <- "t-test"
            if (p_value < 0.05) {
              means <- tapply(numeric_data, categorical_data, mean)
              details <- paste(levels(categorical_data)[order(-means)], collapse = " > ")
            }
          }
        } else {
          if (length(unique(categorical_data)) > 2) {
            test <- kruskal.test(numeric_data ~ categorical_data)
            p_value <- test$p.value
            test_name <- "Kruskal-Wallis"
            if (p_value < 0.05) {
              medians <- tapply(numeric_data, categorical_data, median)
              details <- paste(names(medians)[order(-medians)], collapse = " > ")
            }
          } else {
            test <- wilcox.test(numeric_data ~ categorical_data, exact = FALSE)
            p_value <- test$p.value
            test_name <- "Mann-Whitney-U"
            if (p_value < 0.05) {
              medians <- tapply(numeric_data, categorical_data, median)
              details <- paste(levels(categorical_data)[order(-medians)], collapse = " > ")
            }
          }
        }
        if (!is.na(p_value)) {
          significant <- ifelse(p_value < 0.05, "T", "F")
          sub_results <- rbind(sub_results,
                               data.frame(
                                 Column = paste(colnames(data)[num_col], colnames(data)[cat_col], sep = "_"),
                                 Significant = significant,
                                 Details = details,
                                 Test = test_name,
                                 p.value = ifelse(significant == "T", p_value, NA),
                                 stringsAsFactors = FALSE
                               ))
        }
      }
    }

    ### 2. Correlation Analysis (Numeric vs Numeric) ###
    for (i in 1:(length(numeric_cols) - 1)) {
      for (j in (i + 1):length(numeric_cols)) {
        var1 <- data[[numeric_cols[i]]]
        var2 <- data[[numeric_cols[j]]]
        if (length(unique(var1)) == 1 || length(unique(var2)) == 1) next
        normal1 <- ifelse(length(unique(var1)) > 2, shapiro.test(var1)$p.value > 0.05, FALSE)
        normal2 <- ifelse(length(unique(var2)) > 2, shapiro.test(var2)$p.value > 0.05, FALSE)
        if (normal1 && normal2) {
          test <- cor.test(var1, var2, method = "pearson")
          test_name <- "Pearson"
        } else {
          test <- cor.test(var1, var2, method = "spearman")
          test_name <- "Spearman"
        }
        p_value <- test$p.value
        significant <- ifelse(p_value < 0.05, "T", "F")
        sub_results <- rbind(sub_results,
                             data.frame(
                               Column = paste(colnames(data)[numeric_cols[i]], colnames(data)[numeric_cols[j]], sep = "_"),
                               Significant = significant,
                               Details = ifelse(significant == "T", test$estimate, NA),
                               Test = test_name,
                               p.value = ifelse(significant == "T", p_value, NA),
                               stringsAsFactors = FALSE
                             ))
      }
    }

    ### 3. Categorical vs. Categorical Tests ###
    all_cat_cols <- unique(c(ind_cat_cols, dep_cat_cols))
    if (length(all_cat_cols) > 1) {
      test_categorical_variables <- function(data, cat_col1, cat_col2) {
        contingency_table <- table(data[[cat_col1]], data[[cat_col2]])
        if (any(dim(contingency_table) < 2)) return(NULL)
        expected_frequencies <- suppressWarnings(chisq.test(contingency_table)$expected)
        use_fisher <- FALSE
        if (all(dim(contingency_table) == c(2, 2))) {
          if (any(expected_frequencies < 5)) {
            use_fisher <- TRUE
          }
        } else {
          if (sum(expected_frequencies < 5) / length(expected_frequencies) > 0.2) {
            use_fisher <- TRUE
          }
        }
        if (use_fisher) {
          test <- fisher.test(contingency_table, simulate.p.value = TRUE)
          test_name <- "Fisher's Exact Test"
        } else {
          test <- chisq.test(contingency_table)
          test_name <- "Chi-Square Test"
        }
        p_value <- test$p.value
        significant <- ifelse(p_value < 0.05, "T", "F")
        result <- data.frame(
          Column = paste(colnames(data)[cat_col1], colnames(data)[cat_col2], sep = "_"),
          Significant = significant,
          Details = ifelse(significant == "T", "Association found", "No association"),
          Test = test_name,
          p.value = ifelse(significant == "T", p_value, NA),
          stringsAsFactors = FALSE
        )
        return(result)
      }

      for (i in 1:(length(all_cat_cols) - 1)) {
        for (j in (i + 1):length(all_cat_cols)) {
          if (!is.null(split_col) && (all_cat_cols[i] == split_col || all_cat_cols[j] == split_col)) next
          result <- test_categorical_variables(data, all_cat_cols[i], all_cat_cols[j])
          if (!is.null(result))
            sub_results <- rbind(sub_results, result)
        }
      }
    }

    return(sub_results)
  }

  # Process each split column and each unique level in that column
  for (split_col in split_columns) {
    unique_levels <- unique(file[[split_col]])
    for (lev in unique_levels) {
      split_data <- subset(file, file[[split_col]] == lev)
      if (nrow(split_data) == 0) next
      test_independent_cats <- setdiff(independent_categorical_columns, split_col)

      split_results <- perform_tests_and_correlations(split_data,
                                                      numeric_columns,
                                                      test_independent_cats,
                                                      dependent_categorical_columns,
                                                      split_col)
      file_name <- paste0("result_split_", colnames(file)[split_col], "_", lev, ".csv")
      out_file <- file.path(path, file_name)
      write.csv(split_results, out_file, row.names = FALSE)
      message("HAVijSPLIT results saved to: ", out_file)
    }
  }
}
