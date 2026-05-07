library(readr)
library(dplyr)
library(tidyr)

path <- "data/survey_results_public.csv"

# Read JobSat columns plus columns likely related to AI/time/searching
col_pattern <- "(?i)ai|time|search"
job <- read_csv(path, col_select = c(starts_with("JobSat"), matches(col_pattern)), show_col_types = FALSE)

# Quick per-column NA and unique-value summary
na_counts <- sapply(job, function(x) sum(is.na(x)))
unique_counts <- sapply(job, function(x) length(unique(x)))
pct_na <- na_counts / nrow(job) * 100

na_summary <- tibble(
  column = names(na_counts),
  n_na = as.integer(na_counts),
  n_unique = as.integer(unique_counts),
  pct_na = as.numeric(pct_na)
) %>% arrange(desc(n_na))

cat("Per-column NA / unique summary:\n")
print(na_summary)

# Show first 20 rows to inspect formats
cat("\nFirst 20 rows (inspection):\n")
print(slice_head(job, n = 20), n = 20)

# Columns matching AI/time/search patterns
special_cols <- na_summary %>% filter(grepl(col_pattern, column, perl = TRUE))
cat("\nColumns matching AI/time/search pattern:\n")
print(special_cols)

# If an imputed output exists, compare NA counts before/after imputation
imputed_path <- "outputs/data_imputed.csv"
if (file.exists(imputed_path)) {
  cat("\nFound imputed file at:", imputed_path, "- comparing NA counts...\n")
  imputed <- read_csv(imputed_path, show_col_types = FALSE, col_select = any_of(names(job)))

  imputed_na <- sapply(imputed, function(x) sum(is.na(x)))
  imputed_pct <- imputed_na / nrow(imputed) * 100

  imputed_summary <- tibble(
    column = names(imputed_na),
    n_na_imputed = as.integer(imputed_na),
    pct_na_imputed = as.numeric(imputed_pct)
  )

  compare <- na_summary %>%
    left_join(imputed_summary, by = "column") %>%
    arrange(desc(n_na))

  cat("\nComparison of NA counts (original vs imputed):\n")
  print(compare)

  cat("\nColumns still with NAs after imputation:\n")
  print(compare %>% filter(!is.na(n_na_imputed) & n_na_imputed > 0))
} else {
  cat("\nImputed data file not found at", imputed_path, "- skipping imputed comparison.\n")
}

# See distributions for JobSat itself (unchanged)
if ("JobSat" %in% names(job)) {
  print(table(job$JobSat, useNA = "ifany"))
  # find rows where JobSat looks like "Not applicable"
  na_rows <- job %>% filter(!is.na(JobSat) & grepl("not", as.character(JobSat), ignore.case = TRUE))
  print(head(na_rows, 10))
}

# Pivot the JobSatPoints_* columns to see how points are stored
points_long <- job %>% pivot_longer(cols = starts_with("JobSatPoints_"),
                                   names_to = "point_col",
                                   values_to = "value")
points_long %>% count(point_col, value, sort = TRUE) %>% print(n = Inf)

# Diagnostics on filtered dataset (modeling features/targets)
filtered_path <- "outputs/data_filtered_columns.csv"
if (file.exists(filtered_path)) {
  cat("\nDiagnostics on filtered dataset:\n")
  df <- read_csv(filtered_path, show_col_types = FALSE)

  diag_df <- df %>%
    mutate(
      Salary = as.numeric(Salary),
      Years_Exp = as.numeric(Years_Exp),
      Job_Satisfaction = as.numeric(Job_Satisfaction),
      Uses_AI = factor(Uses_AI, levels = c(0, 1)),
      Is_Remote = factor(Is_Remote, levels = c(0, 1)),
      TimeSearching = factor(TimeSearching)
    )

  key_na <- sapply(diag_df, function(x) sum(is.na(x)))
  cat("\nKey column NA counts:\n")
  print(tibble(column = names(key_na), n_na = as.integer(key_na)))

  num_cols <- c("Salary", "Years_Exp", "Job_Satisfaction")
  num_complete <- diag_df %>%
    select(all_of(num_cols)) %>%
    filter(complete.cases(.))

  if (nrow(num_complete) > 1) {
    cat("\nSpearman correlations (numeric vars):\n")
    print(round(cor(num_complete, method = "spearman"), 3))
  } else {
    cat("\nNot enough complete numeric rows for correlation.\n")
  }

  print_spearman <- function(df_in, x, y) {
    dd <- df_in %>%
      select(all_of(c(x, y))) %>%
      filter(complete.cases(.))
    if (nrow(dd) < 3) {
      cat("\nNot enough complete cases for", x, "vs", y, "\n")
      return(invisible(NULL))
    }
    res <- suppressWarnings(cor.test(dd[[x]], dd[[y]], method = "spearman"))
    cat("\nSpearman", x, "vs", y, "rho =", round(res$estimate, 3),
        "p =", signif(res$p.value, 3), "n =", nrow(dd), "\n")
  }

  print_spearman(diag_df, "Salary", "Job_Satisfaction")
  print_spearman(diag_df, "Salary", "Years_Exp")
  print_spearman(diag_df, "Job_Satisfaction", "Years_Exp")

  summarize_by_factor <- function(df_in, fac, target) {
    df_in %>%
      select(all_of(c(fac, target))) %>%
      filter(!is.na(.data[[fac]])) %>%
      group_by(.data[[fac]]) %>%
      summarize(
        n = n(),
        mean = mean(.data[[target]], na.rm = TRUE),
        median = median(.data[[target]], na.rm = TRUE),
        .groups = "drop"
      )
  }

  cat("\nSalary by Uses_AI:\n")
  print(summarize_by_factor(diag_df, "Uses_AI", "Salary"))

  cat("\nSalary by Is_Remote:\n")
  print(summarize_by_factor(diag_df, "Is_Remote", "Salary"))

  cat("\nSalary by TimeSearching:\n")
  print(summarize_by_factor(diag_df, "TimeSearching", "Salary"))

  cat("\nJob_Satisfaction by Uses_AI:\n")
  print(summarize_by_factor(diag_df, "Uses_AI", "Job_Satisfaction"))

  cat("\nJob_Satisfaction by TimeSearching:\n")
  print(summarize_by_factor(diag_df, "TimeSearching", "Job_Satisfaction"))
} else {
  cat("\nFiltered dataset not found at", filtered_path, "- skipping model diagnostics.\n")
}