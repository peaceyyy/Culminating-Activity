library(dplyr)
library(readr)
library(mice)

set.seed(42)

raw_df <- read_csv("outputs/data_filtered_columns.csv", show_col_types = FALSE)

base_df <- raw_df %>%
  mutate(
    Years_Exp = as.numeric(Years_Exp),
    Uses_AI = factor(Uses_AI, levels = c(0, 1)),
    Is_Remote = factor(Is_Remote, levels = c(0, 1)),
    TimeSearching = as.character(TimeSearching)
  )

time_to_minutes <- function(x) {
  x <- tolower(trimws(x))
  if (is.na(x) || x == "") {
    return(NA_real_)
  }
  if (grepl("less than", x)) {
    return(10)
  }
  if (grepl("more than", x) && grepl("hour", x)) {
    return(150)
  }
  nums <- regmatches(x, gregexpr("[0-9]+", x))
  if (length(nums) > 0 && length(nums[[1]]) > 0) {
    return(as.numeric(nums[[1]][1]))
  }
  return(NA_real_)
}

imp_df <- base_df %>%
  mutate(TimeSearching = factor(TimeSearching)) %>%
  select(TimeSearching, Uses_AI, Years_Exp, Is_Remote)

make_methods <- function(data, numeric_method) {
  m <- make.method(data)
  m[] <- ""
  m["Years_Exp"] <- numeric_method
  m["TimeSearching"] <- "polyreg"
  m["Uses_AI"] <- "logreg"
  m["Is_Remote"] <- "logreg"
  m
}

cv_r2 <- function(data, formula, k = 5, seed = 42) {
  set.seed(seed)
  n <- nrow(data)
  folds <- sample(rep(1:k, length.out = n))
  r2_vals <- sapply(1:k, function(fold_id) {
    train <- data[folds != fold_id, , drop = FALSE]
    test <- data[folds == fold_id, , drop = FALSE]
    fit <- lm(formula, data = train)
    preds <- predict(fit, newdata = test)
    y <- model.response(model.frame(formula, data = test))
    sse <- sum((y - preds) ^ 2, na.rm = TRUE)
    sst <- sum((y - mean(y, na.rm = TRUE)) ^ 2, na.rm = TRUE)
    if (sst == 0) {
      return(NA_real_)
    }
    1 - sse / sst
  })
  mean(r2_vals, na.rm = TRUE)
}

calc_metrics <- function(data, formula, imputation_label, model_label) {
  fit <- lm(formula, data = data)
  s <- summary(fit)
  tibble(
    imputation = imputation_label,
    model = model_label,
    r2 = unname(s$r.squared),
    adj_r2 = unname(s$adj.r.squared),
    cv_r2 = cv_r2(data, formula, k = 5, seed = 42)
  )
}

formula <- TimeSearching_Min ~ Uses_AI + Years_Exp + Is_Remote

run_imputation_eval <- function(data, numeric_method, label) {
  imp <- mice(
    data,
    m = 5,
    method = make_methods(data, numeric_method),
    seed = 42,
    maxit = 5,
    printFlag = FALSE
  )

  results <- lapply(1:imp$m, function(i) {
    completed <- complete(imp, i) %>%
      mutate(TimeSearching_Min = sapply(as.character(TimeSearching), time_to_minutes))
    calc_metrics(completed, formula, label, "model_2_productivity")
  })

  bind_rows(results) %>%
    group_by(imputation, model) %>%
    summarize(
      r2 = mean(r2, na.rm = TRUE),
      adj_r2 = mean(adj_r2, na.rm = TRUE),
      cv_r2 = mean(cv_r2, na.rm = TRUE),
      .groups = "drop"
    )
}

cat("Running imputation + model diagnostics...\n")

# Sensitivity runs across imputation methods for numeric columns

cart_results <- run_imputation_eval(imp_df, "cart", "cart")
pmm_results <- run_imputation_eval(imp_df, "pmm", "pmm")
rf_results <- run_imputation_eval(imp_df, "rf", "rf")

summary_results <- bind_rows(cart_results, pmm_results, rf_results) %>%
  arrange(desc(adj_r2))

print(summary_results)

dir.create("outputs/model summaries/all", recursive = TRUE, showWarnings = FALSE)
write_csv(summary_results, "outputs/model summaries/all/model_2_productivity_summary.csv")
cat("\nSaved summary to outputs/model summaries/all/model_2_productivity_summary.csv\n")

cat("\n======================================================\n")
cat("MODEL 2: THE PRODUCTIVITY PARADOX (Dependent: TimeSearching_Min)\n")
cat("======================================================\n")

primary_method <- "pmm"

primary_imp <- mice(
  imp_df,
  m = 1,
  method = make_methods(imp_df, primary_method),
  seed = 42,
  maxit = 5,
  printFlag = FALSE
)

primary_data <- complete(primary_imp, 1) %>%
  mutate(TimeSearching_Min = sapply(as.character(TimeSearching), time_to_minutes))

print(summary(lm(formula, data = primary_data)))
