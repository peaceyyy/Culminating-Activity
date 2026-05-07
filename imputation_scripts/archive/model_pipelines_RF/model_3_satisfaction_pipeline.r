library(dplyr)
library(readr)

set.seed(42)

df <- read_csv("outputs/data_imputed_rf.csv", show_col_types = FALSE) %>%
  mutate(
    Salary = as.numeric(Salary),
    Job_Satisfaction = as.numeric(Job_Satisfaction),
    Uses_AI = factor(Uses_AI, levels = c(0, 1)),
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

df <- df %>%
  mutate(TimeSearching_Min = sapply(as.character(TimeSearching), time_to_minutes))

formula <- Job_Satisfaction ~ Salary + Uses_AI + TimeSearching_Min

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

calc_metrics <- function(data, formula) {
  fit <- lm(formula, data = data)
  s <- summary(fit)
  tibble(
    imputation = "rf_imputed",
    model = "model_3_satisfaction",
    r2 = unname(s$r.squared),
    adj_r2 = unname(s$adj.r.squared),
    cv_r2 = cv_r2(data, formula, k = 5, seed = 42)
  )
}

summary_results <- calc_metrics(df, formula)

dir.create("outputs/model summaries/rf", recursive = TRUE, showWarnings = FALSE)
write_csv(summary_results, "outputs/model summaries/rf/model_3_satisfaction_summary.csv")

cat("\n======================================================\n")
cat("MODEL 3: Dependent - Job_Satisfaction\n")
cat("======================================================\n")
print(summary_results)
print(summary(lm(formula, data = df)))
