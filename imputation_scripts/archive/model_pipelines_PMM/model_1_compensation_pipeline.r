library(dplyr)
library(readr)

set.seed(42)

df <- read_csv("outputs/data_imputed_pmm.csv", show_col_types = FALSE) %>%
  mutate(
    Salary = as.numeric(Salary),
    Years_Exp = as.numeric(Years_Exp),
    Uses_AI = factor(Uses_AI, levels = c(0, 1)),
    Is_Remote = factor(Is_Remote, levels = c(0, 1))
  )

formula <- Salary ~ Uses_AI + Years_Exp + Is_Remote

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
    imputation = "pmm_imputed",
    model = "model_1_compensation",
    r2 = unname(s$r.squared),
    adj_r2 = unname(s$adj.r.squared),
    cv_r2 = cv_r2(data, formula, k = 5, seed = 42)
  )
}

summary_results <- calc_metrics(df, formula)

dir.create("outputs/model summaries/pmm", recursive = TRUE, showWarnings = FALSE)
write_csv(summary_results, "outputs/model summaries/pmm/model_1_compensation_summary.csv")

cat("\n======================================================\n")
cat("MODEL 1 (Dependent - Salary)\n")
print(summary_results)
print(summary(lm(formula, data = df)))
