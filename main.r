library(dplyr)
library(readr)
library(mice)
library(rpart)
library(rpart.plot)

# 1. Load Raw Data
# Read the filtered columns file used elsewhere in the repo
df_raw <- read_csv("outputs/data_filtered_columns.csv", show_col_types = FALSE)

# Helper: parse TimeSearching text into numeric minutes (robust)
parse_time_searching <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- NA_character_
  s <- trimws(gsub(",", "", tolower(s)))

  parse_one <- function(str) {
    if (is.na(str) || str == "") return(NA_real_)
    # Known categorical choices from the survey
    if (grepl("less than 15|under 15", str)) return(7.5)
    if (grepl("15[- ]?to[- ]?30|15[- ]?30", str)) return(22.5)
    if (grepl("30[- ]?to[- ]?60|30[- ]?60", str)) return(45)
    if (grepl("60[- ]?to[- ]?120|60[- ]?120", str)) return(90)
    if (grepl("over 120|more than 120|120[+]", str)) return(150)
    nums <- as.numeric(unlist(regmatches(str, gregexpr("[0-9]+\\.?[0-9]*", str))))
    if (length(nums) == 0) return(NA_real_)
    val <- mean(nums)
    if (grepl("hour|hr", str)) val <- val * 60
    if (grepl("sec", str)) val <- val / 60
    if (grepl("less than|<", str)) val <- val / 2
    return(val)
  }

  vapply(s, parse_one, FUN.VALUE = numeric(1))
}

# Populate `TimeSearching_Min` from `TimeSearching` when possible, preserving any existing numeric column
if ("TimeSearching" %in% names(df_raw)) {
  if ("TimeSearching_Min" %in% names(df_raw)) {
    df_raw <- df_raw %>%
      mutate(
        TimeSearching_Min = suppressWarnings(as.numeric(TimeSearching_Min)),
        TimeSearching_Min = ifelse(is.na(TimeSearching_Min), parse_time_searching(TimeSearching), TimeSearching_Min)
      )
  } else {
    df_raw <- df_raw %>%
      mutate(TimeSearching_Min = parse_time_searching(TimeSearching))
  }
}

# 2. Isolate and Type-Cast the Variables
# We subset the data before imputation so MICE only learns from relevant variables
df_subset <- df_raw %>%
  select(Job_Satisfaction, Uses_AI, Salary, TimeSearching_Min, Years_Exp, Is_Remote) %>%
  mutate(
    Job_Satisfaction = as.numeric(Job_Satisfaction),
    Salary = as.numeric(Salary),
    TimeSearching_Min = as.numeric(TimeSearching_Min),
    Years_Exp = as.numeric(Years_Exp),
    # Robust coercion for binary-encoded variables that may be "Yes"/"No" or 1/0
    Uses_AI = case_when(
      Uses_AI %in% c("1", 1, "Yes", "YES", "yes", "Y", "y", TRUE) ~ 1,
      Uses_AI %in% c("0", 0, "No", "NO", "no", "N", "n", FALSE) ~ 0,
      TRUE ~ NA_real_
    ),
    Is_Remote = case_when(
      Is_Remote %in% c("1", 1, "Yes", "YES", "yes", "Y", "y", TRUE) ~ 1,
      Is_Remote %in% c("0", 0, "No", "NO", "no", "N", "n", FALSE) ~ 0,
      TRUE ~ NA_real_
    ),
    Uses_AI = factor(Uses_AI, levels = c(0, 1)),
    Is_Remote = factor(Is_Remote, levels = c(0, 1))
  )

#  MICE on the entire subset at once using the CART method
imputed_output_path <- "outputs/data_imputed_primary.csv"

if (file.exists(imputed_output_path)) {
  cat("\nImputed dataset found. Skipping MICE and loading existing file...\n")
  df_clean <- read_csv(imputed_output_path, show_col_types = FALSE)
} else {
  cat("\nRunning MICE imputation...\n")
  set.seed(42)
  imputed_data <- mice(df_subset, method = "cart", m = 5, maxit = 5, printFlag = FALSE)

  # Extract the completed dataset (using the first imputed dataset)
  df_clean <- complete(imputed_data, 1)

  # Save the clean dataset for reuse in future runs
  write_csv(df_clean, imputed_output_path)
}

# Normalize types after loading or imputing so model input stays consistent
df_clean <- df_clean %>%
  mutate(
    Job_Satisfaction = as.numeric(Job_Satisfaction),
    Salary = as.numeric(Salary),
    TimeSearching_Min = as.numeric(TimeSearching_Min),
    Years_Exp = as.numeric(Years_Exp),
    Uses_AI = factor(as.numeric(as.character(Uses_AI)), levels = c(0, 1)),
    Is_Remote = factor(as.numeric(as.character(Is_Remote)), levels = c(0, 1))
  )


cat("\nHyperparameter shuffle (CART)...\n")
predictors <- c("Uses_AI", "Salary", "TimeSearching_Min", "Years_Exp", "Is_Remote")
cart_formula <- as.formula(paste("Job_Satisfaction ~", paste(predictors, collapse = " + ")))

hyper_grid <- expand.grid(
  cp = c(0.001, 0.002, 0.005, 0.008, 0.01, 0.02),
  minsplit = c(20, 40),
  maxdepth = c(5, 8),
  stringsAsFactors = FALSE
)

run_cart <- function(df, formula, predictors, cp, minsplit, maxdepth, seed) {
  set.seed(seed)
  model <- rpart(
    formula,
    data = df,
    method = "anova",
    control = rpart.control(minsplit = minsplit, cp = cp, maxdepth = maxdepth)
  )

  imp_vec <- setNames(rep(0, length(predictors)), predictors)
  if (!is.null(model$variable.importance)) {
    imp_vec[names(model$variable.importance)] <- model$variable.importance
  }

  cpt <- model$cptable
  xerror <- if (is.null(cpt)) NA_real_ else min(cpt[, "xerror"])

  data.frame(
    cp = cp,
    minsplit = minsplit,
    maxdepth = maxdepth,
    nsplit = length(model$splits),
    xerror = xerror,
    t(imp_vec),
    stringsAsFactors = FALSE
  )
}

sweep_results <- lapply(seq_len(nrow(hyper_grid)), function(i) {
  row <- hyper_grid[i, ]
  run_cart(df_clean, cart_formula, predictors, row$cp, row$minsplit, row$maxdepth, seed = 27)
}) %>%
  bind_rows()

sweep_ranked <- sweep_results %>%
  mutate(
    status = case_when(
      nsplit == 0 ~ "no_splits",
      is.na(xerror) ~ "xerror_na",
      TRUE ~ "ok"
    )
  ) %>%
  arrange(is.na(xerror), xerror, desc(nsplit)) %>%
  mutate(rank = row_number()) %>%
  select(rank, status, cp, minsplit, maxdepth, xerror, nsplit, all_of(predictors))

cat("\nFull hyperparameter shuffle results:\n")
print(sweep_ranked)

# Save the shuffle table for reporting/debugging
write_csv(sweep_ranked, "outputs/cart_hyperparam_shuffle.csv")

best_row <- sweep_ranked %>%
  filter(nsplit > 0) %>%
  slice(1)

if (nrow(best_row) == 0) {
  best_row <- sweep_ranked %>% slice(1)
}

cat("\nTraining CART Model (best by xerror)...\n")
tree_model <- rpart(
  cart_formula,
  data = df_clean,
  method = "anova",
  control = rpart.control(
    minsplit = best_row$minsplit[1],
    cp = best_row$cp[1],
    maxdepth = best_row$maxdepth[1]
  )
)

# 5. Output Results
cat("\n======================================================\n")
cat("--- VARIABLE IMPORTANCE ---\n")
if (is.null(tree_model$variable.importance)) {
  cat("None (no splits produced at these settings)\n")
} else {
  print(tree_model$variable.importance)
}

cat("\n--- MODEL SUMMARY ---\n")
print(tree_model)

cat("\n--- BEST HYPERPARAMS ---\n")
print(best_row %>% select(cp, minsplit, maxdepth, xerror, nsplit))

# 6. Visualize the Decision Tree
rpart.plot(
  tree_model,
  type = 2,
  cex = 0.8,
  main = "Predictive CART Model: Job Satisfaction",
  box.palette = "Blues"
)