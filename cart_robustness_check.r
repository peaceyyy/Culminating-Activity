   library(dplyr)
library(readr)
library(mice)
library(rpart)

# MICE seed sensitivity check and CP profile for CART
imputation_seeds <- c(42, 100, 200, 300, 400)
m <- 5
maxit <- 5
cart_params <- list(cp = 0.001, minsplit = 20, maxdepth = 5)
output_dir <- "outputs"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Load Raw Data
df_raw <- read_csv("outputs/data_filtered_columns.csv", show_col_types = FALSE)

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

# Populate TimeSearching_Min from TimeSearching
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

# Subset and cast
df_subset <- df_raw %>%
  select(Job_Satisfaction, Uses_AI, Salary, TimeSearching_Min, Years_Exp, Is_Remote) %>%
  mutate(
    Job_Satisfaction = as.numeric(Job_Satisfaction),
    Salary = as.numeric(Salary),
    TimeSearching_Min = as.numeric(TimeSearching_Min),
    Years_Exp = as.numeric(Years_Exp),
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

normalize_types <- function(df) {
  df %>%
    mutate(
      Job_Satisfaction = as.numeric(Job_Satisfaction),
      Salary = as.numeric(Salary),
      TimeSearching_Min = as.numeric(TimeSearching_Min),
      Years_Exp = as.numeric(Years_Exp),
      Uses_AI = factor(as.numeric(as.character(Uses_AI)), levels = c(0, 1)),
      Is_Remote = factor(as.numeric(as.character(Is_Remote)), levels = c(0, 1))
    )
}

predictors <- c("Uses_AI", "Salary", "TimeSearching_Min", "Years_Exp", "Is_Remote")
cart_formula <- as.formula(paste("Job_Satisfaction ~", paste(predictors, collapse = " + ")))

fit_cart <- function(df, params) {
  rpart(
    cart_formula,
    data = df,
    method = "anova",
    control = rpart.control(
      minsplit = params$minsplit,
      cp = params$cp,
      maxdepth = params$maxdepth
    )
  )
}

extract_importance <- function(model, predictors) {
  imp_vec <- setNames(rep(0, length(predictors)), predictors)
  if (!is.null(model$variable.importance)) {
    imp_vec[names(model$variable.importance)] <- model$variable.importance
  }
  imp_vec
}

get_xerror <- function(model) {
  cpt <- model$cptable
  if (is.null(cpt)) NA_real_ else min(cpt[, "xerror"])
}

cat("\nRunning MICE seed sensitivity check...\n")
seed_details <- list()

for (seed in imputation_seeds) {
  cat("Seed:", seed, "\n")
  set.seed(seed)
  imp <- mice(df_subset, method = "cart", m = m, maxit = maxit, printFlag = FALSE)

  for (i in seq_len(m)) {
    df_i <- complete(imp, i) %>% normalize_types()
    model <- fit_cart(df_i, cart_params)
    imp_vec <- extract_importance(model, predictors)
    imp_df <- as.data.frame(t(imp_vec), check.names = FALSE)

    seed_details[[length(seed_details) + 1]] <- data.frame(
      seed = seed,
      imputation = i,
      nsplit = length(model$splits),
      xerror = get_xerror(model),
      imp_df,
      check.names = FALSE
    )
  }
}

seed_details <- bind_rows(seed_details)
seed_summary <- seed_details %>%
  group_by(seed) %>%
  summarise(
    nsplit = mean(nsplit, na.rm = TRUE),
    xerror = mean(xerror, na.rm = TRUE),
    across(all_of(predictors), mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(xerror, desc(nsplit))

write_csv(seed_details, file.path(output_dir, "cart_mice_seed_runs.csv"))
write_csv(seed_summary, file.path(output_dir, "cart_mice_seed_averages.csv"))

cat("\nSeed sensitivity summary:\n")
print(seed_summary)

# CP profile on a baseline imputation
cat("\nGenerating CP profile...\n")
baseline_seed <- imputation_seeds[1]
set.seed(baseline_seed)
baseline_imp <- mice(df_subset, method = "cart", m = 1, maxit = maxit, printFlag = FALSE)
baseline_df <- complete(baseline_imp, 1) %>% normalize_types()
baseline_model <- fit_cart(baseline_df, cart_params)
cp_table <- as.data.frame(baseline_model$cptable)

write_csv(cp_table, file.path(output_dir, "cart_cp_profile.csv"))

cat("\nCP profile table:\n")
print(cp_table)
