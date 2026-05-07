library(dplyr)
library(readr)
library(mice)

df <- read_csv("outputs/data_filtered_columns.csv", show_col_types = FALSE)

cat("Running MICE imputation (CART for numeric columns)...\n")

# Helper: parse TimeSearching text into numeric minutes
parse_time_searching <- function(x) {
  s <- as.character(x)
  s[is.na(s)] <- NA_character_
  s <- trimws(gsub(",", "", tolower(s)))

  parse_one <- function(str) {
    if (is.na(str) || str == "") return(NA_real_)
    nums <- as.numeric(unlist(regmatches(str, gregexpr("[0-9]+\\.?[0-9]*", str))))
    if (length(nums) == 0) return(NA_real_)
    val <- mean(nums)
    if (grepl("hour|hr", str)) val <- val * 60
    if (grepl("sec", str))   val <- val / 60
    if (grepl("less than|<", str)) val <- val / 2
    return(val)
  }

  vapply(s, parse_one, FUN.VALUE = numeric(1))
}

df <- df %>%
  mutate(TimeSearching_Min = parse_time_searching(TimeSearching))

df_subset <- df %>%
  select(Salary, Uses_AI, Years_Exp, Job_Satisfaction, Is_Remote, TimeSearching_Min) %>%
  mutate(
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
    Is_Remote = factor(Is_Remote, levels = c(0, 1)),
    TimeSearching_Min = as.numeric(TimeSearching_Min)
  )

methods <- make.method(df_subset)
methods[] <- ""
methods["Salary"] <- "cart"
methods["Years_Exp"] <- "cart"
methods["Job_Satisfaction"] <- "cart"
methods["Uses_AI"] <- "logreg"
methods["Is_Remote"] <- "logreg"
# TimeSearching_Min is numeric minutes; use CART
methods["TimeSearching_Min"] <- "cart"

imputed_data <- mice(df_subset, m = 1, method = methods, seed = 42, printFlag = FALSE)
df_clean <- complete(imputed_data, 1)

cat("Rows retained:", nrow(df_clean), "| NAs remaining:", sum(is.na(df_clean)), "\n")

summary(lm(Salary ~ ., data = df_clean))

write_csv(df_clean, "outputs/data_imputed_cart.csv")
cat("Exported to outputs/data_imputed_cart.csv\n")
