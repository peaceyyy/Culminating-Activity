library(dplyr)
library(readr)
library(mice)

df <- read_csv("outputs/data_filtered_columns.csv", show_col_types = FALSE)

cat("Running MICE imputation (PMM for numeric columns)...\n")

# #
df_subset <- df %>% 
  select(Salary, Uses_AI, Years_Exp, Job_Satisfaction, Is_Remote, TimeSearching) %>%
  mutate(
    Uses_AI = factor(Uses_AI, levels = c(0, 1)),
    Is_Remote = factor(Is_Remote, levels = c(0, 1)),
    TimeSearching = factor(TimeSearching)
  )

methods <- make.method(df_subset)
methods[] <- ""
methods["Salary"] <- "pmm"
methods["Years_Exp"] <- "pmm"
methods["Job_Satisfaction"] <- "pmm"
methods["Uses_AI"] <- "logreg"
methods["Is_Remote"] <- "logreg"
methods["TimeSearching"] <- "polyreg"

imputed_data <- mice(df_subset, m = 1, method = methods, seed = 42, printFlag = FALSE)
df_clean <- complete(imputed_data, 1)

cat("Rows retained:", nrow(df_clean), "| NAs remaining:", sum(is.na(df_clean)), "\n")

summary(lm(Salary ~ ., data = df_clean))

write_csv(df_clean, "outputs/data_imputed_pmm.csv")
cat("Exported to outputs/data_imputed_pmm.csv\n")