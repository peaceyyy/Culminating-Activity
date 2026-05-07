library(dplyr)
library(readr)

# Load raw data
df_raw <- read_csv("data/survey_results_public.csv", show_col_types = FALSE)

df_clean <- df_raw %>%

  filter(Employment == "Employed, full-time") %>%
  
  mutate(
    Salary = as.numeric(ConvertedCompYearly),
    
    Uses_AI = case_when(
      AISelect == "Yes" ~ 1L,
      AISelect %in% c("No, but I plan to soon", "No, and I don't plan to") ~ 0L,
      TRUE ~ NA_integer_
    ),
    

    Years_Exp = case_when(
      YearsCodePro == "Less than 1 year" ~ 0.5,
      YearsCodePro == "More than 50 years" ~ 50.0,
      grepl("^[0-9]+$", YearsCodePro) ~ as.numeric(YearsCodePro),
      TRUE ~ NA_real_
    ),
    

    Job_Satisfaction = as.numeric(JobSat),
    
    Is_Remote = case_when(
      RemoteWork == "Remote" ~ 1L,
      RemoteWork %in% c("In-person", "Hybrid (some remote, some in-person)") ~ 0L,
      TRUE ~ NA_integer_
    )
  ) %>%
  
  select(Salary, Uses_AI, Years_Exp, Job_Satisfaction, Is_Remote, TimeSearching)

write_csv(df_clean, "outputs/data_filtered_columns.csv")
cat("\nSuccess: Filtered dataset exported to outputs/data_filtered_columns.csv\n")