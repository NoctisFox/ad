if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("knitr")) install.packages("knitr")

library(tidyverse)
library(lubridate)
library(knitr)

file_name <- "cleaning/stackoverflow_combined.csv"

if (file.exists(file_name)) {
  df <- read_csv(file_name)
} else {
  stop("Error: The file 'stackoverflow_combined.csv' was not found in the current folder!")
}

# Use it in R EDA such as Positron etc
# View(df)


cat("      MAIN CHARACTERISTICS: BEFORE CLEANING\n")
cat("Size of dataset (Rows):", nrow(df), "\n")
cat("Number of variables (Columns):", ncol(df), "\n")
cat("Total number of missing values across all data:", sum(is.na(df)), "\n\n")

missing_counts <- colSums(is.na(df))
missing_counts <- missing_counts[missing_counts > 0]
cat("Columns with missing data:\n")
print(sort(missing_counts, decreasing = TRUE))

# So, from this we can see that there are, in fact, columns that have missing values, which are not
# stated in the description of the dataset: body, where it is only 22 values that missing, 
# so we can delete them; and categories, where there are 24% of the data missing

df_cleaned <- df %>%
  
  select(-comment_count, -favorite_count, -owner_badge_count, -first_response_time_seconds) %>%
  
  drop_na(body) %>%
  
  mutate(categories = replace_na(categories, "Uncategorized")) %>%
  
  mutate(
    got_response = !is.na(first_response_time_hours),
    view_count = ifelse(view_count < 0, NA, view_count)
  ) %>%
  
  
  mutate(
    creation_date = as.POSIXct(creation_date, format="%Y-%m-%dT%H:%M:%S"),
    last_activity_date = as.POSIXct(last_activity_date, format="%Y-%m-%dT%H:%M:%S"),
    
    programming_language = as.factor(programming_language),
    categories = as.factor(categories),
    is_answered = as.factor(is_answered),
    has_accepted_answer = as.factor(has_accepted_answer),
    has_code = as.factor(has_code)
  ) %>%
  
  
  filter(body_word_count > 0)



cat("      MAIN CHARACTERISTICS: AFTER CLEANING\n")
cat("======================================================\n")
cat("Size of dataset (Rows):", nrow(df_cleaned), "\n")
cat("Number of variables (Columns):", ncol(df_cleaned), "\n")
cat("Total number of missing values across all data:", sum(is.na(df_cleaned)), "\n\n")



# This method pivots the raw data first, avoiding all underscore naming errors
stats_table <- df_cleaned %>%
  # Automatically select all numeric columns, but drop the ID column as it's not a true statistic
  select(where(is.numeric), -question_id) %>%
  
  # Pivot the wide dataset into two columns: 'Variable' and 'Value'
  pivot_longer(everything(), names_to = "Variable", values_to = "Value") %>%
  
  # Group by the variable name and calculate everything
  group_by(Variable) %>%
  summarise(
    Missing_Data = sum(is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE)
  )

cat("\n--- Descriptive Statistics for Important Numeric Variables ---\n")

print(kable(stats_table, digits = 2))


write_csv(df_cleaned, "cleaned_df.csv")
cat("\nSuccess: Cleaned dataset saved as 'cleaned_df.csv' in the working directory.\n")