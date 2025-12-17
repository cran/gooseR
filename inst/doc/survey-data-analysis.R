## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(gooseR)
# library(dplyr)
# library(ggplot2)

## -----------------------------------------------------------------------------
# # Example of raw survey data columns
# raw_columns <- c(
#   "ResponseId",
#   "How satisfied are you with our customer service on a scale of 1-5?",
#   "On a scale of 0-10, how likely are you to recommend our product to a friend or colleague?",
#   "What is your annual household income before taxes?",
#   "How often do you use our product? (Daily, Weekly, Monthly, Rarely, Never)",
#   "Please rate your agreement: The product meets my needs",
#   "In which age range do you fall? (18-24, 25-34, 35-44, 45-54, 55-64, 65+)",
#   "What is your primary reason for using our product? (Please select all that apply)"
# )
# 
# print(raw_columns)

## -----------------------------------------------------------------------------
# # Load your survey data
# survey_data <- read.csv("survey_export.csv")
# 
# # Automatically rename columns with intelligent abbreviations
# clean_data <- goose_rename_columns(survey_data)
# 
# # View what happened
# goose_view_column_map(clean_data)

## -----------------------------------------------------------------------------
# # Pattern Recognition Examples:
# 
# # Satisfaction questions
# "How satisfied are you with our customer service?"
# # → "sat_cust_serv"
# 
# # NPS (Net Promoter Score)
# "On a scale of 0-10, how likely are you to recommend..."
# # → "nps"
# 
# # Demographics
# "What is your annual household income before taxes?"
# # → "hh_income"
# 
# "In which age range do you fall?"
# # → "age_range"
# 
# # Frequency questions
# "How often do you use our product?"
# # → "use_freq"
# 
# # Likert scales
# "Please rate your agreement: The product meets my needs"
# # → "agree_meets_needs"
# 
# # Multiple choice
# "What is your primary reason for using our product?"
# # → "primary_reason"

## -----------------------------------------------------------------------------
# # Create custom dictionary for your domain
# custom_dict <- list(
#   "customer service" = "cs",
#   "artificial intelligence" = "ai",
#   "machine learning" = "ml",
#   "return on investment" = "roi",
#   "key performance indicator" = "kpi"
# )
# 
# # Apply with custom dictionary
# clean_data <- goose_rename_columns(
#   survey_data,
#   custom_abbrev = custom_dict,
#   max_length = 20  # Maximum variable name length
# )

## -----------------------------------------------------------------------------
# # Load raw survey data
# survey <- read.csv("qualtrics_export.csv",
#                    stringsAsFactors = FALSE)
# 
# # Check the messy column names
# names(survey)[1:5]
# 
# # Clean the column names
# survey_clean <- goose_rename_columns(survey)
# 
# # Check the clean names
# names(survey_clean)[1:5]
# 
# # Save the mapping for documentation
# mapping <- goose_view_column_map(survey_clean)
# write.csv(mapping, "column_mapping.csv", row.names = FALSE)

## -----------------------------------------------------------------------------
# # Share a sample with goose for context
# goose_give_sample(survey_clean)
# 
# # Get an analysis plan
# plan <- goose_make_a_plan("exploratory")
# cat(plan)
# 
# # Ask specific questions
# goose_ask("What's the best way to analyze Likert scale data in this survey?")
# 
# goose_ask("How should I handle missing data in the income field?")

## -----------------------------------------------------------------------------
# # Now you can use clean names in your analysis
# survey_clean %>%
#   group_by(age_range) %>%
#   summarise(
#     avg_satisfaction = mean(sat_overall, na.rm = TRUE),
#     avg_nps = mean(nps, na.rm = TRUE),
#     n = n()
#   ) %>%
#   arrange(desc(avg_satisfaction))
# 
# # Create visualizations with clean labels
# ggplot(survey_clean, aes(x = age_range, y = sat_overall)) +
#   geom_boxplot() +
#   theme_brand("block") +
#   labs(
#     title = "Satisfaction by Age Group",
#     x = "Age Range",
#     y = "Overall Satisfaction"
#   )

## -----------------------------------------------------------------------------
# # Get feedback on your analysis approach
# goose_honk(severity = "moderate")
# 
# # Create documentation
# goose_handoff()
# 
# # Save your work
# goose_save(
#   survey_clean,
#   category = "survey_data",
#   tags = c("cleaned", "q3_2024", "customer_satisfaction")
# )
# 
# # Create a continuation prompt for next session
# goose_continuation_prompt()

## -----------------------------------------------------------------------------
# # Process multiple survey files consistently
# files <- c("survey_q1.csv", "survey_q2.csv", "survey_q3.csv")
# 
# all_surveys <- lapply(files, function(file) {
#   data <- read.csv(file)
#   goose_rename_columns(data)
# })
# 
# # Combine with consistent naming
# combined <- bind_rows(all_surveys, .id = "quarter")

## -----------------------------------------------------------------------------
# # After renaming
# clean_data <- goose_rename_columns(survey_data)
# 
# # Access the mapping
# attr(clean_data, "column_map")
# 
# # Or use the helper function
# mapping <- goose_view_column_map(clean_data)
# 
# # Use in reports
# library(knitr)
# kable(
#   mapping,
#   caption = "Survey Variable Mapping",
#   col.names = c("Variable Name", "Original Question")
# )

## -----------------------------------------------------------------------------
# # Set maximum length
# clean <- goose_rename_columns(survey, max_length = 15)

## -----------------------------------------------------------------------------
# # gooseR automatically handles duplicates by adding numbers
# # "satisfaction_1", "satisfaction_2", etc.

## -----------------------------------------------------------------------------
# # gooseR automatically removes special characters
# # "What's your opinion?" → "opinion"
# # "Rate 1-5: Service" → "rate_service"

## -----------------------------------------------------------------------------
# # Combine with AI analysis
# goose_give_sample(clean_data)
# advice <- goose_ask("What statistical tests should I use for this Likert scale data?")
# 
# # Get code review
# goose_honk(severity = "gentle")
# 
# # Save for later
# goose_save(clean_data, category = "surveys", tags = c("2024", "cleaned"))
# 
# # Create formatted output
# results <- clean_data %>%
#   group_by(age_range) %>%
#   summarise(mean_sat = mean(sat_overall, na.rm = TRUE))
# 
# goose_format_table(results)

