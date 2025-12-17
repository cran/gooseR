## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(gooseR)

## -----------------------------------------------------------------------------
# # Install from GitHub (CRAN submission pending)
# # install.packages("remotes")
# remotes::install_github("blockbtheriault/gooseR")

## -----------------------------------------------------------------------------
# library(gooseR)
# 
# # Test the connection
# if (goose_test_cli()) {
#   message("✅ Goose CLI is ready!")
# } else {
#   message("❌ Need to configure goose - see next section")
# }

## -----------------------------------------------------------------------------
# # For OpenAI users
# goose_configure(
#   provider = "openai",
#   model = "gpt-4o",
#   api_key = "your-api-key-here"
# )
# 
# # For Anthropic Claude users
# goose_configure(
#   provider = "anthropic",
#   model = "claude-3-opus",
#   api_key = "your-api-key-here"
# )

## -----------------------------------------------------------------------------
# # Ask about your data
# goose_ask("What are the key characteristics of the mtcars dataset?")
# 
# # Get analysis suggestions
# goose_ask("What statistical tests would be appropriate for comparing mpg across different numbers of cylinders?")
# 
# # Request code examples
# response <- goose_ask("Show me how to create a correlation matrix heatmap in R")
# cat(response)

## -----------------------------------------------------------------------------
# # Write some code
# my_analysis <- function(data) {
#   # Calculate mean without checking for NA
#   avg <- mean(data$value)
# 
#   # Using a loop instead of vectorized operation
#   results <- c()
#   for(i in 1:nrow(data)) {
#     results[i] <- data$value[i] * 2
#   }
# 
#   return(list(avg = avg, doubled = results))
# }
# 
# # Get a gentle review
# goose_honk(severity = "gentle")
# 
# # Get more critical feedback
# goose_honk(severity = "moderate")
# 
# # For tough love
# goose_honk(severity = "harsh")

## -----------------------------------------------------------------------------
# # Create a model
# model <- lm(mpg ~ wt + cyl + hp, data = mtcars)
# 
# # Save it with tags for easy retrieval
# goose_save(
#   model,
#   category = "models",
#   tags = c("mtcars", "regression", "fuel_efficiency")
# )
# 
# # List saved objects
# goose_list(category = "models")
# 
# # Load it back (even in a new session)
# my_model <- goose_load("model")
# summary(my_model)

## -----------------------------------------------------------------------------
# library(ggplot2)
# 
# # Create a plot with Block branding
# p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#   geom_point(size = 3, alpha = 0.7) +
#   geom_smooth(method = "lm", se = TRUE) +
#   theme_brand("block") +  # Apply Block theme
#   labs(
#     title = "Fuel Efficiency vs Weight",
#     subtitle = "Linear relationship in mtcars dataset",
#     x = "Weight (1000 lbs)",
#     y = "Miles per Gallon"
#   )
# 
# print(p)
# 
# # Access brand colors
# colors <- brand_palette("block", "categorical")

## -----------------------------------------------------------------------------
# # Load your data
# my_data <- read.csv("my_dataset.csv")
# 
# # Share a sample with goose for context
# goose_give_sample(my_data)
# 
# # Get an analysis plan
# plan <- goose_make_a_plan("exploratory")
# cat(plan)
# 
# # Do your analysis...
# # ...
# 
# # Get feedback on your approach
# goose_honk(severity = "moderate")
# 
# # Save your work for tomorrow
# goose_continuation_prompt()

## -----------------------------------------------------------------------------
# # Load survey data with long question names
# survey <- read.csv("qualtrics_export.csv")
# 
# # Automatically rename columns intelligently
# clean_survey <- goose_rename_columns(survey)
# 
# # View the mapping
# goose_view_column_map(clean_survey)
# # "How satisfied are you with our customer service?" → "sat_cust_serv"
# # "On a scale of 1-10, how likely are you to recommend..." → "nps"

## -----------------------------------------------------------------------------
# # Before starting work, backup existing objects
# goose_backup()
# 
# # Work in a temporary session that auto-cleans
# with_goose_session({
#   # Experimental work here
#   test_model <- lm(mpg ~ ., data = mtcars)
#   goose_save(test_model, category = "temp", tags = "experiment")
# 
#   # This will be auto-cleaned when session ends
# }, cleanup = TRUE)
# 
# # Create a handoff document for your colleague
# goose_handoff()
# 
# # Clean up test objects
# goose_clear_tags(c("test", "temp", "draft"))

## -----------------------------------------------------------------------------
# # Get help on any function
# ?goose_ask
# ?goose_honk
# ?goose_save
# 
# # Ask goose for help!
# goose_ask("How do I use goose_rename_columns with custom abbreviations?")
# 
# # Check your gooseR version
# packageVersion("gooseR")

