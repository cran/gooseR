## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----setup--------------------------------------------------------------------
# library(gooseR)

## -----------------------------------------------------------------------------
# # Gentle - Encouraging and constructive
# goose_honk(severity = "gentle")
# # "I notice you're using a loop here. Have you considered using lapply()?
# #  It might be more efficient! Your code structure looks good overall! 🦆"
# 
# # Moderate - Balanced and professional
# goose_honk(severity = "moderate")
# # "Your loop on line 15 could be replaced with vectorized operations for
# #  better performance. Also, consider adding error handling for the mean()
# #  function in case of NA values."
# 
# # Harsh - Direct and critical
# goose_honk(severity = "harsh")
# # "That loop is killing your performance. Use vectorization. No error
# #  handling? That's asking for production failures. Fix the variable
# #  naming - 'x' and 'df' tell me nothing."
# 
# # Brutal - No holds barred
# goose_honk(severity = "brutal")
# # "This code is a disaster. Loops in R? Really? No error handling,
# #  meaningless variable names, and zero documentation. Did you even
# #  test this? Start over and do it right."

## -----------------------------------------------------------------------------
# # A typical analysis script with issues
# analyze_sales <- function(sales_data) {
#   # Calculate totals
#   total = 0
#   for(i in 1:nrow(sales_data)) {
#     total = total + sales_data$amount[i]
#   }
# 
#   # Get average
#   avg = mean(sales_data$amount)
# 
#   # Find best month
#   best = sales_data[sales_data$amount == max(sales_data$amount),]
# 
#   # Make plot
#   plot(sales_data$amount)
# 
#   return(list(total, avg, best))
# }
# 
# # Get gentle feedback first
# goose_honk(severity = "gentle")

## -----------------------------------------------------------------------------
# # Now let's get more critical feedback
# goose_honk(severity = "harsh")

## -----------------------------------------------------------------------------
# analyze_sales <- function(sales_data) {
#   # Input validation
#   if (is.null(sales_data) || nrow(sales_data) == 0) {
#     stop("sales_data cannot be NULL or empty")
#   }
#   if (!"amount" %in% names(sales_data)) {
#     stop("sales_data must contain 'amount' column")
#   }
# 
#   # Calculate metrics with NA handling
#   total_sales <- sum(sales_data$amount, na.rm = TRUE)
#   avg_sales <- mean(sales_data$amount, na.rm = TRUE)
# 
#   # Find best months (handle ties)
#   max_amount <- max(sales_data$amount, na.rm = TRUE)
#   best_months <- sales_data[sales_data$amount == max_amount &
#                            !is.na(sales_data$amount), ]
# 
#   # Create informative visualization
#   library(ggplot2)
#   p <- ggplot(sales_data, aes(x = seq_along(amount), y = amount)) +
#     geom_line() +
#     geom_point() +
#     theme_brand("block") +
#     labs(title = "Sales Trend", x = "Period", y = "Sales Amount")
# 
#   print(p)
# 
#   # Return named list
#   return(list(
#     total = total_sales,
#     average = avg_sales,
#     best_months = best_months,
#     plot = p
#   ))
# }
# 
# # Check our improvements
# goose_honk(severity = "moderate")

## -----------------------------------------------------------------------------
# # It recognizes dplyr chains
# result <- data %>%
#   filter(x > 10) %>%
#   group_by(category) %>%
#   summarise(mean = mean(value))
# 
# goose_honk()
# # "Good use of dplyr! Consider adding .groups = 'drop' to summarise()
# #  to avoid the grouped data frame warning."

## -----------------------------------------------------------------------------
# # It understands modeling
# model <- lm(mpg ~ wt + cyl, data = mtcars)
# 
# goose_honk()
# # "Linear model looks good. Have you checked assumptions?
# #  Consider plot(model) for diagnostics. Also, you might want
# #  to check for multicollinearity between wt and cyl."

## -----------------------------------------------------------------------------
# # It recognizes ggplot2
# p <- ggplot(data, aes(x, y)) + geom_point()
# 
# goose_honk()
# # "Basic scatter plot. Consider adding labels with labs(),
# #  applying a theme, and perhaps adding a trend line with
# #  geom_smooth() if appropriate."

## -----------------------------------------------------------------------------
# # Your function
# calculate_bmi <- function(weight_kg, height_m) {
#   if (height_m <= 0) stop("Height must be positive")
#   if (weight_kg <= 0) stop("Weight must be positive")
# 
#   bmi <- weight_kg / (height_m ^ 2)
# 
#   category <- if (bmi < 18.5) "Underweight"
#   else if (bmi < 25) "Normal"
#   else if (bmi < 30) "Overweight"
#   else "Obese"
# 
#   return(list(bmi = bmi, category = category))
# }
# 
# # Generate tests
# tests <- goose_generate_tests("calculate_bmi")
# cat(tests)

## -----------------------------------------------------------------------------
# # Your function
# clean_text <- function(text, remove_numbers = FALSE, lowercase = TRUE) {
#   if (lowercase) text <- tolower(text)
#   text <- gsub("[[:punct:]]", " ", text)
#   if (remove_numbers) text <- gsub("[0-9]", "", text)
#   text <- gsub("\\s+", " ", text)
#   trimws(text)
# }
# 
# # Generate documentation
# docs <- goose_document("clean_text")
# cat(docs)

## -----------------------------------------------------------------------------
# # You get an error
# data <- read.csv("myfile.csv")
# model <- lm(y ~ x1 + x2 + x3, data = data)
# # Error: object 'y' not found
# 
# # Get help
# error_help <- goose_explain_error()
# cat(error_help)

## -----------------------------------------------------------------------------
# # 1. Write your function
# my_function <- function(data) {
#   # Initial implementation
#   result <- process_data(data)
#   return(result)
# }
# 
# # 2. Get initial review
# goose_honk(severity = "gentle")
# 
# # 3. Improve based on feedback
# my_function <- function(data) {
#   # Improved implementation with error handling
#   if (is.null(data)) stop("Data cannot be NULL")
#   result <- process_data(data)
#   return(result)
# }
# 
# # 4. Get stricter review
# goose_honk(severity = "moderate")
# 
# # 5. Generate documentation
# docs <- goose_document("my_function")
# 
# # 6. Generate tests
# tests <- goose_generate_tests("my_function")
# 
# # 7. Final review
# goose_honk(severity = "harsh")
# 
# # 8. Save your work
# goose_save(my_function, category = "functions", tags = c("reviewed", "tested"))

