#!/usr/bin/env Rscript

# GooseR Phase 3: CLI Integration Demo
# Author: Brandon Theriault
# Date: December 2025
# 
# This demo showcases the AI-powered features of gooseR Phase 3

# Load the package
library(gooseR)
library(ggplot2)

cat("===========================================\n")
cat("   GooseR Phase 3: CLI Integration Demo   \n")
cat("===========================================\n\n")

# Check if Goose CLI is available
if (!goose_check_installation()) {
  stop("Goose CLI not found. Please install from: https://github.com/block/goose")
}

cat("✅ Goose CLI version:", goose_version(), "\n\n")

# Demo 1: Basic AI Query
cat("Demo 1: Basic AI Query\n")
cat("----------------------\n")

response <- goose_ask("What are the best practices for creating color palettes in R data visualization?")
cat("AI Response:\n", substr(response, 1, 200), "...\n\n")

# Demo 2: Code Review
cat("Demo 2: AI Code Review\n")
cat("----------------------\n")

# Example function to review
calculate_mean <- function(x) {
  total = 0
  for(i in 1:length(x)) {
    total = total + x[i]
  }
  return(total / length(x))
}

review <- goose_review_code(
  calculate_mean,
  focus = c("performance", "style"),
  context = "Calculate mean of numeric vector"
)

cat("Code Review Summary:\n")
print(review)
cat("\n")

# Demo 3: Color Palette Suggestions
cat("Demo 3: AI Color Palette Suggestions\n")
cat("------------------------------------\n")

palette <- goose_suggest_colors(
  purpose = "dashboard for financial data",
  n_colors = 5,
  constraints = "colorblind-safe, professional"
)

cat("Suggested colors:", paste(palette$colors, collapse = ", "), "\n\n")

# Demo 4: Error Explanation
cat("Demo 4: AI Error Explanation\n")
cat("----------------------------\n")

# Intentionally cause an error
tryCatch({
  data.frame(x = 1:3, y = 1:4)
}, error = function(e) {
  explanation <- goose_explain_error(
    error = e,
    context = "Creating a data frame with mismatched column lengths"
  )
  cat("Error explained:\n")
  cat(substr(explanation$explanation, 1, 300), "...\n\n")
})

# Demo 5: Generate Documentation
cat("Demo 5: AI Documentation Generation\n")
cat("-----------------------------------\n")

# Function to document
process_data <- function(df, group_var, summary_func = mean) {
  df %>%
    group_by({{group_var}}) %>%
    summarise(result = summary_func(value))
}

docs <- goose_document(process_data, examples = TRUE)
cat("Generated documentation:\n")
cat(substr(docs, 1, 300), "...\n\n")

# Demo 6: Plot Optimization
cat("Demo 6: AI Plot Optimization\n")
cat("----------------------------\n")

plot_code <- "
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  labs(title = 'Car Data')
"

optimization <- goose_optimize_plot(
  plot_code,
  goals = c("aesthetics", "clarity", "accessibility")
)

cat("Optimization suggestions received\n")
cat("Original code length:", nchar(plot_code), "characters\n")
cat("Optimized code length:", nchar(optimization$optimized), "characters\n\n")

# Demo 7: AI-Enhanced Brand Creation
cat("Demo 7: AI-Enhanced Brand Creation\n")
cat("----------------------------------\n")

# Create a brand with AI assistance (interactive in real use)
brand_path <- goose_create_brand_ai(
  brand_name = "DataViz Pro",
  industry = "data analytics",
  style = "modern and professional",
  use_ai = TRUE,
  output_dir = tempdir()
)

cat("Brand created at:", brand_path, "\n\n")

# Demo 8: Session Management
cat("Demo 8: Session Management\n")
cat("--------------------------\n")

# Create a session for context preservation
session <- goose_session("create", name = "demo_session")
cat("Created session:", session$session_id, "\n")

# Query with session context
response1 <- goose_query("What is R?", session = session)
response2 <- goose_query("What are its main uses?", session = session)

cat("Context preserved across queries\n\n")

# Demo 9: Generate Unit Tests
cat("Demo 9: AI Test Generation\n")
cat("--------------------------\n")

add_numbers <- function(a, b) {
  if (!is.numeric(a) || !is.numeric(b)) {
    stop("Both arguments must be numeric")
  }
  return(a + b)
}

tests <- goose_generate_tests(add_numbers, test_cases = 3, edge_cases = TRUE)
cat("Generated tests:\n")
cat(substr(tests, 1, 300), "...\n\n")

# Demo 10: Brand Palette Optimization
cat("Demo 10: Brand Palette Optimization\n")
cat("-----------------------------------\n")

# Optimize the Block brand palette
optimization <- goose_optimize_palette(
  "block",
  goals = c("accessibility", "contrast"),
  constraints = "keep primary blue"
)

cat("Palette optimization complete\n")
cat("Original colors:", length(optimization$original), "\n")
cat("Optimized colors:", length(optimization$optimized), "\n\n")

# Summary
cat("===========================================\n")
cat("         Demo Complete! 🎉                \n")
cat("===========================================\n\n")

cat("Phase 3 Features Demonstrated:\n")
cat("✅ Direct Goose CLI integration\n")
cat("✅ AI-powered code review\n")
cat("✅ Color palette suggestions\n")
cat("✅ Error explanation\n")
cat("✅ Documentation generation\n")
cat("✅ Plot optimization\n")
cat("✅ AI-enhanced brand creation\n")
cat("✅ Session management\n")
cat("✅ Unit test generation\n")
cat("✅ Palette optimization\n\n")

cat("Next Steps:\n")
cat("1. Configure your API key: goose_configure()\n")
cat("2. Try the AI assistant: goose_ask('your question')\n")
cat("3. Review your code: goose_review_code(your_function)\n")
cat("4. Create custom brands: goose_create_brand_ai()\n\n")

cat("For more information, see:\n")
cat("- ?gooseR for package documentation\n")
cat("- vignette('cli-integration') for detailed guide\n")
cat("- https://github.com/blockbtheriault/gooseR\n")
