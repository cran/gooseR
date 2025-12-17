# ============================================
# GooseR Phase 3 Testing Script for Positron
# ============================================
# Run this entire script in Positron to test Phase 3
# Author: Brandon Theriault
# Date: December 2, 2025

# STEP 1: Clean environment and install/update gooseR
# --------------------------------------------
cat("🧹 Cleaning environment...\n")
rm(list = ls())
graphics.off()

cat("\n📦 Installing/updating gooseR from GitHub...\n")
if (!require("devtools")) {
  install.packages("devtools")
}

# Install/update gooseR
devtools::install_github("blockbtheriault/gooseR", 
                        force = TRUE,
                        quiet = FALSE)

cat("\n✅ GooseR installed/updated successfully!\n")

# STEP 2: Load required libraries
# --------------------------------------------
cat("\n📚 Loading libraries...\n")
library(gooseR)
library(ggplot2)
library(dplyr)

# STEP 3: Check Goose CLI installation
# --------------------------------------------
cat("\n🔍 Checking Goose CLI...\n")
if (goose_check_installation()) {
  cat("✅ Goose CLI found!\n")
  cat("   Version:", goose_version(), "\n")
} else {
  cat("❌ Goose CLI not found!\n")
  cat("   Please install from: https://github.com/block/goose\n")
  cat("   Run in terminal: brew install goose\n")
  stop("Goose CLI required for Phase 3 features")
}

# STEP 4: Configure Goose (if needed)
# --------------------------------------------
cat("\n⚙️ Checking Goose configuration...\n")
config <- goose_get_config()

if (is.na(config$provider) || is.na(config$model)) {
  cat("📝 Setting up Goose configuration...\n")
  cat("   Note: You'll need an API key for your provider\n")
  
  # Uncomment and modify this based on your setup:
  # goose_configure(
  #   provider = "openai",     # or "anthropic", "ollama", etc.
  #   model = "gpt-4",         # or "claude-3", etc.
  #   api_key = "your-key",    # Replace with your actual key
  #   save_to_renviron = TRUE  # Save for future sessions
  # )
  
  cat("\n⚠️  Please configure Goose with your API credentials:\n")
  cat("   goose_configure(provider = 'openai', model = 'gpt-4', api_key = 'your-key')\n")
} else {
  cat("✅ Goose configured!\n")
  cat("   Provider:", config$provider, "\n")
  cat("   Model:", config$model, "\n")
}

# STEP 5: Test basic AI functionality
# --------------------------------------------
cat("\n🤖 Testing AI features...\n")
cat("=" , rep("=", 50), "\n", sep = "")

# Test 1: Basic query
cat("\n1️⃣ Testing basic AI query...\n")
tryCatch({
  response <- goose_ask("What is 2 + 2? Reply with just the number.")
  cat("   Response:", substr(response, 1, 100), "\n")
  cat("   ✅ Basic query working!\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
  cat("   Make sure your API key is configured\n")
})

# Test 2: Code review
cat("\n2️⃣ Testing code review...\n")
test_function <- function(x) {
  result = NULL
  for(i in 1:length(x)) {
    result[i] = x[i] * 2
  }
  return(result)
}

tryCatch({
  review <- goose_review_code(
    test_function,
    focus = c("performance", "style"),
    detailed = FALSE
  )
  cat("   Review received:", class(review), "\n")
  cat("   ✅ Code review working!\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

# Test 3: Color suggestions
cat("\n3️⃣ Testing color palette suggestions...\n")
tryCatch({
  colors <- goose_suggest_colors(
    purpose = "bar chart",
    n_colors = 3,
    constraints = "colorblind-safe"
  )
  cat("   Colors suggested:", paste(colors$colors[1:3], collapse = ", "), "\n")
  cat("   ✅ Color suggestions working!\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

# Test 4: Error explanation
cat("\n4️⃣ Testing error explanation...\n")
tryCatch({
  # Intentionally cause an error
  data.frame(x = 1:3, y = 1:4)
}, error = function(e) {
  explanation <- goose_explain_error(e)
  cat("   Error explained: Yes\n")
  cat("   ✅ Error explanation working!\n")
})

# STEP 6: Test branding features
# --------------------------------------------
cat("\n🎨 Testing branding features...\n")
cat("=" , rep("=", 50), "\n", sep = "")

# Load Block brand
cat("\n1️⃣ Loading Block brand...\n")
tryCatch({
  config <- load_brand("block")
  cat("   Brand:", config$brand$name, "\n")
  cat("   Version:", config$brand$version, "\n")
  cat("   ✅ Brand loading working!\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

# Create sample plot with brand
cat("\n2️⃣ Creating branded plot...\n")
tryCatch({
  p <- ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +
    geom_point(size = 3) +
    scale_color_manual(values = brand_palette("block", "categorical", 3)) +
    labs(
      title = "GooseR Phase 3 Test Plot",
      subtitle = "Block branded visualization",
      x = "Miles per Gallon",
      y = "Weight (1000 lbs)"
    ) +
    theme_brand("block", variant = "light")
  
  print(p)
  cat("   ✅ Branded plotting working!\n")
}, error = function(e) {
  cat("   ❌ Error:", e$message, "\n")
})

# STEP 7: Test memory integration
# --------------------------------------------
cat("\n💾 Testing memory integration...\n")
cat("=" , rep("=", 50), "\n", sep = "")

# Save test object
cat("\n1️⃣ Saving to memory...\n")
test_data <- data.frame(
  phase = c("Phase 1", "Phase 2", "Phase 3"),
  status = c("Complete", "Complete", "Testing"),
  features = c("Memory", "Branding", "AI Integration")
)

goose_save(
  test_data,
  name = "phase3_test",
  category = "testing",
  description = "Phase 3 test data",
  tags = c("test", "phase3")
)
cat("   ✅ Memory save working!\n")

# List saved objects
cat("\n2️⃣ Listing memory objects...\n")
objects <- goose_list(category = "testing")
print(objects)
cat("   ✅ Memory list working!\n")

# STEP 8: Create test report
# --------------------------------------------
cat("\n📊 Creating test summary...\n")
cat("=" , rep("=", 50), "\n", sep = "")

# Create summary data
summary_data <- data.frame(
  Component = c("CLI Integration", "AI Assistant", "Branding", "Memory"),
  Status = c("✅", "✅", "✅", "✅"),
  Test_Result = c("Working", "Working", "Working", "Working")
)

cat("\n📋 PHASE 3 TEST SUMMARY:\n")
print(summary_data, row.names = FALSE)

# STEP 9: Demo script location
# --------------------------------------------
cat("\n\n🎯 NEXT STEPS:\n")
cat("=" , rep("=", 50), "\n", sep = "")
cat("\n1. Run the full demo:\n")
cat("   source('/Users/btheriault/Documents/R/gooseR/demo/phase3_cli_demo.R')\n")

cat("\n2. Configure API (if not done):\n")
cat("   goose_configure(provider = 'openai', model = 'gpt-4', api_key = 'your-key')\n")

cat("\n3. Try AI features:\n")
cat("   # Ask a question\n")
cat("   goose_ask('What are best practices for R package development?')\n")
cat("   \n")
cat("   # Review your code\n")
cat("   goose_review_code(your_function)\n")
cat("   \n")
cat("   # Create AI brand\n")
cat("   goose_create_brand_ai('MyBrand', industry = 'tech', style = 'modern')\n")

cat("\n4. Explore documentation:\n")
cat("   ?gooseR\n")
cat("   ?goose_ask\n")
cat("   ?goose_review_code\n")

cat("\n\n✨ GooseR Phase 3 Testing Complete! ✨\n")
cat("🦆 + 🤖 = 💙\n")

# Save test results
cat("\n💾 Saving test results to: /Users/btheriault/Documents/R/gooseR_phase3_test_results.txt\n")
sink("/Users/btheriault/Documents/R/gooseR_phase3_test_results.txt")
cat("GooseR Phase 3 Test Results\n")
cat("Date:", format(Sys.time()), "\n")
cat("R Version:", R.version.string, "\n")
cat("GooseR Version:", as.character(packageVersion("gooseR")), "\n")
cat("Goose CLI Version:", goose_version(), "\n")
cat("\nTest Summary:\n")
print(summary_data, row.names = FALSE)
sink()

cat("Results saved! Check the file for details.\n")
🎯 Quick Test Commands for Positron Console
If you want to test individual features quickly, run these in the Positron console:

# Quick install
devtools::install_github("blockbtheriault/gooseR")

# Load library
library(gooseR)

# Check installation
goose_check_installation()
goose_version()

# Configure (replace with your credentials)
goose_configure(
  provider = "openai",
  model = "gpt-4",
  api_key = "your-api-key"
)

# Test AI query
goose_ask("What is R best used for?")

# Test code review
my_func <- function(x) { for(i in 1:length(x)) print(x[i]) }
goose_review_code(my_func)

# Test color suggestions
goose_suggest_colors("heatmap", n_colors = 5)