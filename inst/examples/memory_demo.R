# gooseR Memory Integration Demo
# Showcasing the power of Goose + R integration

library(gooseR)
library(tidyverse)

# ============================================================================
# DEMO 1: Basic Memory Operations
# ============================================================================

cat("\n=== DEMO 1: Basic Memory Operations ===\n\n")

# Save a simple dataset
demo_data <- data.frame(
  id = 1:100,
  value = rnorm(100),
  category = sample(c("A", "B", "C"), 100, replace = TRUE)
)

goose_save(
  demo_data,
  name = "demo_dataset",
  category = "examples",
  tags = c("demo", "tutorial"),
  description = "Sample dataset for gooseR demonstration"
)

# Load it back
loaded_data <- goose_load("demo_dataset", category = "examples")
cat("\nLoaded data preview:\n")
print(head(loaded_data))

# ============================================================================
# DEMO 2: Model Versioning Workflow
# ============================================================================

cat("\n\n=== DEMO 2: Model Versioning Workflow ===\n\n")

# Create multiple model versions
cat("Training multiple model versions...\n")

# Version 1: Simple model
model_v1 <- lm(mpg ~ wt, data = mtcars)
goose_save(
  model_v1,
  name = "mpg_model_v1",
  category = "models",
  tags = c("regression", "mtcars", "v1"),
  description = "Simple linear model: mpg ~ wt"
)

# Version 2: Add horsepower
model_v2 <- lm(mpg ~ wt + hp, data = mtcars)
goose_save(
  model_v2,
  name = "mpg_model_v2",
  category = "models",
  tags = c("regression", "mtcars", "v2"),
  description = "Enhanced model: mpg ~ wt + hp"
)

# Version 3: Full model
model_v3 <- lm(mpg ~ wt + hp + cyl + am, data = mtcars)
goose_save(
  model_v3,
  name = "mpg_model_v3",
  category = "models",
  tags = c("regression", "mtcars", "v3", "production"),
  description = "Production model: mpg ~ wt + hp + cyl + am"
)

# List all models
cat("\nAll saved models:\n")
goose_list(category = "models")

# Filter for production models
cat("\n\nProduction models only:\n")
goose_list(tags = "production")

# ============================================================================
# DEMO 3: Analysis Results Tracking
# ============================================================================

cat("\n\n=== DEMO 3: Analysis Results Tracking ===\n\n")

# Calculate model performance metrics
calculate_metrics <- function(model, name) {
  predictions <- predict(model, mtcars)
  residuals <- mtcars$mpg - predictions
  
  metrics <- data.frame(
    model = name,
    r_squared = summary(model)$r.squared,
    adj_r_squared = summary(model)$adj.r.squared,
    rmse = sqrt(mean(residuals^2)),
    mae = mean(abs(residuals)),
    n_predictors = length(coef(model)) - 1
  )
  
  return(metrics)
}

# Calculate metrics for all models
all_metrics <- bind_rows(
  calculate_metrics(model_v1, "v1"),
  calculate_metrics(model_v2, "v2"),
  calculate_metrics(model_v3, "v3")
)

cat("Model comparison:\n")
print(all_metrics)

# Save the comparison
goose_save(
  all_metrics,
  name = "model_comparison_results",
  category = "results",
  tags = c("metrics", "comparison", "mtcars"),
  description = "Performance comparison of all model versions"
)

# ============================================================================
# DEMO 4: Complex Objects (Lists, Nested Data)
# ============================================================================

cat("\n\n=== DEMO 4: Complex Objects ===\n\n")

# Create a complex analysis artifact
analysis_bundle <- list(
  model = model_v3,
  data = mtcars,
  predictions = predict(model_v3, mtcars),
  metrics = all_metrics[all_metrics$model == "v3", ],
  metadata = list(
    created_date = Sys.Date(),
    analyst = "Brandon Theriault",
    purpose = "MPG prediction analysis"
  )
)

goose_save(
  analysis_bundle,
  name = "complete_analysis_v3",
  category = "analysis_bundles",
  tags = c("complete", "production", "mtcars"),
  description = "Complete analysis bundle with model, data, and results"
)

cat("Saved complex analysis bundle\n")

# Load and verify
loaded_bundle <- goose_load("complete_analysis_v3", category = "analysis_bundles")
cat("\nBundle contents:\n")
cat("- Model class:", class(loaded_bundle$model), "\n")
cat("- Data dimensions:", dim(loaded_bundle$data), "\n")
cat("- Predictions length:", length(loaded_bundle$predictions), "\n")
cat("- Analyst:", loaded_bundle$metadata$analyst, "\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n\n" , rep("=", 70), "\n", sep = "")
cat("🎉 DEMO COMPLETE!\n")
cat(rep("=", 70), "\n\n", sep = "")

cat("What you've learned:\n")
cat("✅ Save R objects to Goose memory with goose_save()\n")
cat("✅ Load objects back with goose_load()\n")
cat("✅ List and filter objects with goose_list()\n")
cat("✅ Organize with categories and tags\n")
cat("✅ Track model versions and experiments\n")
cat("✅ Store complex analysis bundles\n")

cat("\n💡 Next steps:\n")
cat("- Explore your own data and models\n")
cat("- Build reproducible analysis workflows\n")
cat("- Share artifacts with your team\n")
cat("- Check out the package documentation: ?goose_save\n")

cat("\n🦆 Happy analyzing with gooseR!\n\n")
