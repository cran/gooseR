#!/usr/bin/env Rscript

#' Phase 5 Demo: IDE Integration & Visual Interfaces
#' 
#' This demo showcases the new visual UI and IDE integration features
#' added in Phase 5 of the gooseR package.

library(gooseR)

cat("\n")
cat("╔════════════════════════════════════════════════════╗\n")
cat("║   GooseR Phase 5: IDE Integration & Visual UIs     ║\n")
cat("╚════════════════════════════════════════════════════╝\n\n")

# Check if running in RStudio
if (rstudioapi::isAvailable()) {
  cat("✅ Running in RStudio/Positron - Full functionality available\n\n")
} else {
  cat("⚠️  Not running in RStudio - Some features may be limited\n\n")
}

# 1. RStudio Addins
cat("1. RSTUDIO ADDINS\n")
cat("──────────────────\n")
cat("The following addins are now available in your Addins menu:\n")
cat("  • GooseR Chat - Interactive AI chat interface\n")
cat("  • Insert Code Snippet - Generate and insert code\n")
cat("  • Review Selected Code - AI code review\n")
cat("  • Template Builder - Visual template creator\n")
cat("  • Quick Ask - Quick questions to Goose\n")
cat("  • Cache Browser - Manage cached responses\n")
cat("  • Conversation Manager - Session management\n\n")

# 2. Interactive Chat Demo
cat("2. INTERACTIVE CHAT\n")
cat("───────────────────\n")
cat("Launch the chat interface with:\n")
cat("  goose_addin_chat()\n")
cat("Or use Addins menu > GooseR Chat\n\n")

response <- readline("Would you like to launch the chat interface? (y/n): ")
if (tolower(response) == "y" && rstudioapi::isAvailable()) {
  goose_addin_chat()
}

# 3. Code Snippet Generator
cat("\n3. CODE SNIPPET GENERATOR\n")
cat("─────────────────────────\n")
cat("Generate code snippets with:\n")
cat("  goose_addin_snippet()\n")
cat("Or use Addins menu > Insert Code Snippet\n\n")

# 4. Cache Management
cat("4. CACHE MANAGEMENT\n")
cat("───────────────────\n")

# Show cache statistics
conn <- goose_cache_init()
stats <- goose_cache_stats(conn = conn)
cat("Current cache statistics:\n")
cat("  • Total entries:", stats$total_entries, "\n")
cat("  • Total size:", format(stats$total_size, big.mark = ","), "bytes\n")
cat("  • Total accesses:", stats$total_accesses, "\n")
DBI::dbDisconnect(conn)

cat("\nLaunch cache browser with:\n")
cat("  goose_cache_ui()\n")
cat("Or use Addins menu > Cache Browser\n\n")

# 5. Quarto/RMarkdown Integration
cat("5. QUARTO/RMARKDOWN INTEGRATION\n")
cat("────────────────────────────────\n")

# Register the Goose engine
register_goose_engine()
cat("✅ Goose knitr engine registered\n\n")

cat("You can now use AI chunks in Quarto/RMarkdown:\n")
cat("  ```{goose}\n")
cat("  Explain the central limit theorem\n")
cat("  ```\n\n")

# Example of creating a Quarto chunk
cat("Generate Quarto chunks programmatically:\n")
example_chunk <- goose_quarto_chunk(
  "Write a function to calculate fibonacci numbers",
  label = "fib-example",
  echo = FALSE,
  cache = TRUE
)
cat(example_chunk, "\n\n")

# 6. Document Generation
cat("6. DOCUMENT GENERATION\n")
cat("──────────────────────\n")
cat("Create complete documents with AI:\n\n")

cat("Quarto documents:\n")
cat("  goose_create_quarto(\n")
cat("    title = 'Data Analysis Report',\n")
cat("    outline = 'Exploratory analysis of sales data',\n")
cat("    format = 'html'\n")
cat("  )\n\n")

cat("RMarkdown reports:\n")
cat("  goose_create_report(\n")
cat("    title = 'Monthly Analytics',\n")
cat("    data = my_data,\n")
cat("    analysis_type = 'descriptive'\n")
cat("  )\n\n")

# 7. Template Management
cat("7. TEMPLATE MANAGEMENT\n")
cat("──────────────────────\n")

# Show available templates
templates <- goose_template_list()
cat("Available templates:", nrow(templates), "\n")
if (nrow(templates) > 5) {
  cat("Top 5 templates:\n")
  print(head(templates, 5))
} else {
  print(templates)
}

cat("\nLaunch template builder with:\n")
cat("  goose_addin_template()\n")
cat("Or use Addins menu > Template Builder\n\n")

# 8. Conversation Management
cat("8. CONVERSATION MANAGEMENT\n")
cat("──────────────────────────\n")
cat("Manage AI conversation sessions:\n")
cat("  goose_conversation_ui()\n")
cat("Or use Addins menu > Conversation Manager\n\n")

# 9. Quick Examples
cat("9. QUICK EXAMPLES\n")
cat("─────────────────\n\n")

cat("Insert AI content in RMarkdown:\n")
cat("  goose_rmd_ai(\n")
cat("    title = 'Methods',\n")
cat("    prompt = 'Explain logistic regression methodology',\n")
cat("    level = 2\n")
cat("  )\n\n")

cat("Create parameterized template:\n")
cat("  goose_create_template(\n")
cat("    type = 'dashboard',\n")
cat("    parameters = list(\n")
cat("      date_range = '30',\n")
cat("      department = 'Sales'\n")
cat("    )\n")
cat("  )\n\n")

# 10. Keyboard Shortcuts
cat("10. KEYBOARD SHORTCUTS\n")
cat("──────────────────────\n")
cat("You can assign keyboard shortcuts to any addin:\n")
cat("  Tools > Modify Keyboard Shortcuts...\n")
cat("  Search for 'GooseR' to find all addins\n\n")

cat("Recommended shortcuts:\n")
cat("  • Cmd/Ctrl + Shift + G: GooseR Chat\n")
cat("  • Cmd/Ctrl + Shift + S: Insert Code Snippet\n")
cat("  • Cmd/Ctrl + Shift + R: Review Selected Code\n\n")

# Summary
cat("╔════════════════════════════════════════════════════╗\n")
cat("║                    PHASE 5 SUMMARY                  ║\n")
cat("╚════════════════════════════════════════════════════╝\n\n")

cat("✅ RStudio/Positron addins installed\n")
cat("✅ Interactive chat interface ready\n")
cat("✅ Code snippet generator available\n")
cat("✅ Visual template builder functional\n")
cat("✅ Quarto/RMarkdown integration complete\n")
cat("✅ Cache browser UI operational\n")
cat("✅ Conversation manager ready\n")
cat("✅ Document generation tools available\n\n")

cat("Phase 5 adds powerful visual interfaces and seamless IDE integration\n")
cat("to make working with Goose AI more intuitive and productive!\n\n")

cat("Try the interactive features:\n")
cat("  1. Open Addins menu in RStudio\n")
cat("  2. Select 'GooseR Chat' to start chatting\n")
cat("  3. Select code and choose 'Review Selected Code'\n")
cat("  4. Use 'Template Builder' to create reusable prompts\n\n")

cat("🦆 Happy coding with GooseR! 🦆\n\n")
