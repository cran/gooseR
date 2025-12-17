# gooseR 🦆

[![Version](https://img.shields.io/badge/version-0.1.1-blue.svg)](#)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](#)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![CRAN Status](https://img.shields.io/badge/CRAN-pending-orange.svg)](#)

gooseR brings goose AI into R. It's a comprehensive, intelligent R development assistant powered by the goose CLI.

## 🚀 What's New in v0.1.1 (Feature Complete!)

### 🔍 Intelligent Code Analysis
- **`goose_honk()`** - Actually reads your code and provides specific feedback
  - 4 severity levels: `gentle`, `moderate`, `harsh`, `brutal`
  - Detects patterns: loops, models, ggplot usage, error handling
  - Data-aware analysis - checks for missing values in your data frames

### 📊 Survey Data Tools
- **`goose_rename_columns()`** - Transform long survey questions into meaningful variable names
  - "How satisfied are you with customer service?" → `sat_cust_serv`
  - "On a scale of 1-10, how likely..." → `nps`
  - Pattern recognition for NPS, satisfaction, demographics, frequency questions
  - Saves mapping CSV for documentation

### 🎯 Essential Workflow Functions
- **`goose_give_sample()`** - Share data samples with AI for context
- **`goose_make_a_plan()`** - Get AI-powered analysis plans (exploratory/predictive/diagnostic)
- **`goose_continuation_prompt()`** - Save your work context for tomorrow
- **`goose_handoff()`** - Create comprehensive project handoffs
- **`goose_summarize_session()`** - Summarize your work session

### 💾 Enhanced Memory Management
- **Bulk operations**: `goose_backup()`, `goose_restore()`, `goose_clear_tags()`
- **Session management**: `with_goose_session()` with auto-cleanup
- **Column mapping**: `goose_view_column_map()` for renamed survey data

### 🎨 Beautiful Formatting
- **`goose_format_response()`** - Beautiful markdown formatting
- **`goose_format_table()`** - Clean table output
- **`goose_format_code()`** - Syntax-highlighted code blocks
- **`goose_format_list()`** - Formatted lists with emojis

## ✨ Core Features

- **Memory integration** for any R object - save, load, list, delete with tags
- **Brand-ready visualization** system - Block and custom themes
- **AI assistant utilities** for code reviews, docs, and debugging
- **Advanced runtime features**: streaming, caching, async, templates
- **IDE addins** (RStudio/Positron) for one-click actions

## About goose

goose is your friendly AI partner who can understand what you want to do and help you do it! The best part is that goose can learn from your preferences and remember them for next time!

- **Open Source**: Built with transparency and collaboration in mind
- **Runs Locally**: Execute tasks efficiently, keeping control in your hands
- **Extensible**: Customize with your preferred LLM and connect to any external MCP server
- **Autonomous**: Independently handles complex tasks, from debugging to deployment

[Learn more about goose](https://block.github.io/goose/docs/quickstart/)

## Installation

```r
# Install from GitHub (soon on CRAN!)
# install.packages("remotes")
remotes::install_github("blockbtheriault/gooseR")
```

## Configuration

If Goose CLI is already working on your machine, you're ready to go! No extra R-side setup needed.

```r
library(gooseR)

# Test your setup
if (goose_test_cli()) message("Goose CLI is ready! 🦆")
```

If you don't have Goose CLI configured yet:

```r
# Configure credentials (example for OpenAI)
goose_configure(provider = "openai", model = "gpt-4o", api_key = "your-key")
```

## Quick Start

```r
library(gooseR)

# Ask Goose a question
goose_ask("Summarize mtcars and suggest 2 visualizations")

# Get intelligent code review
goose_honk(severity = "moderate")  # Reviews your current script

# Clean survey data
survey_data <- read.csv("qualtrics_export.csv")
clean_data <- goose_rename_columns(survey_data)
goose_view_column_map(clean_data)  # See the mapping

# Save and load R objects with memory
model <- lm(mpg ~ wt + cyl, data = mtcars)
goose_save(model, category = "models", tags = c("mtcars", "regression"))
my_model <- goose_load("model")

# Create a branded visualization
library(ggplot2)
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  theme_brand("block") +
  labs(title = "Fuel Efficiency")
```

## Real-World Workflows

### Survey Researcher
```r
# Load messy Qualtrics data
survey <- read.csv("survey_export.csv")

# Clean column names intelligently
clean <- goose_rename_columns(survey)
# "How satisfied are you with..." → sat_overall
# "On a scale of 1-10..." → nps

# Get analysis plan
goose_make_a_plan("exploratory")

# Save for tomorrow
goose_continuation_prompt()
```

### Data Scientist
```r
# Share data context
goose_give_sample(my_data)

# Get analysis plan
plan <- goose_make_a_plan("predictive")

# Write your model...
model <- glm(outcome ~ ., data = my_data, family = binomial)

# Get tough feedback
goose_honk(severity = "harsh")

# Create handoff document
goose_handoff()
```

### Team Lead
```r
# Backup team's work
goose_backup()

# Clean up test objects
goose_clear_tags(c("test", "temp", "draft"))

# Summarize sprint work
goose_summarize_session()

# Create continuation for next sprint
goose_continuation_prompt()
```

## Key Functions by Category

### 🤖 AI Assistant
- `goose_ask()` - General AI queries
- `goose_review_code()` - AI code review
- `goose_document()` - Generate roxygen2 docs
- `goose_generate_tests()` - Create test suites
- `goose_explain_error()` - Debug errors

### 🔍 Intelligent Analysis (NEW!)
- `goose_honk()` - Context-aware code review
- `goose_make_a_plan()` - Analysis planning
- `goose_give_sample()` - Share data context
- `goose_handoff()` - Project documentation
- `goose_continuation_prompt()` - Save work context

### 📊 Data Tools (NEW!)
- `goose_rename_columns()` - Smart column renaming
- `goose_view_column_map()` - View rename mappings

### 💾 Memory Management
- `goose_save()`, `goose_load()` - Save/load any R object
- `goose_list()`, `goose_delete()` - Manage saved objects
- `goose_backup()`, `goose_restore()` - Bulk operations
- `goose_clear_tags()` - Clean by tags
- `with_goose_session()` - Temporary work sessions

### 🎨 Visualization & Branding
- `theme_brand()` - Apply brand themes
- `brand_palette()` - Access color palettes
- `brand_css()` - Export CSS styles
- `brand_rmd_template()` - RMarkdown templates

### 🎯 Formatting (NEW!)
- `goose_format_response()` - Beautiful markdown
- `goose_format_table()` - Clean tables
- `goose_format_code()` - Syntax highlighting
- `goose_format_list()` - Formatted lists

### ⚡ Advanced Features
- `goose_stream()` - Streaming responses
- `goose_cache_*()` - Caching system
- `goose_batch()` - Parallel processing
- `goose_template_*()` - Template system

## Visual Examples

<p align="center">
  <img src="docs/assets/block_line_chart.png" width="45%" />
  <img src="docs/assets/block_bar_chart.png" width="45%" />
</p>

More examples in `docs/assets/` and `inst/examples/`.

## Documentation & Examples

- **Vignettes**: Run `browseVignettes("gooseR")` for tutorials
- **Examples**: See `examples/` directory for use cases
- **Function docs**: Use `?function_name` for detailed help

## Version History

### v0.1.1 (Current) - Feature Complete! 🎉
- ✅ Intelligent code analysis with `goose_honk()`
- ✅ Survey data tools for column renaming
- ✅ Essential workflow functions (5 new)
- ✅ Enhanced memory management (10+ functions)
- ✅ Beautiful formatting utilities (4 new)
- ✅ 30+ total new functions

### v0.1.0 (Initial Release)
- Core memory integration
- Basic AI assistant
- Branding system
- Initial CLI integration

## Contributing

We welcome contributions! Please see the CONTRIBUTING.md and CODE_OF_CONDUCT.md files in the source repository at <https://github.com/blockbtheriault/gooseR>.

## License

MIT License. See [LICENSE](LICENSE) for details.

## Acknowledgments

Built with ❤️ by the Block People Analytics & Research team. Special thanks to the goose team for creating such an amazing AI platform!

---

*gooseR: Making R development more intelligent, one honk at a time!* 🦆
