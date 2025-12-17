# gooseR Essential Functions
# Advanced utilities for seamless R-Goose integration
# Version: 0.1.1
# Date: 2025-12-03

#' @importFrom stats coef formula
#' @importFrom utils head sessionInfo
NULL

#' Share R Object Structure with Goose
#'
#' @description
#' Captures the structure and sample values of an R object and saves it
#' in a format that Goose can understand and reference when providing code.
#' This enables Goose to write accurate code that references actual column names
#' and understands data types.
#'
#' @param object An R object (data.frame, matrix, tibble, list, etc.)
#' @param name Optional custom name for the object. Defaults to the object's name.
#' @param rows Number of sample rows to include (default: 5)
#' @param save_to_memory Whether to save to Goose memory (default: TRUE)
#'
#' @return Invisible NULL. Prints object summary and saves to memory.
#'
#' @examples
#' \dontrun{
#' # Share a data frame with Goose
#' goose_give_sample(mtcars)
#' 
#' # Share with custom name
#' goose_give_sample(iris, "flower_data")
#' 
#' # Share just structure without saving
#' goose_give_sample(my_model, save_to_memory = FALSE)
#' }
#'
#' @export
goose_give_sample <- function(object, 
                             name = NULL, 
                             rows = 5,
                             save_to_memory = TRUE) {
  
  # Get object name if not provided
  if (is.null(name)) {
    name <- deparse(substitute(object))
  }
  
  # Create comprehensive object summary
  summary_list <- list(
    name = name,
    class = class(object),
    dimensions = dim(object),
    size = format(object.size(object), units = "auto"),
    timestamp = Sys.time()
  )
  
  # Handle different object types
  if (is.data.frame(object) || is.matrix(object)) {
    summary_list$columns <- colnames(object)
    summary_list$column_types <- sapply(object, class)
    summary_list$sample_data <- utils::head(object, rows)
    summary_list$summary_stats <- summary(object)
    summary_list$missing_values <- colSums(is.na(object))
  } else if (is.list(object)) {
    summary_list$structure <- utils::str(object, max.level = 2)
    summary_list$names <- names(object)
    summary_list$element_types <- sapply(object, class)
  } else if (inherits(object, "lm") || inherits(object, "glm")) {
    summary_list$model_summary <- summary(object)
    summary_list$coefficients <- coef(object)
    summary_list$formula <- formula(object)
  } else {
    summary_list$structure <- utils::str(object)
  }
  
  # Format output for display
  cat("\n", cli::style_bold(cli::col_blue("[Data] Object Shared with Goose")), "\n")
  cat(rep("-", 50), "\n", sep = "")
  cat("Name:", cli::col_green(name), "\n")
  cat("Class:", paste(summary_list$class, collapse = ", "), "\n")
  
  if (!is.null(summary_list$dimensions)) {
    cat("Dimensions:", paste(summary_list$dimensions, collapse = " x "), "\n")
  }
  cat("Size:", summary_list$size, "\n")
  
  if (!is.null(summary_list$columns)) {
    cat("\n", cli::style_bold("Columns:"), "\n")
    col_info <- data.frame(
      Column = summary_list$columns,
      Type = as.character(summary_list$column_types),
      Missing = summary_list$missing_values
    )
    print(col_info, row.names = FALSE)
    
    cat("\n", cli::style_bold("Sample Data:"), "\n")
    print(summary_list$sample_data)
  }
  
  # Save to memory if requested
  if (save_to_memory) {
    memory_data <- paste0(
      "R_OBJECT_STRUCTURE: ", name, "\n",
      "CLASS: ", paste(summary_list$class, collapse = ", "), "\n",
      "SIZE: ", summary_list$size, "\n",
      if (!is.null(summary_list$columns)) {
        paste0("COLUMNS: ", paste(summary_list$columns, collapse = ", "), "\n",
               "TYPES: ", paste(summary_list$column_types, collapse = ", "))
      } else {
        paste0("STRUCTURE: ", capture.output(str(object, max.level = 1)))
      }
    )
    
    tryCatch({
      goose_save(object, name, 
                category = "shared_objects",
                tags = c("structure", "reference"),
                description = paste("Object structure shared for code generation"),
                global = FALSE)  # Save to local project memory
      cli::cli_alert_success("Object structure saved to Goose memory")
    }, error = function(e) {
      cli::cli_alert_warning("Could not save to memory: {e$message}")
    })
  }
  
  invisible(summary_list)
}

#' Generate Analysis Plan from Shared Objects
#'
#' @description
#' Reviews all objects shared in the current session and generates
#' a comprehensive, phased analysis plan with specific recommendations.
#'
#' @param focus Optional focus area: "exploratory", "predictive", "descriptive", "diagnostic"
#' @param output_format Format for the plan: "console", "markdown", "html"
#'
#' @return Analysis plan as text or formatted output
#'
#' @examples
#' \dontrun{
#' # Generate comprehensive plan
#' goose_make_a_plan()
#' 
#' # Focus on predictive modeling
#' goose_make_a_plan(focus = "predictive")
#' 
#' # Save plan as markdown
#' goose_make_a_plan(output_format = "markdown")
#' }
#'
#' @export
goose_make_a_plan <- function(focus = NULL, 
                              output_format = "console") {
  
  cli::cli_h1("[Goose] Goose Analysis Planner")
  
  # Get all shared objects from memory
  shared_objects <- tryCatch({
    goose_list(category = "shared_objects", global = FALSE)
  }, error = function(e) {
    return(NULL)
  })
  
  # Check if we have any shared objects
  if (is.null(shared_objects) || (is.data.frame(shared_objects) && nrow(shared_objects) == 0)) {
    cat("No objects have been shared yet.\n")
    cat("Use goose_give_sample() to share data with Goose.\n")
    return(invisible(NULL))
  }
  
  # Build analysis plan
  plan <- list(
    timestamp = Sys.time(),
    objects = shared_objects,
    focus = focus %||% "comprehensive"
  )
  
  # Generate plan sections
  # Get object names based on the structure of shared_objects
  if (is.data.frame(shared_objects)) {
    object_names <- shared_objects$name
  } else {
    object_names <- names(shared_objects)
  }
  
  plan_text <- c(
    "# [Data] Data Analysis Plan",
    paste0("Generated: ", format(plan$timestamp, "%Y-%m-%d %H:%M:%S")),
    "",
    "## [Files] Available Objects",
    paste0("- ", object_names),
    "",
    "## [Target] Phase 1: Data Understanding",
    "- [ ] Explore data structure and types",
    "- [ ] Check for missing values and outliers",
    "- [ ] Generate summary statistics",
    "- [ ] Create initial visualizations",
    "",
    "## [Search] Phase 2: Data Quality",
    "- [ ] Identify data quality issues",
    "- [ ] Handle missing values",
    "- [ ] Detect and handle outliers",
    "- [ ] Validate data consistency",
    "",
    "## [Chart] Phase 3: Exploratory Analysis",
    "- [ ] Univariate analysis",
    "- [ ] Bivariate relationships",
    "- [ ] Correlation analysis",
    "- [ ] Feature importance"
  )
  
  # Add focus-specific sections
  if (!is.null(focus)) {
    if (focus == "predictive") {
      plan_text <- c(plan_text,
        "",
        "## [AI] Phase 4: Predictive Modeling",
        "- [ ] Feature engineering",
        "- [ ] Train/test split",
        "- [ ] Model selection",
        "- [ ] Model evaluation",
        "- [ ] Hyperparameter tuning"
      )
    } else if (focus == "diagnostic") {
      plan_text <- c(plan_text,
        "",
        "## [Lab] Phase 4: Diagnostic Analysis",
        "- [ ] Root cause analysis",
        "- [ ] Hypothesis testing",
        "- [ ] Causal inference",
        "- [ ] Sensitivity analysis"
      )
    }
  }
  
  plan_text <- c(plan_text,
    "",
    "## [List] Phase 5: Reporting",
    "- [ ] Create executive summary",
    "- [ ] Generate visualizations",
    "- [ ] Document methodology",
    "- [ ] Prepare recommendations",
    "",
    "## [Idea] Suggested Next Steps",
    "1. Run `goose_give_sample()` on additional datasets",
    "2. Use `goose_ask()` for specific analysis questions",
    "3. Use `goose_honk()` to review and improve your approach"
  )
  
  # Output based on format
  if (output_format == "console") {
    cat(paste(plan_text, collapse = "\n"))
  } else if (output_format == "markdown") {
    filename <- paste0("analysis_plan_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".md")
    writeLines(plan_text, filename)
    cli::cli_alert_success("Plan saved to {.file {filename}}")
    return(filename)
  } else if (output_format == "html") {
    if (requireNamespace("markdown", quietly = TRUE)) {
      md_text <- paste(plan_text, collapse = "\n")
      html <- markdown::markdownToHTML(text = md_text)
      filename <- paste0("analysis_plan_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html")
      writeLines(html, filename)
      cli::cli_alert_success("Plan saved to {.file {filename}}")
      return(filename)
    } else {
      cli::cli_alert_warning("Package 'markdown' needed for HTML output")
    }
  }
  
  invisible(plan_text)
}

#' Convert Code to Loop Structure
#'
#' @description
#' Takes a script or code block and converts it to an efficient loop structure
#' based on the specified iteration requirements.
#'
#' @param code Character string or file path containing the code to loop
#' @param loop_over What to loop over (e.g., "files", "columns", "rows", "list elements")
#' @param iterator_name Name for the loop variable (default: "i")
#' @param parallel Whether to use parallel processing (default: FALSE)
#'
#' @return Modified code with loop structure
#'
#' @examples
#' \dontrun{
#' # Convert file processing to loop
#' code <- "data <- read.csv('file.csv')\nsummary(data)"
#' goose_loop_me(code, loop_over = "files")
#' 
#' # Create parallel loop
#' goose_loop_me("process_data(df)", loop_over = "datasets", parallel = TRUE)
#' }
#'
#' @export
goose_loop_me <- function(code, 
                         loop_over, 
                         iterator_name = "i",
                         parallel = FALSE) {
  
  cli::cli_h2("[Loop] Loop Generator")
  
  # Check if code is a file path
  if (length(code) == 1 && file.exists(code)) {
    code_lines <- readLines(code)
    code <- paste(code_lines, collapse = "\n")
    cli::cli_alert_info("Reading code from file: {.file {code}}")
  }
  
  # Determine loop type
  loop_templates <- list(
    files = list(
      setup = "files <- list.files(pattern = '*.csv', full.names = TRUE)",
      loop = "for (file in files) {",
      iterator = "file",
      parallel_setup = "library(parallel)\nfiles <- list.files(pattern = '*.csv', full.names = TRUE)\nresults <- mclapply(files, function(file) {"
    ),
    columns = list(
      setup = "columns <- names(data)",
      loop = "for (col in columns) {",
      iterator = "col",
      parallel_setup = "library(parallel)\ncolumns <- names(data)\nresults <- mclapply(columns, function(col) {"
    ),
    rows = list(
      setup = "n_rows <- nrow(data)",
      loop = "for (i in 1:n_rows) {",
      iterator = "i",
      parallel_setup = "library(parallel)\nn_rows <- nrow(data)\nresults <- mclapply(1:n_rows, function(i) {"
    ),
    datasets = list(
      setup = "datasets <- list(df1, df2, df3)  # Add your datasets",
      loop = "for (dataset in datasets) {",
      iterator = "dataset",
      parallel_setup = "library(parallel)\ndatasets <- list(df1, df2, df3)\nresults <- mclapply(datasets, function(dataset) {"
    )
  )
  
  # Match loop type
  loop_type <- tolower(loop_over)
  template <- NULL
  
  for (type in names(loop_templates)) {
    if (grepl(type, loop_type)) {
      template <- loop_templates[[type]]
      break
    }
  }
  
  if (is.null(template)) {
    # Generic loop
    template <- list(
      setup = paste0(iterator_name, "_list <- c()  # Define your items"),
      loop = paste0("for (", iterator_name, " in ", iterator_name, "_list) {"),
      iterator = iterator_name
    )
  }
  
  # Build looped code
  if (parallel) {
    looped_code <- c(
      "# Parallel Loop Structure",
      template$parallel_setup %||% template$setup,
      "  # Your code here:",
      paste0("  ", strsplit(code, "\n")[[1]]),
      "}, mc.cores = detectCores() - 1)"
    )
  } else {
    looped_code <- c(
      "# Loop Structure",
      template$setup,
      "",
      template$loop,
      "  # Your code here:",
      paste0("  ", strsplit(code, "\n")[[1]]),
      "}"
    )
  }
  
  # Add progress bar option
  if (!parallel) {
    looped_code <- c(
      looped_code[1:2],
      "# Optional: Add progress bar",
      "# library(progress)",
      "# pb <- progress_bar$new(total = length(items))",
      looped_code[3:length(looped_code)],
      "  # pb$tick()  # Update progress"
    )
  }
  
  # Display result
  cat(cli::col_green("Generated Loop Structure:\n"))
  cat(rep("-", 50), "\n", sep = "")
  cat(paste(looped_code, collapse = "\n"))
  cat("\n", rep("-", 50), "\n", sep = "")
  
  # Save to clipboard if possible
  if (requireNamespace("clipr", quietly = TRUE) && clipr::clipr_available()) {
    clipr::write_clip(paste(looped_code, collapse = "\n"))
    cli::cli_alert_success("Code copied to clipboard!")
  }
  
  invisible(paste(looped_code, collapse = "\n"))
}

#' Review and Challenge Current Work
#'
#' @description
#' Analyzes your actual code and data to provide tailored suggestions and challenges.
#' Reads R scripts, RMarkdown files, and available data objects to give specific,
#' contextual feedback rather than generic advice.
#'
#' @param path Path to script, RMarkdown file, or directory (default: current working directory)
#' @param focus Area to focus review on: "statistics", "visualization", "performance", "methodology", NULL for comprehensive
#' @param severity Level of critique: 
#'   - "gentle" = Encouraging feedback with light suggestions
#'   - "moderate" = Balanced critique with actionable improvements (default)
#'   - "harsh" = Direct, no-nonsense feedback for maximum improvement
#'   - "brutal" = Unfiltered critique (use with caution!)
#'
#' @return List containing review comments, specific code issues, and tailored suggestions
#'
#' @examples
#' \dontrun{
#' # Review current directory with moderate critique
#' goose_honk()
#' 
#' # Review specific script with gentle feedback
#' goose_honk("analysis.R", severity = "gentle")
#' 
#' # Focus on statistics with harsh critique
#' goose_honk(focus = "statistics", severity = "harsh")
#' 
#' # Get brutal honesty about your visualization code
#' goose_honk(focus = "visualization", severity = "brutal")
#' }
#'
#' @export
goose_honk <- function(path = ".", 
                      focus = NULL,
                      severity = "moderate") {
  
  cli::cli_h1("[Goose] HONK! Code Review")
  
  # Validate severity
  valid_severities <- c("gentle", "moderate", "harsh", "brutal")
  if (!severity %in% valid_severities) {
    cli::cli_alert_warning("Invalid severity. Using 'moderate'. Valid options: {.val {valid_severities}}")
    severity <- "moderate"
  }
  
  # Determine what to review
  if (path == ".") {
    path <- getwd()
    cli::cli_alert_info("Reviewing current directory: {.path {path}}")
  }
  
  review_items <- list()
  code_content <- list()
  
  # Collect and READ files to review
  if (dir.exists(path)) {
    r_files <- list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = FALSE)
    rmd_files <- list.files(path, pattern = "\\.Rmd$", full.names = TRUE, recursive = FALSE)
    review_items$r_files <- r_files
    review_items$rmd_files <- rmd_files
    
    # Actually read the code files (limit to first 5 to avoid overwhelming)
    for (file in head(c(r_files, rmd_files), 5)) {
      tryCatch({
        code_content[[basename(file)]] <- readLines(file, warn = FALSE)
      }, error = function(e) NULL)
    }
  } else if (file.exists(path)) {
    if (grepl("\\.(R|Rmd)$", path)) {
      review_items$single_file <- path
      code_content[[basename(path)]] <- readLines(path, warn = FALSE)
    }
  }
  
  # Check for available data in memory
  available_data <- tryCatch({
    goose_list(category = "shared_objects", global = FALSE)
  }, error = function(e) NULL)
  
  # Also check global environment for data
  env_objects <- ls(envir = .GlobalEnv)
  data_objects <- character()
  for (obj in env_objects) {
    if (tryCatch(is.data.frame(get(obj, envir = .GlobalEnv)), error = function(e) FALSE)) {
      data_objects <- c(data_objects, obj)
    }
  }
  
  # Initialize review structure
  review <- list(
    timestamp = Sys.time(),
    path = path,
    severity = severity,
    focus = focus,
    code_files = length(code_content),
    data_available = !is.null(available_data) || length(data_objects) > 0,
    specific_issues = list(),
    tailored_suggestions = list(),
    code_patterns = list()
  )
  
  # Analyze actual code patterns
  if (length(code_content) > 0) {
    all_code <- unlist(code_content)
    
    # Check for specific patterns in the actual code
    review$code_patterns <- list(
      has_loops = any(grepl("for\\s*\\(|while\\s*\\(", all_code)),
      has_apply = any(grepl("apply|lapply|sapply|mapply", all_code)),
      has_tidyverse = any(grepl("library\\(tidyverse\\)|library\\(dplyr\\)", all_code)),
      has_ggplot = any(grepl("ggplot|geom_", all_code)),
      has_models = any(grepl("lm\\(|glm\\(|t\\.test\\(|aov\\(", all_code)),
      has_seed = any(grepl("set\\.seed\\(", all_code)),
      has_comments = sum(grepl("^\\s*#", all_code)) / length(all_code),
      has_functions = any(grepl("function\\s*\\(", all_code)),
      has_error_handling = any(grepl("tryCatch|try\\(", all_code)),
      line_count = length(all_code)
    )
  }
  
  # Severity-based messages
  severity_intros <- list(
    gentle = "[Star] Great work! Here are some friendly suggestions:",
    moderate = "[Search] Good foundation! Let's level up with these considerations:",
    harsh = "[Warning] Time for tough love! Critical improvements needed:",
    brutal = "[Skull] Brace yourself! Here's the unvarnished truth about your code:"
  )
  
  cat("\n", severity_intros[[severity]], "\n\n")
  
  # Generate SPECIFIC feedback based on actual code analysis
  cli::cli_h2("Specific Code Analysis")
  
  if (length(code_content) > 0) {
    # Provide specific feedback based on what we found
    if (review$code_patterns$has_loops && !review$code_patterns$has_apply) {
      cat("[Loop] Found for/while loops. Consider vectorization or apply functions for better performance.\n")
      if (severity %in% c("harsh", "brutal")) {
        cat("   Your loops are probably 10-100x slower than they need to be!\n")
      }
    }
    
    if (!review$code_patterns$has_seed && review$code_patterns$has_models) {
      cat("[Dice] Statistical models detected but no set.seed(). Your results aren't reproducible!\n")
    }
    
    if (review$code_patterns$has_comments < 0.1) {
      comment_percent <- round(review$code_patterns$has_comments * 100, 1)
      cat(sprintf("[Note] Only %.1f%% of your code has comments. ", comment_percent))
      if (severity == "brutal") {
        cat("Future you will hate current you!\n")
      } else {
        cat("Consider adding more documentation.\n")
      }
    }
    
    if (!review$code_patterns$has_error_handling && review$code_patterns$line_count > 50) {
      cat("[Warning] No error handling detected in", review$code_patterns$line_count, "lines of code.\n")
      if (severity %in% c("harsh", "brutal")) {
        cat("   When this breaks (not if, WHEN), you'll have no idea why!\n")
      }
    }
    
    if (review$code_patterns$has_ggplot) {
      cat("[Data] ggplot2 usage detected. ")
      if (!any(grepl("theme_|scale_|labs\\(", unlist(code_content)))) {
        cat("But no custom themes or proper labels found!\n")
      } else {
        cat("Good use of themes and labels!\n")
      }
    }
  } else {
    cat("No code files found to analyze. Share some code for specific feedback!\n")
  }
  
  # Data-specific feedback
  if (review$data_available || length(data_objects) > 0) {
    cat("\n")
    cli::cli_h2("Data-Specific Suggestions")
    
    if (!is.null(available_data)) {
      cat("[Data] Found", nrow(available_data), "shared data objects:\n")
      for (i in seq_len(min(3, nrow(available_data)))) {
        cat("  -", available_data$name[i], "-", available_data$class[i], "\n")
      }
    }
    
    if (length(data_objects) > 0) {
      cat("[Chart] Found", length(data_objects), "data frames in environment:\n")
      for (obj in head(data_objects, 3)) {
        df <- get(obj, envir = .GlobalEnv)
        cat(sprintf("  - %s: %d rows x %d columns\n", obj, nrow(df), ncol(df)))
        
        # Check for specific data issues
        if (any(is.na(df))) {
          na_percent <- round(sum(is.na(df)) / (nrow(df) * ncol(df)) * 100, 1)
          cat(sprintf("    [Warning] Contains %.1f%% missing values\n", na_percent))
        }
      }
    }
  }
  
  # Focus-specific TAILORED suggestions based on actual code
  if (!is.null(focus)) {
    cat("\n")
    cli::cli_h2(paste("Focus Area:", tools::toTitleCase(focus)))
    
    tailored_checks <- list(
      statistics = function() {
        if (review$code_patterns$has_models) {
          c("- Models detected. Are you checking residuals and assumptions?",
            "- Consider adding confidence intervals to your estimates",
            if (severity %in% c("harsh", "brutal")) "- P-values without effect sizes are meaningless!" else NULL)
        } else {
          c("No statistical models found. Consider adding inferential statistics.")
        }
      },
      visualization = function() {
        if (review$code_patterns$has_ggplot) {
          c("- ggplot2 detected. Consider adding:",
            "  - Color-blind friendly palettes (viridis, RColorBrewer)",
            "  - Proper axis labels with units",
            "  - Informative titles and captions")
        } else {
          c("No visualization code detected. Data without plots is just numbers!")
        }
      },
      performance = function() {
        suggestions <- c()
        if (review$code_patterns$has_loops) {
          suggestions <- c(suggestions, 
            "- Replace loops with vectorized operations",
            "- Use data.table for large datasets")
        }
        if (review$code_patterns$line_count > 200) {
          suggestions <- c(suggestions,
            "- Consider profiling with profvis::profvis()",
            "- Cache expensive computations")
        }
        if (length(suggestions) == 0) {
          suggestions <- "Code seems reasonably efficient. Profile to find bottlenecks."
        }
        suggestions
      },
      methodology = function() {
        c(if (!review$code_patterns$has_functions) "- No functions found. Modularize your code!" else NULL,
          if (!review$code_patterns$has_seed) "- Add set.seed() for reproducibility" else NULL,
          if (review$code_patterns$has_comments < 0.15) "- Document your methodology in comments" else NULL,
          "- Consider creating a research compendium")
      }
    )
    
    if (focus %in% names(tailored_checks)) {
      suggestions <- tailored_checks[[focus]]()
      for (suggestion in suggestions) {
        cat(suggestion, "\n")
      }
    }
  }
  
  # Advanced techniques based on what's actually in the code
  cat("\n")
  cli::cli_h2("Next Level Techniques for Your Code")
  
  if (review$code_patterns$has_models) {
    cat("Since you're doing modeling:\n")
    cat("- Try tidymodels for consistent model workflows\n")
    cat("- Implement cross-validation with rsample\n")
    cat("- Use broom to tidy your model outputs\n")
  }
  
  if (review$code_patterns$has_ggplot) {
    cat("Since you're using ggplot2:\n")
    cat("- Try plotly::ggplotly() for interactive plots\n")
    cat("- Use patchwork to combine multiple plots\n")
    cat("- Consider gganimate for temporal data\n")
  }
  
  if (review$code_patterns$has_loops || review$code_patterns$line_count > 300) {
    cat("For better performance:\n")
    cat("- Profile with profvis to find bottlenecks\n")
    cat("- Use future/furrr for parallel processing\n")
    cat("- Consider Rcpp for computationally intensive parts\n")
  }
  
  # Motivational close based on severity
  severity_closes <- list(
    gentle = "\n[Rainbow] Keep up the excellent work! Every iteration makes it better.",
    moderate = "\n[Strong] You're on the right track! These improvements will make your analysis shine.",
    harsh = "\n[Fire] No excuses! Implement these changes and become a data science legend!",
    brutal = "\n[Danger] Your code has potential... buried deep. Very deep. Now dig it out and make it shine!"
  )
  
  cat(severity_closes[[severity]], "\n")
  
  # Store detailed analysis in return object
  review$files_analyzed <- names(code_content)
  review$suggestions_given <- TRUE
  
  invisible(review)
}

#' Generate Continuation Prompt for Next Session
#'
#' @description
#' Reviews current work and generates a comprehensive continuation prompt
#' with progress tracking, file mapping, and next steps for seamless handoff
#' to the next working session.
#'
#' @param path Path to review (default: current working directory)
#' @param include_files Whether to include file listing (default: TRUE)
#' @param include_todos Whether to scan for TODO comments (default: TRUE)
#' @param save_to Path to save the continuation prompt (auto-generated if NULL)
#'
#' @return Path to the saved continuation prompt
#'
#' @examples
#' \dontrun{
#' # Generate continuation prompt for current project
#' goose_continuation_prompt()
#' 
#' # Generate without file listing
#' goose_continuation_prompt(include_files = FALSE)
#' 
#' # Save to specific location
#' goose_continuation_prompt(save_to = "project_docs/continuation.md")
#' }
#'
#' @export
goose_continuation_prompt <- function(path = ".",
                                    include_files = TRUE,
                                    include_todos = TRUE,
                                    save_to = NULL) {
  
  cli::cli_h1("[Note] Generating Continuation Prompt")
  
  # Set up paths
  if (path == ".") {
    path <- getwd()
  }
  
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  if (is.null(save_to)) {
    save_to <- file.path(path, paste0("CONTINUATION_PROMPT_", timestamp, ".md"))
  }
  
  # Initialize prompt sections
  prompt <- list()
  
  # Header
  prompt$header <- c(
    "# [Goose] GooseR Continuation Prompt",
    paste0("Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("Project: ", basename(path)),
    paste0("Path: ", path),
    "",
    "---",
    ""
  )
  
  # Session Summary
  prompt$summary <- c(
    "## [Data] Session Summary",
    "",
    "### Working Directory",
    paste0("```\n", path, "\n```"),
    "",
    "### R Session Info",
    "```r",
    capture.output(sessionInfo()),
    "```",
    ""
  )
  
  # File Mapping
  if (include_files) {
    cli::cli_alert_info("Mapping project files...")
    
    r_files <- list.files(path, pattern = "\\.R$", full.names = FALSE, recursive = TRUE)
    rmd_files <- list.files(path, pattern = "\\.Rmd$", full.names = FALSE, recursive = TRUE)
    data_files <- list.files(path, pattern = "\\.(csv|xlsx|rds|RData)$", 
                            full.names = FALSE, recursive = TRUE)
    
    prompt$files <- c(
      "## [Files] Project Structure",
      "",
      "### R Scripts",
      if (length(r_files) > 0) paste0("- ", r_files) else "- No R scripts found",
      "",
      "### R Markdown Files",
      if (length(rmd_files) > 0) paste0("- ", rmd_files) else "- No Rmd files found",
      "",
      "### Data Files",
      if (length(data_files) > 0) paste0("- ", data_files) else "- No data files found",
      ""
    )
  }
  
  # TODO scanning
  if (include_todos) {
    cli::cli_alert_info("Scanning for TODOs...")
    
    todos <- list()
    all_files <- c(
      list.files(path, pattern = "\\.R$", full.names = TRUE, recursive = TRUE),
      list.files(path, pattern = "\\.Rmd$", full.names = TRUE, recursive = TRUE)
    )
    
    for (file in all_files) {
      lines <- readLines(file, warn = FALSE)
      todo_lines <- grep("TODO|FIXME|NOTE", lines, ignore.case = TRUE)
      if (length(todo_lines) > 0) {
        todos[[basename(file)]] <- lines[todo_lines]
      }
    }
    
    prompt$todos <- c(
      "## [Done] TODOs and Notes",
      ""
    )
    
    if (length(todos) > 0) {
      for (file in names(todos)) {
        prompt$todos <- c(prompt$todos,
          paste0("### ", file),
          paste0("- ", todos[[file]]),
          ""
        )
      }
    } else {
      prompt$todos <- c(prompt$todos, "No TODOs found in project files.", "")
    }
  }
  
  # Recent Memory
  cli::cli_alert_info("Checking Goose memory...")
  recent_memory <- tryCatch({
    memories <- goose_list(category = "*", global = FALSE)
    if (length(memories) > 0) {
      c(
        "## [Memory] Recent Goose Memory",
        "",
        "### Saved Objects",
        paste0("- ", names(memories)[1:min(10, length(memories))]),
        if (length(memories) > 10) paste0("... and ", length(memories) - 10, " more"),
        ""
      )
    } else {
      NULL
    }
  }, error = function(e) NULL)
  
  if (!is.null(recent_memory)) {
    prompt$memory <- recent_memory
  }
  
  # Progress Status
  prompt$progress <- c(
    "## [Chart] Progress Status",
    "",
    "### Completed in This Session",
    "- [ ] Add completed tasks here",
    "",
    "### In Progress",
    "- [ ] Add ongoing work here",
    "",
    "### Blocked/Waiting",
    "- [ ] Add blockers here",
    ""
  )
  
  # Next Steps
  prompt$next_steps <- c(
    "## [Rocket] Next Steps",
    "",
    "### Immediate Priorities",
    "1. ",
    "2. ",
    "3. ",
    "",
    "### Future Enhancements",
    "- ",
    "- ",
    "",
    "### Questions to Address",
    "- ",
    "- ",
    ""
  )
  
  # Key Commands
  prompt$commands <- c(
    "## [Idea] Quick Start Commands",
    "",
    "```r",
    "# Load gooseR",
    "library(gooseR)",
    "",
    "# Restore session",
    "# source('restore_session.R')",
    "",
    "# Load saved objects",
    "# goose_load('object_name')",
    "",
    "# Continue analysis",
    "# goose_ask('What should we work on next?')",
    "```",
    ""
  )
  
  # Notes section
  prompt$notes <- c(
    "## [Note] Session Notes",
    "",
    "_Add any important context, decisions, or observations from this session:_",
    "",
    "- ",
    "- ",
    "- ",
    "",
    "---",
    "",
    paste0("*This continuation prompt was generated by gooseR v", 
           utils::packageVersion("gooseR"), "*"),
    paste0("*Use this to resume work seamlessly in your next session*")
  )
  
  # Combine all sections
  full_prompt <- unlist(prompt)
  
  # Write to file
  writeLines(full_prompt, save_to)
  
  # Display summary
  cli::cli_alert_success("Continuation prompt saved to:")
  cat(cli::col_blue(save_to), "\n\n")
  
  cat("Summary:\n")
  cat("- Project:", basename(path), "\n")
  if (include_files) {
    total_files <- length(r_files) + length(rmd_files) + length(data_files)
    cat("- Files tracked:", total_files, "\n")
  }
  if (include_todos && length(todos) > 0) {
    cat("- TODOs found:", sum(lengths(todos)), "\n")
  }
  cat("- Timestamp:", timestamp, "\n")
  
  cli::cli_alert_info("Copy this prompt to start your next session exactly where you left off!")
  
  invisible(save_to)
}

# Helper function for null-coalescing
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
