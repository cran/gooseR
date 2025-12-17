# GooseR AI Assistant Module
# Phase 3: AI-Powered R Development Assistant
# Author: Brandon Theriault
# Date: December 2025

#' Review R Code with AI
#'
#' Get AI-powered code review and suggestions for improvement.
#'
#' @param code Character string or function containing R code to review
#' @param focus Character vector of focus areas (e.g., "performance", "style", "bugs")
#' @param context Optional context about the code's purpose
#' @param detailed Logical, whether to request detailed analysis
#'
#' @return List with review results including suggestions, issues, and improvements
#' @export
#'
#' @examples
#' \dontrun{
#' # Review a function
#' my_func <- function(x) {
#'   for(i in 1:length(x)) {
#'     x[i] <- x[i] * 2
#'   }
#'   return(x)
#' }
#' review <- goose_review_code(my_func, focus = c("performance", "style"))
#' 
#' # Review code string
#' code <- "df$new_col = df$col1 + df$col2"
#' review <- goose_review_code(code, context = "Adding columns in data frame")
#' }
goose_review_code <- function(code, 
                             focus = c("performance", "style", "bugs", "documentation"),
                             context = NULL,
                             detailed = TRUE) {
  
  # Convert function to string if needed
  if (is.function(code)) {
    code_str <- paste(deparse(code), collapse = "\n")
  } else {
    code_str <- as.character(code)
  }
  
  # Build the prompt
  prompt <- "Please review this R code and provide suggestions for improvement.\n\n"
  
  if (!is.null(context)) {
    prompt <- paste0(prompt, "Context: ", context, "\n\n")
  }
  
  if (length(focus) > 0) {
    prompt <- paste0(prompt, "Focus areas: ", paste(focus, collapse = ", "), "\n\n")
  }
  
  prompt <- paste0(prompt, "Code:\n```r\n", code_str, "\n```\n\n")
  
  if (detailed) {
    prompt <- paste0(prompt, 
                    "Please provide:\n",
                    "1. Overall assessment\n",
                    "2. Specific issues found\n",
                    "3. Suggested improvements with code examples\n",
                    "4. Best practices recommendations\n",
                    "5. Performance considerations")
  } else {
    prompt <- paste0(prompt, "Please provide a concise review with key improvements.")
  }
  
  # Get AI response
  response <- goose_ask(prompt, output_format = "text", timeout = 60)
  
  # Parse response into structured format
  review <- parse_code_review(response)
  
  # Add metadata
  review$timestamp <- Sys.time()
  review$code_length <- nchar(code_str)
  review$focus_areas <- focus
  
  class(review) <- c("goose_code_review", "list")
  review
}

#' Suggest Color Palette with AI
#'
#' Get AI-powered color palette suggestions for data visualization.
#'
#' @param purpose Character, the purpose of the palette (e.g., "heatmap", "categorical", "diverging")
#' @param n_colors Integer, number of colors needed
#' @param brand Optional brand name for brand-appropriate colors
#' @param constraints Optional constraints (e.g., "colorblind-safe", "print-friendly")
#'
#' @return List with hex colors and rationale
#' @export
#'
#' @examples
#' \dontrun{
#' # Get heatmap colors
#' colors <- goose_suggest_colors("heatmap", n_colors = 9)
#' 
#' # Get brand colors
#' colors <- goose_suggest_colors("categorical", n_colors = 5, brand = "Block")
#' 
#' # Get accessible colors
#' colors <- goose_suggest_colors("bar chart", n_colors = 4, 
#'                                constraints = "colorblind-safe")
#' }
goose_suggest_colors <- function(purpose, 
                                n_colors = 5,
                                brand = NULL,
                                constraints = NULL) {
  
  # Build prompt
  prompt <- sprintf("Suggest a color palette for %s with %d colors.", purpose, n_colors)
  
  if (!is.null(brand)) {
    prompt <- paste0(prompt, " The palette should align with ", brand, " brand guidelines.")
  }
  
  if (!is.null(constraints)) {
    prompt <- paste0(prompt, " Constraints: ", paste(constraints, collapse = ", "), ".")
  }
  
  prompt <- paste0(prompt, 
                  "\n\nPlease provide:\n",
                  "1. Hex color codes\n",
                  "2. RGB values\n", 
                  "3. Rationale for the palette\n",
                  "4. Usage recommendations\n",
                  "5. Example R code to use the palette")
  
  # Get response
  response <- goose_ask(prompt, output_format = "text")
  
  # Extract colors
  colors <- extract_hex_colors(response)
  
  # Create palette object
  palette <- list(
    colors = colors,
    purpose = purpose,
    n_colors = n_colors,
    brand = brand,
    constraints = constraints,
    rationale = response,
    timestamp = Sys.time()
  )
  
  class(palette) <- c("goose_palette", "list")
  palette
}

#' Explain R Error with AI
#'
#' Get AI-powered explanation and solution for R errors.
#'
#' @param error The error object or error message
#' @param code Optional code that caused the error
#' @param context Optional context about what you were trying to do
#'
#' @return List with explanation and suggested solutions
#' @export
#'
#' @examples
#' \dontrun{
#' # Explain last error
#' tryCatch({
#'   data.frame(x = 1:3, y = 1:4)
#' }, error = function(e) {
#'   explanation <- goose_explain_error(e)
#'   print(explanation)
#' })
#' }
goose_explain_error <- function(error = NULL, code = NULL, context = NULL) {
  
  # Get error message
  if (is.null(error)) {
    # Try to get last error
    error_msg <- geterrmessage()
    if (error_msg == "") {
      stop("No error to explain. Pass an error object or run after an error occurs.")
    }
  } else if (inherits(error, "error")) {
    error_msg <- conditionMessage(error)
  } else {
    error_msg <- as.character(error)
  }
  
  # Build prompt
  prompt <- "Please explain this R error and provide solutions:\n\n"
  prompt <- paste0(prompt, "Error: ", error_msg, "\n\n")
  
  if (!is.null(code)) {
    if (is.function(code)) {
      code <- paste(deparse(code), collapse = "\n")
    }
    prompt <- paste0(prompt, "Code that caused the error:\n```r\n", code, "\n```\n\n")
  }
  
  if (!is.null(context)) {
    prompt <- paste0(prompt, "Context: ", context, "\n\n")
  }
  
  prompt <- paste0(prompt,
                  "Please provide:\n",
                  "1. Simple explanation of what went wrong\n",
                  "2. Common causes of this error\n",
                  "3. Step-by-step solution\n",
                  "4. Corrected code example\n",
                  "5. How to prevent this error in the future")
  
  # Get AI response
  response <- goose_ask(prompt, output_format = "text", timeout = 45)
  
  # Structure the response
  explanation <- list(
    error = error_msg,
    explanation = response,
    code = code,
    context = context,
    timestamp = Sys.time()
  )
  
  class(explanation) <- c("goose_error_explanation", "list")
  explanation
}

#' Generate R Documentation with AI
#'
#' Generate roxygen2 documentation for R functions.
#'
#' @param func Function object or function name
#' @param style Character, documentation style ("roxygen2", "detailed", "minimal")
#' @param examples Logical, whether to generate examples
#'
#' @return Character string with documentation
#' @export
#'
#' @examples
#' \dontrun{
#' my_func <- function(x, y, method = "pearson") {
#'   cor(x, y, method = method)
#' }
#' 
#' docs <- goose_document(my_func)
#' cat(docs)
#' }
goose_document <- function(func, style = "roxygen2", examples = TRUE) {
  
  # Get function code
  if (is.function(func)) {
    func_code <- paste(deparse(func), collapse = "\n")
    func_name <- deparse(substitute(func))
  } else {
    stop("Please provide a function object")
  }
  
  # Build prompt
  prompt <- paste0("Generate ", style, " documentation for this R function:\n\n")
  prompt <- paste0(prompt, "```r\n", func_code, "\n```\n\n")
  prompt <- paste0(prompt, "Include:\n")
  prompt <- paste0(prompt, "1. Clear description of what the function does\n")
  prompt <- paste0(prompt, "2. @param tags for all parameters with descriptions\n")
  prompt <- paste0(prompt, "3. @return tag describing the return value\n")
  prompt <- paste0(prompt, "4. @export tag if appropriate\n")
  
  if (examples) {
    prompt <- paste0(prompt, "5. @examples section with working examples\n")
  }
  
  prompt <- paste0(prompt, "\nFormat as roxygen2 comments starting with #'")
  
  # Get response
  response <- goose_ask(prompt, output_format = "text")
  
  # Clean up response
  docs <- clean_documentation(response)
  
  docs
}

#' Optimize ggplot2 Code with AI
#'
#' Get AI suggestions to improve a ggplot2 visualization.
#'
#' @param plot_code Character string or expression with ggplot2 code
#' @param goals Character vector of optimization goals
#' @param data_sample Optional sample of the data being plotted
#'
#' @return List with optimized code and suggestions
#' @export
#'
#' @examples
#' \dontrun{
#' plot_code <- "
#' ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#' "
#' 
#' optimized <- goose_optimize_plot(plot_code, 
#'                                  goals = c("aesthetics", "clarity"))
#' }
goose_optimize_plot <- function(plot_code, 
                               goals = c("aesthetics", "clarity", "accessibility"),
                               data_sample = NULL) {
  
  # Convert to string if needed
  if (!is.character(plot_code)) {
    plot_code <- paste(deparse(substitute(plot_code)), collapse = "\n")
  }
  
  # Build prompt
  prompt <- "Please optimize this ggplot2 visualization:\n\n"
  prompt <- paste0(prompt, "```r\n", plot_code, "\n```\n\n")
  
  if (length(goals) > 0) {
    prompt <- paste0(prompt, "Optimization goals: ", paste(goals, collapse = ", "), "\n\n")
  }
  
  if (!is.null(data_sample)) {
    prompt <- paste0(prompt, "Data structure:\n")
    prompt <- paste0(prompt, "```\n", capture.output(str(data_sample)), "\n```\n\n")
  }
  
  prompt <- paste0(prompt,
                  "Please provide:\n",
                  "1. Optimized ggplot2 code\n",
                  "2. Explanation of improvements\n",
                  "3. Additional theme suggestions\n",
                  "4. Color palette recommendations\n",
                  "5. Accessibility considerations")
  
  # Get response
  response <- goose_ask(prompt, output_format = "text", timeout = 45)
  
  # Extract optimized code
  optimized_code <- extract_r_code(response)
  
  # Create result object
  result <- list(
    original = plot_code,
    optimized = optimized_code,
    suggestions = response,
    goals = goals,
    timestamp = Sys.time()
  )
  
  class(result) <- c("goose_plot_optimization", "list")
  result
}

#' Generate Unit Tests with AI
#'
#' Generate testthat unit tests for R functions.
#'
#' @param func Function object to test
#' @param test_cases Number of test cases to generate
#' @param edge_cases Logical, include edge case tests
#'
#' @return Character string with testthat code
#' @export
goose_generate_tests <- function(func, test_cases = 5, edge_cases = TRUE) {
  
  if (!is.function(func)) {
    stop("Please provide a function object")
  }
  
  func_code <- paste(deparse(func), collapse = "\n")
  
  # Build prompt
  prompt <- paste0("Generate testthat unit tests for this R function:\n\n")
  prompt <- paste0(prompt, "```r\n", func_code, "\n```\n\n")
  prompt <- paste0(prompt, "Generate ", test_cases, " test cases")
  
  if (edge_cases) {
    prompt <- paste0(prompt, " including edge cases")
  }
  
  prompt <- paste0(prompt, ".\n\n",
                  "Use testthat syntax with test_that() blocks.\n",
                  "Include tests for:\n",
                  "1. Normal inputs\n",
                  "2. Edge cases\n",
                  "3. Error conditions\n",
                  "4. Return value types\n",
                  "5. Side effects if any")
  
  # Get response
  response <- goose_ask(prompt, output_format = "text")
  
  # Extract test code
  test_code <- extract_r_code(response)
  
  test_code
}

# Helper functions ----

#' Parse code review response
#' @keywords internal
parse_code_review <- function(response) {
  list(
    raw_response = response,
    assessment = extract_section(response, "assessment|overall"),
    issues = extract_section(response, "issues|problems"),
    improvements = extract_section(response, "improvements|suggestions"),
    best_practices = extract_section(response, "best practices"),
    performance = extract_section(response, "performance")
  )
}

#' Extract hex colors from text
#' @keywords internal
extract_hex_colors <- function(text) {
  pattern <- "#[0-9A-Fa-f]{6}"
  matches <- gregexpr(pattern, text)
  colors <- regmatches(text, matches)[[1]]
  unique(colors)
}

#' Extract R code blocks from text
#' @keywords internal
extract_r_code <- function(text) {
  # Extract code between ```r and ```
  pattern <- "```r?\n(.*?)\n```"
  matches <- regmatches(text, gregexec(pattern, text, perl = TRUE))
  
  if (length(matches[[1]]) > 0) {
    # Return all code blocks concatenated
    code_blocks <- sapply(matches[[1]], function(x) x[2])
    paste(code_blocks, collapse = "\n\n")
  } else {
    # No code blocks found, return original
    text
  }
}

#' Extract section from text
#' @keywords internal
extract_section <- function(text, pattern) {
  lines <- strsplit(text, "\n")[[1]]
  
  # Find lines matching pattern (case insensitive)
  header_idx <- grep(pattern, lines, ignore.case = TRUE)
  
  if (length(header_idx) == 0) {
    return(NULL)
  }
  
  # Extract content after first match until next header or end
  start_idx <- header_idx[1]
  
  # Find next header (line starting with number or bullet)
  next_headers <- grep("^[0-9]+\\.|^-|^\\*|^#+", lines[(start_idx + 1):length(lines)])
  
  if (length(next_headers) > 0) {
    end_idx <- start_idx + next_headers[1] - 1
  } else {
    end_idx <- length(lines)
  }
  
  if (end_idx > start_idx) {
    content <- lines[(start_idx + 1):end_idx]
    paste(content, collapse = "\n")
  } else {
    NULL
  }
}

#' Clean documentation output
#' @keywords internal
clean_documentation <- function(docs) {
  # Ensure proper roxygen2 formatting
  lines <- strsplit(docs, "\n")[[1]]
  
  # Add #' prefix if not present
  lines <- sapply(lines, function(line) {
    if (!grepl("^#'", line) && nchar(trimws(line)) > 0) {
      paste0("#' ", line)
    } else {
      line
    }
  })
  
  paste(lines, collapse = "\n")
}

# Print methods ----

#' Print method for goose_code_review
#' @param x goose_code_review object
#' @param ... additional arguments (unused)
#' @export
print.goose_code_review <- function(x, ...) {
  cat("=== Goose Code Review ===\n")
  cat("Timestamp:", format(x$timestamp), "\n")
  cat("Code length:", x$code_length, "characters\n")
  cat("Focus areas:", paste(x$focus_areas, collapse = ", "), "\n\n")
  
  if (!is.null(x$assessment)) {
    cat("Overall Assessment:\n")
    cat(x$assessment, "\n\n")
  }
  
  if (!is.null(x$issues)) {
    cat("Issues Found:\n")
    cat(x$issues, "\n\n")
  }
  
  if (!is.null(x$improvements)) {
    cat("Suggested Improvements:\n")
    cat(x$improvements, "\n")
  }
  
  invisible(x)
}

#' Print method for goose_palette
#' @param x goose_palette object
#' @param ... additional arguments (unused)
#' @export
print.goose_palette <- function(x, ...) {
  cat("=== Goose Color Palette ===\n")
  cat("Purpose:", x$purpose, "\n")
  cat("Colors:", paste(x$colors, collapse = ", "), "\n")
  
  if (!is.null(x$brand)) {
    cat("Brand:", x$brand, "\n")
  }
  
  if (!is.null(x$constraints)) {
    cat("Constraints:", paste(x$constraints, collapse = ", "), "\n")
  }
  
  # Show color swatches if in RStudio
  if (interactive()) {
    try({
      # visualize swatches inline if htmltools is available
      if (requireNamespace("htmltools", quietly = TRUE)) {
        sw <- paste(sprintf("<span style='display:inline-block;width:16px;height:16px;background:%s;margin-right:4px;border:1px solid #ddd'></span>", x$colors), collapse = "")
        cat("Swatches:", sw, "\n")
      }
    }, silent = TRUE)
  }
  
  invisible(x)
}

#' Print method for goose_error_explanation
#' @param x goose_error_explanation object
#' @param ... additional arguments (unused)
#' @export
print.goose_error_explanation <- function(x, ...) {
  cat("=== Goose Error Explanation ===\n")
  cat("Error:", x$error, "\n\n")
  cat(x$explanation, "\n")
  invisible(x)
}
