#' Prompt Template Module for GooseR
#'
#' Create and manage reusable prompt templates
#' @importFrom glue glue
#' @importFrom yaml write_yaml read_yaml
#' @importFrom rappdirs user_data_dir
#' @importFrom stats setNames
NULL

#' Create a Prompt Template
#'
#' Define a reusable prompt template with variables
#' @param name Template name
#' @param template Template text with \{variable\} placeholders
#' @param description Optional description
#' @param variables List of variable definitions
#' @param examples Optional usage examples
#' @return Template object
#' @export
#' @examples
#' \dontrun{
#' # Create a code review template
#' review_template <- goose_template(
#'   name = "code_review",
#'   template = "Review this {language} code:\n\n{code}\n\nFocus on: {focus}",
#'   variables = list(
#'     language = "Programming language",
#'     code = "Code to review",
#'     focus = "Specific areas to focus on"
#'   )
#' )
#' }
goose_template <- function(name, template, description = NULL, 
                          variables = NULL, examples = NULL) {
  
  # Extract variables from template if not provided
  if (is.null(variables)) {
    var_pattern <- "\\{([^}]+)\\}"
    matches <- gregexpr(var_pattern, template, perl = TRUE)
    var_names <- unique(regmatches(template, matches)[[1]])
    var_names <- gsub("[{}]", "", var_names)
    variables <- as.list(setNames(rep("", length(var_names)), var_names))
  }
  
  structure(
    list(
      name = name,
      template = template,
      description = description,
      variables = variables,
      examples = examples,
      created = Sys.time()
    ),
    class = "goose_template"
  )
}

#' Apply Template with Variables
#'
#' Fill in template variables and execute query
#' @param template Template object or name
#' @param ... Variable values
#' @param execute Whether to execute the query
#' @return Filled template or query response
#' @export
#' @examples
#' \dontrun{
#' # Use the template
#' result <- goose_template_apply(
#'   review_template,
#'   language = "R",
#'   code = "function(x) x^2",
#'   focus = "efficiency and style"
#' )
#' }
goose_template_apply <- function(template, ..., execute = TRUE) {
  # Load template if given name
  if (is.character(template)) {
    template <- goose_template_load(template)
  }
  
  # Get variables
  vars <- list(...)
  
  # Check required variables
  required <- names(template$variables)
  missing <- setdiff(required, names(vars))
  if (length(missing) > 0) {
    stop("Missing required variables: ", paste(missing, collapse = ", "))
  }
  
  # Fill template using glue
  filled <- glue::glue(template$template, .envir = vars)
  
  # Execute if requested
  if (execute) {
    return(goose_ask(as.character(filled)))
  } else {
    return(as.character(filled))
  }
}

#' Save Template to Library
#'
#' Save a template for future use
#' @param template Template object
#' @param overwrite Overwrite if exists
#' @param template_dir Directory to save templates (default: tempdir() for CRAN compliance)
#' @return Logical indicating success
#' @export
#' @examples
#' \dontrun{
#' # Create and save template in temp directory
#' template <- goose_template("test", "Hello {name}")
#' goose_template_save(template, template_dir = tempdir())
#' }
goose_template_save <- function(template, overwrite = FALSE, template_dir = NULL) {
  # Template directory - use tempdir() by default for CRAN compliance
  if (is.null(template_dir)) {
    template_dir <- file.path(tempdir(), "gooseR_templates")
  }
  
  if (!dir.exists(template_dir)) {
    dir.create(template_dir, recursive = TRUE)
  }
  
  # Template file
  template_file <- file.path(template_dir, paste0(template$name, ".yaml"))
  
  if (file.exists(template_file) && !overwrite) {
    stop("Template '", template$name, "' already exists. Use overwrite=TRUE to replace.")
  }
  
  # Save as YAML
  yaml::write_yaml(template, template_file)
  message("Template saved: ", template$name)
  return(invisible(TRUE))
}

#' Load Template from Library
#'
#' Load a saved template
#' @param name Template name
#' @return Template object
#' @export
goose_template_load <- function(name) {
  template_dir <- file.path(rappdirs::user_data_dir("gooseR"), "templates")
  template_file <- file.path(template_dir, paste0(name, ".yaml"))
  
  if (!file.exists(template_file)) {
    # Check built-in templates
    builtin <- goose_template_builtin(name)
    if (!is.null(builtin)) {
      return(builtin)
    }
    stop("Template not found: ", name)
  }
  
  # Load from YAML
  template <- yaml::read_yaml(template_file)
  class(template) <- "goose_template"
  return(template)
}

#' List Available Templates
#'
#' Show all available templates
#' @param include_builtin Include built-in templates
#' @return Data frame of templates
#' @export
goose_template_list <- function(include_builtin = TRUE) {
  templates <- data.frame(
    name = character(),
    description = character(),
    type = character(),
    stringsAsFactors = FALSE
  )
  
  # User templates
  template_dir <- file.path(rappdirs::user_data_dir("gooseR"), "templates")
  if (dir.exists(template_dir)) {
    files <- list.files(template_dir, pattern = "\\.yaml$", full.names = TRUE)
    for (file in files) {
      template <- yaml::read_yaml(file)
      templates <- rbind(templates, data.frame(
        name = template$name,
        description = template$description %||% "",
        type = "user",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Built-in templates
  if (include_builtin) {
    builtin <- goose_template_builtin()
    templates <- rbind(templates, builtin)
  }
  
  return(templates)
}

#' Get Built-in Template
#'
#' Access built-in template library
#' @param name Optional template name
#' @return Template or list of templates
#' @export
goose_template_builtin <- function(name = NULL) {
  templates <- list(
    # Code templates
    code_review = goose_template(
      "code_review",
      "Review this {language} code for best practices, efficiency, and potential issues:\n\n```{language}\n{code}\n```\n\nFocus areas: {focus}",
      "Comprehensive code review",
      list(language = "Programming language", 
           code = "Code to review", 
           focus = "Specific focus areas")
    ),
    
    function_doc = goose_template(
      "function_doc",
      "Generate comprehensive documentation for this {language} function:\n\n```{language}\n{code}\n```\n\nInclude: description, parameters, return value, examples, and any important notes.",
      "Generate function documentation",
      list(language = "Programming language", code = "Function code")
    ),
    
    optimize_code = goose_template(
      "optimize_code",
      "Optimize this {language} code for {goal}:\n\n```{language}\n{code}\n```\n\nConstraints: {constraints}",
      "Code optimization",
      list(language = "Language", code = "Code", goal = "Optimization goal", 
           constraints = "Any constraints")
    ),
    
    # Data analysis templates
    data_summary = goose_template(
      "data_summary",
      "Analyze this dataset and provide insights:\n\nDataset: {dataset_name}\nShape: {shape}\nColumns: {columns}\nSample:\n{sample}\n\nAnalysis goals: {goals}",
      "Data analysis summary",
      list(dataset_name = "Name", shape = "Dimensions", 
           columns = "Column names", sample = "Sample data", goals = "Analysis goals")
    ),
    
    viz_suggest = goose_template(
      "viz_suggest",
      "Suggest appropriate visualizations for this data:\n\nData type: {data_type}\nVariables: {variables}\nGoal: {goal}\nAudience: {audience}",
      "Visualization recommendations",
      list(data_type = "Type of data", variables = "Variables to visualize",
           goal = "Visualization goal", audience = "Target audience")
    ),
    
    # Writing templates
    technical_doc = goose_template(
      "technical_doc",
      "Write technical documentation for {topic}:\n\nTarget audience: {audience}\nDetail level: {detail_level}\nKey points: {key_points}\nFormat: {format}",
      "Technical documentation",
      list(topic = "Topic", audience = "Target audience", 
           detail_level = "Detail level", key_points = "Key points", format = "Format")
    ),
    
    explain_concept = goose_template(
      "explain_concept",
      "Explain {concept} to someone with {background} background.\n\nUse {complexity} language and include {examples_count} examples.\n\nFocus on: {focus}",
      "Concept explanation",
      list(concept = "Concept", background = "Background level",
           complexity = "Language complexity", examples_count = "Number of examples",
           focus = "Focus areas")
    ),
    
    # Testing templates
    unit_tests = goose_template(
      "unit_tests",
      "Generate comprehensive unit tests for this {language} code:\n\n```{language}\n{code}\n```\n\nTest framework: {framework}\nCoverage goals: {coverage}",
      "Generate unit tests",
      list(language = "Language", code = "Code to test",
           framework = "Test framework", coverage = "Coverage goals")
    ),
    
    test_cases = goose_template(
      "test_cases",
      "Generate test cases for {feature}:\n\nDescription: {description}\nInputs: {inputs}\nExpected behavior: {expected}\nEdge cases to consider: {edge_cases}",
      "Generate test cases",
      list(feature = "Feature name", description = "Feature description",
           inputs = "Input types", expected = "Expected behavior",
           edge_cases = "Edge cases")
    ),
    
    # Debugging templates
    debug_error = goose_template(
      "debug_error",
      "Debug this error:\n\nError message:\n{error}\n\nCode context:\n```{language}\n{code}\n```\n\nWhat I've tried: {attempts}",
      "Debug error",
      list(error = "Error message", language = "Language",
           code = "Code context", attempts = "Previous attempts")
    ),
    
    performance_analysis = goose_template(
      "performance_analysis",
      "Analyze performance of this {language} code:\n\n```{language}\n{code}\n```\n\nCurrent performance: {current_perf}\nTarget: {target_perf}\nConstraints: {constraints}",
      "Performance analysis",
      list(language = "Language", code = "Code",
           current_perf = "Current performance", target_perf = "Target performance",
           constraints = "Constraints")
    )
  )
  
  if (!is.null(name)) {
    return(templates[[name]])
  }
  
  # Return summary
  template_df <- data.frame(
    name = names(templates),
    description = sapply(templates, function(t) t$description %||% ""),
    type = "builtin",
    stringsAsFactors = FALSE
  )
  
  return(template_df)
}

#' Print Template
#'
#' @param x Template object
#' @param ... Additional arguments (unused)
#' @export
print.goose_template <- function(x, ...) {
  cat("GooseR Template: ", x$name, "\n")
  cat("================", paste(rep("=", nchar(x$name)), collapse = ""), "\n")
  if (!is.null(x$description)) {
    cat("Description: ", x$description, "\n")
  }
  cat("\nTemplate:\n")
  cat(x$template, "\n")
  cat("\nVariables:\n")
  for (var in names(x$variables)) {
    cat("  - ", var, ": ", x$variables[[var]], "\n")
  }
  if (!is.null(x$examples)) {
    cat("\nExamples:\n")
    cat(x$examples, "\n")
  }
}

#' Create Template from History
#'
#' Create a template from a previous query
#' @param query Previous query text
#' @param name Template name
#' @param variables Variables to extract
#' @return Template object
#' @export
goose_template_from_query <- function(query, name, variables = NULL) {
  # Auto-detect potential variables (quoted strings, numbers, etc.)
  if (is.null(variables)) {
    # Simple heuristic: find quoted strings
    quoted <- regmatches(query, gregexpr('"[^"]*"', query))[[1]]
    if (length(quoted) > 0) {
      variables <- paste0("var", seq_along(quoted))
      template <- query
      for (i in seq_along(quoted)) {
        template <- sub(quoted[i], paste0("{", variables[i], "}"), template, fixed = TRUE)
      }
    } else {
      template <- query
      variables <- list()
    }
  } else {
    template <- query
    for (var in names(variables)) {
      template <- gsub(variables[[var]], paste0("{", var, "}"), template, fixed = TRUE)
    }
  }
  
  goose_template(name, template, variables = variables)
}

#' Validate Template
#'
#' Check if template is valid
#' @param template Template object or text
#' @return List with validation results
#' @export
goose_template_validate <- function(template) {
  if (is.character(template)) {
    template_text <- template
  } else {
    template_text <- template$template
  }
  
  # Extract variables
  var_pattern <- "\\{([^}]+)\\}"
  matches <- gregexpr(var_pattern, template_text, perl = TRUE)
  var_names <- unique(regmatches(template_text, matches)[[1]])
  var_names <- gsub("[{}]", "", var_names)
  
  # Check for issues
  issues <- list()
  
  # Check for unclosed braces using internal str_count function
  if (gooseR_str_count(template_text, "\\{") != gooseR_str_count(template_text, "\\}")) {
    issues <- append(issues, "Mismatched braces")
  }
  
  # Check for empty variables
  if (any(var_names == "")) {
    issues <- append(issues, "Empty variable names")
  }
  
  # Check for duplicate variables
  if (length(var_names) != length(unique(var_names))) {
    issues <- append(issues, "Duplicate variable names")
  }
  
  valid <- length(issues) == 0
  
  return(list(
    valid = valid,
    variables = var_names,
    issues = issues
  ))
}

# Helper function for NULL default
`%||%` <- function(x, y) if (is.null(x)) y else x

# Internal helper function to count pattern occurrences
gooseR_str_count <- function(string, pattern) {
  lengths(regmatches(string, gregexpr(pattern, string)))
}
