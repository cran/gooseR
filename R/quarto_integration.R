#' Quarto and RMarkdown Integration for GooseR
#'
#' AI-powered chunks and document generation
#' @name goose_quarto
#' @importFrom knitr knit_engines
NULL

#' Create AI-Powered Quarto Chunk
#'
#' Generate a Quarto chunk that executes AI queries
#' @param prompt The AI prompt to execute
#' @param label Optional chunk label
#' @param echo Whether to show the prompt in output
#' @param eval Whether to evaluate the chunk
#' @param cache Whether to cache the result
#' @return Quarto chunk text
#' @export
#' @examples
#' \dontrun{
#' # In a Quarto document
#' goose_quarto_chunk("Explain linear regression", label = "explain-lm")
#' }
goose_quarto_chunk <- function(prompt, 
                              label = NULL,
                              echo = FALSE,
                              eval = TRUE,
                              cache = TRUE) {
  
  if (is.null(label)) {
    label <- paste0("ai-", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
  
  chunk <- sprintf('```{goose, %s, echo=%s, eval=%s, cache=%s}
%s
```',
    label,
    ifelse(echo, "TRUE", "FALSE"),
    ifelse(eval, "TRUE", "FALSE"),
    ifelse(cache, "TRUE", "FALSE"),
    prompt
  )
  
  if (interactive() && rstudioapi::isAvailable()) {
    rstudioapi::insertText(chunk)
  }
  
  return(invisible(chunk))
}

#' Create RMarkdown AI Section
#'
#' Generate an RMarkdown section with AI content
#' @param title Section title
#' @param prompt AI prompt for content
#' @param level Heading level (1-6)
#' @param include_code Include code examples
#' @return RMarkdown text
#' @export
goose_rmd_ai <- function(title, 
                        prompt,
                        level = 2,
                        include_code = TRUE) {
  
  # Generate content
  content <- goose_ask(prompt)
  
  # Build markdown
  heading <- paste(rep("#", level), collapse = "")
  
  rmd_text <- sprintf("%s %s\n\n%s\n",
                     heading, title, content)
  
  if (include_code && grepl("```", content)) {
    # Content already has code blocks
  } else if (include_code) {
    # Ask for code example
    code_prompt <- paste("Provide R code example for:", prompt)
    code <- goose_ask(code_prompt)
    rmd_text <- paste0(rmd_text, "\n", code, "\n")
  }
  
  if (interactive() && rstudioapi::isAvailable()) {
    rstudioapi::insertText(rmd_text)
  }
  
  return(invisible(rmd_text))
}

#' Register Goose Chunk Engine
#'
#' Register custom knitr engine for Goose chunks
#' @export
register_goose_engine <- function() {
  knitr::knit_engines$set(goose = function(options) {
    # Get the prompt from chunk content
    prompt <- paste(options$code, collapse = "\n")
    
    # Check cache
    if (options$cache) {
      cached <- goose_cache_get(prompt)
      if (!is.null(cached)) {
        response <- cached
      } else {
        response <- goose_ask(prompt)
        goose_cache_set(prompt, response)
      }
    } else {
      response <- goose_ask(prompt)
    }
    
    # Format output based on options
    if (options$results == "asis") {
      # Return as-is for markdown rendering
      return(response)
    } else {
      # Wrap in code block for verbatim output
      return(sprintf("```\n%s\n```", response))
    }
  })
  
  invisible(TRUE)
}

#' Create Quarto AI Document
#'
#' Generate a complete Quarto document with AI assistance
#' @param title Document title
#' @param author Document author
#' @param outline Topic outline or description
#' @param format Output format (html, pdf, docx)
#' @param output_file Output file path
#' @return Path to created document
#' @export
goose_create_quarto <- function(title,
                               author = Sys.info()["user"],
                               outline,
                               format = "html",
                               output_file = NULL) {
  
  if (is.null(output_file)) {
    output_file <- paste0(
      gsub("[^A-Za-z0-9]", "_", tolower(title)),
      ".qmd"
    )
  }
  
  # Generate document structure
  structure_prompt <- sprintf(
    "Create a detailed outline for a Quarto document about: %s",
    outline
  )
  structure <- goose_ask(structure_prompt)
  
  # Build Quarto header
  yaml_header <- sprintf('---
title: "%s"
author: "%s"
date: "%s"
format: %s
execute:
  echo: true
  warning: false
---

', title, author, Sys.Date(), format)
  
  # Generate content for each section
  sections <- strsplit(structure, "\n")[[1]]
  content <- yaml_header
  
  withProgress(message = "Generating document...", {
    for (i in seq_along(sections)) {
      section <- sections[i]
      if (nchar(trimws(section)) > 0) {
        setProgress(value = i/length(sections),
                   detail = paste("Section", i))
        
        # Generate section content
        section_prompt <- sprintf(
          "Write content for this section of a technical document: %s",
          section
        )
        section_content <- goose_ask(section_prompt)
        
        # Add to document
        content <- paste0(content, "\n", section_content, "\n")
      }
    }
  })
  
  # Add setup chunk
  setup_chunk <- '
```{r setup, include=FALSE}
library(gooseR)
register_goose_engine()
```
'
  
  content <- sub("---\n\n", paste0("---\n", setup_chunk, "\n"), content)
  
  # Write file
  writeLines(content, output_file)
  
  # Open in RStudio if available
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(output_file)
  }
  
  message("Created Quarto document: ", output_file)
  return(output_file)
}

#' Create RMarkdown Report with AI
#'
#' Generate a complete RMarkdown report
#' @param title Report title
#' @param data Data frame to analyze
#' @param analysis_type Type of analysis
#' @param output_file Output file path
#' @return Path to created report
#' @export
goose_create_report <- function(title,
                               data = NULL,
                               analysis_type = "exploratory",
                               output_file = NULL) {
  
  if (is.null(output_file)) {
    output_file <- paste0(
      gsub("[^A-Za-z0-9]", "_", tolower(title)),
      ".Rmd"
    )
  }
  
  # Analyze data structure if provided
  data_info <- ""
  if (!is.null(data)) {
    data_info <- sprintf(
      "Data: %d rows, %d columns\nColumns: %s",
      nrow(data), ncol(data),
      paste(names(data), collapse = ", ")
    )
  }
  
  # Generate report structure
  structure_prompt <- sprintf(
    "Create an R Markdown report structure for %s analysis. %s",
    analysis_type, data_info
  )
  
  structure <- goose_ask(structure_prompt)
  
  # Build RMarkdown header
  yaml_header <- sprintf('---
title: "%s"
author: "%s"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    toc: true
    toc_float: true
    code_folding: hide
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE,
  warning = FALSE,
  message = FALSE,
  fig.width = 10,
  fig.height = 6
)

library(gooseR)
library(tidyverse)
library(DT)
register_goose_engine()
```

', title, Sys.info()["user"])
  
  # Add executive summary
  exec_summary <- '
## Executive Summary

```{goose, echo=FALSE, results="asis"}
Provide a brief executive summary for this analysis report
```
'
  
  # Generate analysis sections
  analysis_sections <- ""
  
  if (!is.null(data)) {
    # Data overview
    analysis_sections <- paste0(analysis_sections, '
## Data Overview

```{r data-overview}
# Display data structure
glimpse(data)

# Summary statistics
summary(data)

# Interactive table
datatable(head(data, 100))
```
')
    
    # Generate visualizations
    viz_prompt <- sprintf(
      "Generate R code for %s visualizations of this data: %s",
      analysis_type, data_info
    )
    viz_code <- goose_ask(viz_prompt)
    
    analysis_sections <- paste0(analysis_sections, '
## Visualizations

', viz_code, '
')
  }
  
  # Add AI-powered insights
  insights_section <- '
## Key Insights

```{goose, echo=FALSE, results="asis", cache=TRUE}
Provide key insights and recommendations based on the analysis
```
'
  
  # Combine all sections
  content <- paste0(
    yaml_header,
    exec_summary,
    structure,
    analysis_sections,
    insights_section
  )
  
  # Write file
  writeLines(content, output_file)
  
  # Open in RStudio if available
  if (rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(output_file)
  }
  
  message("Created RMarkdown report: ", output_file)
  return(output_file)
}

#' Insert AI Chunk in Current Document
#'
#' Interactive function to insert AI chunk at cursor
#' @export
goose_insert_chunk <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This function requires RStudio")
  }
  
  # Get document context
  context <- rstudioapi::getActiveDocumentContext()
  doc_path <- context$path
  
  # Determine document type
  is_quarto <- grepl("\\.qmd$", doc_path)
  is_rmd <- grepl("\\.Rmd$", doc_path)
  
  if (!is_quarto && !is_rmd) {
    stop("Current document must be .qmd or .Rmd")
  }
  
  # Prompt for chunk content
  prompt <- rstudioapi::showPrompt(
    "AI Chunk",
    "Enter your AI prompt:"
  )
  
  if (!is.null(prompt)) {
    # Ask for chunk options
    label <- rstudioapi::showPrompt(
      "Chunk Label",
      "Enter chunk label (optional):"
    )
    
    if (is_quarto) {
      chunk <- goose_quarto_chunk(prompt, label = label)
    } else {
      # RMarkdown version
      chunk <- sprintf('```{r %s}
# AI-generated content
ai_response <- goose_ask("%s")
cat(ai_response)
```', 
        ifelse(is.null(label) || nchar(label) == 0, "", label),
        prompt)
    }
    
    # Insert at cursor
    rstudioapi::insertText(chunk)
  }
}

#' Create Parameterized Report Template
#'
#' Generate a parameterized Quarto/RMarkdown template
#' @param type Report type (analysis, dashboard, presentation)
#' @param parameters List of parameters
#' @param output_file Output file path
#' @return Path to template file
#' @export
goose_create_template <- function(type = "analysis",
                                 parameters = list(),
                                 output_file = NULL) {
  
  if (is.null(output_file)) {
    output_file <- paste0(type, "_template.qmd")
  }
  
  # Build parameter YAML
  param_yaml <- ""
  if (length(parameters) > 0) {
    param_yaml <- "params:\n"
    for (name in names(parameters)) {
      param_yaml <- paste0(
        param_yaml,
        sprintf("  %s: %s\n", name, parameters[[name]])
      )
    }
  }
  
  # Generate template based on type
  template_prompt <- sprintf(
    "Create a Quarto template for a %s report with these parameters: %s",
    type, paste(names(parameters), collapse = ", ")
  )
  
  template_content <- goose_ask(template_prompt)
  
  # Build complete template
  template <- sprintf('---
title: "%s Report"
author: "Your Name"
date: "`r Sys.Date()`"
format:
  html:
    theme: cosmo
    toc: true
    code-fold: true
%s---

```{r setup, include=FALSE}
library(gooseR)
library(tidyverse)
register_goose_engine()

# Access parameters
%s
```

%s
', 
    (if (requireNamespace("stringr", quietly = TRUE)) stringr::str_to_title(type) else tools::toTitleCase(type)), 
    param_yaml,
    paste(sprintf("%s <- params$%s", 
                 names(parameters), 
                 names(parameters)), 
          collapse = "\n"),
    template_content
  )
  
  # Write template
  writeLines(template, output_file)
  
  message("Created template: ", output_file)
  return(output_file)
}
