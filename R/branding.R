#' GooseR Universal Branding System
#' 
#' Functions for creating and applying consistent branding across R outputs
#' @name gooseR-branding
#' @import yaml
#' @import ggplot2
NULL

#' Get brand configuration directory
#' 
#' @param use_temp Use temporary directory (default: TRUE for CRAN compliance)
#' @return Path to brand configuration directory
#' @keywords internal
get_brand_dir <- function(use_temp = TRUE) {
  # Check for package installation directory first
  pkg_dir <- system.file("brands", package = "gooseR")
  if (pkg_dir != "") {
    return(pkg_dir)
  }
  
  # Use temporary directory by default (CRAN policy)
  if (use_temp) {
    temp_dir <- file.path(tempdir(), "gooseR_brands")
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
    }
    return(temp_dir)
  }
  
  # Only use user directory if explicitly requested
  user_dir <- file.path(Sys.getenv("HOME"), ".config", "goose", "brands")
  if (!dir.exists(user_dir)) {
    dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(user_dir)
}

#' Load brand configuration
#' 
#' @param brand Name of the brand to load
#' @return List containing brand configuration
#' @export
#' @examples
#' \dontrun{
#' config <- load_brand("block")
#' }
load_brand <- function(brand = "block") {
  brand_dir <- get_brand_dir()
  
  # Try multiple locations
  possible_paths <- c(
    file.path(brand_dir, brand, paste0(brand, "_brand.yaml")),
    file.path(brand_dir, paste0(brand, "_brand.yaml")),
    file.path(brand_dir, brand, "brand.yaml"),
    file.path(brand_dir, paste0(brand, ".yaml"))
  )
  
  for (path in possible_paths) {
    if (file.exists(path)) {
      config <- yaml::read_yaml(path)
      message(sprintf("Loaded brand configuration from: %s", path))
      return(config)
    }
  }
  
  stop(sprintf("Brand configuration not found for: %s", brand))
}

#' Validate brand configuration
#' 
#' @param config Brand configuration list
#' @return Logical indicating if configuration is valid
#' @export
validate_brand <- function(config) {
  errors <- character()
  
  # Check required top-level fields
  required_fields <- c("brand", "colors", "typography")
  for (field in required_fields) {
    if (!field %in% names(config)) {
      errors <- c(errors, sprintf("Missing required field: %s", field))
    }
  }
  
  # Validate brand metadata
  if ("brand" %in% names(config)) {
    if (!all(c("name", "version") %in% names(config$brand))) {
      errors <- c(errors, "Brand must have 'name' and 'version' fields")
    }
  }
  
  # Validate colors
  if ("colors" %in% names(config)) {
    if (!any(c("primary", "core") %in% names(config$colors))) {
      errors <- c(errors, "Colors must have 'primary' or 'core' section")
    }
    
    # Check color format (should be hex)
    validate_colors <- function(color_list, prefix = "") {
      for (name in names(color_list)) {
        if (is.list(color_list[[name]])) {
          validate_colors(color_list[[name]], paste0(prefix, name, "."))
        } else if (is.character(color_list[[name]])) {
          if (!grepl("^#[0-9A-Fa-f]{6}$", color_list[[name]])) {
            errors <<- c(errors, sprintf("Invalid color format: %s%s = %s", 
                                        prefix, name, color_list[[name]]))
          }
        }
      }
    }
    validate_colors(config$colors)
  }
  
  # Validate typography
  if ("typography" %in% names(config)) {
    if (!"fonts" %in% names(config$typography)) {
      errors <- c(errors, "Typography must have 'fonts' section")
    }
    if (!"sizes" %in% names(config$typography)) {
      errors <- c(errors, "Typography must have 'sizes' section")
    }
  }
  
  if (length(errors) > 0) {
    warning("Brand validation errors:\n", paste(errors, collapse = "\n"))
    return(FALSE)
  }
  
  message("Brand configuration is valid")
  return(TRUE)
}

#' Create a new brand configuration interactively
#' 
#' @param brand_name Name for the new brand
#' @param template Template to use (default: "default")
#' @param interactive Use interactive prompts (default: TRUE)
#' @return Path to created brand configuration
#' @export
#' @examples
#' \dontrun{
#' goose_create_brand("my_company")
#' }
goose_create_brand <- function(brand_name = NULL, 
                              template = "default",
                              interactive = TRUE) {
  
  if (is.null(brand_name) && interactive) {
    brand_name <- readline("Enter brand name: ")
  }
  
  if (is.null(brand_name) || brand_name == "") {
    stop("Brand name is required")
  }
  
  # Sanitize brand name for file system
  brand_slug <- tolower(gsub("[^a-zA-Z0-9_-]", "_", brand_name))
  
  # Create brand directory
  brand_dir <- file.path(get_brand_dir(), brand_slug)
  if (!dir.exists(brand_dir)) {
    dir.create(brand_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Initialize configuration
  config <- list(
    brand = list(
      name = brand_name,
      version = "1.0.0",
      description = paste("Brand configuration for", brand_name)
    )
  )
  
  if (interactive) {
    message("\n=== Brand Configuration Setup ===\n")
    
    # Colors
    message("Define your brand colors:")
    config$colors <- list(
      primary = list(
        main = readline("Primary color (hex, e.g., #0055FF): "),
        contrast = readline("Primary contrast color (hex, e.g., #FFFFFF): ")
      )
    )
    
    add_secondary <- tolower(readline("Add secondary colors? (y/n): "))
    if (add_secondary == "y") {
      config$colors$secondary <- list(
        main = readline("Secondary color (hex): "),
        light = readline("Secondary light color (hex): "),
        dark = readline("Secondary dark color (hex): ")
      )
    }
    
    # Typography
    message("\nDefine your typography:")
    config$typography <- list(
      fonts = list(
        primary = readline("Primary font family: "),
        body = readline("Body font family: "),
        monospace = readline("Monospace font family (default: 'Courier New'): ")
      ),
      sizes = list(
        base = as.numeric(readline("Base font size (default: 12): ")),
        h1 = as.numeric(readline("H1 size (default: 28): ")),
        h2 = as.numeric(readline("H2 size (default: 24): "))
      )
    )
    
    # Set defaults for empty values
    if (config$typography$fonts$monospace == "") {
      config$typography$fonts$monospace <- "Courier New"
    }
    if (is.na(config$typography$sizes$base)) {
      config$typography$sizes$base <- 12
    }
    if (is.na(config$typography$sizes$h1)) {
      config$typography$sizes$h1 <- 28
    }
    if (is.na(config$typography$sizes$h2)) {
      config$typography$sizes$h2 <- 24
    }
    
  } else {
    # Load template
    template_path <- system.file("brands", "templates", "brand_template.yaml", 
                                package = "gooseR")
    if (file.exists(template_path)) {
      config <- yaml::read_yaml(template_path)
      config$brand$name <- brand_name
      config$brand$description <- paste("Brand configuration for", brand_name)
    }
  }
  
  # Save configuration
  config_path <- file.path(brand_dir, paste0(brand_slug, "_brand.yaml"))
  yaml::write_yaml(config, config_path)
  
  message(sprintf("\nBrand configuration created: %s", config_path))
  message(sprintf("To use: theme_brand('%s')", brand_slug))
  
  return(invisible(config_path))
}

#' Generate ggplot2 theme from brand configuration
#' 
#' @param brand Name of the brand to use
#' @param variant Theme variant ("light", "dark", or NULL for default)
#' @param base_theme Base ggplot2 theme to build upon (default: theme_minimal())
#' @param base_size Base font size (overrides brand config if specified)
#' @return A ggplot2 theme object
#' @export
#' @examples
#' \dontrun{
#' library(ggplot2)
#' # Light theme
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_brand("block", variant = "light")
#'   
#' # Dark theme
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   theme_brand("block", variant = "dark")
#' }
theme_brand <- function(brand = "block", 
                       variant = "light",
                       base_theme = ggplot2::theme_minimal(),
                       base_size = NULL) {
  
  # Load brand configuration
  config <- load_brand(brand)
  
  # Extract key values with defaults
  colors <- config$colors
  typography <- config$typography
  spacing <- config$spacing
  themes <- config$themes
  
  # Get base size from config or parameter
  if (is.null(base_size)) {
    base_size <- ifelse(!is.null(typography$sizes$base), 
                       typography$sizes$base, 
                       12)
  }
  
  # Determine colors based on variant
  if (!is.null(variant) && !is.null(themes) && !is.null(themes[[variant]])) {
    # Use theme-specific colors
    theme_config <- themes[[variant]]
    text_color <- theme_config$text %||% "#000000"
    background_color <- theme_config$background %||% "#FFFFFF"
    grid_color <- theme_config$grid %||% "#F5F5F5"
    axis_color <- theme_config$axis %||% text_color
  } else {
    # Fall back to default colors
    text_color <- colors$primary$black %||% colors$primary$main %||% "#000000"
    background_color <- colors$primary$white %||% colors$primary$contrast %||% "#FFFFFF"
    grid_color <- colors$gray$`5` %||% colors$gray$`10` %||% "#F5F5F5"
    axis_color <- text_color
  }
  
  # Enforce sans-serif chain (no serifs): Inter → Cash Sans → Helvetica → Arial → sans
  preferred_chain <- c("Inter", "Cash Sans", "Helvetica", "Arial", "sans")
  chosen_font <- NULL
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    avail <- unique(systemfonts::system_fonts()$family)
    chosen_font <- preferred_chain[preferred_chain %in% avail][1]
  }
  if (is.na(chosen_font) || is.null(chosen_font)) chosen_font <- preferred_chain[length(preferred_chain)]
  font_family <- chosen_font
  
  # Determine if we should show grid lines
  show_grid <- ifelse(!is.null(config$plots$grid$major), 
                      config$plots$grid$major, 
                      FALSE)
  
  # Build theme
  theme <- base_theme +
    ggplot2::theme(
      # Text elements
      text = ggplot2::element_text(
        family = font_family,
        size = base_size,
        color = text_color
      ),
      
      # Title elements
      plot.title = ggplot2::element_text(
        size = base_size * 1.5,
        face = "bold",
        hjust = 0,
        margin = ggplot2::margin(b = base_size)
      ),
      plot.subtitle = ggplot2::element_text(
        size = base_size * 1.1,
        hjust = 0,
        margin = ggplot2::margin(b = base_size * 0.5)
      ),
      plot.caption = ggplot2::element_text(
        size = base_size * 0.8,
        hjust = 1,
        margin = ggplot2::margin(t = base_size)
      ),
      
      # Axis elements
      axis.title = ggplot2::element_text(
        size = base_size,
        face = "bold"
      ),
      axis.text = ggplot2::element_text(
        size = base_size * 0.9,
        color = text_color
      ),
      axis.line = ggplot2::element_line(
        color = text_color,
        linewidth = 0.5
      ),
      axis.ticks = ggplot2::element_line(
        color = text_color,
        linewidth = 0.5
      ),
      
      # Panel elements
      panel.background = ggplot2::element_rect(
        fill = background_color,
        color = NA
      ),
      panel.grid.major = if (show_grid) {
        ggplot2::element_line(color = grid_color, linewidth = 0.25)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      
      # Legend elements
      legend.background = ggplot2::element_rect(
        fill = background_color,
        color = NA
      ),
      legend.key = ggplot2::element_rect(
        fill = background_color,
        color = NA
      ),
      legend.text = ggplot2::element_text(
        size = base_size * 0.9
      ),
      legend.title = ggplot2::element_text(
        size = base_size,
        face = "bold"
      ),
      
      # Strip elements (for facets)
      strip.background = ggplot2::element_rect(
        fill = grid_color,
        color = text_color
      ),
      strip.text = ggplot2::element_text(
        size = base_size,
        face = "bold",
        margin = ggplot2::margin(base_size * 0.5)
      ),
      
      # Overall plot
      plot.background = ggplot2::element_rect(
        fill = background_color,
        color = NA
      ),
      plot.margin = ggplot2::margin(
        t = base_size,
        r = base_size,
        b = base_size,
        l = base_size
      )
    )
  
  return(theme)
}

#' Get brand color palette
#' 
#' @param brand Name of the brand
#' @param palette Type of palette ("categorical", "sequential", "diverging")
#' @param n Number of colors to return (NULL for all)
#' @return Character vector of hex colors
#' @export
#' @examples
#' \dontrun{
#' colors <- brand_palette("block", "categorical")
#' }
brand_palette <- function(brand = "block", 
                         palette = "categorical",
                         n = NULL) {
  
  config <- load_brand(brand)
  
  # Block brand: enforce monochrome (black/white/gray only)
  if (tolower(brand) == "block") {
    if (palette == "categorical") {
      colors <- c("#000000", "#666666", "#B3B3B3", "#E5E5E5")
    } else if (palette == "sequential") {
      colors <- c("#000000", "#333333", "#666666", "#999999", "#CCCCCC", "#E5E5E5", "#FFFFFF")
    } else if (palette == "diverging") {
      colors <- c("#000000", "#666666", "#FFFFFF")
    } else {
      colors <- c("#000000", "#FFFFFF")
    }
  } else if (!is.null(config$plots$palettes[[palette]])) {
    colors <- unlist(config$plots$palettes[[palette]])
  } else {
    # Fall back to extracting from color definitions
    if (palette == "categorical") {
      # Extract main colors
      colors <- c(
        config$colors$core$blue,
        config$colors$orange$`30`,
        config$colors$teal$`40`,
        config$colors$accent$gold_30,
        config$colors$accent$red_40
      )
    } else {
      colors <- c("#000000", "#666666", "#CCCCCC", "#FFFFFF")
    }
  }
  
  # Filter out NULL values
  colors <- colors[!sapply(colors, is.null)]
  
  # Return requested number of colors
  if (!is.null(n)) {
    if (n <= length(colors)) {
      return(colors[1:n])
    } else {
      # Recycle colors if more requested than available
      return(rep_len(colors, n))
    }
  }
  
  return(colors)
}

#' Generate CSS from brand configuration
#' 
#' @param brand Name of the brand
#' @param output_file Path to save CSS file (NULL to return as string)
#' @param minify Minify the CSS output (default: FALSE)
#' @return CSS content as string (invisibly if saved to file)
#' @export
#' @examples
#' \dontrun{
#' # Save to file in temp directory
#' temp_file <- file.path(tempdir(), "block.css")
#' brand_css("block", temp_file)
#' 
#' # Get as string
#' css <- brand_css("block")
#' }
brand_css <- function(brand = "block", 
                     output_file = NULL,
                     minify = FALSE) {
  
  config <- load_brand(brand)
  
  # Start building CSS
  css_lines <- c(
    sprintf("/* %s Brand Styles */", config$brand$name),
    sprintf("/* Generated by gooseR v%s */", 
            utils::packageVersion("gooseR")),
    "",
    ":root {"
  )
  
  # Add color variables
  if (!is.null(config$colors)) {
    css_lines <- c(css_lines, "  /* Colors */")
    add_color_vars <- function(colors, prefix = "") {
      vars <- character()
      for (name in names(colors)) {
        if (is.list(colors[[name]])) {
          vars <- c(vars, add_color_vars(colors[[name]], 
                                        paste0(prefix, name, "-")))
        } else {
          var_name <- gsub("_", "-", paste0(prefix, name))
          vars <- c(vars, sprintf("  --%s: %s;", var_name, colors[[name]]))
        }
      }
      return(vars)
    }
    css_lines <- c(css_lines, add_color_vars(config$colors, "color-"))
  }
  
  # Add typography variables
  if (!is.null(config$typography)) {
    css_lines <- c(css_lines, "", "  /* Typography */")
    
    if (!is.null(config$typography$fonts)) {
      for (name in names(config$typography$fonts)) {
        css_lines <- c(css_lines, 
                      sprintf("  --font-%s: %s;", name, 
                             config$typography$fonts[[name]]))
      }
    }
    
    if (!is.null(config$typography$sizes)) {
      for (name in names(config$typography$sizes)) {
        css_lines <- c(css_lines, 
                      sprintf("  --size-%s: %spx;", name, 
                             config$typography$sizes[[name]]))
      }
    }
  }
  
  # Add spacing variables
  if (!is.null(config$spacing)) {
    css_lines <- c(css_lines, "", "  /* Spacing */")
    if (!is.null(config$spacing$unit)) {
      css_lines <- c(css_lines, 
                    sprintf("  --spacing-unit: %spx;", config$spacing$unit))
    }
    if (!is.null(config$spacing$scale)) {
      for (name in names(config$spacing$scale)) {
        value <- config$spacing$scale[[name]] * (config$spacing$unit %||% 8)
        css_lines <- c(css_lines, 
                      sprintf("  --spacing-%s: %spx;", name, value))
      }
    }
  }
  
  css_lines <- c(css_lines, "}", "")
  
  # Add base styles
  css_lines <- c(css_lines,
    "/* Base Styles */",
    "body {",
    "  font-family: var(--font-body, var(--font-primary));",
    "  font-size: var(--size-base);",
    "  color: var(--color-primary-black, var(--color-primary-main));",
    "  background-color: var(--color-primary-white, var(--color-primary-contrast));",
    "  line-height: 1.5;",
    "}",
    "",
    "h1 { font-size: var(--size-h1); font-weight: bold; }",
    "h2 { font-size: var(--size-h2); font-weight: bold; }",
    "h3 { font-size: var(--size-h3); font-weight: bold; }",
    "h4 { font-size: var(--size-h4); font-weight: bold; }",
    "h5 { font-size: var(--size-h5); font-weight: bold; }",
    "h6 { font-size: var(--size-h6); font-weight: bold; }",
    "",
    "a {",
    "  color: var(--color-core-blue, var(--color-primary-main));",
    "  text-decoration: none;",
    "}",
    "",
    "a:hover {",
    "  text-decoration: underline;",
    "}"
  )
  
  # Combine all CSS
  css_content <- paste(css_lines, collapse = "\n")
  
  # Minify if requested
  if (minify) {
    css_content <- gsub("\\s+", " ", css_content)
    css_content <- gsub("\\s*([{}:;,])\\s*", "\\1", css_content)
    css_content <- gsub("/\\*[^*]*\\*/", "", css_content)
  }
  
  # Save or return
  if (!is.null(output_file)) {
    writeLines(css_content, output_file)
    message(sprintf("CSS saved to: %s", output_file))
    return(invisible(css_content))
  }
  
  return(css_content)
}

#' Generate RMarkdown template with brand styling
#' 
#' @param brand Name of the brand
#' @param title Document title
#' @param output_format RMarkdown output format (default: "html_document")
#' @param output_file Path to save template (NULL to return as string)
#' @return RMarkdown template content
#' @export
#' @examples
#' \dontrun{
#' # Create branded RMarkdown template in temp directory
#' temp_file <- file.path(tempdir(), "report.Rmd")
#' brand_rmd_template("block", "My Report", output_file = temp_file)
#' }
brand_rmd_template <- function(brand = "block",
                              title = "Report",
                              output_format = "html_document",
                              output_file = NULL) {
  
  config <- load_brand(brand)
  
  # Generate CSS for the brand
  css_content <- brand_css(brand, minify = TRUE)
  
  # Build RMarkdown template
  template <- c(
    "---",
    sprintf('title: "%s"', title),
    sprintf('author: "%s"', config$metadata$author %||% ""),
    sprintf('date: "`r Sys.Date()`"'),
    sprintf('output:'),
    sprintf('  %s:', output_format)
  )
  
  if (output_format == "html_document") {
    template <- c(template,
      '    toc: true',
      '    toc_float: true',
      '    toc_depth: 3',
      '    number_sections: true',
      '    theme: null',
      '    highlight: tango',
      '    css: !expr gooseR::brand_css_inline()'
    )
  }
  
  template <- c(template,
    "---",
    "",
    "```{r setup, include=FALSE}",
    "knitr::opts_chunk$set(",
    "  echo = FALSE,",
    "  message = FALSE,",
    "  warning = FALSE,",
    "  fig.width = 7,",
    "  fig.height = 5,",
    "  fig.align = 'center'",
    ")",
    "",
    "# Load libraries",
    "library(ggplot2)",
    "library(gooseR)",
    "",
    sprintf("# Set brand theme as default"),
    sprintf('theme_set(theme_brand("%s"))', brand),
    "",
    sprintf("# Load brand colors"),
    sprintf('brand_colors <- brand_palette("%s", "categorical")', brand),
    "```",
    "",
    "# Executive Summary",
    "",
    "Key findings and recommendations go here.",
    "",
    "# Introduction",
    "",
    "Background and context for the analysis.",
    "",
    "# Methods",
    "",
    "Description of data sources and analytical approach.",
    "",
    "# Results",
    "",
    "## Data Overview",
    "",
    "```{r example-plot}",
    "# Example plot with brand theme",
    "ggplot(mtcars, aes(x = mpg, y = wt, color = factor(cyl))) +",
    "  geom_point(size = 3) +",
    sprintf('  scale_color_manual(values = brand_palette("%s", "categorical", 3)) +', brand),
    '  labs(',
    '    title = "Fuel Efficiency vs Weight",',
    '    subtitle = "By number of cylinders",',
    '    x = "Miles per Gallon",',
    '    y = "Weight (1000 lbs)",',
    '    color = "Cylinders"',
    '  )',
    "```",
    "",
    "# Discussion",
    "",
    "Interpretation of results and implications.",
    "",
    "# Conclusions",
    "",
    "Summary of key findings and next steps.",
    "",
    "# Appendix",
    "",
    "Additional tables, figures, and technical details."
  )
  
  # Combine template
  template_content <- paste(template, collapse = "\n")
  
  # Save or return
  if (!is.null(output_file)) {
    writeLines(template_content, output_file)
    message(sprintf("RMarkdown template saved to: %s", output_file))
    return(invisible(template_content))
  }
  
  return(template_content)
}

#' Get inline CSS for RMarkdown
#' 
#' Helper function to embed CSS directly in RMarkdown
#' @param brand Brand name
#' @return HTML style tag with CSS
#' @export
#' @keywords internal
brand_css_inline <- function(brand = "block") {
  css <- brand_css(brand, minify = TRUE)
  return(htmltools::HTML(paste0("<style>", css, "</style>")))
}

# AI-Enhanced Branding Functions ----

#' Create Brand with AI Assistance
#'
#' Enhanced brand creation with AI-powered suggestions for colors and typography.
#'
#' @param brand_name Name of the brand
#' @param industry Optional industry context for better suggestions
#' @param style Optional style preference (e.g., "modern", "classic", "playful")
#' @param use_ai Whether to use AI for suggestions (requires goose_ask)
#' @param output_dir Directory to save brand configuration
#'
#' @return Path to created brand configuration
#' @export
#'
#' @examples
#' \dontrun{
#' # Create brand with AI assistance
#' goose_create_brand_ai("TechStartup", 
#'                      industry = "fintech",
#'                      style = "modern")
#' }
goose_create_brand_ai <- function(brand_name,
                                 industry = NULL,
                                 style = NULL,
                                 use_ai = TRUE,
                                 output_dir = NULL) {
  
  if (is.null(output_dir)) {
    output_dir <- get_brand_dir()
  }
  
  # Sanitize brand name
  brand_slug <- tolower(gsub("[^a-zA-Z0-9_-]", "_", brand_name))
  
  # Initialize configuration
  config <- list(
    brand = list(
      name = brand_name,
      version = "1.0.0",
      description = paste("Brand configuration for", brand_name),
      created_with_ai = use_ai
    )
  )
  
  if (use_ai) {
    # Check if AI functions are available
    if (!exists("goose_ask")) {
      source(system.file("R", "cli_integration.R", package = "gooseR"))
      source(system.file("R", "ai_assistant.R", package = "gooseR"))
    }
    
    message("[AI] Using AI to generate brand suggestions...")
    
    # Build context for AI
    context <- sprintf("Creating a brand identity for %s", brand_name)
    if (!is.null(industry)) {
      context <- paste0(context, " in the ", industry, " industry")
    }
    if (!is.null(style)) {
      context <- paste0(context, " with a ", style, " style")
    }
    
    # Get color suggestions
    color_prompt <- paste0(
      context, ".\n\n",
      "Suggest a professional color palette with:\n",
      "1. Primary color (main brand color)\n",
      "2. Primary contrast color\n",
      "3. Secondary colors (3-5 colors)\n",
      "4. Neutral colors (grays)\n",
      "5. Semantic colors (success, warning, error)\n\n",
      "Provide hex codes and explain the rationale."
    )
    
    color_response <- goose_ask(color_prompt, timeout = getOption("goose.timeout", 300))
    colors_hex <- extract_hex_colors(color_response)
    
    # Parse AI color suggestions
    if (length(colors_hex) >= 2) {
      config$colors <- list(
        primary = list(
          main = colors_hex[1],
          contrast = colors_hex[2]
        )
      )
      
      if (length(colors_hex) >= 5) {
        config$colors$secondary <- list(
          main = colors_hex[3],
          light = colors_hex[4],
          dark = colors_hex[5]
        )
      }
      
      # Add semantic colors if we have enough
      if (length(colors_hex) >= 8) {
        config$colors$semantic <- list(
          success = colors_hex[6],
          warning = colors_hex[7],
          error = colors_hex[8]
        )
      }
    }
    
    # Get typography suggestions
    typo_prompt <- paste0(
      context, ".\n\n",
      "Suggest typography for this brand:\n",
      "1. Primary font (for headings)\n",
      "2. Body font (for text)\n",
      "3. Monospace font (for code)\n\n",
      "Consider web-safe fonts or Google Fonts.\n",
      "Explain why these fonts work for this brand."
    )
    
    typo_response <- goose_ask(typo_prompt, timeout = getOption("goose.timeout", 300))
    
    # Extract font suggestions (simple pattern matching)
    fonts <- extract_font_suggestions(typo_response)
    
    config$typography <- list(
      fonts = list(
        primary = fonts$primary %||% "Inter",
        body = fonts$body %||% "Inter",
        monospace = fonts$monospace %||% "Courier New",
        fallback = "system-ui, -apple-system, sans-serif"
      ),
      sizes = list(
        base = 12,
        h1 = 28,
        h2 = 24,
        h3 = 20,
        h4 = 16,
        h5 = 14,
        h6 = 12
      )
    )
    
    # Store AI rationale
    config$ai_rationale <- list(
      colors = color_response,
      typography = typo_response,
      generated = Sys.time()
    )
    
    message("[AI] Suggestions generated successfully!")
    
  } else {
    # Use defaults without AI
    config$colors <- list(
      primary = list(
        main = "#0055FF",
        contrast = "#FFFFFF"
      ),
      secondary = list(
        main = "#FF5500",
        light = "#FFE0B1",
        dark = "#8B3A00"
      )
    )
    
    config$typography <- list(
      fonts = list(
        primary = "Inter",
        body = "Inter",
        monospace = "Courier New"
      ),
      sizes = list(
        base = 12,
        h1 = 28,
        h2 = 24
      )
    )
  }
  
  # Add plot configuration
  config$plots <- list(
    grid = list(
      major = FALSE,
      minor = FALSE
    ),
    palettes = list(
      categorical = unname(unlist(config$colors$primary)),
      sequential = generate_sequential_palette(config$colors$primary$main),
      diverging = generate_diverging_palette(
        config$colors$primary$main,
        config$colors$secondary$main %||% "#FF5500"
      )
    )
  )
  
  # Add themes
  config$themes <- list(
    light = list(
      background = "#FFFFFF",
      text = config$colors$primary$main,
      grid = "#F5F5F5"
    ),
    dark = list(
      background = "#1A1A1A",
      text = "#FFFFFF",
      grid = "#333333"
    )
  )
  
  # Create brand directory
  brand_dir <- file.path(output_dir, brand_slug)
  if (!dir.exists(brand_dir)) {
    dir.create(brand_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Save configuration
  config_path <- file.path(brand_dir, paste0(brand_slug, "_brand.yaml"))
  yaml::write_yaml(config, config_path)
  
  message(sprintf("\n[Brand] Configuration created: %s", config_path))
  message(sprintf("To use: theme_brand('%s')", brand_slug))
  
  # Generate preview if possible
  if (interactive() && requireNamespace("ggplot2", quietly = TRUE)) {
    message("\nGenerating brand preview...")
    preview_brand(brand_slug)
  }
  
  return(invisible(config_path))
}

#' Optimize Brand Palette with AI
#'
#' Get AI suggestions to improve an existing brand palette.
#'
#' @param brand Name of the brand to optimize
#' @param goals Character vector of optimization goals
#' @param constraints Optional constraints (e.g., "keep primary color")
#'
#' @return List with original and optimized palettes
#' @export
goose_optimize_palette <- function(brand,
                                  goals = c("accessibility", "harmony", "contrast"),
                                  constraints = NULL) {
  
  # Load current brand
  config <- load_brand(brand)
  current_colors <- unlist(config$colors)
  
  # Build optimization prompt
  prompt <- sprintf(
    "Optimize this color palette for %s:\n\n",
    paste(goals, collapse = ", ")
  )
  
  prompt <- paste0(prompt, "Current colors:\n")
  for (name in names(current_colors)) {
    if (!is.null(current_colors[[name]])) {
      prompt <- paste0(prompt, sprintf("- %s: %s\n", name, current_colors[[name]]))
    }
  }
  
  if (!is.null(constraints)) {
    prompt <- paste0(prompt, "\nConstraints: ", paste(constraints, collapse = ", "))
  }
  
  prompt <- paste0(prompt, 
                  "\n\nProvide optimized hex codes with:\n",
                  "1. WCAG accessibility scores\n",
                  "2. Color harmony analysis\n",
                  "3. Specific improvements made")
  
  # Get AI response
  response <- goose_ask(prompt, timeout = getOption("goose.timeout", 300))
  
  # Extract optimized colors
  optimized_colors <- extract_hex_colors(response)
  
  result <- list(
    original = current_colors,
    optimized = optimized_colors,
    analysis = response,
    goals = goals,
    timestamp = Sys.time()
  )
  
  class(result) <- c("goose_palette_optimization", "list")
  result
}

#' Preview Brand Configuration
#'
#' Generate a preview of brand colors and typography.
#'
#' @param brand Name of the brand to preview
#' @param output_file Optional file to save preview
#'
#' @return ggplot object with brand preview
#' @export
preview_brand <- function(brand, output_file = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 is required for brand preview")
  }
  
  config <- load_brand(brand)
  
  # Create sample plot with brand theme
  sample_data <- data.frame(
    x = 1:5,
    y = c(3, 5, 4, 7, 6),
    category = factor(c("A", "B", "A", "B", "C"))
  )
  
  p <- ggplot2::ggplot(sample_data, ggplot2::aes(x = x, y = y, color = category)) +
    ggplot2::geom_line(size = 2) +
    ggplot2::geom_point(size = 4) +
    ggplot2::scale_color_manual(values = brand_palette(brand, "categorical", 3)) +
    ggplot2::labs(
      title = sprintf("%s Brand Preview", config$brand$name),
      subtitle = "Sample visualization with brand theme",
      x = "X Axis Label",
      y = "Y Axis Label",
      color = "Category"
    ) +
    theme_brand(brand)
  
  if (!is.null(output_file)) {
    ggplot2::ggsave(output_file, p, width = 8, height = 6)
    message(sprintf("Preview saved to: %s", output_file))
  }
  
  return(p)
}

# Helper functions for AI branding ----

#' Extract font suggestions from AI response
#' @keywords internal
extract_font_suggestions <- function(text) {
  fonts <- list()
  
  # Common font patterns
  font_patterns <- c(
    "Primary font:?\\s*([A-Za-z\\s]+)",
    "Heading font:?\\s*([A-Za-z\\s]+)",
    "Body font:?\\s*([A-Za-z\\s]+)",
    "Text font:?\\s*([A-Za-z\\s]+)",
    "Monospace:?\\s*([A-Za-z\\s]+)",
    "Code font:?\\s*([A-Za-z\\s]+)"
  )
  
  # Try to extract fonts
  lines <- strsplit(text, "\n")[[1]]
  
  for (line in lines) {
    if (grepl("primary|heading", line, ignore.case = TRUE)) {
      # Extract font name (assuming it's in quotes or after colon)
      font_match <- regmatches(line, regexpr('"[^"]+"', line))
      if (length(font_match) > 0) {
        fonts$primary <- gsub('"', '', font_match[1])
      }
    }
    if (grepl("body|text", line, ignore.case = TRUE) && !grepl("monospace", line, ignore.case = TRUE)) {
      font_match <- regmatches(line, regexpr('"[^"]+"', line))
      if (length(font_match) > 0) {
        fonts$body <- gsub('"', '', font_match[1])
      }
    }
    if (grepl("monospace|code", line, ignore.case = TRUE)) {
      font_match <- regmatches(line, regexpr('"[^"]+"', line))
      if (length(font_match) > 0) {
        fonts$monospace <- gsub('"', '', font_match[1])
      }
    }
  }
  
  # Defaults if not found
  if (is.null(fonts$primary)) fonts$primary <- "Inter"
  if (is.null(fonts$body)) fonts$body <- fonts$primary
  if (is.null(fonts$monospace)) fonts$monospace <- "Courier New"
  
  fonts
}

#' Generate sequential color palette
#' @keywords internal
generate_sequential_palette <- function(base_color, n = 5) {
  # Simple sequential palette generation
  # This is a placeholder - could be enhanced with proper color theory
  colors <- character(n)
  colors[1] <- "#FFFFFF"
  colors[n] <- base_color
  
  # Interpolate middle colors
  if (n > 2) {
    for (i in 2:(n-1)) {
      # Simple linear interpolation
      alpha <- (i - 1) / (n - 1)
      colors[i] <- blend_colors("#FFFFFF", base_color, alpha)
    }
  }
  
  colors
}

#' Generate diverging color palette
#' @keywords internal
generate_diverging_palette <- function(color1, color2, n = 5) {
  colors <- character(n)
  mid <- ceiling(n / 2)
  
  colors[1] <- color1
  colors[n] <- color2
  colors[mid] <- "#FFFFFF"
  
  # Fill in remaining colors
  if (n > 3) {
    for (i in 2:(mid-1)) {
      alpha <- i / mid
      colors[i] <- blend_colors(color1, "#FFFFFF", alpha)
    }
    for (i in (mid+1):(n-1)) {
      alpha <- (i - mid) / (n - mid)
      colors[i] <- blend_colors("#FFFFFF", color2, alpha)
    }
  }
  
  colors
}

#' Blend two colors
#' @keywords internal
blend_colors <- function(color1, color2, alpha = 0.5) {
  # Convert hex to RGB
  rgb1 <- col2rgb(color1)[,1]
  rgb2 <- col2rgb(color2)[,1]
  
  # Blend
  blended <- rgb1 * (1 - alpha) + rgb2 * alpha
  
  # Convert back to hex
  rgb(blended[1], blended[2], blended[3], maxColorValue = 255)
}

#' Extract hex colors from text
#' @keywords internal
extract_hex_colors <- function(text) {
  pattern <- "#[0-9A-Fa-f]{6}"
  matches <- gregexpr(pattern, text)
  colors <- regmatches(text, matches)[[1]]
  unique(colors)
}
