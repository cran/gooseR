#' RStudio/Positron Addins for GooseR
#' @import shiny
#' @import miniUI
#' @importFrom shiny reactive reactiveVal reactiveValues observe observeEvent req
#' @importFrom shiny insertUI removeUI stopApp renderPrint verbatimTextOutput
#' @importFrom shiny hr icon uiOutput tagList modalButton modalDialog titlePanel
#' @importFrom shiny updateTextInput
#' @importFrom miniUI dialogViewer runGadget


#'
#' Interactive UI components for IDE integration
#' @importFrom shiny shinyApp fluidPage sidebarLayout sidebarPanel mainPanel
#' @importFrom shiny textInput actionButton textAreaInput selectInput
#' @importFrom shiny renderText renderUI observeEvent reactive
#' @importFrom shiny tags HTML br div h3 h4 p span
#' @importFrom shiny updateTextAreaInput updateSelectInput
#' @importFrom shiny showNotification removeNotification
#' @importFrom shiny withProgress setProgress
#' @importFrom miniUI miniPage gadgetTitleBar miniContentPanel
#' @importFrom miniUI miniButtonBlock miniTitleBarButton
#' @importFrom rstudioapi getActiveDocumentContext insertText
#' @importFrom rstudioapi documentSave documentNew
#' @importFrom rstudioapi getSourceEditorContext
NULL

#' GooseR Chat Interface
#'
#' Interactive chat interface for AI conversations
#' @export
goose_addin_chat <- function() {
  # Check if running in RStudio
  if (!rstudioapi::isAvailable()) {
    stop("This addin requires RStudio or Positron IDE")
  }
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("GooseR AI Chat",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      tags$head(
        tags$style(HTML("
          .chat-container {
            height: 400px;
            overflow-y: auto;
            border: 1px solid #ddd;
            padding: 10px;
            margin-bottom: 10px;
            background: #f9f9f9;
            border-radius: 5px;
          }
          .user-message {
            background: #007bff;
            color: white;
            padding: 8px 12px;
            border-radius: 10px;
            margin: 5px 0;
            margin-left: 20%;
            text-align: right;
          }
          .ai-message {
            background: #e9ecef;
            color: #333;
            padding: 8px 12px;
            border-radius: 10px;
            margin: 5px 0;
            margin-right: 20%;
          }
          .streaming-indicator {
            color: #007bff;
            font-style: italic;
          }
        "))
      ),
      
      div(class = "chat-container",
        div(id = "chat-messages")
      ),
      
      fluidRow(
        column(9,
          textAreaInput("user_input", NULL, 
            placeholder = "Type your message here...",
            rows = 2, width = "100%")
        ),
        column(3,
          actionButton("send_btn", "Send", 
            icon = icon("paper-plane"),
            width = "100%",
            class = "btn-primary"),
          br(), br(),
          actionButton("clear_btn", "Clear", 
            icon = icon("trash"),
            width = "100%")
        )
      ),
      
      fluidRow(
        column(12,
          selectInput("session_select", "Session:",
            choices = c("New Session" = "new"),
            width = "100%")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Reactive values for chat state
    chat_state <- reactiveValues(
      messages = list(),
      current_session = NULL,
      streaming = FALSE
    )
    
    # Initialize session
    observeEvent(TRUE, {
      chat_state$current_session <- goose_session()
      updateSelectInput(session, "session_select",
        choices = c("New Session" = "new",
                   chat_state$current_session$id))
    }, once = TRUE)
    
    # Send message
    observeEvent(input$send_btn, {
      req(input$user_input)
      
      # Add user message to chat
      user_msg <- input$user_input
      chat_state$messages <- append(chat_state$messages, list(
        list(role = "user", content = user_msg)
      ))
      
      # Clear input
      updateTextAreaInput(session, "user_input", value = "")
      
      # Update UI with user message
      insertUI(
        selector = "#chat-messages",
        where = "beforeEnd",
        ui = div(class = "user-message", user_msg)
      )
      
      # Show streaming indicator
      insertUI(
        selector = "#chat-messages",
        where = "beforeEnd",
        ui = div(id = "streaming-msg", 
                class = "streaming-indicator",
                "Goose is thinking...")
      )
      
      # Get AI response
      withProgress(message = "Getting response...", {
        response <- tryCatch({
          goose_ask(user_msg, session_id = chat_state$current_session$id)
        }, error = function(e) {
          paste("Error:", e$message)
        })
      })
      
      # Remove streaming indicator
      removeUI(selector = "#streaming-msg")
      
      # Add AI response
      chat_state$messages <- append(chat_state$messages, list(
        list(role = "assistant", content = response)
      ))
      
      # Update UI with AI message
      insertUI(
        selector = "#chat-messages",
        where = "beforeEnd",
        ui = div(class = "ai-message", HTML(if (requireNamespace("markdown", quietly = TRUE)) markdown::markdownToHTML(
          text = response, fragment.only = TRUE) else response))
      )
      
      # Scroll to bottom
      session$sendCustomMessage("scrollToBottom", TRUE)
    })
    
    # Clear chat
    observeEvent(input$clear_btn, {
      chat_state$messages <- list()
      removeUI(selector = "#chat-messages > div", multiple = TRUE)
      showNotification("Chat cleared", type = "info", duration = 2)
    })
    
    # Handle session selection
    observeEvent(input$session_select, {
      if (input$session_select == "new") {
        chat_state$current_session <- goose_session()
        chat_state$messages <- list()
        removeUI(selector = "#chat-messages > div", multiple = TRUE)
      }
    })
    
    # Handle Done button
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  viewer <- dialogViewer("GooseR Chat", width = 600, height = 700)
  runGadget(ui, server, viewer = viewer)
}

#' Code Snippet Generator
#'
#' Generate and insert code snippets
#' @export
goose_addin_snippet <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This addin requires RStudio or Positron IDE")
  }
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Generate Code Snippet",
      right = miniUI::miniTitleBarButton("done", "Insert", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      selectInput("snippet_type", "Snippet Type:",
        choices = list(
          "Function" = "function",
          "Data Analysis" = "analysis",
          "Visualization" = "plot",
          "Data Cleaning" = "cleaning",
          "Model" = "model",
          "Test" = "test",
          "Documentation" = "docs",
          "Custom" = "custom"
        )
      ),
      
      conditionalPanel(
        condition = "input.snippet_type == 'function'",
        textInput("func_name", "Function Name:"),
        textInput("func_params", "Parameters (comma-separated):"),
        textAreaInput("func_desc", "Description:", rows = 2)
      ),
      
      conditionalPanel(
        condition = "input.snippet_type == 'analysis'",
        textInput("data_name", "Dataset Name:"),
        checkboxGroupInput("analysis_types", "Analysis Types:",
          choices = list(
            "Summary Statistics" = "summary",
            "Correlation" = "correlation",
            "Distribution" = "distribution",
            "Missing Values" = "missing",
            "Outliers" = "outliers"
          ))
      ),
      
      conditionalPanel(
        condition = "input.snippet_type == 'plot'",
        selectInput("plot_type", "Plot Type:",
          choices = list(
            "Scatter" = "scatter",
            "Line" = "line",
            "Bar" = "bar",
            "Histogram" = "histogram",
            "Box" = "box",
            "Heatmap" = "heatmap"
          )),
        textInput("plot_data", "Data:"),
        textInput("plot_x", "X Variable:"),
        textInput("plot_y", "Y Variable:")
      ),
      
      conditionalPanel(
        condition = "input.snippet_type == 'custom'",
        textAreaInput("custom_prompt", "Describe what you need:",
          rows = 4,
          placeholder = "E.g., 'Create a function to read multiple CSV files and combine them'")
      ),
      
      hr(),
      
      h4("Generated Code:"),
      verbatimTextOutput("generated_code"),
      
      actionButton("generate_btn", "Generate", 
        icon = icon("magic"),
        class = "btn-primary")
    )
  )
  
  server <- function(input, output, session) {
    generated <- reactiveVal("")
    
    # Generate code based on inputs
    observeEvent(input$generate_btn, {
      prompt <- switch(input$snippet_type,
        "function" = {
          sprintf("Generate an R function named '%s' with parameters (%s). %s",
                  input$func_name, input$func_params, input$func_desc)
        },
        "analysis" = {
          analyses <- paste(input$analysis_types, collapse = ", ")
          sprintf("Generate R code to perform %s on dataset '%s'",
                  analyses, input$data_name)
        },
        "plot" = {
          sprintf("Generate R ggplot2 code for a %s plot using data '%s' with x='%s' and y='%s'",
                  input$plot_type, input$plot_data, input$plot_x, input$plot_y)
        },
        "custom" = input$custom_prompt,
        "Generate R code"
      )
      
      withProgress(message = "Generating code...", {
        code <- tryCatch({
          goose_ask(prompt)
        }, error = function(e) {
          paste("# Error generating code:", e$message)
        })
      })
      
      generated(code)
    })
    
    output$generated_code <- renderText({
      generated()
    })
    
    # Insert code when Done is clicked
    observeEvent(input$done, {
      if (nchar(generated()) > 0) {
        rstudioapi::insertText(generated())
      }
      stopApp()
    })
  }
  
  viewer <- dialogViewer("Code Snippet Generator", width = 500, height = 600)
  runGadget(ui, server, viewer = viewer)
}

#' Code Review Interface
#'
#' Review selected code with AI
#' @export
goose_addin_review <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This addin requires RStudio or Positron IDE")
  }
  
  # Get selected text
  context <- rstudioapi::getActiveDocumentContext()
  selected_code <- context$selection[[1]]$text
  
  if (nchar(selected_code) == 0) {
    stop("Please select some code to review")
  }
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Code Review",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      h4("Selected Code:"),
      verbatimTextOutput("selected_code"),
      
      hr(),
      
      selectInput("review_focus", "Review Focus:",
        choices = list(
          "General Review" = "general",
          "Performance" = "performance",
          "Best Practices" = "practices",
          "Security" = "security",
          "Documentation" = "documentation",
          "Testing" = "testing",
          "Refactoring" = "refactoring"
        ),
        selected = "general"
      ),
      
      actionButton("review_btn", "Start Review",
        icon = icon("search"),
        class = "btn-primary"),
      
      hr(),
      
      h4("Review Results:"),
      uiOutput("review_results")
    )
  )
  
  server <- function(input, output, session) {
    output$selected_code <- renderText({
      selected_code
    })
    
    observeEvent(input$review_btn, {
      focus_prompt <- switch(input$review_focus,
        "performance" = "Focus on performance optimizations",
        "practices" = "Focus on R best practices and style",
        "security" = "Focus on security concerns",
        "documentation" = "Focus on documentation and comments",
        "testing" = "Suggest test cases",
        "refactoring" = "Suggest refactoring improvements",
        ""
      )
      
      prompt <- sprintf(
        "Review this R code. %s\n\n```r\n%s\n```",
        focus_prompt, selected_code
      )
      
      withProgress(message = "Reviewing code...", {
        review <- tryCatch({
          goose_review_code(selected_code, focus = input$review_focus)
        }, error = function(e) {
          # Fallback to regular ask
          goose_ask(prompt)
        })
      })
      
      output$review_results <- renderUI({
        HTML(if (requireNamespace("markdown", quietly = TRUE)) markdown::markdownToHTML(text = review, fragment.only = TRUE) else review)
      })
    })
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  viewer <- dialogViewer("Code Review", width = 700, height = 600)
  runGadget(ui, server, viewer = viewer)
}

#' Template Builder UI
#'
#' Visual interface for creating prompt templates
#' @export
goose_addin_template <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This addin requires RStudio or Positron IDE")
  }
  
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Template Builder",
      left = miniUI::miniTitleBarButton("load", "Load"),
      right = miniUI::miniTitleBarButton("save", "Save", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      textInput("template_name", "Template Name:",
        placeholder = "e.g., code_review"),
      
      textAreaInput("template_text", "Template:",
        rows = 8,
        placeholder = "Use {variable} for placeholders"),
      
      textAreaInput("template_desc", "Description:",
        rows = 2),
      
      hr(),
      
      h4("Variables:"),
      div(id = "variables_list"),
      
      actionButton("add_var", "Add Variable", icon = icon("plus")),
      
      hr(),
      
      h4("Preview:"),
      verbatimTextOutput("template_preview"),
      
      actionButton("test_btn", "Test Template", 
        icon = icon("play"),
        class = "btn-info")
    )
  )
  
  server <- function(input, output, session) {
    variables <- reactiveValues(vars = list())
    
    # Extract variables from template
    observe({
      text <- input$template_text
      var_pattern <- "\\{([^}]+)\\}"
      matches <- gregexpr(var_pattern, text, perl = TRUE)
      var_names <- unique(regmatches(text, matches)[[1]])
      var_names <- gsub("[{}]", "", var_names)
      
      # Update variables list
      for (var in var_names) {
        if (!(var %in% names(variables$vars))) {
          variables$vars[[var]] <- ""
        }
      }
      
      # Remove variables not in template
      current_vars <- names(variables$vars)
      for (var in current_vars) {
        if (!(var %in% var_names)) {
          variables$vars[[var]] <- NULL
        }
      }
    })
    
    # Render variables UI
    observe({
      removeUI(selector = "#variables_list > div", multiple = TRUE)
      
      for (var in names(variables$vars)) {
        var_id <- paste0("var_", var)
        insertUI(
          selector = "#variables_list",
          where = "beforeEnd",
          ui = div(
            textInput(var_id, paste0(var, ":"),
              placeholder = paste("Value for", var))
          )
        )
      }
    })
    
    # Preview template
    output$template_preview <- renderText({
      text <- input$template_text
      for (var in names(variables$vars)) {
        var_id <- paste0("var_", var)
        value <- input[[var_id]]
        if (!is.null(value) && nchar(value) > 0) {
          text <- gsub(paste0("\\{", var, "\\}"), value, text)
        }
      }
      text
    })
    
    # Test template
    observeEvent(input$test_btn, {
      if (nchar(input$template_name) == 0) {
        showNotification("Please provide a template name", type = "error")
        return()
      }
      
      # Create template object
      template <- goose_template(
        name = input$template_name,
        template = input$template_text,
        description = input$template_desc,
        variables = variables$vars
      )
      
      # Test with current values
      test_vars <- list()
      for (var in names(variables$vars)) {
        var_id <- paste0("var_", var)
        test_vars[[var]] <- input[[var_id]]
      }
      
      withProgress(message = "Testing template...", {
        result <- tryCatch({
          do.call(goose_template_apply, 
                 c(list(template = template), test_vars, execute = FALSE))
        }, error = function(e) {
          paste("Error:", e$message)
        })
      })
      
      showNotification(paste("Template test successful!"), type = "success")
    })
    
    # Save template
    observeEvent(input$save, {
      if (nchar(input$template_name) == 0) {
        showNotification("Please provide a template name", type = "error")
        return()
      }
      
      template <- goose_template(
        name = input$template_name,
        template = input$template_text,
        description = input$template_desc,
        variables = variables$vars
      )
      
      tryCatch({
        goose_template_save(template, overwrite = TRUE)
        showNotification("Template saved!", type = "success")
        stopApp()
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
      })
    })
    
    # Load template
    observeEvent(input$load, {
      # Show template selection dialog
      templates <- goose_template_list()
      if (nrow(templates) > 0) {
        showModal(modalDialog(
          title = "Load Template",
          selectInput("load_template", "Select Template:",
            choices = setNames(templates$name, 
                              paste(templates$name, "-", templates$description))),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("do_load", "Load", class = "btn-primary")
          )
        ))
      } else {
        showNotification("No templates available", type = "info")
      }
    })
    
    observeEvent(input$do_load, {
      template <- goose_template_load(input$load_template)
      updateTextInput(session, "template_name", value = template$name)
      updateTextAreaInput(session, "template_text", value = template$template)
      updateTextAreaInput(session, "template_desc", value = template$description %||% "")
      removeModal()
    })
  }
  
  viewer <- dialogViewer("Template Builder", width = 600, height = 700)
  runGadget(ui, server, viewer = viewer)
}

#' Quick Ask
#'
#' Quick question to Goose (non-interactive)
#' @export
goose_addin_quick <- function() {
  if (!rstudioapi::isAvailable()) {
    stop("This addin requires RStudio or Positron IDE")
  }
  
  # Get selected text if any
  context <- rstudioapi::getActiveDocumentContext()
  selected <- context$selection[[1]]$text
  
  # Prompt for question
  question <- if (nchar(selected) > 0) {
    rstudioapi::showPrompt("Quick Question", 
      "Ask about the selected code:",
      default = "Explain this code")
  } else {
    rstudioapi::showPrompt("Quick Question", 
      "What would you like to know?")
  }
  
  if (!is.null(question)) {
    # Build full prompt
    prompt <- if (nchar(selected) > 0) {
      sprintf("%s\n\n```r\n%s\n```", question, selected)
    } else {
      question
    }
    
    # Get response
    response <- goose_ask(prompt)
    
    # Show in viewer
    temp_html <- tempfile(fileext = ".html")
    html_content <- sprintf("
      <html>
      <head>
        <style>
          body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif; 
                 padding: 20px; line-height: 1.6; }
          pre { background: #f4f4f4; padding: 10px; border-radius: 5px; overflow-x: auto; }
          code { background: #f4f4f4; padding: 2px 4px; border-radius: 3px; }
        </style>
      </head>
      <body>
        <h2>Goose Response</h2>
        %s
      </body>
      </html>
    ", (if (requireNamespace("markdown", quietly = TRUE)) markdown::markdownToHTML(text = response, fragment.only = TRUE) else response))
    
    writeLines(html_content, temp_html)
    rstudioapi::viewer(temp_html)
  }
}
