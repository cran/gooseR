#' Visual UI Components for GooseR
#' 
#' Shiny-based visual interfaces for cache and conversation management
#' @name goose_ui_components
#' @import shiny
#' @import miniUI
#' @import graphics
#' @importFrom shiny reactive reactiveVal reactiveValues observe observeEvent req
#' @importFrom shiny insertUI removeUI stopApp renderPrint verbatimTextOutput
#' @importFrom shiny hr icon uiOutput tagList modalButton modalDialog titlePanel
#' @importFrom shiny updateTextInput tabPanel tabsetPanel numericInput
#' @importFrom shiny shinyApp fluidPage fluidRow column
#' @importFrom shiny dataTableOutput renderDataTable
#' @importFrom shiny plotOutput renderPlot
#' @importFrom shiny downloadHandler downloadButton
#' @importFrom shiny dialogViewer runGadget
#' @importFrom graphics par hist
#' @importFrom grDevices col2rgb rgb
#' @importFrom utils capture.output object.size str
NULL

#' Cache Browser UI
#'
#' Visual interface for browsing and managing cache
#' @export
goose_cache_ui <- function() {
  if (!rstudioapi::isAvailable()) {
    # Run as standalone Shiny app if not in RStudio
    run_cache_app()
  } else {
    # Run as RStudio gadget
    run_cache_gadget()
  }
}

# Internal function for gadget version
run_cache_gadget <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("GooseR Cache Browser",
      left = miniUI::miniTitleBarButton("refresh", "Refresh"),
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      tabsetPanel(
        tabPanel("Browse",
          br(),
          (if (requireNamespace("DT", quietly = TRUE)) DT::dataTableOutput("cache_table") else tableOutput("cache_table_basic")), 
          br(),
          fluidRow(
            column(4,
              actionButton("view_btn", "View Selected", 
                icon = icon("eye"),
                class = "btn-info")
            ),
            column(4,
              actionButton("delete_btn", "Delete Selected",
                icon = icon("trash"),
                class = "btn-danger")
            ),
            column(4,
              downloadButton("export_btn", "Export Cache",
                class = "btn-success")
            )
          )
        ),
        
        tabPanel("Statistics",
          br(),
          verbatimTextOutput("cache_stats"),
          br(),
          plotOutput("cache_plot", height = "300px")
        ),
        
        tabPanel("Settings",
          br(),
          h4("Cache Management"),
          numericInput("max_age", "Max Cache Age (days):",
            value = 30, min = 1, max = 365),
          numericInput("max_size", "Max Cache Size (MB):",
            value = 100, min = 10, max = 1000),
          hr(),
          actionButton("clear_old", "Clear Old Entries",
            icon = icon("broom"),
            class = "btn-warning"),
          br(), br(),
          actionButton("clear_all", "Clear All Cache",
            icon = icon("trash-alt"),
            class = "btn-danger")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Initialize cache connection
    cache_conn <- goose_cache_init()
    onStop(function() DBI::dbDisconnect(cache_conn))
    
    # Reactive cache data
    cache_data <- reactiveVal()
    
    # Load cache data
    load_cache <- function() {
      data <- DBI::dbGetQuery(cache_conn, "
        SELECT hash, query, 
               LENGTH(response) as response_size,
               model, timestamp, access_count, last_accessed
        FROM cache
        ORDER BY last_accessed DESC
      ")
      cache_data(data)
    }
    
    # Initial load
    load_cache()
    
    # Refresh button
    observeEvent(input$refresh, {
      load_cache()
      showNotification("Cache refreshed", type = "info", duration = 2)
    })
    
    # Render cache table
    output$cache_table <- DT::renderDataTable({
      req(cache_data())
      DT::datatable(
        cache_data(),
        selection = "single",
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          columnDefs = list(
            list(visible = FALSE, targets = 0),  # Hide hash
            list(width = "40%", targets = 1),     # Query column
            list(width = "10%", targets = 2)      # Size column
          )
        )
      ) %>%
        DT::formatDate(c("timestamp", "last_accessed")) %>%
        DT::formatStyle("access_count",
          backgroundColor = DT::styleInterval(c(5, 10), 
            c("white", "lightblue", "lightgreen")))
    })
    
    # View selected entry
    observeEvent(input$view_btn, {
      selected <- input$cache_table_rows_selected
      if (length(selected) > 0) {
        entry <- cache_data()[selected, ]
        response <- DBI::dbGetQuery(cache_conn,
          "SELECT response FROM cache WHERE hash = ?",
          params = list(entry$hash))$response[1]
        
        showModal(modalDialog(
          title = "Cache Entry",
          size = "l",
          h4("Query:"),
          verbatimTextOutput("view_query"),
          h4("Response:"),
          verbatimTextOutput("view_response"),
          footer = modalButton("Close")
        ))
        
        output$view_query <- renderText(entry$query)
        output$view_response <- renderText(response)
      } else {
        showNotification("Please select an entry", type = "warning")
      }
    })
    
    # Delete selected entry
    observeEvent(input$delete_btn, {
      selected <- input$cache_table_rows_selected
      if (length(selected) > 0) {
        entry <- cache_data()[selected, ]
        DBI::dbExecute(cache_conn,
          "DELETE FROM cache WHERE hash = ?",
          params = list(entry$hash))
        load_cache()
        showNotification("Entry deleted", type = "success")
      } else {
        showNotification("Please select an entry", type = "warning")
      }
    })
    
    # Export cache
    output$export_btn <- downloadHandler(
      filename = function() {
        paste0("goose_cache_", format(Sys.Date(), "%Y%m%d"), ".json")
      },
      content = function(file) {
        goose_cache_export(file, format = "json", conn = cache_conn)
      }
    )
    
    # Cache statistics
    output$cache_stats <- renderPrint({
      stats <- goose_cache_stats(conn = cache_conn)
      print(stats)
    })
    
    # Cache visualization
    output$cache_plot <- renderPlot({
      req(cache_data())
      data <- cache_data()
      if (nrow(data) > 0) {
        par(mfrow = c(1, 2))
        
        # Access frequency
        hist(data$access_count, 
             main = "Access Frequency",
             xlab = "Number of Accesses",
             col = "lightblue")
        
        # Cache age distribution
        data$age_days <- as.numeric(difftime(Sys.time(), 
                                             data$timestamp, 
                                             units = "days"))
        hist(data$age_days,
             main = "Cache Age",
             xlab = "Age (days)",
             col = "lightgreen")
      }
    })
    
    # Clear old entries
    observeEvent(input$clear_old, {
      age_seconds <- input$max_age * 86400
      count <- goose_cache_clear(older_than = age_seconds, conn = cache_conn)
      load_cache()
      showNotification(paste("Cleared", count, "old entries"), type = "success")
    })
    
    # Clear all cache
    observeEvent(input$clear_all, {
      showModal(modalDialog(
        title = "Confirm Clear All",
        "Are you sure you want to clear ALL cache entries?",
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_clear", "Clear All", class = "btn-danger")
        )
      ))
    })
    
    observeEvent(input$confirm_clear, {
      count <- goose_cache_clear(all = TRUE, conn = cache_conn)
      load_cache()
      removeModal()
      showNotification(paste("Cleared", count, "entries"), type = "success")
    })
    
    # Done button
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  viewer <- dialogViewer("Cache Browser", width = 800, height = 600)
  runGadget(ui, server, viewer = viewer)
}

#' Conversation Manager UI
#'
#' Manage and replay AI conversation sessions
#' @export
goose_conversation_ui <- function() {
  if (!rstudioapi::isAvailable()) {
    run_conversation_app()
  } else {
    run_conversation_gadget()
  }
}

# Internal function for conversation gadget
run_conversation_gadget <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Conversation Manager",
      right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)
    ),
    miniUI::miniContentPanel(
      fluidRow(
        column(4,
          h4("Sessions"),
          selectInput("session_list", NULL,
            choices = list("Loading..." = ""),
            size = 10,
            selectize = FALSE,
            width = "100%"),
          actionButton("new_session", "New Session",
            icon = icon("plus"),
            width = "100%",
            class = "btn-success"),
          br(), br(),
          actionButton("delete_session", "Delete Session",
            icon = icon("trash"),
            width = "100%",
            class = "btn-danger")
        ),
        column(8,
          h4("Conversation"),
          div(
            style = "height: 400px; overflow-y: auto; border: 1px solid #ddd; padding: 10px;",
            uiOutput("conversation_display")
          ),
          br(),
          fluidRow(
            column(6,
              downloadButton("export_session", "Export",
                class = "btn-info",
                style = "width: 100%;")
            ),
            column(6,
              actionButton("replay_session", "Replay",
                icon = icon("play"),
                class = "btn-primary",
                style = "width: 100%;")
            )
          )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Store sessions
    sessions <- reactiveVal(list())
    current_session <- reactiveVal(NULL)
    
    # Load sessions from memory
    load_sessions <- function() {
      # Get saved sessions from goose memory
      saved <- goose_list()
      session_rows <- saved[grepl("^session_", saved$name), ]
      
      if (nrow(session_rows) > 0) {
        session_list <- list()
        for (i in 1:nrow(session_rows)) {
          session_data <- goose_load(session_rows$name[i])
          session_list[[session_rows$name[i]]] <- session_data
        }
        sessions(session_list)
        
        # Update UI
        updateSelectInput(session, "session_list",
          choices = setNames(
            names(session_list),
            sapply(names(session_list), function(x) {
              paste0(x, " (", length(session_list[[x]]$messages), " messages)")
            })
          )
        )
      } else {
        updateSelectInput(session, "session_list",
          choices = list("No sessions found" = ""))
      }
    }
    
    # Initial load
    load_sessions()
    
    # Display conversation
    observeEvent(input$session_list, {
      if (input$session_list != "" && input$session_list != "No sessions found") {
        session_data <- sessions()[[input$session_list]]
        current_session(session_data)
        
        output$conversation_display <- renderUI({
          if (!is.null(session_data$messages)) {
            tags$div(
              lapply(session_data$messages, function(msg) {
                if (msg$role == "user") {
                  div(
                    class = "alert alert-primary",
                    strong("You: "), msg$content
                  )
                } else {
                  div(
                    class = "alert alert-secondary",
                    strong("Goose: "),
                    HTML(if (requireNamespace("markdown", quietly = TRUE)) markdown::markdownToHTML(
                      text = msg$content,
                      fragment.only = TRUE))
                  )
                }
              })
            )
          } else {
            p("No messages in this session")
          }
        })
      }
    })
    
    # New session
    observeEvent(input$new_session, {
      session_name <- paste0("session_", format(Sys.time(), "%Y%m%d_%H%M%S"))
      new_session <- list(
        id = session_name,
        created = Sys.time(),
        messages = list()
      )
      
      # Save to memory
      goose_save(new_session, session_name, 
                tags = c("conversation", "session"))
      
      # Reload sessions
      load_sessions()
      
      # Select new session
      updateSelectInput(session, "session_list", selected = session_name)
      
      showNotification("New session created", type = "success")
    })
    
    # Delete session
    observeEvent(input$delete_session, {
      if (!is.null(input$session_list) && 
          input$session_list != "" && 
          input$session_list != "No sessions found") {
        
        showModal(modalDialog(
          title = "Confirm Delete",
          paste("Delete session", input$session_list, "?"),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("confirm_delete", "Delete", class = "btn-danger")
          )
        ))
      }
    })
    
    observeEvent(input$confirm_delete, {
      goose_delete(input$session_list)
      removeModal()
      load_sessions()
      showNotification("Session deleted", type = "success")
    })
    
    # Export session
    output$export_session <- downloadHandler(
      filename = function() {
        paste0(input$session_list, ".json")
      },
      content = function(file) {
        session_data <- current_session()
        jsonlite::write_json(session_data, file, pretty = TRUE)
      }
    )
    
    # Replay session
    observeEvent(input$replay_session, {
      session_data <- current_session()
      if (!is.null(session_data$messages)) {
        showModal(modalDialog(
          title = "Replay Session",
          size = "l",
          p("This will replay the conversation in a new session."),
          p("Each message will be sent to Goose again."),
          footer = tagList(
            modalButton("Cancel"),
            actionButton("start_replay", "Start Replay", class = "btn-primary")
          )
        ))
      }
    })
    
    observeEvent(input$start_replay, {
      removeModal()
      session_data <- current_session()
      
      withProgress(message = "Replaying session...", {
        new_messages <- list()
        total <- length(session_data$messages)
        
        for (i in seq_along(session_data$messages)) {
          msg <- session_data$messages[[i]]
          if (msg$role == "user") {
            setProgress(value = i/total, 
                       detail = paste("Message", i, "of", total))
            
            # Get new response
            response <- goose_ask(msg$content)
            
            # Store messages
            new_messages <- append(new_messages, list(msg))
            new_messages <- append(new_messages, list(
              list(role = "assistant", content = response)
            ))
          }
        }
        
        # Save replayed session
        replay_name <- paste0("replay_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        replay_session <- list(
          id = replay_name,
          created = Sys.time(),
          original = input$session_list,
          messages = new_messages
        )
        
        goose_save(replay_session, replay_name,
                  tags = c("conversation", "session", "replay"))
        
        # Reload and select
        load_sessions()
        updateSelectInput(session, "session_list", selected = replay_name)
      })
      
      showNotification("Replay complete!", type = "success")
    })
    
    # Done button
    observeEvent(input$done, {
      stopApp()
    })
  }
  
  viewer <- dialogViewer("Conversation Manager", width = 900, height = 600)
  runGadget(ui, server, viewer = viewer)
}

# Standalone Shiny app versions (for use outside RStudio)
run_cache_app <- function() {
  ui <- fluidPage(
    titlePanel("GooseR Cache Browser"),
    sidebarLayout(
      sidebarPanel(
        h4("Cache Management"),
        actionButton("refresh", "Refresh", icon = icon("sync")),
        hr(),
        downloadButton("export", "Export Cache"),
        hr(),
        actionButton("clear_old", "Clear Old Entries", class = "btn-warning"),
        actionButton("clear_all", "Clear All", class = "btn-danger")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Browse", DT::dataTableOutput("cache_table")),
          tabPanel("Statistics", verbatimTextOutput("stats")),
          tabPanel("Visualization", plotOutput("plot"))
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    # Similar server logic as gadget version
    # ... (implementation details)
  }
  
  shinyApp(ui, server)
}

run_conversation_app <- function() {
  ui <- fluidPage(
    titlePanel("GooseR Conversation Manager"),
    sidebarLayout(
      sidebarPanel(
        h4("Sessions"),
        selectInput("sessions", NULL, choices = list()),
        actionButton("new", "New Session"),
        actionButton("delete", "Delete Session")
      ),
      mainPanel(
        h4("Conversation"),
        uiOutput("conversation"),
        hr(),
        downloadButton("export", "Export"),
        actionButton("replay", "Replay")
      )
    )
  )
  
  server <- function(input, output, session) {
    # Similar server logic as gadget version
    # ... (implementation details)
  }
  
  shinyApp(ui, server)
}
