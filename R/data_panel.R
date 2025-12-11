################################################################################
# Data Panel Module - Keep All Columns Except Unselected Traits
################################################################################

data_panel_ui <- function(id) {
  ns <- NS(id)

  tagList(
    layout_columns(
      col_widths = c(4, 8),

      # Left panel - Filters
      card(
        card_header("Data Selection & Filters"),
        card_body(
          h5("Select Data File"),

          selectInput(
            ns("data_select"),
            "Choose Dataset:",
            choices = NULL,
            width = "100%"
          ),

          actionButton(
            ns("refresh_files"),
            "Refresh File List",
            icon = icon("refresh"),
            class = "btn-secondary",
            width = "100%"
          ),

          actionButton(
            ns("load_data"),
            "Load Selected Data",
            icon = icon("upload"),
            class = "btn-primary",
            width = "100%"
          ),

          hr(),

          # Dynamic filter section
          uiOutput(ns("dynamic_filters_ui")),

          hr(),

          # Dynamic trait selection
          uiOutput(ns("dynamic_traits_ui")),

          hr(),

          actionButton(
            ns("filter_action"),
            "Apply Filters",
            icon = icon("filter"),
            class = "btn-primary",
            width = "100%"
          ),

          br(), br(),

          downloadButton(
            ns("download_data"),
            "Download data",
            class = "btn-success",
            width = "100%"
          )
        )
      ),

      # Right panel - Data display
      card(
        card_header("Data Preview"),
        card_body(
          verbatimTextOutput(ns("data_summary")),
          hr(),
          DTOutput(ns("data_table")) %>%
            withSpinner(color = "#005F5F")
        )
      )
    )
  )
}

data_panel_server <- function(id) {
  moduleServer(id, function(input, output, session) {

    # Reactive values
    rv <- reactiveValues(
      data = NULL,
      filtered_data = NULL,
      available_files = NULL,
      current_config = NULL,
      filter_columns = NULL,
      trait_columns = NULL,
      spectral_columns = NULL
    )

    # Function to get available CSV files
    get_available_files <- function() {
      data_dir <- here::here("data")

      if (!dir.exists(data_dir)) {
        dir.create(data_dir, recursive = TRUE)
        return(NULL)
      }

      csv_files <- list.files(
        data_dir,
        pattern = "\\.csv$",
        full.names = FALSE,
        ignore.case = TRUE
      )

      if (length(csv_files) == 0) {
        return(NULL)
      }

      return(csv_files)
    }

    # Initialize file list on startup
    observe({
      files <- get_available_files()

      if (!is.null(files) && length(files) > 0) {
        updateSelectInput(
          session,
          "data_select",
          choices = c("Select a file..." = "", files),
          selected = ""
        )
        rv$available_files <- files
      } else {
        updateSelectInput(
          session,
          "data_select",
          choices = c("No CSV files found in data/ directory" = ""),
          selected = ""
        )
        showNotification(
          "No CSV files found in data/ directory. Please add CSV files to SpecTraits/data/",
          type = "warning",
          duration = 5
        )
      }
    })

    # Refresh file list
    observeEvent(input$refresh_files, {
      files <- get_available_files()

      if (!is.null(files) && length(files) > 0) {
        updateSelectInput(
          session,
          "data_select",
          choices = c("Select a file..." = "", files),
          selected = ""
        )
        rv$available_files <- files
        showNotification(
          paste("Found", length(files), "CSV file(s)"),
          type = "message"
        )
      } else {
        updateSelectInput(
          session,
          "data_select",
          choices = c("No CSV files found in data/ directory" = ""),
          selected = ""
        )
        showNotification(
          "No CSV files found in data/ directory",
          type = "warning"
        )
      }
    })

    # Load selected data
    observeEvent(input$load_data, {
      req(input$data_select)

      if (input$data_select == "") {
        showNotification("Please select a file first", type = "warning")
        return()
      }

      tryCatch({
        data_path <- here::here("data", input$data_select)

        if (!file.exists(data_path)) {
          showNotification(
            "File not found. Please refresh the file list.",
            type = "error"
          )
          return()
        }

        rv$data <- fread(data_path)

        # Get configuration for this file
        config <- get_data_config(input$data_select)

        if (!is.null(config)) {
          # Use predefined configuration
          rv$current_config <- config

          # Filter columns are those used for UI filtering
          rv$filter_columns <- config$filter_columns[
            config$filter_columns %in% names(rv$data)
          ]

          # Trait columns are those that can be selected/deselected
          rv$trait_columns <- config$traits[
            config$traits %in% names(rv$data)
          ]

          # Spectral columns
          rv$spectral_columns <- get_spectral_columns(rv$data, config)

          showNotification(
            paste("Loaded:", config$display_name),
            type = "message",
            duration = 3
          )
        } else {
          # Auto-detect columns
          detected <- auto_detect_columns(rv$data)
          rv$filter_columns <- detected$filter_columns
          rv$trait_columns <- detected$traits
          rv$spectral_columns <- detected$spectral
          rv$current_config <- list(
            filter_columns = detected$filter_columns,
            traits = detected$traits,
            spectral_pattern = detected$spectral_pattern,
            display_name = input$data_select,
            citation = NULL
          )

          showNotification(
            paste("Loaded:", input$data_select, "(auto-detected columns)"),
            type = "message",
            duration = 3
          )
        }

        # Initialize filtered data
        rv$filtered_data <- rv$data

      }, error = function(e) {
        showNotification(
          paste("Error loading data:", e$message),
          type = "error"
        )
      })
    })

    # Dynamic filter UI
    output$dynamic_filters_ui <- renderUI({
      req(rv$data, rv$filter_columns)

      ns <- session$ns

      filter_inputs <- lapply(rv$filter_columns, function(col) {

        # Get unique values for this column
        unique_vals <- unique(rv$data[[col]])
        unique_vals <- unique_vals[!is.na(unique_vals)]

        # Create a nice label
        label <- gsub("_", " ", col)
        label <- tools::toTitleCase(label)

        selectInput(
          ns(paste0("filter_", col)),
          paste0(label, ":"),
          choices = c("All" = "", sort(unique_vals)),
          multiple = TRUE,
          selected = NULL
        )
      })

      tagList(
        h5("Filter by Metadata"),
        filter_inputs
      )
    })

    # ========================================================================
    # UPDATED SECTION: Dynamic traits UI with citation notice
    # ========================================================================
    output$dynamic_traits_ui <- renderUI({
      req(rv$data, rv$trait_columns, rv$current_config)

      ns <- session$ns

      if (length(rv$trait_columns) == 0) {
        return(
          div(
            h5("Select Traits"),
            p("No trait columns detected in this dataset.",
              style = "color: #888;")
          )
        )
      }

      # Check if citation exists
      citation_note <- if (!is.null(rv$current_config$citation)) {
        p(
          icon("file-text"),
          " Download will include citation information.",
          style = "color: #2E8B57; font-size: 0.9em; margin-top: 5px;"
        )
      } else {
        NULL
      }

      tagList(
        h5("Select Traits to Include"),
        p(
          icon("info-circle"),
          " Only unselected traits will be removed. All other columns will be kept.",
          style = "color: #666; font-size: 0.9em; margin-bottom: 10px;"
        ),
        checkboxGroupInput(
          ns("traits_input"),
          NULL,
          choices = rv$trait_columns,
          selected = rv$trait_columns
        ),
        citation_note
      )
    })
    # ========================================================================
    # END OF UPDATED SECTION
    # ========================================================================

    # Filter data
    observeEvent(input$filter_action, {
      req(rv$data)

      filtered <- rv$data

      # Apply metadata filters dynamically (filter rows)
      if (!is.null(rv$filter_columns) && length(rv$filter_columns) > 0) {
        for (col in rv$filter_columns) {
          filter_value <- input[[paste0("filter_", col)]]

          if (!is.null(filter_value) && length(filter_value) > 0 && filter_value != "") {
            filtered <- filtered[get(col) %in% filter_value]
          }
        }
      }

      # Determine which columns to remove (only unselected traits)
      if (!is.null(rv$trait_columns) && length(rv$trait_columns) > 0) {
        # Get selected traits (traits to KEEP)
        selected_traits <- input$traits_input
        if (is.null(selected_traits)) {
          selected_traits <- character(0)
        }

        # Get unselected traits (traits to REMOVE)
        unselected_traits <- setdiff(rv$trait_columns, selected_traits)

        # Remove only unselected traits
        if (length(unselected_traits) > 0) {
          cols_to_keep <- setdiff(names(filtered), unselected_traits)
          filtered <- filtered[, ..cols_to_keep]
        }
      }

      rv$filtered_data <- filtered

      # Count what we're keeping
      filtered_cols <- names(rv$filtered_data)
      n_spectral <- sum(rv$spectral_columns %in% filtered_cols)
      n_traits_kept <- sum(rv$trait_columns %in% filtered_cols)
      n_traits_removed <- sum(!rv$trait_columns %in% filtered_cols)

      showNotification(
        paste0(
          "Filtered to ", nrow(rv$filtered_data), " rows with ",
          ncol(rv$filtered_data), " columns\n",
          "(", n_traits_kept, " traits kept, ",
          n_traits_removed, " traits removed)"
        ),
        type = "message",
        duration = 5
      )
    })

    # Data summary
    output$data_summary <- renderText({
      req(rv$filtered_data, rv$current_config)

      filtered_cols <- names(rv$filtered_data)

      # Count column types
      n_filter_cols <- sum(rv$filter_columns %in% filtered_cols)
      n_traits <- sum(rv$trait_columns %in% filtered_cols)
      n_spectral <- sum(rv$spectral_columns %in% filtered_cols)

      # Count other columns (not filter cols, not traits, not spectral)
      other_cols <- setdiff(
        filtered_cols,
        c(rv$filter_columns, rv$trait_columns, rv$spectral_columns)
      )
      n_other <- length(other_cols)

      summary_text <- paste0(
        "Dataset: ", rv$current_config$display_name, "\n",
        "Rows: ", nrow(rv$filtered_data), "\n",
        "Total Columns: ", ncol(rv$filtered_data), "\n",
        "  - Filter columns: ", n_filter_cols, "\n",
        "  - Trait columns: ", n_traits, "\n",
        "  - Spectral columns: ", n_spectral, "\n",
        "  - Other columns: ", n_other, "\n\n"
      )

      # Add unique counts for key filter columns
      for (col in rv$filter_columns) {
        if (col %in% filtered_cols) {
          label <- gsub("_", " ", col)
          label <- tools::toTitleCase(label)
          unique_count <- length(unique(rv$filtered_data[[col]]))
          summary_text <- paste0(
            summary_text,
            label, ": ", unique_count, "\n"
          )
        }
      }

      return(summary_text)
    })

    # Data table (show preview without all spectral columns)
    output$data_table <- renderDT({
      req(rv$filtered_data)

      # For display, limit spectral columns to every 10th wavelength
      display_data <- rv$filtered_data

      spectral_in_filtered <- intersect(rv$spectral_columns, names(display_data))

      if (length(spectral_in_filtered) > 50) {
        # Show only sample of spectral columns for preview
        spectral_sample <- spectral_in_filtered[seq(1, length(spectral_in_filtered), by = 10)]
        non_spectral <- setdiff(names(display_data), rv$spectral_columns)
        display_cols <- c(non_spectral, spectral_sample)
        display_data <- display_data[, ..display_cols]
      }

      datatable(
        display_data,
        options = list(
          scrollX = TRUE,
          scrollY = "400px",
          pageLength = 10,
          dom = 'Bfrtip'
        ),
        rownames = FALSE,
        caption = if(length(spectral_in_filtered) > 50) {
          "Note: Showing sampled spectral columns for preview. All columns will be included in download."
        } else {
          NULL
        }
      )
    })

    # ========================================================================
    # UPDATED SECTION: Download handler with citation support
    # ========================================================================
    output$download_data <- downloadHandler(
      filename = function() {
        req(input$data_select, rv$current_config)
        base_name <- tools::file_path_sans_ext(input$data_select)

        # If citation exists, download as zip, otherwise as csv
        if (!is.null(rv$current_config$citation)) {
          paste0(base_name, "_filtered_", Sys.Date(), ".zip")
        } else {
          paste0(base_name, "_filtered_", Sys.Date(), ".csv")
        }
      },
      content = function(file) {
        req(rv$filtered_data, rv$current_config)

        base_name <- tools::file_path_sans_ext(input$data_select)

        # Check if citation exists
        if (!is.null(rv$current_config$citation)) {
          # Create temporary directory
          temp_dir <- tempdir()

          # Save CSV file
          csv_filename <- paste0(base_name, "_", Sys.Date(), ".csv")
          csv_path <- file.path(temp_dir, csv_filename)
          fwrite(rv$filtered_data, csv_path)

          # Save citation file
          citation_filename <- "README.txt"
          citation_path <- file.path(temp_dir, citation_filename)

          # Format citation nicely
          citation_text <- paste0(
            rv$current_config$citation, "\n\n"
          )

          writeLines(citation_text, citation_path)

          # Create README file
          #readme_filename <- "README.txt"
          #readme_path <- file.path(temp_dir, readme_filename)

          # readme_text <- paste0(
          #   "SpecTraits\n",
          #   "=================================\n\n",
          #   "Dataset: ", rv$current_config$display_name, "\n",
          #   "Download date: ", Sys.Date(), "\n\n",
          #   "Contents:\n",
          #   "- ", csv_filename, " : Filtered data\n",
          #   "- ", citation_filename, " : Data citation and attribution\n",
          #   "- ", readme_filename, " : This file\n\n",
          #   "Filter Summary:\n",
          #   "- Original rows: ", nrow(rv$data), "\n",
          #   "- Filtered rows: ", nrow(rv$filtered_data), "\n",
          #   "- Total columns: ", ncol(rv$filtered_data), "\n\n",
          #   "Please cite the original data source when using this data.\n"
          # )
          #
          # writeLines(readme_text, readme_path)

          # Create zip file
          zip::zip(
            zipfile = file,
            files = c(csv_filename, citation_filename),
            root = temp_dir
          )

          # Clean up temporary files
          file.remove(csv_path)
          file.remove(citation_path)
          # file.remove(readme_path)

        } else {
          # No citation, just download CSV
          fwrite(rv$filtered_data, file)
        }
      }
    )
    # ========================================================================
    # END OF UPDATED SECTION
    # ========================================================================

  })
}
