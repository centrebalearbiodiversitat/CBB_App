#--------------#
# Taxonomy v.2 #
#--------------#

# Reactive values to store CSV input and taxonomy output
temp_df <- reactiveValues(df_data = NULL) # Uploaded CSV
temp_df_2 <- reactiveValues(df_data = NULL) # Reviewed taxonomy
temp_df_3 <- reactiveValues(df_data = NULL) # Table containing also ambiguous values
checklist_bank <- reactiveValues(n = NULL) # COL ChecklistBank ID


# Readtive value for COL ChecklistBank ID ----
observe({
  req(input$dataset_number)
  num <- suppressWarnings(as.numeric(input$dataset_number))
  if (!is.na(num)) {
    checklist_bank$n <- num
  }
})


# Option to select the column name to perform the analysis ----
observe({
  # Don't run unless a data have been imported
  req(temp_df$df_data)
  colmn.names <- colnames(temp_df$df_data)

  # "text.db" is the column name object
  updateSelectInput(session = session, "text.db", choices = colmn.names)
})


# ReactiveValues for storing resolved and ambiguous taxa ---
rv <- reactiveValues(
  resolved_df = NULL,
  ambiguous_list = list()
)


# Show table with uploaded CSV ----
output$inputDataframe <- DT::renderDataTable(
  {
    req(input$file1)
    temp_df$df_data <- fread(input$file1$datapath, sep = ",") %>%
      as.data.frame()
    temp_df$df_data
  },
  options = list(scrollX = TRUE, paging = FALSE),
  rownames = FALSE
)

output$uiTaxonomy <- renderUI({
  req(input$file1)
  card(
    card_header("Taxonomy"),
    full_screen = TRUE, fill = FALSE,
    DT::dataTableOutput("inputDataframe")
  )
})


# Retrive information from COL ----
observeEvent(input$taxa.run.button, {
  req(temp_df$df_data) # CSV must be uploaded

  # Check if dataset number is entered
  if (is.null(checklist_bank$n) || checklist_bank$n == "") {
    showNotification(
      "Please enter a valid COL dataset number before proceeding.",
      type = "error"
    )
    return() # stop execution
  }


  if (input$text.db %in% colnames(temp_df$df_data)) {
    spTaxa <- unique(stringr::str_squish(temp_df$df_data[, input$text.db]))

    # COL query
    cbb_result <- col_by_name(spTaxa, dataset_number = checklist_bank$n)

    rv$resolved_df <- cbb_result$resolved
    rv$ambiguous_list <- cbb_result$ambiguous

    # Always show the resolved table
    temp_df_2$df_data <- rv$resolved_df

    # Handle ambiguous taxa
    output$choose_ids_ui <- renderUI({
      if (is.null(rv$ambiguous_list) || length(rv$ambiguous_list) == 0) {
        return(
          tags$div(
            style = "padding:10px;",
            "No ambiguous taxa detected."
          )
        )
      }

      tagList(
        tags$div(
          style = "overflow-y: auto; border: 1px solid #ddd;
                  padding: 12px; border-radius: 8px;
                  background-color: #fafafa; max-height: 400px;",
          lapply(seq_along(rv$ambiguous_list), function(i) {
            taxon_name <- names(rv$ambiguous_list)[i]
            ids <- rv$ambiguous_list[[i]]

            tagList(
              tags$h5(style = "margin-top: 10px;", taxon_name),
              radioButtons(
                inputId = paste0("id_select_", i),
                label = NULL,
                choiceNames = lapply(ids, function(id) {
                  HTML(paste0(
                    "<a href='https://www.catalogueoflife.org/data/taxon/", id,
                    "' target='_blank'>", id, "</a>"
                  ))
                }),
                choiceValues = ids,
                selected = character(0)
              )
            )
          })
        ),
        actionButton(
          "confirm_ids",
          "Confirm Selected IDs",
          style = "width: 190px; height: 35px;
                  font-size: 90%; font-weight: bold;
                  margin-top:10px;"
        )
      )
    })

    # Show download button
    output$downloadButton <- renderUI({
      downloadButton("downloadData", "Download Dataset",
        style = "display: flex; align-items: center; justify-content: center;
                                  padding:6px; font-weight: bold; font-size:100%; height:63px;"
      )
    })
  } else {
    showNotification("No taxa column was found.")
  }
})


# Process user-selected ambiguous IDs. This is only for COL database ----
observeEvent(input$confirm_ids, {
  req(rv$ambiguous_list)

  results <- lapply(seq_along(rv$ambiguous_list), function(i) {
    selected_id <- input[[paste0("id_select_", i)]]

    if (is.null(selected_id) || selected_id == "") {
      return(NULL)
    }

    col_by_id(selected_id, dataset_number = checklist_bank$n)
  })

  # Remove NULL entries
  results <- Filter(Negate(is.null), results)

  if (length(results) > 0) {
    new_rows <- do.call(rbind, results)

    temp_df_3$df_data <- rbind(rv$resolved_df, new_rows)
  }

  # Make ambiguous UI disappear
  rv$ambiguous_list <- NULL
})


# Table whith and/or without ambiguities resolved ----
output$uiRevTaxonomy <- renderUI({
  req(temp_df_2$df_data)
  card(
    card_header("Reviewed taxonomy"),
    full_screen = TRUE, fill = FALSE,
    DT::dataTableOutput("dataTaxonomy")
  )
})

output$dataTaxonomy <- DT::renderDataTable(
  {
    if (!is.null(temp_df_3$df_data) && nrow(temp_df_3$df_data) > 0) {
      temp_df_3$df_data
    } else {
      req(temp_df_2$df_data)
      temp_df_2$df_data
    }
  },
  options = list(scrollX = TRUE, paging = FALSE),
  rownames = FALSE
)


# Compare row counts ----
observe({
  # Determine which final dataset to compare
  target_df <- if (is.null(temp_df_3$df_data)) {
    temp_df_2$df_data
  } else {
    temp_df_3$df_data
  }

  # Make sure both datasets exist
  req(temp_df$df_data, target_df)

  # Compare number of rows
  if (nrow(temp_df$df_data) != nrow(target_df)) {
    showNotification(
      "The initial and final datasets DO NOT HAVE the same number of rows.",
      type = "warning", duration = 5
    )
  }
})


# Download reviewed taxonomy ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste0("CBB_Taxa_dataset_", Sys.Date(), ".csv")
  },
  content = function(file) {
    # Determine which final dataset to download
    target_df <- if (is.null(temp_df_3$df_data)) {
      temp_df_2$df_data
    } else {
      temp_df_3$df_data
    }

    # Ensure it exists
    req(target_df)

    write.csv(target_df, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
