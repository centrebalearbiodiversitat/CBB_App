#--------------#
# Taxonomy v.2 #
#--------------#

# Reactive values to store CSV input and taxonomy output
temp_df <- reactiveValues(df_data = NULL)      # Uploaded CSV
temp_df.2 <- reactiveValues(df_data = NULL)    # Reviewed taxonomy

# ReactiveValues for storing resolved and ambiguous taxa
rv <- reactiveValues(
  resolved_df = NULL,
  ambiguous_list = list()
)

# Update column selection input based on uploaded CSV
observe({
  req(temp_df$df_data)
  updateSelectInput(session, "text.db", choices = colnames(temp_df$df_data))
})

# Show table with uploaded CSV
output$inputDataframe <- DT::renderDataTable({
  req(input$file1)
  temp_df$df_data <- fread(input$file1$datapath, sep = ",") %>% as.data.frame()
  temp_df$df_data
}, options = list(scrollX = TRUE, paging = FALSE), rownames = FALSE)

output$uiTaxonomy <- renderUI({
  req(input$file1)
  card(
    card_header("Taxonomy"),
    full_screen = TRUE, fill = FALSE,
    DT::dataTableOutput("inputDataframe")
  )
})

# Taxonomy check observeEvent
observeEvent(input$taxa.run.button, {
  req(temp_df$df_data)
  
  if(input$text.db %in% colnames(temp_df$df_data)){
    
    spTaxa <- unique(stringr::str_trim(temp_df$df_data[, input$text.db], side = "both"))
    
    # Dataset number for CBB_DB_COL
    dataset_number <- ifelse(is.null(input$dataset_number) | input$dataset_number == "",
                             312092,
                             as.numeric(input$dataset_number))
    
    # Specify COL
    if(input$taxon.an == "Specify_COL"){
      temp_df.2$df_data <- specifyTaxon(spTaxa)$colNames
      rv$resolved_df <- temp_df.2$df_data
      rv$ambiguous_list <- list()
    }
    
    # CBB_DB_COL
    # CBB_DB_COL
    if(input$taxon.an == "CBB_DB_COL"){
      cbb_result <- cbbdbCol(spTaxa, dataset_number = dataset_number)
      rv$resolved_df <- cbb_result$resolved
      rv$ambiguous_list <- cbb_result$ambiguous
      
      # Always assign resolved_df to temp_df.2 to show table
      temp_df.2$df_data <- rv$resolved_df
      
      # Render ambiguous ID selection only if there are ambiguous taxa
      if(length(rv$ambiguous_list) > 0){
        output$choose_ids_ui <- renderUI({
          tagList(
            lapply(seq_along(rv$ambiguous_list), function(i){
              taxon_name <- names(rv$ambiguous_list)[i]
              selectInput(
                inputId = paste0("id_select_", i),
                label = taxon_name,
                choices = rv$ambiguous_list[[i]]
              )
            }),
            actionButton("confirm_ids", "Confirm Selected IDs",
                         style = "width: 190px; height: 35px; font-size: 90%; font-weight: bold;")
          )
        })
      } else {
        # No ambiguous taxa → remove selection UI
        output$choose_ids_ui <- renderUI({ NULL })
      }
    }
    
    
    # Specify WORMS
    if(input$taxon.an == "Specify_WORMS"){
      temp_df.2$df_data <- specifyWorms(spTaxa)
      rv$resolved_df <- temp_df.2$df_data
      rv$ambiguous_list <- list()
    }
    
    # Render ambiguous ID selection only if ambiguous taxa exist
    output$choose_ids_ui <- renderUI({
      req(input$taxon.an)
      if (input$taxon.an == "CBB_DB_COL" && length(rv$ambiguous_list) > 0) {
        dataset_number <- ifelse(is.null(input$dataset_number) | input$dataset_number == "",
                                 312092,
                                 as.numeric(input$dataset_number))
        
        tagList(
          # Scrollable wrapper for all taxon groups
          tags$div(
            style = "overflow-y: auto; border: 1px solid #ddd; 
                 padding: 12px; border-radius: 8px; background-color: #fafafa;
                 max-height: 400px;",   # you can tweak/remove this
            
            lapply(seq_along(rv$ambiguous_list), function(i) {
              taxon_name <- names(rv$ambiguous_list)[i]
              ids <- rv$ambiguous_list[[i]]
              
              tagList(
                tags$h5(style = "margin-top: 10px;", taxon_name),
                lapply(ids, function(id) {
                  tags$div(
                    style = "margin-bottom: 6px;",
                    tags$input(
                      type = "radio",
                      name = paste0("id_select_", i),
                      value = id,
                      style = "margin-right: 6px;",
                      onchange = sprintf("Shiny.setInputValue('%s','%s')", 
                                         paste0("id_select_", i), id)
                    ),
                    tags$a(
                      href = paste0("https://www.checklistbank.org/dataset/",
                                    dataset_number, "/taxon/", id),
                      target = "_blank",
                      id
                    )
                  )
                })
              )
            })
          ),
          
          # Confirm button
          actionButton("confirm_ids", "Confirm Selected IDs",
                       style = "width: 190px; height: 35px; font-size: 90%; 
                            font-weight: bold; margin-top:10px;")
        )
      }
    })
    
    
    
    
    
    
    # Show download button
    output$downloadButton <- renderUI({
      downloadButton("downloadData", "Download Dataset",
                     style = "display: flex; align-items: center; justify-content: center;
                              padding:6px; font-weight: bold; font-size:100%; height:63px;")
    })
    
  } else {
    showNotification("No taxa column was found.")
  }
})

# Process user-selected ambiguous IDs
observeEvent(input$confirm_ids, {
  req(rv$ambiguous_list)
  dataset_number <- ifelse(is.null(input$dataset_number) | input$dataset_number == "",
                           312092,
                           as.numeric(input$dataset_number))
  
  ambiguous_full <- data.frame()
  
  for(i in seq_along(rv$ambiguous_list)){
    selected_id <- input[[paste0("id_select_", i)]]
    taxon_name <- names(rv$ambiguous_list)[i]
    
    # Lower classification
    classificationLower <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                           dataset_number, "/taxon/", selected_id))
    taxonLower <- ch0_to_Na(classificationLower$name$scientificName)
    authorLower <- ch0_to_Na(classificationLower$name$authorship)
    
    # Higher classification
    classificationHigher <- fromJSON(paste0("https://api.checklistbank.org/dataset/",
                                            dataset_number, "/taxon/", selected_id, "/classification"))
    
    # Helper functions
    getHigher <- function(rank){
      val <- classificationHigher$name[classificationHigher$rank == rank]
      if(length(val) == 0) return(NA) else return(val)
    }
    getAuthor <- function(rank){
      val <- classificationHigher$authorship[classificationHigher$rank == rank]
      if(length(val) == 0) return(NA) else return(val)
    }
    
    # Build row
    row <- data.frame(
      originalName = taxon_name,
      colNamesAccepted = taxonLower,
      colID = selected_id,
      Kingdom = getHigher("kingdom"),
      kingdomAuthor = getAuthor("kingdom"),
      Phylum = getHigher("phylum"),
      phylumAuthor = getAuthor("phylum"),
      Class = getHigher("class"),
      classAuthor = getAuthor("class"),
      Order = getHigher("order"),
      orderAuthor = getAuthor("order"),
      Family = getHigher("family"),
      familyAuthor = getAuthor("family"),
      Genus = getHigher("genus"),
      genusAuthor = getAuthor("genus"),
      Species = word(taxonLower, -1),
      speciesAuthor = authorLower,
      Subspecies = NA,
      subspeciesAuthor = NA,
      Variety = NA,
      varietyAuthor = NA,
      originalStatus = "accepted",
      taxonRank = "species",
      brackish = "brackish" %in% classificationLower$environments,
      freshwater = "freshwater" %in% classificationLower$environments,
      marine = "marine" %in% classificationLower$environments,
      terrestrial = "terrestrial" %in% classificationLower$environments
    )
    
    ambiguous_full <- rbind(ambiguous_full, row)
  }
  
  # Merge resolved + user-selected ambiguous
  temp_df.2$df_data <- rbind(rv$resolved_df, ambiguous_full)
  
  # Remove confirm button after processing
  output$choose_ids_ui <- renderUI({})
})

# Reset dataset_number if NA
observe({
  if(is.na(input$dataset_number)){
    updateNumericInput(session, "dataset_number", value = 312092)
  }
})

# Show reviewed taxonomy table
output$dataTaxonomy <- DT::renderDataTable({
  req(temp_df.2$df_data)
  temp_df.2$df_data
}, options = list(scrollX = TRUE, paging = FALSE), rownames = FALSE)

output$uiRevTaxonomy <- renderUI({
  req(temp_df.2$df_data)
  card(
    card_header("Reviewed taxonomy"),
    full_screen = TRUE, fill = FALSE,
    DT::dataTableOutput("dataTaxonomy")
  )
})

# Compare row counts
observe({
  req(temp_df$df_data)
  req(temp_df.2$df_data)
  
  if(nrow(temp_df$df_data) == nrow(temp_df.2$df_data)){
    showNotification("The initial and Final datasets HAVE the same number of rows.",
                     type = "message", duration = 5)
  } else {
    showNotification("The initial and Final datasets DO NOT HAVE the same number of rows.",
                     type = "warning", duration = NULL)
  }
})

# Download reviewed taxonomy
output$downloadData <- downloadHandler(
  filename = function() {
    paste0("CBB_Taxa_dataset_", Sys.Date(), ".csv")
  },
  content = function(file){
    write.csv(temp_df.2$df_data, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
