#--------------#
# Taxonomy v.2 #
#--------------#

# Objects to store files ----
temp_df <- reactiveValues(df_data = NULL) # <- Store .csv file input
temp_df.2 <- reactiveValues(df_data = NULL) # <- Store taxonomy data

# We can use this option to select the column name to perform the analysis
observe({
  # Don't run unless a data have been imported
  req(temp_df$df_data)
  colmn.names <- colnames(temp_df$df_data)
  
  updateSelectInput(session = session, "text.db", choices = colmn.names)
})

# Show table whit uploaded data (temp_df) in the card
output$inputDataframe <- DT::renderDataTable({

  req(input$file1)

  temp_df$df_data <-  fread(input$file1$datapath, sep = ",") %>%
    as.data.frame()

  temp_df$df_data

},
options = (list(scrollX = TRUE, paging = FALSE)), rownames= FALSE)


output$uiTaxonomy <- renderUI({
  req(input$file1)
  
  card(card_header("Taxonomy"),
    full_screen = TRUE, fill = FALSE,
    DT::dataTableOutput("inputDataframe")
  )
})



# Taxonomy check ----
observeEvent(input$taxa.run.button, {
  
  if(!is.null(temp_df$df_data)){
    if(input$text.db %in% colnames(temp_df$df_data)){
      
      spTaxa <- unique(stringr::str_trim(temp_df$df_data[ ,input$text.db], side = c("both")))
      
      # Select function to retrieve taxonomy in specify format (DB: COL)
      if(input$taxon.an == "Specify_COL") {
        temp_df.2$df_data <- specifyTaxon(spTaxa)
        temp_df.2$df_data <- temp_df.2$df_data$colNames
      }
      
      # Select function to retrieve taxonomy in CBB_DB format (DB: COL)
      if(input$taxon.an == "CBB_DB_COL") {
        
        dataset_number <- ifelse(input$dataset_number == "" | is.null(input$dataset_number),
                                 309796, 
                                 as.numeric(input$dataset_number))
        
        temp_df.2$df_data <- cbbdbCol(
          spTaxa,
          dataset_number = dataset_number
        )
      }
      
      # Select function to retrieve taxonomy in Specify format (DB: WORMS)
      if(input$taxon.an == "Specify_WORMS") {
        temp_df.2$df_data <- specifyWorms(spTaxa)
      }
      
      output$downloadButton <- renderUI({
        downloadButton("downloadData", "Download Dataset",
                       style = "padding:6px; font-size:80%")
      })
      
    } else{
      showNotification("No taxa column was found.")
    }
  } else {
    showNotification("No data was upload.")
  }
  
})

observe({
  if (is.na(input$dataset_number)) {
    updateNumericInput(session, "dataset_number", value = 309796)
  }
})

# Show table whit reviewed taxonomy information data (temp_df.2)
output$dataTaxonomy <- DT::renderDataTable({
  
  req(temp_df.2$df_data)
  
  temp_df.2$df_data
  
}, 
options = (list(scrollX = TRUE, paging = FALSE)), rownames= FALSE) 


output$uiRevTaxonomy <- renderUI({
  req(temp_df.2$df_data)
  
  card(
    card_header("Reviewed taxonomy"),
    full_screen = FALSE, fill = FALSE,
    DT::dataTableOutput("dataTaxonomy")
  )
})


# Message to check if the rows of initial and final dataset have the same number.
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


# Download temp_df.2 ----
output$downloadData <- downloadHandler(
  filename = function() {
    paste("CBB_Taxa_dataset_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(temp_df.2$df_data, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
