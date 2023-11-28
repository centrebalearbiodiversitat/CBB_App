#----------#
# Taxonomy #
#----------#

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

# Show table whit uploaded data (temp_df)
output$inputDataframe <- DT::renderDataTable({
  
  req(input$file1)
  
  temp_df$df_data <-  fread(input$file1$datapath, sep = ",") %>% 
    as.data.frame()
  
  temp_df$df_data
  
}, 
options = (list(scrollX = TRUE, paging = FALSE, scrollY = "300px")),
rownames= FALSE, caption = HTML("<h3> Taxonomy </h3>"))


# Taxonomy check ----
observeEvent(input$taxa.run.button,{
  
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
        temp_df.2$df_data <- cbbdbCol(spTaxa)
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


# Show table whit reviewed taxonomy information data (temp_df.2)
output$dataTaxonomy <- DT::renderDataTable({
  
  req(temp_df.2$df_data)
  
  temp_df.2$df_data
  
}, 
options = (list(scrollX = TRUE, paging = FALSE, scrollY = "300px")),
rownames= FALSE, caption = HTML("<h3> Reviewed taxonomy </h3>"))

# Message to check if the rows of initial and final dataset have the same number.
observe({

  req(temp_df$df_data)
  req(temp_df.2$df_data)
  
  if(nrow(temp_df$df_data) == nrow(temp_df.2$df_data)){

    showNotification("The initial and Final datasets HAVE the same number of rows.",
                     type = "message", duration = NULL)

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
