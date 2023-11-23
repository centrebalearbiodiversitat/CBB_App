#------------#
# biomonitoR # 
#------------#

# Temporaty files --------------------------------------------------------------

temp_br_taxa <- reactiveValues(df_br_taxa = NULL) # <- Store .csv taxa biomonitoR
temp_br_agr <- reactiveValues(df_br_agr = NULL) # <- Store .csv reference DB biomonitoR
temp_br_index <- reactiveValues(df_br_index = NULL) # <- Store .csv reference DB biomonitoR
temp_br_rich <- reactiveValues(df_br_rich = NULL) # <- Store .csv reference DB biomonitoR
temp_br_nmds <- reactiveValues(df_br_nmds = NULL) # <- Store .csv reference DB biomonitoR
temp_br_nmdsEnv <- reactiveValues(df_br_nmdsEnv = NULL) # <- Store .csv reference DB biomonitoR
temp_br_acc <- reactiveValues(df_br_acc = NULL) # <- Store Accumulation curves results
temp_br_rar <- reactiveValues(df_br_rar = NULL) # <- Store Rarefaction curves results


temp_br_nmdsSPdow <- reactiveValues(df_br_nmdsSPdow = NULL) # <- NMDS Species to download
temp_br_nmdsENVdow <- reactiveValues(df_br_nmdsENVdow = NULL) # <- NMDS Environment to download


# Load file and transformation it to biomonitoR format -------------------------

# Read .csv and create biomonitoR
observeEvent(input$fileBr_taxa, {
  req(input$fileBr_taxa)

  # Load main .csv
  temp_br_taxa$df_br_taxa <- fread(input$fileBr_taxa$datapath)
  
  
  # Check if there is a column named taxa into the csv
  if ("Taxa" %in% colnames(temp_br_taxa$df_br_taxa)) {
    
    # IF we want to load a Custom Reference Dataset (CRD)
    if (!is.null(input$fileBr_refdb$datapath)) {
      
      # Load CRD
      refdb.cust <- fread(input$fileBr_refdb$datapath)
      
      # Perform as_biomonitor and aggregate_taxa
      data_bio <- as_biomonitor(temp_br_taxa$df_br_taxa, dfref = refdb.cust)
      temp_br_agr$df_br_agr <- aggregate_taxa(data_bio)
      
    } else {
      
      # Perform as_biomonitor and aggregate_taxa
      data_bio <- as_biomonitor(temp_br_taxa$df_br_taxa, group = input$refdb.group)
      temp_br_agr$df_br_agr <- aggregate_taxa(data_bio) 
      
    }
    
  } else {
    
    showNotification(paste("There is no column named 'Taxa' in your dataset"),
                     type = "error",
                     duration = 10)
    
    temp_br_taxa$df_br_taxa <- NULL
  }
    
})

# Show table whit uploaded data set (temp_br_taxa)
output$inputDataframe_bioR <- DT::renderDataTable({
  req(temp_br_taxa$df_br_taxa)
  
  temp_br_taxa$df_br_taxa

},
options = (list(scrollX = TRUE, paging = FALSE, scrollY = "300px")),
rownames = FALSE, caption = HTML("<h3> Upload data set </h3>"))


# Create a table containing general information about the uploaded data set
output$generalInfo_bioR <- DT::renderDataTable({
  req(temp_br_taxa$df_br_taxa)
  
    general_info(temp_br_agr$df_br_agr) %>% 
      as.data.frame() %>% 
      t()  
},
options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)),
rownames = FALSE, caption = HTML("<h3> Data set main info </h3>"))


# Index calculation ------------------------------------------------------------

# Button to perform diversity indices
observeEvent(input$div.run.button.bior, {
  req(temp_br_taxa$df_br_taxa)

  # Calculate all indices
  temp_br_index$df_br_index <- allindices(temp_br_agr$df_br_agr, 
                                          tax_lev = "Taxa", 
                                          base = exp(1))
  
  output$dowButtonBiorIndex <- renderUI({
    downloadButton("dowDataBiorIndex", "Download Index",
                   style = "padding:6px; font-size:80%")
  })
  
  })

# Show table whit index information
output$modify_index_bioR <- DT::renderDataTable({
  req(temp_br_index$df_br_index)
  
  round(temp_br_index$df_br_index, digits = 2)
  
}, 
options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)),
rownames= TRUE, caption = HTML("<h3> Diversity indices </h3>"))

# Community composition --------------------------------------------------------

# Button to analyze community composition
observeEvent(input$com.run.button.bior, {
  req(temp_br_taxa$df_br_taxa)

  # Calculate community composition
  temp_br_rich$df_br_rich <- allrich(temp_br_agr$df_br_agr)
  
  output$dowButtonBiorComp <- renderUI({
    downloadButton("dowDataBiorComp", "Download Comp.",
                   style = "padding:6px; font-size:80%")
  })
  
})

# Show table whit community composition
output$modify_rich_bioR <- DT::renderDataTable({
  req(temp_br_rich$df_br_rich)
  
  round(temp_br_rich$df_br_rich, digits = 2)

  }, 
options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)),
rownames= TRUE, caption = HTML("<h3> Richness </h3>"))

# NMDS -------------------------------------------------------------------------

# We can use this option to select the column name for grouping NMDS
observe({
  
  # Required NMDS environmental file
  req(input$envNMDS)
  
  temp_br_nmdsEnv$df_br_nmdsEnv <- fread(input$envNMDS$datapath)
  
  # Create an object to contain column name to show into the text box
  colmn.names.nmds <- colnames(temp_br_nmdsEnv$df_br_nmdsEnv)

  updateSelectInput(session = session, "nmdsGroup", choices = colmn.names.nmds)
})

# Button to perform NMDS
observeEvent(input$nmds.run.button.bior, {
  req(temp_br_taxa$df_br_taxa)
  
  # Convert to vegan format
  df_br_vegan <- convert_to_vegan(temp_br_agr$df_br_agr)
  
  # Calculate NMDS
  temp_br_nmds$df_br_nmds = metaMDS(df_br_vegan, 
                                    k = input$nmdsk,
                                    try = input$nmdsTry,
                                    trymax = input$nmdsTrymax) 
  
})

# Render plot NMDS
output$nmdsPlot <- renderPlot({
  req(temp_br_nmds$df_br_nmds)
  
  # Object with NMDS results
  nmds <- temp_br_nmds$df_br_nmds
  
  # Species NMDS
  sp <- as.data.frame(nmds$species)

  # Environment statements
  if(is.null(temp_br_nmdsEnv$df_br_nmdsEnv)){
    
    # Environment/Sites NMDS
    env <- as.data.frame(nmds$points)
    
    if(input$nmdsPlotType == "General") {
      
      p1 <- ggplot() +
        geom_point(data = env, aes(x = MDS1, y = MDS2)) +
        geom_segment(data = sp,
                     mapping = aes(x = 0, y = 0, xend = MDS1, yend = MDS2),
                     arrow = arrow(length = unit(0.015, "npc"), type = "closed"),
                     colour = "darkgray",
                     linewidth = 0.4) +
        geom_text(data = sp,
                  mapping = aes(label = rownames(sp), x = MDS1 * 1.1, y = MDS2 * 1.1), size = 2) +
        geom_hline(aes(yintercept = 0) , linetype = "dashed", linewidth = 0.6, colour = "blue") +
        geom_vline(aes(xintercept = 0) , linetype = "dashed", linewidth = 0.6, colour = "blue") +
        ggtitle(label = "NMDS result - General Plot",
                subtitle = paste("Stress NMDS:", round(temp_br_nmds$df_br_nmds$stress, digits = 2))) +
        theme_bw()
      
    } 
    
  } else {

    # Merge NMDS points with env
    env <- as.data.frame(nmds$points) 
    env$Site <- rownames(env)
    env <- merge(env, temp_br_nmdsEnv$df_br_nmdsEnv, by = "Site")
   
    if(input$nmdsPlotType == "Ellipse") {
      
      # Check before if the column is not numeric
      
      p1 <- ggplot(data = env, aes(x = MDS1, y = MDS2, color = env[ ,input$nmdsGroup])) +
        geom_point() +
        stat_ellipse() +
        geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 0.6, colour = "blue") +
        geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.6, colour = "blue") +
        ggtitle(label = "NMDS result - Ellipse Plot",
                subtitle = paste("Stress NMDS:", round(temp_br_nmds$df_br_nmds$stress, digits = 2))) +
        guides(fill = guide_legend(title = paste(input$nmdsGroup))) +
        theme_bw()
      
    } else {
      
      showNotification(HTML("<h3>Error: Change NMDS plot type</h3>"), 
                       type = "error",
                       duration = 15)
    }
    
  }
  
  temp_br_nmdsSPdow$df_br_nmdsSPdow <- sp
  temp_br_nmdsENVdow$df_br_nmdsENVdow <- env
  
  output$dowButtonNMDS <- renderUI({
    downloadButton("dowDataNMDS", "Download NMDS data",
                   style = "padding:6px; font-size:80%")
  })
  
  
  return(p1)
  
  })


# Accumulation curves ----------------------------------------------------------

# Button to perform Accumulation curves
observeEvent(input$acc.run.button.bior, {
  req(temp_br_taxa$df_br_taxa)
  
  # Convert to vegan format
  df_br_vegan <- convert_to_vegan(temp_br_agr$df_br_agr)
  
  # Calculate NMDS
  sp = specaccum(df_br_vegan)
  
  temp_br_acc$df_br_acc <- data.frame(sites = sp$sites, 
                                      richness = sp$richness, 
                                      sd = sp$sd)
  
})

# Render plot Accumulation curves
output$accPlot <- renderPlot({
  
  req(temp_br_acc$df_br_acc)
  
  sp1.df <- temp_br_acc$df_br_acc
  
  p1 <- ggplot() +
    geom_point(data = sp1.df, aes(x = sites, y = richness), colour = "orange") +
    geom_line(data = sp1.df, aes(x = sites, y = richness), colour = "darkorange") +
    geom_ribbon(data = sp1.df ,aes(x = sites, ymin = (richness-2*sd),
                                   ymax = (richness+2*sd)),
                alpha = 0.2, fill = "orange") +
    ggtitle("Accumulation curve") + 
    theme_bw()
  
  return(p1)
  
})


# Rarefaction curves -----------------------------------------------------------

# Button to perform Rarefaction curves
observeEvent(input$rar.run.button.bior, {
  
  req(temp_br_taxa$df_br_taxa)
  
  # Convert to vegan format
  df_br_vegan <- convert_to_vegan(temp_br_agr$df_br_agr)
  
  # Calculate rarefaction curves
  rarCurv <- rarecurve(df_br_vegan, step = 10, sample = 400, label = FALSE)
  
  
  # Prepare data for ggplot2
  names(rarCurv) <- paste("species", 1:length(rarCurv), sep = "")
  df_list <- mapply(function(data, species_name) {
    df.1 <- data.frame(data) %>% 
      rename(value = colnames(.)) %>% 
      mutate(species = species_name, subsample = attr(data, "Subsample")) 
    return(df.1)
  }, rarCurv, as.list(names(rarCurv)), SIMPLIFY = FALSE)
  
  
  temp_br_rar$df_br_rar <- do.call(rbind, df_list)
  
})

# Render plot Rarefaction cuurves
output$rarPlot <- renderPlot({
  
  req(temp_br_rar$df_br_rar)
  
  df <- temp_br_rar$df_br_rar
  
  p1 <- ggplot(df, aes(x = subsample, y = value, color = species)) +
    scale_color_discrete() +
    geom_line() + 
    ggtitle("Rarefaction curve") + 
    theme_bw() +
    theme(legend.position = "none")
  
    return(p1)
  
})


# Download buttons -------------------------------------------------------------

# Download Index
output$dowDataBiorIndex <- downloadHandler(
  filename = function() {
    paste("CBB_Div_Index_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(temp_br_index$df_br_index, file, row.names = FALSE, 
              fileEncoding = "UTF-8")
  }
)

# Download Composition
output$dowDataBiorComp <- downloadHandler(
  filename = function() {
    paste("CBB_Comp_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(temp_br_rich$df_br_rich, file, row.names = FALSE, 
              fileEncoding = "UTF-8")
  }
)

# Download NMDS Data
output$dowDataNMDS <- downloadHandler(
  filename = function() {
    paste0("CBB_NMDS_", Sys.Date(), ".xlsx", sep = "")
  },
  content = function(file) {

    wb <- createWorkbook()
    
    addWorksheet(wb, sheetName = "species")
    addWorksheet(wb, sheetName = "site")
    
    writeData(wb, sheet = "species", x = temp_br_nmdsSPdow$df_br_nmdsSPdow)
    writeData(wb, sheet = "site", x = temp_br_nmdsENVdow$df_br_nmdsENVdow)
    
    saveWorkbook(wb, file, overwrite = FALSE) ## save to working directory
  }
)


