#-----------------#
# biomonitoR v. 2 # 
#-----------------#

# Temporaty files --------------------------------------------------------------

temp_br_taxa <- reactiveValues(df_br_taxa = NULL) # <- Store .csv taxa biomonitoR
temp_br_as <- reactiveValues(df_br_as = NULL) # <- Results of as_biomonitor
temp_br_agr <- reactiveValues(df_br_agr = NULL) # <- Results of aggregate_taxa
temp_br_info <- reactiveValues(df_br_info = NULL) # <- General Info about the community
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
  
})
  
# Show table whit uploaded data set (temp_br_taxa)
output$inputDataframe_bioR <- DT::renderDT({
  req(temp_br_taxa$df_br_taxa)
  
  temp_br_taxa$df_br_taxa
  
},
options = (list(scrollX = TRUE, paging = FALSE)),
rownames = FALSE)
  
# Button to prepare data in biomonitoR format
observeEvent(input$div.run.button.biorPrep, {
  req(temp_br_taxa$df_br_taxa)
  
  # Check if there is a column named taxa into the csv
  if ("Taxa" %in% colnames(temp_br_taxa$df_br_taxa)) {

    # IF we want to load a Custom Reference Dataset (CRD)
    if (!is.null(input$fileBr_refdb$datapath)) {

      # Load CRD
      refdb.cust <- fread(input$fileBr_refdb$datapath)

      # Perform as_biomonitor and aggregate_taxa
      temp_br_as$df_br_as <- as_biomonitor(temp_br_taxa$df_br_taxa, dfref = refdb.cust)
      temp_br_agr$df_br_agr <- aggregate_taxa(temp_br_as$df_br_as)
      
    } else {

      # Perform as_biomonitor and aggregate_taxa
      temp_br_as$df_br_as <- as_biomonitor(temp_br_taxa$df_br_taxa, group = input$refdb.group)
      temp_br_agr$df_br_agr <- aggregate_taxa(temp_br_as$df_br_as)
      
    }

  } else {

    showNotification(paste("There is no column named 'Taxa' in your dataset"),
                     type = "error",
                     duration = 10)

    temp_br_taxa$df_br_taxa <- NULL
  }
  
})

observeEvent(input$div.run.button.biorPrep, {
  req(temp_br_taxa$df_br_taxa)
  
  temp_br_info$df_br_info <- general_info(temp_br_agr$df_br_agr) %>% 
    as.data.frame() %>% 
    t()   
  })


# Create card containing Input data and general info
output$uiBiorTable <- renderUI({
  req(temp_br_taxa$df_br_taxa)
  
  card(card_header("Input table"), 
       full_screen = FALSE, fill = FALSE, 
       DT::DTOutput("inputDataframe_bioR"))
  
})

# Create a table containing general information about the uploaded data set
output$generalInfo_bioR <- DT::renderDataTable({
  req(temp_br_info$df_br_info)
  
  temp_br_info$df_br_info
  
},
options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)),
rownames = FALSE)

output$commPlot1 <- renderPlotly({
  req(temp_br_info$df_br_info)
  
  plot(temp_br_as$df_br_as,
       parent = "Family",
       child = "Species",
       type = "abundance",
       remove_empty_child = FALSE)
})

output$commPlot2 <- renderPlotly({
  req(temp_br_info$df_br_info)
  
  plot(temp_br_agr$df_br_agr,
       parent = "Family",
       child = "Species",
       type = "abundance",
       remove_empty_child = FALSE)
})




output$uiBiorInfo <- renderUI({
  
  req(temp_br_info$df_br_info)
  
  layout_columns(col_widths = c(6, 6),
                 card(card_header("General info"),
                      full_screen = FALSE, fill = FALSE,
                      DT::dataTableOutput("generalInfo_bioR")
                      ),
                 
                 card(card_header("Community plot"), 
                      full_screen = FALSE, fill = FALSE, 
                      tabsetPanel(
                        tabPanel("Pie", plotlyOutput("commPlot1")),
                        tabPanel("Bar", plotlyOutput("commPlot2"))
                        )
                      )
                 )
})

# Index calculation ------------------------------------------------------------

# Button to perform diversity indices
observeEvent(input$div.run.button.bior, {
  req(temp_br_taxa$df_br_taxa)
  
  # Calculate all indices
  temp_br_index$df_br_index <- allindices(temp_br_agr$df_br_agr, 
                                          tax_lev = "Taxa", 
                                          base = exp(1))
  
  # Create card containing diversity indices
  output$uiBiorIndex <- renderUI({
    card(card_header("Diversity index"),
         full_screen = FALSE,
         DT::DTOutput("modify_index_bioR"))
  })
  
  output$dowButtonBiorIndex <- renderUI({
    downloadButton("dowDataBiorIndex", "Download Index",
                   style = "padding:6px; font-size:80%")
  })
})

# Create table output whit index information
output$modify_index_bioR <- DT::renderDataTable({
  req(temp_br_index$df_br_index)
  
  round(temp_br_index$df_br_index, digits = 2)
  
}, 
options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)),
rownames= TRUE)

# Community composition --------------------------------------------------------

# Button to analyze community composition
observeEvent(input$com.run.button.bior, {
  req(temp_br_taxa$df_br_taxa)
  
  # Calculate community composition
  temp_br_rich$df_br_rich <- allrich(temp_br_agr$df_br_agr)
  
  # Create card containing taxa richness
  output$uiBiorRich <- renderUI({
    card(card_header("Taxa richness"),
         full_screen = FALSE,
         DT::DTOutput("modify_rich_bioR"))
  })
  
  output$dowButtonBiorComp <- renderUI({
    downloadButton("dowDataBiorComp", "Download Comp.",
                   style = "padding:6px; font-size:80%")
  })
})

# Create table output whit community composition
output$modify_rich_bioR <- DT::renderDataTable({
  req(temp_br_rich$df_br_rich)
  
  round(temp_br_rich$df_br_rich, digits = 2)
  
}, 
options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)),
rownames= TRUE)

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
  df_br_vegan <- convert_to_vegan(temp_br_agr$df_br_agr, tax_lev = input$NMDSAggreg)
  
  # Calculate NMDS
  temp_br_nmds$df_br_nmds = metaMDS(df_br_vegan, 
                                    k = input$nmdsk,
                                    try = input$nmdsTry,
                                    trymax = input$nmdsTrymax) 
  
})

# Render plot NMDS
output$nmdsPlot <- renderPlotly({
  
  req(temp_br_nmds$df_br_nmds)
  
  # Object with NMDS results
  nmds <- temp_br_nmds$df_br_nmds
  
  # Species NMDS
  sp <- as.data.frame(nmds$species) %>% 
    mutate(across(c("MDS1", "MDS2"), round, 2))
  
  # Environment statements
  if(is.null(temp_br_nmdsEnv$df_br_nmdsEnv)){
    
    # Environment/Sites NMDS
    env <- as.data.frame(nmds$points) %>% 
      mutate(across(c("MDS1", "MDS2"), round, 2)) %>% 
      mutate(site = rownames(.))
    
    
    #if(input$nmdsPlotType == "General")
      # if(in.null(temp_br_nmdsEnv$df_br_nmdsEnv)){
      
      p1 <- ggplot() +
        geom_point(data = env, aes(x = MDS1, y = MDS2, color = site)) +
        geom_point(data = sp, size = 1, alpha = 0.8, color="darkorange", aes(x = MDS1, y = MDS2)) +
        geom_text(data = sp,
                  mapping = aes(label = rownames(sp), x = MDS1 * 1.1, y = MDS2 * 1.1), size = 2) +
        geom_hline(aes(yintercept = 0) , linetype = "dashed", linewidth = 0.6, colour = "blue") +
        geom_vline(aes(xintercept = 0) , linetype = "dashed", linewidth = 0.6, colour = "blue") +
        ggtitle(label = "NMDS result - General Plot - Stress: ", round(nmds$stress, digits = 2)) +
        theme_bw()
    # } 
    
  } else {
    
    # Merge NMDS points with env
    env <- as.data.frame(nmds$points) %>% 
      mutate(across(c("MDS1", "MDS2"), round, 2)) %>% 
      mutate(site = rownames(.))
    # env$Site <- rownames(env)
    env <- merge(env, temp_br_nmdsEnv$df_br_nmdsEnv, by = "site")
    
    # if(input$nmdsPlotType == "Ellipse") {
      
      # Check before if the column is not numeric
      
      p1 <- ggplot(data = env, aes(x = MDS1, y = MDS2, color = env[ ,input$nmdsGroup])) +
        geom_point() +
        stat_ellipse() +
        geom_hline(aes(yintercept = 0), linetype = "dashed", linewidth = 0.6, colour = "blue") +
        geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = 0.6, colour = "blue") +
        ggtitle(label = paste("NMDS result - Ellipse Plot - Stress: ", round(nmds$stress, digits = 2))) +
        labs(colour = input$nmdsGroup) +
        theme_bw()
      
    # } else {
    #   
    #   showNotification(markdown("### Error: Change NMDS plot type"), 
    #                    type = "error", duration = 15)
    # }
    
  }
  
  temp_br_nmdsSPdow$df_br_nmdsSPdow <- sp
  temp_br_nmdsENVdow$df_br_nmdsENVdow <- env
  
  output$dowButtonNMDS <- renderUI({
    downloadButton("dowDataNMDS", "Download NMDS data",
                   style = "padding:6px; font-size:80%")
  })
  
  
  return(ggplotly(p1))
  
})

# Create card containing NMDS plot
output$uiBiorNmds <- renderUI({
  req(temp_br_nmds$df_br_nmds)
  
  card(card_header("NMDS plot"),
       full_screen = FALSE, fill = FALSE,
       plotlyOutput("nmdsPlot"))
})

# Accumulation curves ----------------------------------------------------------

# Button to perform Accumulation curves
observeEvent(input$acc.run.button.bior, {
  req(temp_br_taxa$df_br_taxa)
  
  # Convert to vegan format
  df_br_vegan <- convert_to_vegan(temp_br_agr$df_br_agr, tax_lev = input$AccAggreg)
  
  # Calculate NMDS
  sp = specaccum(df_br_vegan, method = input$accMethod, permutations = 100,
                 conditioned =TRUE, gamma = "jack1",  w = NULL)
  
  temp_br_acc$df_br_acc <- data.frame(sites = sp$sites, 
                                      richness = sp$richness, 
                                      sd = sp$sd)
  
})

# Render plot Accumulation curves
output$accPlot <- renderPlotly({
  
  req(temp_br_acc$df_br_acc)
  
  sp1.df <- temp_br_acc$df_br_acc %>% 
    mutate(across(c("richness", "sd"), round, 2))
  
  p1 <- ggplot() +
    geom_point(data = sp1.df, aes(x = sites, y = richness), colour = "orange") +
    geom_line(data = sp1.df, aes(x = sites, y = richness), colour = "darkorange") +
    geom_ribbon(data = sp1.df ,aes(x = sites, ymin = (richness-2*sd),
                                   ymax = (richness+2*sd)),
                alpha = 0.2, fill = "orange") +
    ggtitle("Accumulation curve") + 
    theme_bw() +
    theme(axis.text.x=element_blank(), axis.ticks.x=element_blank(), axis.title.x=element_blank())
  
  return(ggplotly(p1))
  
})

# Create card containing Accumulation curves
output$uiBiorAcc <- renderUI({
  req(temp_br_acc$df_br_acc)
  
  card(card_header("Accumulation curves"),
       full_screen = FALSE, fill = FALSE,
       plotlyOutput("accPlot"))
})

# Rarefaction curves -----------------------------------------------------------

# Button to perform Rarefaction curves
observeEvent(input$rar.run.button.bior, {
  
  req(temp_br_taxa$df_br_taxa)
  
  # Convert to vegan format
  df_br_vegan <- convert_to_vegan(temp_br_agr$df_br_agr, tax_lev = input$RareAggreg)
  
  # Calculate rarefaction curves
  rarCurv <- rarecurve(df_br_vegan, step = 10, sample = 400, label = FALSE)
  
  
  # Prepare data for ggplot2
  names(rarCurv) <- paste("species", 1:length(rarCurv), sep = "")
  df_list <- mapply(function(data, species_name) {
    df.1 <- data.frame(data) %>% 
      rename(value = colnames(.)) %>% 
      mutate(species = species_name, subsample = attr(data, "Subsample")) %>% 
      mutate(across(c("value"), round, 2))
    return(df.1)
  }, rarCurv, as.list(names(rarCurv)), SIMPLIFY = FALSE)
  
  
  temp_br_rar$df_br_rar <- do.call(rbind, df_list)
  
})

# Render plot Rarefaction curves
output$rarPlot <- renderPlotly({
  
  req(temp_br_rar$df_br_rar)
  
  df <- temp_br_rar$df_br_rar
  
  p1 <- ggplot(df, aes(x = subsample, y = value, color = species)) +
    scale_color_discrete() +
    geom_line() + 
    ggtitle("Rarefaction curve") + 
    theme_bw() +
    theme(legend.position = "none")
  
  return(ggplotly(p1))
  
})

# Create card containing Rarefaction curves
output$uiBiorRar <- renderUI({
  req(temp_br_rar$df_br_rar)
  
  card(card_header("Rarefaction curves"),
       full_screen = FALSE, fill = FALSE,
       plotlyOutput("rarPlot"))
})


# Download buttons -------------------------------------------------------------

# Download Index
output$dowDataBiorIndex <- downloadHandler(
  filename = function() {
    paste("CBB_Div_Index_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(temp_br_index$df_br_index, file, row.names = TRUE, 
              fileEncoding = "UTF-8")
  }
)

# Download Composition
output$dowDataBiorComp <- downloadHandler(
  filename = function() {
    paste("CBB_Comp_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(temp_br_rich$df_br_rich, file, row.names = TRUE, 
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