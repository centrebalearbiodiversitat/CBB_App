#------#
# Maps #
#------#

# IUCN KEY: "adfd090eb40f601150b22d9676a3c228c1190bd7af733ac4e7172d683e31e1b4"
# coordinateCleaner tutorial: https://ropensci.github.io/CoordinateCleaner/articles/Cleaning_GBIF_data_with_CoordinateCleaner.html

# Habitat names
hb <- c("Freshwater/Terrestrial", "Marine")

# Temporary files --------------------------------------------------------------

# temp_habitatIn <- reactiveValues(habitatIn = NULL) # Occurrences inside the habitat. Needed to calculate AOO and EOO.
# temp_dat_ne <- reactiveValues(dat_ne = NULL) # Data of Balearic islands with habitat specification

temp_data <- list(habitatIn = reactiveValues(habitatIn = NULL), # Occurrences inside the habitat. Needed to calculate AOO and EOO.
                  # dat_ne = reactiveValues(dat_ne = NULL), # Data of Balearic islands with habitat specification
                  cordCleaner = reactiveValues(cordCleaner = NULL)) # Cooridnate Cleaner results

tempIUCN <- list(aoo = reactiveValues(aoo = NULL), # AOO results
                 eoo = reactiveValues(eoo = NULL), # EOO results
                 cat = reactiveValues(cat = NULL),
                 habitatIUCN = reactiveValues(habitatIUCN = NULL)) # Store IUCN category


# Connect to GBIF and create maps ----------------------------------------------

observe({
  
  # Don't run unless a data have been imported
  req(input$text.gbif)
  
  observeEvent(input$gbif.map.button, {
    if(input$habitat.gbif == "Marine"){
      
      # Polygon for the Balearic sea extension
      balearic <- "POLYGON((0.194 37.762, 5.217 37.762,5.217 41.058,0.194 41.058,0.194 37.762))"
      shp <- st_read("./shp/CBB_Balearic_Sea/CBBBalearicSea.shp")
      st_crs(shp) = 4326
      
    } else {
      
      # Polygon for the Balearic terrestrial and freshwater extension
      balearic <- "POLYGON((0.898 38,4.592 38,4.592 40.295,0.898 40.295,0.898 38))"
      shp <- st_read("./shp/Balearic_Islands/Balearic_4326.shp")
      st_crs(shp) = 4326
      
      r <- rast("./raster/elevation_Balearic.tif")
      
    }
    
    # Find GBIF name  
    # tax_key <- name_backbone(name = "Abralia veranyi", kingdom = "Animalia")
    # tax_key <- name_backbone(name = "Podarcis pityusensis", kingdom = "Animalia")
    tax_key <- name_backbone(name = input$text.gbif, kingdom = input$kingdom.gbif)
    # Key ID from GBIF for the accepted name
    key <- ifelse("acceptedUsageKey" %in% colnames(tax_key), tax_key$acceptedUsageKey, tax_key$usageKey)
    
    # List of occurrence with coordinates within the Balearic islands
    dat_ne <- occ_search(taxonKey = key, hasCoordinate = TRUE, 
                         geometry = balearic, limit = 99999, 
                         occurrenceStatus = "PRESENT") %>% 
      .$data %>% 
      as.data.frame()
    
    # Create main data frames
    if(nrow(dat_ne) == 0){
      
      showNotification(paste(input$text.gbif, "has no occurrence whithin the Balearic territory"), 
                       type = "error")
      
    } else{
      
      # Dataframe for coordinateCleaner
      cordCleaner <- dat_ne %>%
        select(species, decimalLongitude, 
               decimalLatitude, countryCode, individualCount,
               gbifID, family, taxonRank, coordinateUncertaintyInMeters,
               year, basisOfRecord, institutionCode, datasetName) %>% 
        filter(!is.na(decimalLongitude)) %>% # remove records without coordinates
        filter(!is.na(decimalLatitude)) # remove records without coordinates
      
      # Data frame with records inside the habitat type
      # dat_ne <- cordCleaner %>%
      #   select(decimalLatitude, decimalLongitude) %>%
      #   rename(longitude = decimalLongitude, latitude = decimalLatitude)
      
      #-----------------------------------#
      # Create dataframe with habitat type -------------------------------------
      #-----------------------------------#
      
      # Add ID column
      # dat_ne$idOcc <- rownames(dat_ne)
      cordCleaner$idOcc <- rownames(cordCleaner)
      
      # Transform the dataframe to spatial dataframe and assign crs
      cordCleaner.sf <- st_as_sf(cordCleaner, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326)
      
      # rm(cordCleaner.sf) ??
      
      # Polygon points intersection
      habitatIn <- st_intersection(cordCleaner.sf, shp) 
      
      habitatIn.df <- habitatIn %>% 
        as.data.frame() %>% 
        select(-c(geometry, Shape_Leng, Shape_Area, BEGINLIFES, CODNUT3, ENDLIFESPA, ID, NOM))
      
      # Save habitatIn ID
      habitatInId <- habitatIn$idOcc
      
      # Retrieve coordinate from habitatIn
      habitatIn.cord <- habitatIn %>% 
        st_coordinates() %>% 
        as.data.frame() %>% 
        rename(longitude = X, latitude = Y)
      
      habitatIn <- cbind(habitatIn.df, habitatIn.cord)
      
      # Attribute the habitat to each occurrence
      # cordCleaner$inOut <- ifelse(cordCleaner$idOcc %in% habitatIn$idOcc, hb[hb == "Freshwater/Terrestrial"], hb[hb != "Freshwater/Terrestrial"])
      cordCleaner$inOut <- ifelse(cordCleaner$idOcc %in% habitatIn$idOcc, hb[hb == input$habitat.gbif], hb[hb != input$habitat.gbif])
      
      # Save habitat inOut ID
      # dat_neId <- dat_ne$inOut
      
      rm(habitatIn.df, habitatInId, cordCleaner.sf, habitatIn.cord)
      
      
      #### WE NEED TO SAVE CORDCLEANER!!!!!!!!!!
      
      # Create dataframe only with coordinates
      # dat_ne <- dat_ne %>% 
      #   st_coordinates() %>% 
      #   as.data.frame() %>% 
      #   rename(longitude = X, latitude = Y)
      
      # Create complete file that will download object
      # dat_ne <- cbind(tax_key$scientificName, dat_ne, dat_neId)
      # colnames(dat_ne)[1] <- "scientificName"
      # temp_dat_ne$dat_ne <- cbind(tax_key$scientificName, dat_ne, dat_neId)
      # colnames(temp_dat_ne$dat_ne)[1] <- "scientificName"
      
      
      #-----------------------#
      # coordinateCleaner Flag ------------------------------------------------- 
      #-----------------------#
      
      # We perform the coordinate cleaning with all records
      
      #convert country code from ISO2c to ISO3c
      cordCleaner$countryCode <-  countrycode::countrycode(cordCleaner$countryCode, 
                                                           origin =  "iso2c",
                                                           destination = "iso3c")
      
      # TRUE = clean coordinate, FALSE = potentially problematic
      
      # value = "spatialvalid" return an object of class spatialvalid similar 
      # to x with one column added for each test. TRUE = clean coordinate entry, 
      # FALSE = potentially problematic coordinate entries. 
      # The .summary column is FALSE if any test flagged the respective coordinate.
      
      # Number of flagged records
      flags <- clean_coordinates(x = cordCleaner, 
                                 lon = "decimalLongitude", 
                                 lat = "decimalLatitude",
                                 countries = "countryCode",
                                 species = "species",
                                 value = "spatialvalid",
                                 tests = c("duplicates", "institutions",
                                           "urban", "outliers"))
      
      # sum(flags$.summary): Number of TRUE
      # sum(!flags$.summary): Number of FALSE
      
      negFlag <- round(sum(!flags$.summary)/length(flags$.summary), digits = 2) # Number of Negative flagged records
      
      
      temp_data$cordCleaner$cordCleaner <- flags
      
      
      # Plot map
      output$myMap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          setView(lng = 3.00, lat = 39.71, zoom = 7) %>%
          addPolygons(data = shp, fillColor = "orange", fillOpacity =  0.2, weight = 0.6,
                      color = "darkgray") %>% 
          addMarkers(data = habitatIn, clusterOptions = markerClusterOptions())
      })
      
      # Create object only with IN habitat, in order to calculate AOO and EOO
      temp_data$habitatIn$habitatIn <- habitatIn[ ,c("longitude", "latitude")]
      
      # Extract min, max, and mean elevation for the species elevation 
      elev <- extract(r, habitatIn[ ,c("longitude", "latitude")]) %>% 
        na.omit()
      elev <- data.frame(min = min(elev$wc2.1_30s_elev),
                         max = max(elev$wc2.1_30s_elev),
                         mean = mean(elev$wc2.1_30s_elev))
      
      
      # Show text whit statistics
      output$mapStatistics <- renderText({
        
        paste0("Taxonomy: ", tax_key$scientificName, "(Taxon Key:", key, ")", "\n\n",
               "Number of occurrence with coordinates: ", nrow(dat_ne), "\n",
               "Number of occurrences inside the selected habitat: ", nrow(habitatIn),
               " (", round(nrow(habitatIn)/nrow(dat_ne) * 100, digits = 2), "%)", "\n\n",
               "Min Elevation: ", elev$min, "\n",
               "Max Elevation: ", elev$max, "\n", 
               "Mean Elevation: ", round(elev$mean, digits = 2))
      })
    }
  })
  
  # Download button for data
  output$downloadButtonOCC <- renderUI({
    downloadButton("downloadDataOCC", "Download Dataset OCC",
                   style = "padding:6px; font-size:80%")
  }) 
})

# UI output map
output$uiMapStat <- renderUI({
  req(input$text.gbif)
  
  card(card_header("Map and Statistics"),
       full_screen = FALSE, fill = FALSE,
       leaflet::leafletOutput("myMap"),     
       verbatimTextOutput(outputId = "mapStatistics")              
  )
})

# AOO and EOO ------------------------------------------------------------------

# AOO calculation
observeEvent(input$aoo.button, {
  
  req(temp_data$habitatIn$habitatIn)
  
  if(input$IUCNKey == ""){
    
    showNotification(markdown("Please insert YOUR IUCN Key"), type = "warning")
    
  } else{
    
    tempIUCN$aoo$aoo <- aoo(temp_data$habitatIn$habitatIn)
    
    tempIUCN$eoo$eoo <- eoo(temp_data$habitatIn$habitatIn)
    tempIUCN$cat$cat <- rl_history(input$text.gbif, key = input$IUCNKey, region = "global")
    tempIUCN$habitatIUCN$habitatIUCN <- rl_habitats(input$text.gbif, key = input$IUCNKey, region = "global")
    
    # Show text whit statistics
    output$mapAOO <- renderText({
      
      paste0("AOO:", tempIUCN$aoo$aoo, "Km2", "\n",
             "EOO:", tempIUCN$eoo$eoo, "Km2")
      
    })
    
    output$iucnCat <- DT::renderDataTable({
      
      as.data.frame(tempIUCN$cat$cat$result)
      
    }, options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)), 
    rownames = FALSE)
    
    output$iucnHabitat <- DT::renderDataTable({
      
      as.data.frame(tempIUCN$habitatIUCN$habitatIUCN$result)
      
    }, options = (list(scrollX = TRUE, paging = FALSE, searching = FALSE)), 
    rownames = FALSE)
    
  }
  
})

output$uiAooEoo <- renderUI({
  
  req(tempIUCN$aoo$aoo)
  
  layout_column_wrap(style = htmltools::css(grid_template_columns = "1fr 2fr 2fr"),
                     card(card_header("AOO and EOO"), height = "70%",
                          full_screen = FALSE, fill = FALSE,
                          verbatimTextOutput(outputId = "mapAOO")
                     ),
                     card(card_header("IUCN category (global evaluation)"), height = "70%",
                          full_screen = FALSE, fill = FALSE,
                          DT::dataTableOutput(outputId = "iucnCat")
                     ),
                     card(card_header("IUCN habitat (global evaluation)"), height = "70%",
                          full_screen = FALSE, fill = FALSE,
                          DT::dataTableOutput(outputId = "iucnHabitat")
                     )
  )
})

# Download buttons -------------------------------------------------------------

# Download temp_df.2 ----
output$downloadDataOCC <- downloadHandler(
  filename = function() {
    paste("CBB_OCC_dataset_", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(temp_data$cordCleaner$cordCleaner, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
