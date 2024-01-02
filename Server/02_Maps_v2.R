#------#
# Maps #
#------#

# IUCN KEY: "adfd090eb40f601150b22d9676a3c228c1190bd7af733ac4e7172d683e31e1b4"


# Habitat names
hb <- c("Freshwater/Terrestrial", "Marine")

# Temporary files --------------------------------------------------------------

temp_habitatIn <- reactiveValues(habitatIn = NULL) # Occurrences inside the habitat. Needed to calculate AOO and EOO.
temp_dat_ne <- reactiveValues(dat_ne = NULL) #

# temp_aoo <- reactiveValues(aoo = NULL) # Store AOO value
# temp_eoo <- reactiveValues(eoo = NULL) # Store EOO value
# temp_iucnCat <- reactiveValues(cat = NULL) # Store IUCN category

tempIUCN <- list(aoo = reactiveValues(aoo = NULL),
                 eoo = reactiveValues(eoo = NULL),
                 cat = reactiveValues(cat = NULL),
                 habitatIUCN = reactiveValues(habitatIUCN = NULL)) # Store IUCN category


# Connect to GBIF and create maps ----------------------------------------------

observe({
  
  # Don't run unless a data have been imported
  req(input$text.gbif)
  
  observeEvent(input$gbif.map.button, {
  if(input$habitat.gbif == "mar"){
    
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
  dat_ne <- occ_search(taxonKey = key, hasCoordinate = T, 
                       geometry = balearic, limit = 99999, occurrenceStatus = "PRESENT") %>% 
    .$data %>% 
    as.data.frame()  # %>% 
    # select(decimalLatitude, decimalLongitude) %>% 
    # rename(longitude = decimalLongitude, latitude = decimalLatitude)
  
  if(nrow(dat_ne) == 0){
    
    showNotification(paste(input$text.gbif, "has no occurrence whithin the Balearic territory"), 
                     type = "error")
    
  } else{
    
    dat_ne <- dat_ne %>% 
      select(decimalLatitude, decimalLongitude) %>% 
      rename(longitude = decimalLongitude, latitude = decimalLatitude)
    
    # Add ID column
    dat_ne$idOcc <- rownames(dat_ne) 
    
    # Transform the dataframe to spatial dataframe and assign crs
    dat_ne <- st_as_sf(dat_ne, coords = c("longitude", "latitude"), crs = 4326)
    
    # Polygon points intersection
    habitatIn <- st_intersection(dat_ne, shp)
    
    habitatInId <- habitatIn$idOcc
    
    habitatIn <- habitatIn %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      rename(longitude = X, latitude = Y)
    
    habitatIn <- cbind(habitatIn, habitatInId)
    
    # Attribute the habitat to each occurrence
    # dat_ne$inOut <- ifelse(dat_ne$idOcc %in% habitatIn$habitatInId, hb[hb == "Freshwater/Terrestrial"], hb[hb != "Marine"])
    dat_ne$inOut <- ifelse(dat_ne$idOcc %in% habitatIn$habitatInId, hb[hb == input$habitat.gbif], hb[hb != input$habitat.gbif])
    
    dat_neId <- dat_ne$inOut
    
    # Create dataframe only with coordinates
    dat_ne <- dat_ne %>% 
      st_coordinates() %>% 
      as.data.frame() %>% 
      rename(longitude = X, latitude = Y)
    
    # Create complete file that will download object
    # dat_ne <- cbind(tax_key$scientificName, dat_ne, dat_neId)
    temp_dat_ne$dat_ne <- cbind(tax_key$scientificName, dat_ne, dat_neId)
    
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
    temp_habitatIn$habitatIn <- habitatIn[ ,1:2]
    
    
    # Extract min, max, and mean elevation for the species elevation 
    elev <- extract(r, habitatIn[ ,1:2]) %>% 
      na.omit()
    elev <- data.frame(min = min(elev$wc2.1_30s_elev),
                       max = max(elev$wc2.1_30s_elev),
                       mean = mean(elev$wc2.1_30s_elev))
    
    
    # Show text whit statistics
    output$mapStatistics <- renderText({
      
      paste0("Taxonomy: ", tax_key$scientificName, "\n\n",
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
  
  req(temp_habitatIn$habitatIn)
  
  if(input$IUCNKey == ""){
    
    showNotification(markdown("Please insert YOUR IUCN Key"), type = "warning")
    
  } else{
    
    tempIUCN$aoo$aoo <- aoo(temp_habitatIn$habitatIn)
    
    tempIUCN$eoo$eoo <- eoo(temp_habitatIn$habitatIn)
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
    write.csv(temp_dat_ne$dat_ne, file, row.names = FALSE, fileEncoding = "UTF-8")
  }
)
