#------#
# Maps #
#------#

hb <- c("Freshwater/Terrestrial", "Marine")

# Temporary files --------------------------------------------------------------

temp_habitatIn <- reactiveValues(habitatIn = NULL) # <- Store AOO output
temp_dat_ne <- reactiveValues(dat_ne = NULL) # <- Store EOO output


# Connect to GBIF and create maps ----------------------------------------------

observe({
  
  # Don't run unless a data have been imported
  req(input$text.gbif)
  
  observeEvent(input$gbif.map.button, {
  if(input$habitat.gbif == "mar"){
    
    # Polygon for balearic sea extension
    balearic <- "POLYGON((0.194 37.762, 5.217 37.762,5.217 41.058,0.194 41.058,0.194 37.762))"
    shp <- st_read("./shp/CBB_Balearic_Sea/CBBBalearicSea.shp")
    st_crs(shp) = 4326
    
  } else {

    # Polygon for balearic terrestrial and freshater extension
    balearic <- "POLYGON((0.898 38,4.592 38,4.592 40.295,0.898 40.295,0.898 38))"
    shp <- st_read("./shp/Balearic_Islands/Balearic_4326.shp")
    st_crs(shp) = 4326
    
  }
  
  # Find GBIF name  
  # tax_key <- name_backbone(name = "Podarcis pityusensis", kingdom = "Animalia")
  tax_key <- name_backbone(name = input$text.gbif, kingdom = input$kingdom.gbif)
  # Key ID from GBIF for the accepted name
  key <- ifelse("acceptedUsageKey" %in% colnames(tax_key), tax_key$acceptedUsageKey, tax_key$usageKey)
  
  # List of occurrence within the Balearic islands
  dat_ne <- occ_search(taxonKey = key, hasCoordinate = T, 
                       geometry = balearic, limit = 99999) %>% 
    .$data %>% 
    as.data.frame() %>% 
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
  dat_ne$inOut <- ifelse(dat_ne$idOcc %in% habitatIn$habitatInId, hb[hb == input$habitat.gbif], hb[hb != input$habitat.gbif])
  
  dat_neId <- dat_ne$inOut
  
  # Create dataframe only with coordinates
  dat_ne <- dat_ne %>% 
    st_coordinates() %>% 
    as.data.frame() %>% 
    rename(longitude = X, latitude = Y)
  
  # Create complete file that will download object
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
    
    # Show text whit statistics
    output$mapStatistics <- renderText({
      
      paste("Number of occurrence with coordinates:", nrow(dat_ne), "\n",
            "Number of occurrences inside de habitat:", nrow(habitatIn),
            "-", round(nrow(habitatIn)/nrow(dat_ne) * 100, digits = 2), "%")
      
    })
    
    # Download button for data
    output$downloadButtonOCC <- renderUI({
      downloadButton("downloadDataOCC", "Download Dataset OCC",
                     style = "padding:6px; font-size:80%")
    }) 
    
    
    # Create object only with IN habitat, in order to calculate AOO and EOO
    temp_habitatIn$habitatIn <- habitatIn[ ,1:2]
    
    
    })
  
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

# AOO calculation
observeEvent(input$aoo.button, {
  
  req(temp_habitatIn$habitatIn)
  
  habitatIn.aoo <- aoo(temp_habitatIn$habitatIn)
  
  # Show text whit statistics
  output$mapAOO <- renderText({
    
    paste("AOO:", habitatIn.aoo)
  
  })
  
})

# EOO calculation
observeEvent(input$eoo.button, {
  
  req(temp_habitatIn$habitatIn)
  
  habitatIn.eoo <- eoo(temp_habitatIn$habitatIn)
  
  # Show text whit statistics
  output$mapEOO <- renderText({

  HTML(paste("EOO:", habitatIn.eoo))
    
    #paste("EOO", temp_eoo$eoo)
  })
  
})

