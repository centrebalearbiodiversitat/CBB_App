#-----------#
# Card Maps #
#-----------#

cards.maps <- list(
  
  # Cards for taxonomy
  card(full_screen = FALSE,
       card_header("Maps"),
       
       # Sidebar ----
       layout_sidebar(
         open = TRUE, # Open sidebar
         fillable = TRUE,
         
         # Side bar
         sidebar = sidebar(width = 400, # sidebar width
               
               # Taxon scientific name
               textInput("text.gbif", markdown("#### Name of taxon"),
                         value = NULL),
               
           layout_columns(col_widths = c(12, 12),
                          # Select the kingdom of the taxon to optimize the search
                          selectInput("kingdom.gbif", markdown("##### Kingdom"), 
                                      choices = c("Animalia", "Plantae", "Fungi", "Protista", 
                                                  "Archaea", "Bacteria"),
                                      selected = "Animalia"),
                          
                          # Select the habitat of the taxon to remove occurrence outside the selected
                          # habitat
                          selectInput("habitat.gbif", markdown("##### Taxon Habitat"), 
                                      choices = c("Freshwater/Terrestrial" = "fw", "Marine" = "mar")),
                          
                          textInput(inputId = "IUCNKey", label = markdown("##### IUCN Key"), value = "")
                          
                          ),
           
           # Button to plot the map
           layout_columns(col_widths = c(6, 6, 4),
                          actionButton("gbif.map.button", markdown("Map and Stats"),
                                       style = "padding:6px; font-size:90%"),
                          
                          actionButton("aoo.button", markdown("IUCN Info"),
                                       style = "padding:6px; font-size:90%"),
                          uiOutput("downloadButtonOCC")           
                          )
         ),
         
         # Main content ----
         uiOutput("uiMapStat"),
         uiOutput("uiAooEoo")
         )
       )
  )

# cards.maps