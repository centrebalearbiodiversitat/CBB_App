#-------------------#
# Maps sidebarPanel #
#-------------------#


fluidRow(
  HTML("<h3> Maps </h3>
<br> This panel includes .... <br>"),

hr(),

# Input: Select a .csv file
div(style="display: inline-block;vertical-align:top; width: 400px;",   
    
    # Taxon scientific name
    textInput("text.gbif", h5("Name of taxon"),
              value = NULL),
    
    # Select the kingdom of the taxon to optimize the search
    selectInput("kingdom.gbif", h5("Choose the taxon Kingdom"), 
                choices = c("Animalia", "Plantae", "Fungi", "Protista", 
                            "Archaea", "Bacteria"),
                selected = "Animalia"),
    
    # Select the habitat of the taxon to remove occurrence outside the selected
    # habitat
    selectInput("habitat.gbif", h5("Choose the taxon Habitat"), 
                choices = c("Freshwater/Terrestrial" = "fw", "Marine" = "mar"))
    ),

hr(),

# Button to plot the map
div(style="display: inline-block;horizontal-align:bottom; width: 400px;",
    actionButton("gbif.map.button", HTML("<b>Plot Map</b>")),
    actionButton("aoo.button", HTML("<b>Run AOO</b>")),
    actionButton("eoo.button", HTML("<b>Run EOO</b>")),
    hr(),
    uiOutput("downloadButtonOCC")
    )

)