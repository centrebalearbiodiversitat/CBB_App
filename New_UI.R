#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

# Load libraries
pacman::p_load(bslib, data.table, DT, jsonlite, leaflet, shiny, tidyverse, 
               rgbif, sf, biomonitoR, vegan, openxlsx, red, rredlist, plotly,
               terra)

# Tutorial: https://rstudio.github.io/bslib/articles/dashboards/index.html

#----------#
# Function ---------------------------------------------------------------------
#----------#
source("./function/cbbdbCol.R")
source("./function/ch0_to_Na.R")
source("./function/specifyTaxon.R")
source("./function/wormsTaxon.R")

#------#
# Cards ------------------------------------------------------------------------
#------#

# Home 
cards.home <- source("./Cards/card_home.R")$value
# cards.home[[5]]

# Taxonomy
cards.taxonomy <- source("./Cards/card_taxonomy.R")$value
# cards.taxonomy[[1]]

# Maps
cards.maps <- source("./Cards/card_maps.R")$value
# cards.maps[[1]]

# Ecology
cards.ecology <- source("./Cards/card_ecology.R")$value
# cards.ecology[[1]]


#------# 
# Links ------------------------------------------------------------------------
#------#

link_git <- tags$a(
  shiny::icon("github"), "GitHub",
  href = "https://github.com/centrebalearbiodiversitat",
  target = "_blank"
)

#---#
# UI ---------------------------------------------------------------------------
#---#

ui <- page_navbar(
  
  theme = bs_theme(version = 5, 
                   bg = "#FFF",
                   fg = "#101010",
                   primary = "#ef8957",
                   secondary = "#34bbcc",
                   success = "#8dc47b",
                   base_font = font_google("Inter"),
                   code_font = font_google("JetBrains Mono"),
                   font_scale = 0.8), # Bootstrap version and theme: bootswatch = "superhero"
  
  title = "Cbb App", # title of the app
  
  # Home panel ----
  nav_panel(title = "Home",
            cards.home[[1]]
            ),
            
  # Taxonomy panel ----
  nav_panel(title = "Taxonomy",
            layout_columns(
              fill = TRUE, 
              col_widths = 12,
              cards.taxonomy[[1]]
              )
            ),
  
  # Maps panel ----
  nav_panel(title = "Map",
            layout_columns(
              fill = FALSE, 
              col_widths = 12,
              cards.maps[[1]]
              )
            ),
  
  # Ecology panel ----
  nav_panel(title = "Ecology",
            layout_columns(
              fill = TRUE, 
              col_widths = 12,
              cards.ecology[[1]]
              )
            ),
  
  # Help panel
  nav_panel(title = "Help"),
  
  # GitHub link
  nav_spacer(),
  nav_item(link_git)
  
)

server <- function(input, output, session) {
  # Taxonomy -------------------------------------------------------------------
  source("./Server/01_Taxonomy_v2.R", local = T)
  
  # Maps -----------------------------------------------------------------------
  source("./Server/02_Maps_v2.R", local = T)
  
  # biomonitoR -----------------------------------------------------------------
  source("./Server/03_biomonitoR_v2.R", local = T)
}

shinyApp(ui, server)

