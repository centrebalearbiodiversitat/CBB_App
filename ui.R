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

# # Load libraries
# pacman::p_load(bslib, data.table, DT, jsonlite, leaflet, shiny, tidyverse, 
#                shinycssloaders, rgbif, shinydashboard, sf, biomonitoR,
#                vegan, openxlsx, shinydashboardPlus)
# 
# # red, shinyjs, shinyWidgets
# 
# #----------#
# # Function ---------------------------------------------------------------------
# #----------#
# source("./function/cbbdbCol.R")
# source("./function/ch0_to_Na.R")
# source("./function/specifyTaxon.R")
# source("./function/wormsTaxon.R")
# 
# 
# #----------#
# # Start UI ---------------------------------------------------------------------
# #----------#
# 
# 
# ui <- dashboardPage(
#   # App title (long and short name)
#   dashboardHeader(
#     title = tagList(
#       tags$span(class = "logo-mini", "CBB"), # Short
#       tags$span(class = "logo-lg", "CBB App") # Long
#     )
#   ),
#   
#   
#   # Sidebar menu
#   dashboardSidebar(
#     
#     sidebarMenu(id = "tabs",  # Setting id makes input$tabs give the tabName of currently-selected tab
#                 
#                 # Home Menu
#                 menuItem("Home", tabName = "home", icon = icon("house")),
#                 
#                 # Taxonomy Menu 
#                 menuItem("Taxonomy", tabName = "taxonomy", icon = icon("bars-staggered")),
#                 
#                 # Maps Menu
#                 menuItem("Maps", tabName = "maps", icon = icon("globe")),
#                 
#                 # Ecology Menu
#                 menuItem("Ecology", tabName = "ecology", icon = icon("frog")),
#                 
#                 # Help Menu
#                 menuItem("Help", tabName = "help", icon = icon("info"))
#     )
#   ),
#   
#   # Main 
#   dashboardBody(
#     
#     tabItems(
#       # Home tab content
#       tabItem(tabName = "home",
#               fluidPage(
#                 # Side panel About ----
#                 sidebarPanel(source("./Ui/About_sidebarPanel.R")$value),
#                 # Main panel About ----
#                 mainPanel(source("./Ui/About_mainPanel.R")$value))
#               ),
#       
#       # Taxonomy tab content
#       tabItem(tabName = "taxonomy",
#               fluidPage(
#                 # Side panel Taxonomy ----
#                 sidebarPanel(source("./Ui/Taxonomy_sidebarPanel.R")$value),
#                 # Main panel Taxonomy ----
#                 mainPanel(source("./Ui/Taxonomy_mainPanel.R")$value))
#               ),
#       
#       # Maps tab content
#       tabItem(tabName = "maps",
#               fluidPage(
#                 ## sidebarLayout Maps ----
#                 sidebarPanel(source("./Ui/Maps_sidebarPanel.R")$value),
#                 ## mainPanel Maps ----
#                 mainPanel(source("./Ui/Maps_mainPanel.R")$value))
#               ),
#       
#       # Ecology tab content
#       tabItem(tabName = "ecology",
#               fluidPage(
#                 ## sidebarLayout Maps ----
#                 sidebarPanel(source("./Ui/biomonitoR_sidebarPanel.R")$value),
#                 ## mainPanel Maps ----
#                 mainPanel(source("./Ui/biomonitoR_mainPanel.R")$value))
#       )
#     )
#   )
# )