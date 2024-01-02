#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

# Load libraries
pacman::p_load(bslib, data.table, DT, jsonlite, leaflet, shiny, tidyverse, 
               shinycssloaders, rgbif, shinydashboard, sf, biomonitoR,
               vegan, openxlsx, shinydashboardPlus)

# red, shinyjs, shinyWidgets

#----------#
# Function ---------------------------------------------------------------------
#----------#
source("./function/cbbdbCol.R")
source("./function/ch0_to_Na.R")
source("./function/specifyTaxon.R")
source("./function/wormsTaxon.R")


#----------#
# Start UI ---------------------------------------------------------------------
#----------#


ui <- dashboardPage(
  # App title (long and short name)
  dashboardHeader(
    title = tagList(
      tags$span(class = "logo-mini", "CBB"), # Short
      tags$span(class = "logo-lg", "CBB App") # Long
    )
  ),
  
  
  # Sidebar menu
  dashboardSidebar(
    
    sidebarMenu(id = "tabs",  # Setting id makes input$tabs give the tabName of currently-selected tab
                
                # Home Menu
                menuItem("Home", tabName = "home", icon = icon("house")),
                
                # Taxonomy Menu 
                menuItem("Taxonomy", tabName = "taxonomy", icon = icon("bars-staggered")),
                
                # Maps Menu
                menuItem("Maps", tabName = "maps", icon = icon("globe")),
                
                # Ecology Menu
                menuItem("Ecology", tabName = "ecology", icon = icon("frog")),
                
                # Help Menu
                menuItem("Help", tabName = "help", icon = icon("info"))
    )
  ),
  
  # Main 
  dashboardBody(
    
    tabItems(
      # Home tab content
      tabItem(tabName = "home",
              fluidPage(
                # Side panel About ----
                sidebarPanel(source("./Ui/About_sidebarPanel.R")$value),
                # Main panel About ----
                mainPanel(source("./Ui/About_mainPanel.R")$value))
              ),
      
      # Taxonomy tab content
      tabItem(tabName = "taxonomy",
              fluidPage(
                # Side panel Taxonomy ----
                sidebarPanel(source("./Ui/Taxonomy_sidebarPanel.R")$value),
                # Main panel Taxonomy ----
                mainPanel(source("./Ui/Taxonomy_mainPanel.R")$value))
              ),
      
      # Maps tab content
      tabItem(tabName = "maps",
              fluidPage(
                ## sidebarLayout Maps ----
                sidebarPanel(source("./Ui/Maps_sidebarPanel.R")$value),
                ## mainPanel Maps ----
                mainPanel(source("./Ui/Maps_mainPanel.R")$value))
              ),
      
      # Ecology tab content
      tabItem(tabName = "ecology",
              fluidPage(
                ## sidebarLayout Maps ----
                sidebarPanel(source("./Ui/biomonitoR_sidebarPanel.R")$value),
                ## mainPanel Maps ----
                mainPanel(source("./Ui/biomonitoR_mainPanel.R")$value))
      )
    )
  )
)