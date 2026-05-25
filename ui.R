#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

# Tutorial: https://rstudio.github.io/bslib/articles/dashboards/index.html

# Load libraries
pacman::p_load(
  bslib, data.table, DT, jsonlite, leaflet, shiny, tidyverse,
  rgbif, sf, biomonitoR, vegan, openxlsx, red, rredlist, plotly,
  terra
)

#----------#
# Function ---------------------------------------------------------------------
#----------#

source("./utils.R")

#------#
# Cards ------------------------------------------------------------------------
#------#

# Home
cards.taxonomy <- source("./Cards/card_home.R")$value
# cards.taxonomy[[1]]

# Taxonomy
cards.taxonomy <- source("./Cards/card_taxonomy.R")$value
# cards.taxonomy[[1]]

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

# Define UI
ui <- navbarPage(
  title = "BioDivers",
  theme = bs_theme(
    version = 5,
    bg = "#FFF",
    fg = "#101010",
    primary = "#00acba",
    secondary = "#7ebc00",
    success = "#be4358",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    font_scale = 0.8
  ),
  fluid = TRUE,
  collapsible = TRUE,
  header = tags$style(HTML("
        .navbar .nav-link:hover {
          background-color: #00acba !important;
          color: #ffffff !important;
          border-radius: 6px;
        }
    ")),

  # -----------------------
  # Tab 1: Landing
  # tabPanel(
  #   "Home",
  #   includeHTML("Cards/card_landing.html"),
  #   tags$script(src = "plugins/scripts.js"),
  #   tags$head(
  #     tags$link(
  #       rel = "stylesheet",
  #       type = "text/css",
  #       href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"
  #     ),
  #     tags$link(
  #       rel = "icon",
  #       type = "image/png",
  #       href = "images/logo_icon.png"
  #     )
  #   )
  # ),

  # -----------------------
  # Tab 1: Home
  tabPanel(
    "Home",
    layout_columns(
      fill = TRUE,
      col_widths = 12,
      cards.home[[1]]
    )
  ),

  # -----------------------
  # Tab 2: Taxonomy Overview (without table)
  tabPanel(
    "Taxonomy",
    layout_columns(
      fill = TRUE,
      col_widths = 12,
      cards.taxonomy[[1]]
    )
  ),

  # -----------------------
  # Footer / GitHub link
  footer = tags$div(
    style = "text-align:center; padding:10px;",
    tags$a(
      shiny::icon("github"),
      "GitHub",
      href = "https://github.com/centrebalearbiodiversitat/CBB_App",
      target = "_blank"
    )
  )
)
