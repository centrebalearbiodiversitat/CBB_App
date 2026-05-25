#-------------------------------#
# Shiny App CBB Taxonomy v. 0.1 #
#-------------------------------#

server <- function(input, output, session) {
  # Taxonomy -------------------------------------------------------------------
  source("./Server/01_Taxonomy_v2.R", local = TRUE)
}
